package vexriscv.plugin.riscvdebug


import vexriscv._
import spinal.core._
import spinal.lib._
import vexriscv.Riscv.CSR
import vexriscv.plugin.{CsrInterface, CsrMapping, IContextSwitching, IWake, Plugin}

import scala.collection.mutable.ArrayBuffer


case class CoreDebugModuleIo() extends Bundle with IMasterSlave {

  val injectionPort = Stream(Bits(32 bit))
  val resetRequest = Bool()
  val haltRequest = Bool()
  val resumeRequest = Bool()

  val rsp = Flow(Bits(32 bit))
  val resetAck = Bool()
  val running = Bool()
  val halted = Bool()
  val resumeAck = Bool()
  val resetOut = Bool()


  override def asMaster(): Unit = {
    master(injectionPort)
    out(resetRequest)
    out(haltRequest)
    out(resumeRequest)

    in(rsp)
    in(resetAck)
    in(running)
    in(halted)
    in(resumeAck)
    in(resetOut)
  }
}


class CoreDebugModulePlugin(var debugClockDomain: ClockDomain) extends Plugin[VexRiscv] {

  var io: CoreDebugModuleIo = null

  val injectionAsks = ArrayBuffer[(Stage, Bool)]()
  var injectionPort: Stream[Bits] = null


  var jumpInterface: Flow[UInt] = null
  var privilegeService: PrivilegeService = null
  var step: Bool = null
  var ebreakm: Bool = null


  object IS_EBREAK extends Stageable(Bool)
  object DO_EBREAK extends Stageable(Bool)
  override def setup(pipeline: VexRiscv): Unit = {
    import Riscv._
    import pipeline.config._

    io = slave(CoreDebugModuleIo()).setName("CodeDebugModuleIo")

    step = Reg(Bool) init False
    ebreakm = Reg(Bool) init False

    val decoderService = pipeline.service(classOf[DecoderService])


    decoderService.addDefault(IS_EBREAK, False)
    decoderService.add(EBREAK, List(IS_EBREAK -> True))

    val pcManagerService = pipeline.service(classOf[JumpService])
    jumpInterface = pcManagerService.createJumpInterface(pipeline.stages.last)

    privilegeService = pipeline.serviceElse(classOf[PrivilegeService], PrivilegeServiceDefault())

    injectionPort = pipeline.service(classOf[IBusFetcher]).getInjectionPort()

  }


  override def build(pipeline: VexRiscv): Unit = {
    import pipeline._
    import pipeline.config._

    val logic = debugClockDomain {
      pipeline plug new Area {
        val iBusFetcher = service(classOf[IBusFetcher])
        val csrService = pipeline.service(classOf[CsrInterface])


        val haltIt = RegInit(False)
        val isPipBusy = RegNext(stages.map(_.arbitration.isValid).orR || iBusFetcher.incoming())
        val godmode = RegInit(False) setWhen (haltIt && !isPipBusy)
        val haltedByBreak = RegInit(False)
        val debugUsed = RegInit(False) setWhen (io.resetRequest || io.haltRequest || io.resumeRequest) addAttribute (Verilator.public)
        val disableEbreak = RegInit(False).allowUnsetRegToAvoidLatch  //
        val haltReqPending = Bool()
        val cause = Reg(UInt(3 bits)) init 0

        val allowEBreak = debugUsed && !disableEbreak


        /*val busReadDataReg = Reg(Bits(32 bit))
        when(stages.last.arbitration.isValid) {
          busReadDataReg :=
        } */
        io.rsp.valid := stages.last.arbitration.isValid
        io.rsp.payload := stages.last.output(REGFILE_WRITE_DATA)

        haltReqPending := io.haltRequest && !haltIt

        io.halted := haltIt
        io.running := !haltIt

        io.injectionPort >> injectionPort

        jumpInterface.valid := False
        jumpInterface.payload.assignDontCare()

        io.resumeAck := RegNext(io.resumeRequest)
        io.resetAck := RegNext(io.resetRequest)


        haltedByBreak clearWhen (io.resumeRequest)
        haltIt clearWhen io.resumeRequest.rise()
        godmode clearWhen (io.resumeRequest)

        val isDoingEbreak = execute.arbitration.isValid && execute.input(DO_EBREAK)
        val isRequestingHalt = (execute.arbitration.isValid || iBusFetcher.pcValid(execute)) &&  io.haltRequest
        val pcValid = stages.map(iBusFetcher.pcValid(_)).orR
        decode.insert(DO_EBREAK) := !haltIt && decode.input(IS_EBREAK) && allowEBreak
        when(isDoingEbreak || isRequestingHalt){
          execute.arbitration.haltByOther := True
          when(stagesFromExecute.tail.map(_.arbitration.isValid).orR === False) {
            iBusFetcher.haltIt()
            execute.arbitration.flushIt := True
            execute.arbitration.flushNext := True
            haltIt := True
          }
        }

        when(haltIt) {
          iBusFetcher.haltIt()
        }

        when(step && iBusFetcher.incoming()) {
          iBusFetcher.haltIt()
          when(decode.arbitration.isValid) {
            haltIt := True
          }
        }

        //Avoid having two C instruction executed in a single step
        if (pipeline.config.withRvc) {
          val cleanStep = RegNext(step && decode.arbitration.isFiring) init (False)
          execute.arbitration.flushNext setWhen (cleanStep)
        }

        io.resetOut := RegNext(io.resetRequest)

        if (serviceExist(classOf[InterruptionInhibitor])) {
          when(haltIt || step) {
            service(classOf[InterruptionInhibitor]).inhibateInterrupts()
          }
        }

        when(godmode) {
          pipeline.plugins.foreach {
            case p: ExceptionInhibitor => p.inhibateException()
            case _ =>
          }
          pipeline.plugins.foreach {
            case p: PrivilegeService => p.forceMachine()
            case _ =>
          }
          if (pipeline.things.contains(DEBUG_BYPASS_CACHE)) pipeline(DEBUG_BYPASS_CACHE) := True
        }
        when(allowEBreak) {
          pipeline.plugins.foreach {
            case p: ExceptionInhibitor => p.inhibateEbreakException()
            case _ =>
          }
        }

        val wakeService = serviceElse(classOf[IWake], null)
        if (wakeService != null) when(haltIt) {
          wakeService.askWake()
        }


        val DPC_iswriting = csrService.isWriting(CSR.DPC) addAttribute (Verilator.public)
        val DPC_isreading = csrService.isReading(CSR.DPC) addAttribute (Verilator.public)
        val dpc = Reg(UInt(32 bits))
        when(io.resetRequest){
          dpc := execute.input(PC)
        }
        csrService.w(CSR.DPC, jumpInterface.payload)
        csrService.w(CSR.DPC,  dpc)
        when(io.resumeRequest.rise()) {
          jumpInterface.valid := True
          jumpInterface.payload := dpc
        }


        csrService.r(CSR.DPC, dpc)

        val debugver = UInt(4 bits)
        debugver := 4

        val priv = UInt(2 bit)
        priv := U"10"
        when(privilegeService.isUser()) {
          priv := U"00"
        }
        when(privilegeService.isSupervisor()) {
          priv := U"01"
        }
        when(privilegeService.isMachine()) {
          priv := U"11"
        }

        csrService.r(CSR.DCSR, 28 -> debugver, // debugver
          17 -> B"0", // ebreakvs
          16 -> B"0", // ebreakvu
          15 -> ebreakm, // ebreakm
          13 -> B"0", // ebreaks
          12 -> B"0", // ebreaku
          11 -> B"0", // stepie
          10 -> B"0", // stopcount
          9 -> B"0", // stoptime
          6 -> cause, // cause
          5 -> B"0", // v
          4 -> B"0", // mprven
          3 -> B"0", // nmip
          2 -> step, // step
          0 -> priv)
        csrService.w(CSR.DCSR, 15 -> ebreakm,
          2 -> step
        )
        val DCSR_iswriting = csrService.isWriting(CSR.DCSR) addAttribute (Verilator.public)
        val DCSR_isreading = csrService.isReading(CSR.DCSR) addAttribute (Verilator.public)

        // halt request
        // resetrequest + haltrequest
        // step done
        /*
        DPC Value upon debug mode entry:
         */
        // 4 reason to stop
        // execute EBREAK instr
        when(isDoingEbreak) {
          // ebreak: address of the ebreak instr.
          dpc := execute.input(PC)
          cause := 1
        }
        when(haltReqPending.fall && haltIt && !io.injectionPort.valid) {
          //haltreq: address of the next instruction to be executed (so the one in the execute stage)
          cause := 3
          dpc := execute.input(PC)
        }
        when(io.resetRequest && haltReqPending) {
          // haltonresetreq
          cause := 5
          dpc := execute.input(PC)
        }
        when(io.resumeRequest && step && haltIt && !io.injectionPort.valid) {
          cause := 4
          dpc := execute.input(PC)
        }

        //Avoid having two C instruction executed in a single step
        if (pipeline.config.withRvc) {
          val cleanStep = RegNext(step && decode.arbitration.isFiring) init (False)
          execute.arbitration.flushNext setWhen (cleanStep)
        }
        if (serviceExist(classOf[InterruptionInhibitor])) {
          when(step) {
            service(classOf[InterruptionInhibitor]).inhibateInterrupts()
          }
        }

        /*
        val tselect = Bits(32 bit)
        tselect := B"x00000000"
        csrService.r(CSR.TSELECT, 0 -> tselect)
        val TSELECT_iswriting = csrService.isWriting(CSR.TSELECT) addAttribute (Verilator.public)
        val TSELECT_isreading = csrService.isReading(CSR.TSELECT) addAttribute (Verilator.public)
        */
      }
    }
  }
}
