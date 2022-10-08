package vexriscv.plugin.riscvdebug;

import spinal.core.Component.push
import spinal.core._
import spinal.core.internals.Operator
import spinal.lib._
import spinal.lib.com.jtag.{Jtag, JtagTap, JtagTapInstructionCtrl}
import spinal.lib.cpu.riscv.impl.Utils.CSR
import spinal.lib.fsm._
import spinal.lib.system.debugger
import spinal.lib.system.debugger.{SystemDebuggerConfig, _}
import vexriscv.Riscv
import vexriscv.plugin.{DebugExtensionBus, DebugExtensionCmd}


/*
The Debug Module implements a translation interface between abstract debug operations and their specific implementation.

The registers described in this section are accessed over the DMI bus. Each DM has a base address (which is 0 for the
first DM). The register addresses below are offsets from this base address.


This implementation uses a progbufsize of 2 (needed for memory access)
*/


case class DebugModuleParameter(version : Int,
                                harts : Int,
                                progBufSize : Int,
                                datacount : Int)

case class HartDMStatus() extends Bundle{
    val HART_hasreset = RegInit(False)
    val HART_haltRequest = RegInit(False)
    val HART_resetRequest = RegInit(False)
    val HART_resumeRequest = RegInit(False)
    val HART_resumeack = RegInit(False)
    val HART_haltonresetrequest = False
    val HART_running = Bool()
}

class DebugModule(config_progbufsize:Int = 2, cpuCount: Int = 1) extends Component{
    val io = new Bundle{
        val dmi = slave(DMI(7))
        val debug = Vec(master(CoreDebugModuleIo()), cpuCount)
        val platformReset = out(Bool())
    }

    // default assigns
    for(bus <- io.debug){
        bus.injectionPort.valid := False
        bus.injectionPort.payload.clearAll()
        //bus.resetRequest := False
        //bus.haltRequest := False
        //bus.resumeRequest := False
    }
    val hasResetHaltReq = false

    val bus = DMISlaveFactory(io.dmi)


    val pipelineWaitCycles = 3
    val startExec = Bool()
    startExec := False
    val selfprogbuf = Vec(Reg(Bits(32 bits)), 2)
    val selfExec = Bool()   // no bool because immediate (no clock delay)
    selfExec := False
    val progbufExec = Reg(Bool()) init False       // registered because should be delay when doing selfExec -> bufExec

    val autoexecdata0 = Bool()
    val autoexec = Bool()   // single cycle
    autoexec := False

    val hartStatus = Vec(HartDMStatus(),cpuCount)


    // data0 @ 0x04
    val data0 = new Area {
        val registerAddress = DMRegisters.data0
        val data_r = Reg(Bits(32 bit))
        bus.read(data_r, registerAddress)
        val data_w = Reg(Bits(32 bits))
        bus.write(data_w, registerAddress)
        when((bus.doRead  && bus.readAddress() === registerAddress) || (bus.doWrite && bus.writeAddress() === registerAddress)){
            autoexec := autoexecdata0
        }
    }
    // dmcontrol @ 0x10
    val dmcontrol = new Area {
        val registerAddress = DMRegisters.dmcontrol

        val haltreq = False
        bus.write(haltreq, registerAddress, 31)

        val resumereq = Reg(Bool()) init False
        bus.write(resumereq, registerAddress, 30)

        val hartreset = Reg(Bool()) init False
        bus.readAndWrite(hartreset, registerAddress, 29)

        val ackhavereset = False
        bus.write(ackhavereset, registerAddress, 28)


        val ackunavail = Bool()
        ackunavail := False
        bus.write(ackunavail, registerAddress, 27)


        val hasel = Bool()
        hasel := False
        bus.read(hasel, registerAddress, 26)

        val hartsello = Reg(UInt(log2Up(cpuCount%1024) bit)) init 0
        val hartselhi = Reg(UInt(log2Up(cpuCount/1024) bit)) init 0
        val hartsel = (hartselhi ## hartsello).asUInt
        bus.readAndWrite(hartselhi, registerAddress, 6)
        bus.readAndWrite(hartsello, registerAddress, 16)

        val keepalive = Reg(Bool) init False //???
        bus.setOnSet(keepalive, registerAddress, 5)
        bus.clearOnSet(keepalive, registerAddress, 4)

        val resethaltreq = hasResetHaltReq generate {Reg(Bool) init False}

        hasResetHaltReq generate {
            bus.setOnSet(resethaltreq, registerAddress, 3)
            bus.clearOnSet(resethaltreq, registerAddress, 2)
        }


        val ndmreset = Reg(Bool) init False
        bus.readAndWrite(ndmreset, registerAddress, 1)


        val dmactive = Reg(Bool) init False
        bus.readAndWrite(dmactive, registerAddress, 0)
    }

    // dmstatus @ 0x11
    val dmstatus = new Area {
        val registerAddress = DMRegisters.dmstatus

        val ndmresetpending = dmcontrol.ndmreset
        bus.read(ndmresetpending, registerAddress, 24)

        val stickyunavail = Bool()
        stickyunavail := False
        bus.read(stickyunavail, registerAddress, 23)

        val impebreak = Bool()
        impebreak := True   // technically, since we are stoped, ebreak is implied?
        bus.read(impebreak, registerAddress, 22)

        val allhavereset = Bool()
        allhavereset := hartStatus(dmcontrol.hartsel).HART_hasreset
        bus.read(allhavereset, registerAddress, 19)

        val anyhavereset = Bool()
        anyhavereset := hartStatus(dmcontrol.hartsel).HART_hasreset
        bus.read(anyhavereset, registerAddress, 18)

        val allresumeack = hartStatus(dmcontrol.hartsel).HART_resumeack
        bus.read(allresumeack, registerAddress, 17)

        val anyresumeack = hartStatus(dmcontrol.hartsel).HART_resumeack
        bus.read(anyresumeack, registerAddress, 16)


        val allnonexistent = Bool()
        allnonexistent := dmcontrol.hartsel > (cpuCount-1) // only accept hart zero
        bus.read(allnonexistent, registerAddress, 15)

        val anynonexistent = Bool()
        anynonexistent := dmcontrol.hartsel > (cpuCount-1)
        bus.read(anynonexistent, registerAddress, 14)

        val allunavail = Bool()
        allunavail := False
        bus.read(allunavail, registerAddress,13)

        val anyunavail = Bool()
        anyunavail := False
        bus.read(anyunavail, registerAddress, 12)

        val allrunning = Bool()
        allrunning := hartStatus(dmcontrol.hartsel).HART_running
        bus.read(allrunning, registerAddress, 11)

        val anyrunning = Bool()
        anyrunning := hartStatus(dmcontrol.hartsel).HART_running
        bus.read(anyrunning, registerAddress, 10)

        val allhalted = Bool()
        allhalted := !hartStatus(dmcontrol.hartsel).HART_running
        bus.read(allhalted, registerAddress, 9)

        val anyhalted = Bool()
        anyhalted := !hartStatus(dmcontrol.hartsel).HART_running
        bus.read(anyhalted, registerAddress, 8)

        val authenticated = Bool()
        authenticated := True   // authenticated at start, no security
        bus.read(authenticated, registerAddress, 7)

        val authbusy = Bool()
        authbusy := False
        bus.read(authbusy, registerAddress, 6)

        val hasresethaltreq = Bool(hasResetHaltReq)
        bus.read(hasresethaltreq, registerAddress, 5)

        val confstrptrvalid = Bool()
        confstrptrvalid:= False
        bus.read(confstrptrvalid, registerAddress, 4)

        val version = UInt(4 bit)
        version := 2
        bus.read(version, registerAddress, 0)
    }


    // hartinfo @ 0x12
    val hartinfo = new Area {
        val registerAddress = DMRegisters.hartinfo
        val unimplemented = Bits(32 bits)
        unimplemented := B"x00000000"
        bus.read(unimplemented, registerAddress)
    }

    // abstractcs @ 0x16
    val abstractcs = new Area{
        val registerAddress = DMRegisters.abstractcs

        val progbufsize = UInt(5 bits)
        progbufsize := config_progbufsize
        bus.read(progbufsize, registerAddress, 24)
        val busy = Bool()
        bus.read(busy, registerAddress, 12)
        val relaxedpriv = Bool()
        relaxedpriv := True // relaxed permissions
        bus.read(relaxedpriv, registerAddress, 11)
        val cmderr = Reg(UInt(3 bit)) init 0
        bus.readAndClearOnSet(cmderr, registerAddress, 8)
        val datacount = UInt(4 bits)
        datacount := 1
        bus.read(datacount, registerAddress, 0)
    }

    // command @ 0x17
    val command = new Area {
        val registerAddress = DMRegisters.command
        // this register needs to be Reg'd because autoexecdata will need to execute it again when set
        val cmdtype = UInt(8 bits)
        //cmdtype := 0
        bus.drive(cmdtype, registerAddress, 24)

        val control = Bits(24 bit)
        //control := 0
        bus.drive(control, registerAddress, 0)
        val aarsize = UInt(3 bit)
        aarsize := control(22 downto 20).asUInt
        val aarpostincrement = Bool
        aarpostincrement := control(19)
        val postexec = Bool
        postexec := control(18)
        val transfer = Bool
        transfer := control(17)
        val write = Bool
        write := control(16)
        val regno = UInt(16 bits)
        regno := control(15 downto 0).asUInt
        // fall because we need it to be 1 cycle late, so that the Reg'd command is accessible.
        // autoexec is delayed for the same reason, but for Reg'd data0.
        when((bus.doWrite.fall && bus.writeAddress() === registerAddress) || Delay(autoexec, 1)){
            when(cmdtype === 0) { // access register
                when(aarsize =/= 2){
                    abstractcs.cmderr := 2
                } otherwise  {
                    when(regno(15 downto 12) === 0) { // CSR access
                        abstractcs.cmderr := 2
                    }
                    when(regno(15 downto 12) === 1) //GPR access
                    {
                        when(!write) {  // do read register
                            // nop should be first or we miss the regfile write by one instr
                            // nop seems to be implemented as mv x0, x0
                            selfprogbuf(0) := B"x00000013"  // nop
                            selfprogbuf(1) := B"0000000" ## B"00000" ## regno(4 downto 0) ## B"000" ## regno(4 downto 0) ## B"0110011" // add regno, regno, 0
                        }
                        when(write){
                            // for write
                            // lui #reg 20bit upper imediate
                            // add immediate 12 bits
                            // we need to check bit 11 because lower 12 bits will be sign extended.
                            when(data0.data_w(11) === False)
                            {
                                selfprogbuf(0) := data0.data_w(31 downto 12) ## regno(4 downto 0) ## B"0110111"   // lui regno, upper immediate, normal
                            } otherwise {
                                selfprogbuf(0) := ~data0.data_w(31 downto 12) ## regno(4 downto 0) ## B"0110111"   // lui regno, upper immediate, normal

                            }
                            selfprogbuf(1) := data0.data_w(11 downto 0) ## regno(4 downto 0) ## B"100" ## regno(4 downto 0) ## B"0010011" // xori regno, regno, low immediate
                            // using xori allows us to counterbalance the inversion done before.
                            // using only addi or ori would end up with a wrong value in the register if the 11th bit
                            // would be 1 because of the sign extension. if bit is 0, xori works because all extended
                            // bits are 0's so they do not invert it. but if the bit is 1, then the sign extension
                            // is all 1's, turning the 1's from ui into 0 and vice versa, compensating what we did when
                            // building the instruction.
                        }
                        // when transfer is set, do the transfer
                        when(transfer){
                            selfExec := True
                        } otherwise{
                            selfExec := False
                        }

                    }
                    progbufExec := postexec
                    // start instruction execution if there is a transfer or postexec is set
                    startExec := transfer || postexec
                }
            }
        }
    }




    // abstractauto @ 0x18
    val abstractauto = new Area {
        val registerAddress = DMRegisters.abstractauto
        val autoexecdata = Bits(1 bit)
        bus.drive(autoexecdata, registerAddress) init B"0"
        autoexecdata0 := autoexecdata(0)
    }

    val progbuf = new Area {
        val registerAddress = DMRegisters.progbuf0
        val progbuf0 = Vec(Reg(Bits(32 bits)), config_progbufsize)
        for(off <- 0 until config_progbufsize) {
            bus.readAndWrite(progbuf0(off), registerAddress + off)
        }
    }
    // sbcs @ 0x38
    val sbcs = new Area{
        val registerAddress = DMRegisters.sbcs
        val sbversion  = bus.read(U(1, 3 bits), registerAddress, 29)
        val sbaccess   = bus.read(U(2, 3 bits), registerAddress, 17)
    }


    io.platformReset := dmcontrol.ndmreset

    when(dmcontrol.ndmreset.rise() || dmcontrol.hartreset.rise())
    {
        hasResetHaltReq generate {
            when(dmcontrol.resethaltreq) {
                hartStatus(dmcontrol.hartsel).HART_haltRequest := True
            }
        }
    }

    when(dmcontrol.haltreq.rise()){
        hartStatus(dmcontrol.hartsel).HART_haltRequest := True
    }
    when(dmcontrol.resumereq.rise()){
        hartStatus(dmcontrol.hartsel).HART_resumeRequest := True
        hartStatus(dmcontrol.hartsel).HART_resumeack := False
    }
    when(dmcontrol.resumereq.fall())
    {
        hartStatus(dmcontrol.hartsel).HART_resumeRequest := False
        hartStatus(dmcontrol.hartsel).HART_resumeack := False
    }

    // connect all harts hasReset and Running flags
    (hartStatus, io.debug).zipped.map{ (dm, core) => {
        dm.HART_hasreset := core.resetAck
        dm.HART_running := core.running
        core.resumeRequest := dm.HART_resumeRequest && dmcontrol.dmactive
        core.haltRequest := dm.HART_haltRequest && dmcontrol.dmactive
        core.resetRequest := dm.HART_resetRequest && dmcontrol.dmactive
        when(core.halted && dm.HART_haltRequest){
            dm.HART_haltRequest := False
        }
    }}
   /* when(!dmcontrol.dmactive){
        io.debug.map(x =>{
            x.resetRequest := False
            x.resumeRequest := False
            x.haltRequest := False
        })
        //io.debug(dmcontrol.hartsel).resetRequest := False
        //io.debug(dmcontrol.hartsel).resumeRequest := False
        //io.debug(dmcontrol.hartsel).haltRequest := False
    }*/
    hartStatus(dmcontrol.hartsel).HART_resetRequest := dmcontrol.ndmreset || dmcontrol.hartreset

    //io.debug(dmcontrol.hartsel).haltRequest := hartStatus(dmcontrol.hartsel).HART_haltRequest
    //io.debug(dmcontrol.hartsel).resetRequest := hartStatus(dmcontrol.hartsel).HART_resetRequest

    //hartStatus(dmcontrol.hartsel).HART_hasreset := io.debug(dmcontrol.hartsel).resetAck
    //hartStatus(dmcontrol.hartsel).HART_running := io.debug(dmcontrol.hartsel).running
    //io.debug(dmcontrol.hartsel).resumeRequest := hartStatus(dmcontrol.hartsel).HART_resumeRequest

    when(dmcontrol.resumereq && !hartStatus(dmcontrol.hartsel).HART_resumeack) {
        hartStatus(dmcontrol.hartsel).HART_resumeack := io.debug(dmcontrol.hartsel).resumeAck
    }
    //when(io.debug(dmcontrol.hartsel).halted && hartStatus(dmcontrol.hartsel).HART_haltRequest){
    //    hartStatus(dmcontrol.hartsel).HART_haltRequest := False
    //}



    val debugbusManager = new Area {

        io.debug(dmcontrol.hartsel).injectionPort.valid := False
        io.debug(dmcontrol.hartsel).injectionPort.payload := Bits(32 bit).clearAll()
        val max_instr = Math.max(2, config_progbufsize) // 2 is for selfInstr, or progbufsize
        val instrCnt = Counter(log2Up(max_instr) bits)

        val fsm:StateMachine = new StateMachine {
            val statePollStatus = new State with EntryPoint
            val stateExecSelfProgBuf = new State
            val stateWaitForRegFileWrite = new State
            val stateExecProgBuf = new State

            abstractcs.busy := !isActive(statePollStatus)

            statePollStatus.whenIsActive {
                when(startExec){
                    instrCnt.clear()
                    when(selfExec) {
                        goto(stateExecSelfProgBuf)
                    }otherwise{
                        goto(stateExecProgBuf)
                    }
                }
            }
            stateExecSelfProgBuf.whenIsActive{
                when(io.debug(dmcontrol.hartsel).injectionPort.ready)
                {
                    when(instrCnt.value === 2-1){   // second value is last selfinstr instruction
                            goto(stateWaitForRegFileWrite)
                            instrCnt.clear()
                    } otherwise{
                        instrCnt.increment()
                    }
                }
                val instr = Bits(32 bit)
                instr := selfprogbuf(instrCnt.value(0).asUInt)

                io.debug(dmcontrol.hartsel).injectionPort.payload := instr
                io.debug(dmcontrol.hartsel).injectionPort.valid := True
            }
            stateWaitForRegFileWrite.whenIsActive{
                when(io.debug(dmcontrol.hartsel).rsp.valid){
                    data0.data_r := io.debug(dmcontrol.hartsel).rsp.payload
                    when(progbufExec) {
                        goto(stateExecProgBuf)
                    }otherwise{
                        goto(statePollStatus)
                    }
                }
            }
            stateExecProgBuf.whenIsActive{
                when(io.debug(dmcontrol.hartsel).injectionPort.ready)
                {
                    when(instrCnt === config_progbufsize-1){
                        goto(statePollStatus)
                    } otherwise {
                        instrCnt.increment()
                    }
                }
                val instr = Bits(32 bit)

                instr := progbuf.progbuf0(instrCnt)
                io.debug(dmcontrol.hartsel).injectionPort.payload := instr
                io.debug(dmcontrol.hartsel).injectionPort.valid := True
            }
        }
    }



    object DMRegisters{
        def data0 = 0x04 // Abstract Data 0 (data0)
        def data1 = 0x05 // Abstract Data 1 (data1)
        def data2 = 0x06 // Abstract Data 2 (data2)
        def data3 = 0x07 // Abstract Data 3 (data3)
        def data4 = 0x08 // Abstract Data 4 (data4)
        def data5 = 0x09 // Abstract Data 5 (data5)
        def data6 = 0x0a // Abstract Data 6 (data6)
        def data7 = 0x0b // Abstract Data 7 (data7)
        def data8 = 0x0c // Abstract Data 8 (data8)
        def data9 = 0x0d // Abstract Data 9 (data9)
        def data10 = 0x0e // Abstract Data 10 (data10)
        def data11 = 0x0f // Abstract Data 11 (data11)

        def dmcontrol = 0x10    // Debug Module Control
        def dmstatus = 0x11     // Debug Module Status

        def hartinfo = 0x12     // Hart Info
        def haltsum1 = 0x13     // Halt Summary 1
        def hawindowsel = 0x14  // Hart Array Window Select
        def hawindow = 0x15     // Hart Array Window

        def abstractcs = 0x16   // Abstract Control and Status
        def command = 0x17      // Abstract Command
        def abstractauto = 0x18 // Abstract Command Autoexec

        def confstrptr0 = 0x19  // Configuration String Pointer 0
        def confstrptr1 = 0x1a  // Configuration String Pointer 1
        def confstrptr2 = 0x1b  // Configuration String Pointer 2
        def confstrptr3 = 0x1c  // Configuration String Pointer 3

        def nextdm      = 0x1d  // Next Debug Module

        def progbuf0     = 0x20  // progbuf0

        def sbcs        = 0x38  // System Bus Access Control and Status
    }

    /**
     * Connect the Debug Module to a Jtag interface
     * @return Jtag port
     */
    def fromJtag(): Jtag = {
        val dtm = new DebugTransportModuleJtagTap(
            p = DebugTransportModuleParameter(
                addressWidth = 7,
                version      = 1,
                idle         = 7),
            debugCd = this.clockDomain
        )
        io.dmi <> dtm.io.bus
        dtm.io.jtag
    }

    /**
     * Connect the debugModule to a tunnelled JTAG trough JTAG.
     * Usefull for debugging the tunnel.
     * @return Jtag Port
     */
    def fromTunneledJtagTroughJtag(): Jtag = {
        val dtm = new DebugTransportModuleJtagTapWithTunnel(
            p = DebugTransportModuleParameter(
                addressWidth = 7,
                version = 1,
                idle = 7),
            debugCd = this.clockDomain
            )
        io.dmi <> dtm.io.bus
        dtm.io.jtag
    }

    /**
     * Connect the Debug Module to a Tunnelled Jtag, after a bscane2
     * primitive (Xilinx's jtag tap instruction access)
     */
    def fromBscane2(): Unit ={
        val instruction = JtagTapInstructionCtrl()
        val bscan = instruction.fromXilinxBscane2(4)
        val jtagCd = ClockDomain(bscan.TCK)
        val dtm = new DebugTransportModuleTunneled(p = DebugTransportModuleParameter(
            addressWidth = 7,
            version      = 1,
            idle         = 7),
            jtagCd,
            this.clockDomain
        )
        io.dmi <> dtm.io.bus
        dtm.io.instruction <> instruction
    }
}
