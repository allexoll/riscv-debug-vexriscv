package vexriscv.plugin.riscvdebug

import spinal.core._
import spinal.lib._
import spinal.lib.com.jtag.{Jtag, JtagTapFactory, JtagTapFunctions, JtagTapInstructionCtrl}

case class DebugTransportModuleParameter(addressWidth : Int,
                                         version : Int,
                                         idle : Int)

class DebugTransportModuleJtag(p : DebugTransportModuleParameter,
                               tap : JtagTapFunctions,
                               jtagCd : ClockDomain,
                               debugCd : ClockDomain) extends Area{
  import p._

  val jtagLogic = jtagCd on new Area {
    val dmiReq = Flow(DMIReq(addressWidth))
    val dmiRsp = Flow(DMIRsp())

    val dtmcs = tap.readAndWriteWithEvents(Bits(32 bits), Bits(32 bits))(0x10)
    val dmi   = tap.readAndWriteWithEvents(DMICapture(addressWidth), DMIUpdate(addressWidth))(0x11)

    val dmiStat = new Area{
      val value = Reg(DMICaptureOp())
      val failure = False
      val busy = False
      val clear = False

      when(value === DMICaptureOp.SUCCESS){
        when(failure){ value := DMICaptureOp.FAILED }
        when(busy){ value := DMICaptureOp.OVERRUN }
      }
      when(clear) { value := DMICaptureOp.SUCCESS }
    }


    val pending = Reg(Bool()) setWhen (dmiReq.valid) clearWhen (dmiRsp.fire)

    val trigger = new Area {
      val dmiHardReset = dtmcs.updateData(17) && dtmcs.updateValid || tap.isReseting()
      val dmiReset     = dtmcs.updateData(16) && dtmcs.updateValid || tap.isReseting()
      val dmiCmd       = False
      when(dmi.updateValid){
        switch(dmi.updateData.op){
          is(DMIUpdateOp.NOP){ }
          is(DMIUpdateOp.READ){ dmiCmd := True }
          is(DMIUpdateOp.WRITE){ dmiCmd := True }
          default{ dmiStat.failure := True }
        }
      }
      when(dmiReset) { dmiStat.clear := True }
      when(dmiHardReset) { pending := False }
    }
    dtmcs.captureData := B(0, 17 bits) ## B(idle, 3 bits) ## dmiStat.value ## B(addressWidth, 6 bits) ## B(version, 4 bits)


    val reqLogic = new Area {
      dmiReq.valid := trigger.dmiCmd
      dmiReq.write := dmi.updateData.op === DMIUpdateOp.WRITE
      dmiReq.address := dmi.updateData.address
      dmiReq.data := dmi.updateData.data
    }

    val rspLogic = new Area{
      val buffer = Reg(Bits(32 bits))
      when(dmiRsp.fire){
        buffer := dmiRsp.data
        when(dmiRsp.error){ dmiStat.failure := True }
      }

      dmi.captureData.op   := dmiStat.value.getAheadValue()
      dmi.captureData.data := buffer
      dmi.captureData.padding := 0
      when(dmi.captureValid && pending){
        dmiStat.busy := True
      }
    }
  }

  val systemLogic = debugCd on new Area{
    val bus = DMI(addressWidth)
    val req = jtagLogic.dmiReq.ccToggle(
      pushClock = jtagCd,
      popClock = debugCd,
      withOutputM2sPipe = false
    ).toStream.m2sPipe()

    bus.req << req

    jtagLogic.dmiRsp << bus.rsp.ccToggle(
      pushClock = debugCd,
      popClock = jtagCd
    )
  }
}

case class DebugTransportModuleJtagTap(p : DebugTransportModuleParameter,
                                       debugCd : ClockDomain) extends Component{
  val io = new Bundle {
    val jtag = slave(Jtag())
    val ctrl = master(DMI(p.addressWidth))
  }

  val jtagCd = ClockDomain(io.jtag.tck)

  val tap = jtagCd on JtagTapFactory(io.jtag, instructionWidth = 5)
  val idcodeArea = jtagCd on tap.idcode(B"x10002FFF")(1)
  val logic = new DebugTransportModuleJtag(
    p       = p,
    tap     = tap,
    jtagCd  = jtagCd,
    debugCd = debugCd
  )

  io.ctrl <> logic.systemLogic.bus
}

case class DebugTransportModuleTunneled(p : DebugTransportModuleParameter,
                                        jtagCd : ClockDomain,
                                        debugCd : ClockDomain) extends Component{
  val io = new Bundle {
    val instruction = slave(JtagTapInstructionCtrl())
    val ctrl = master(DMI(p.addressWidth))
  }

  val tap = jtagCd on new JtagTunnel(io.instruction, instructionWidth = 6)
  //  val idcodeArea = tap.idcode(B"x10002FFF")(1)
  val logic = new DebugTransportModuleJtag(
    p       = p,
    tap     = tap,
    jtagCd  = jtagCd,
    debugCd = debugCd
  )

  io.ctrl <> logic.systemLogic.bus
}

case class DebugTransportModuleJtagTapWithTunnel(p : DebugTransportModuleParameter,
                                                 debugCd : ClockDomain) extends Component{
  val io = new Bundle {
    val jtag = slave(Jtag())
    val bus = master(DMI(p.addressWidth))
  }

  val jtagCd = ClockDomain(io.jtag.tck)

  val tap = jtagCd on JtagTapFactory(io.jtag, instructionWidth = 6)
  val idcodeArea = jtagCd on tap.idcode(B"x10003FFF")(1)
  val tunnel = DebugTransportModuleTunneled(
    p       = p,
    jtagCd  = jtagCd,
    debugCd = debugCd
  )
  tap.map(tunnel.io.instruction, 0x23)


  io.bus <> tunnel.io.ctrl
}