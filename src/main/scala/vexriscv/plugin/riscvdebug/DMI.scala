package vexriscv.plugin.riscvdebug

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactoryDelayed, BusSlaveFactoryElement, SingleMapping}
import spinal.lib.com.jtag.{Jtag, JtagTap, JtagTapFunctions, JtagTapInstructionCtrl}

import scala.collection.Seq

/* Debug modules are slaves to a bus called the debug module inteface (DMI) the master of the bus is the debug transport
 module(s). the debug module interface can be a trivial bus with one master and one slave.

the DMI uses between 7 and 32 address bits. it supports read and write operations. the bottom of the address space is
used for the first DM extra space can be used for custom debug devices, other...
The debug module is controlled via register accesses to its DMI address space

*/


object DMIUpdateOp extends SpinalEnum(binarySequential){
  val NOP, READ, WRITE, RESERVED = newElement()
}

object DMICaptureOp extends SpinalEnum(binarySequential){
  val SUCCESS, RESERVED, FAILED, OVERRUN = newElement()
}

case class DMIUpdate(addressWidth : Int) extends Bundle{
  val op = DMIUpdateOp()
  val data = Bits(32 bits)
  val address = UInt(addressWidth bits)
}

case class DMICapture(addressWidth : Int) extends Bundle{
  val op = DMICaptureOp()
  val data = Bits(32 bits)
  val padding = UInt(addressWidth bits)
}

case class DMIReq(addressWidth : Int) extends Bundle{
  val write = Bool()
  val data = Bits(32 bits)
  val address = UInt(addressWidth bits)
}

case class DMIRsp() extends Bundle{
  val error = Bool()
  val data = Bits(32 bits)
}


case class DMI(addressWidth : Int) extends Bundle with IMasterSlave {
  val req = Stream(DMIReq(addressWidth))
  val rsp = Flow(DMIRsp())

  override def asMaster() = {
    master(req)
    slave(rsp)
  }
}


object DMISlaveFactory {
  def apply(bus: DMI) = new DMISlaveFactory(bus)
}

class DMISlaveFactory(bus: DMI) extends BusSlaveFactoryDelayed{
  bus.req.ready := True

  val reqToRsp = Flow(DMIRsp())
  val rspBuffer = reqToRsp.stage()

  val askWrite = (bus.req.valid && bus.req.write).allowPruning()
  val askRead  = (bus.req.valid && !bus.req.write).allowPruning()
  val doWrite  = (askWrite && bus.req.ready).allowPruning()
  val doRead   = (askRead && bus.req.ready).allowPruning()
  //  val forceError = False

  //  def error() = forceError := True

  bus.rsp.valid := rspBuffer.valid
  bus.rsp.payload  := rspBuffer.payload

  reqToRsp.valid := bus.req.fire
  reqToRsp.error := True
  reqToRsp.data := 0

  override def readAddress() : UInt = bus.req.address
  override def writeAddress() : UInt = bus.req.address

  override def readHalt(): Unit = bus.req.ready := False
  override def writeHalt(): Unit = bus.req.ready := False

  override def build(): Unit = {
    super.doNonStopWrite(bus.req.data)

    def doMappedElements(jobs : Seq[BusSlaveFactoryElement]) = super.doMappedElements(
      jobs = jobs,
      askWrite = askWrite,
      askRead = askRead,
      doWrite = doWrite,
      doRead = doRead,
      writeData = bus.req.data,
      readData = reqToRsp.data
    )

    switch(bus.req.address) {
      for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]) {
        is(address.asInstanceOf[SingleMapping].address) {
          reqToRsp.error := False
          doMappedElements(jobs)
        }
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(bus.req.address)){
        reqToRsp.error := False
        doMappedElements(jobs)
      }
    }

    //    cmdToRsp.error setWhen(forceError)
  }

  override def busDataWidth: Int = 32
  override def wordAddressInc: Int = 1
}

