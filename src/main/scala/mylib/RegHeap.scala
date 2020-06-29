package mylib

import spinal.core._
import spinal.lib.{IMasterSlave, slave}

class RegHeapReadPort(regNum:Int=32) extends Bundle with IMasterSlave {
  val readAddrs = Vec(Bits(log2Up(regNum) bits),2)
  val readDatas =  Vec(Bits(GlobalConfig.dataBitsWidth),2)
  val readEns = Vec(Bool,2)

  override def asMaster(): Unit = {
    in(readDatas)
    out(readAddrs,readEns)
  }
}

class RegHeapWritePort(regNum:Int=32) extends Bundle with IMasterSlave {
  val writeEn = Bool
  val writeAddr =Bits(log2Up(regNum) bits)
  val writeData = Bits(GlobalConfig.dataBitsWidth)

  override def asMaster(): Unit = {
    out(writeEn,writeAddr,writeData)
  }
}

class RegHeap(regNum: Int = 32) extends  Component {
  val readPort= slave(new RegHeapReadPort)
  val writePort = slave(new RegHeapWritePort)

  val heap = Vec(Reg(Bits(GlobalConfig.dataBitsWidth)).init(0),regNum)

  readPort.readDatas(0) := 0
  readPort.readDatas(1) := 0

  when(writePort.writeEn && (writePort.writeAddr =/= B(0))){
      heap(writePort.writeAddr.asUInt) := writePort.writeData
  }otherwise{

    for(i <- 0 until 2){
      when(readPort.readEns(i)) {
        readPort.readDatas(i) := heap(readPort.readAddrs(i).asUInt)
      }
    }
  }
}
