package mylib

import spinal.core._
import spinal.lib.bus.bmb.BmbParameter.BurstAlignement.WORD
import spinal.lib.{IMasterSlave, master, slave}

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
  val writeType = RegWriteType()

  override def asMaster(): Unit = {
    out(writeEn,writeAddr,writeData,writeType)
  }
}

object RegWriteType extends SpinalEnum{
  val WORD,HIGH_HALF,LOW_HALF=newElement()
}

class RegHeap(regNum: Int = 32) extends  Component {
  val readPort= slave(new RegHeapReadPort)
  val writePort = slave(new RegHeapWritePort)
  val writePort2 = slave(new RegHeapWritePort)

  val bypassBack = master(Reg(new RegHeapWritePort))

  val heap = Vec(Reg(Bits(GlobalConfig.dataBitsWidth)).init(0),regNum)

  readPort.readDatas(0) := 0
  readPort.readDatas(1) := 0

  bypassBack := writePort

  when(writePort.writeEn && (writePort.writeAddr =/= B(0))){
    switch(writePort.writeType){
      is(RegWriteType.WORD){
        heap(writePort.writeAddr.asUInt) := writePort.writeData
      }
      is(RegWriteType.HIGH_HALF){
        heap(writePort.writeAddr.asUInt) :=
          writePort.writeData.take(GlobalConfig.dataBitsWidth.value/2) ##
          heap(writePort.writeAddr.asUInt).take(GlobalConfig.dataBitsWidth.value/2)
      }
      is(RegWriteType.LOW_HALF){
        heap(writePort.writeAddr.asUInt) :=
          heap(writePort.writeAddr.asUInt).takeHigh(GlobalConfig.dataBitsWidth.value/2) ##
            writePort.writeData.take(GlobalConfig.dataBitsWidth.value/2)
      }
    }


  }otherwise{
    for(i <- 0 until 2){
      when(readPort.readEns(i)) {
        readPort.readDatas(i) := heap(readPort.readAddrs(i).asUInt)
      }
    }
  }
}
