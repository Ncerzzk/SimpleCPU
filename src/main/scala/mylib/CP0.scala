package mylib

import spinal.core._
import spinal.lib.master

import scala.collection.immutable.HashMap

object ExceptionEnum extends SpinalEnum{
  val INT,MOD,TLBL,TLBS,ADEL,ADES,IBS,DBE = newElement()
  val SYS,BP,RI,CPU,OV,TR=newElement()
  val WATCH,MCHECK=newElement()

  def getHandleAddress(a : SpinalEnumCraft[ExceptionEnum.type])={
    var result = Bits(GlobalConfig.dataBitsWidth)
    switch(a){
      for(i<- elements){
        is(i){result := 2*i.position + GlobalConfig.vectorStartAddress}
      }
    }
    result
  }
}

class CP0 extends Component {

  val io = new Bundle{
    val addr = in Bits(6 bits)
    val writeData = in Bits(GlobalConfig.dataBitsWidth)
    val writeEn = in Bool
    val interrups = in Bits(6 bits)
    val except = in Bool
    val exception = in (ExceptionEnum())
    val isDelaySlot = in Bool
    val pc = in Bits(GlobalConfig.dataBitsWidth)
    val readData = out Bits(GlobalConfig.dataBitsWidth)
  }

  val pcPort = master(new PCPort)

  pcPort.setDefaultValue(pcPort.writeData,pcPort.writeEN)

  val Counter = Reg(Bits(GlobalConfig.dataBitsWidth)).init(0)
  val Status = Reg(Bits(GlobalConfig.dataBitsWidth)).init(0)
  val Cause = Reg(Bits(GlobalConfig.dataBitsWidth)).init(0)
  val Compare = Reg(Bits(GlobalConfig.dataBitsWidth)).init(0)
  val EPC = Reg(Bits(GlobalConfig.dataBitsWidth)).init(0)
  val PRid = Reg(Bits(GlobalConfig.dataBitsWidth)).init(0)
  val Config = Reg(Bits(GlobalConfig.dataBitsWidth)).init(0)

  val regsList = List(
    // number->reg
    9-> Counter,
    11->Compare,
    12->Status,
    13->Cause,
    14->EPC,
    15->PRid,
    16->Config,
    default->Counter
  )


  val EXL = Status(1)
  val ExcCode = Cause(2 to 6)
  val BD = Cause(31)

  Cause(2 to 7) := io.interrups

  val jmpAddress= ExceptionEnum.getHandleAddress(io.exception)
  when(io.except){
    when(EXL){ // 已经处于异常处理之中
      when(io.exception =/= ExceptionEnum.INT){  // 如果是中断，不处理
        pcPort.JMP(jmpAddress) // 跳转到新的异常的处理地址中
        ExcCode := io.exception.asBits.resized
      }
    }otherwise{
      // 目前并不在异常处理中
      EXL :=True
      ExcCode := io.exception.asBits.resized
      BD := io.isDelaySlot
      when(io.isDelaySlot){
        EPC := (io.pc.asUInt -1).asBits
      }otherwise{
        EPC := io.pc
      }
    }

  }

  Counter := (Counter.asUInt+1).asBits

  io.readData := 0
  when(io.writeEn){
    switch(io.addr){
      for(i <- regsList){
        i._1 match {
          case `default` =>
          case a => is(a){i._2 := io.writeData}
        }
      }
    }
  }otherwise{
    io.readData := io.addr.muxList(regsList)
  }
}
