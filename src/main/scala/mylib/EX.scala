package mylib

import spinal.core._
import spinal.lib.master


class EXOut extends Bundle{
  val writeReg = out Bool
  val writeRegAddr = out Bits(log2Up(GlobalConfig.regNum) bits)
  val writeData = out Bits(GlobalConfig.dataBitsWidth)
}

class EX extends Component{
  val lastStage= new IDOut().flip()
  val exOut = new EXOut
  val hi = Reg(Bits(GlobalConfig.dataBitsWidth)).init(0)
  val lo = Reg(Bits(GlobalConfig.dataBitsWidth)).init(0)
  exOut.writeReg := lastStage.writeReg
  exOut.writeRegAddr := lastStage.writeRegAddr
  exOut.writeData :=0

  for(i <-OpEnum.OPs){
    when(lastStage.op === i._1.asBits.resize(lastStage.op.getWidth)){
      for((opsel,func) <- i._2.funcs){
        when(lastStage.opSel === opsel.asBits.resize(lastStage.opSel.getWidth)){
          if(opsel == OPArith.MUL || opsel== OPArith.MULU){

            hi := func(lastStage.opRnd1,lastStage.opRnd2).takeHigh(GlobalConfig.dataBitsWidth.value)
            lo := func(lastStage.opRnd1,lastStage.opRnd2).take(GlobalConfig.dataBitsWidth.value)
          }else{
            exOut.writeData := func(lastStage.opRnd1,lastStage.opRnd2)
          }

        }
      }
    }
  }

  //OpEnum.caculate(lastStage.op,lastStage.opSel,lastStage.opRnd1,lastStage.opRnd2,exOut.writeData)

}


class MEMOut extends Bundle{
  val writeReg = out Bool
  val writeRegAddr = out Bits(log2Up(GlobalConfig.regNum) bits)
  val writeData = out Bits(GlobalConfig.dataBitsWidth)
}

class MEM extends Component{
  val lastStage = new EXOut().flip()

  val memOut = new MEMOut

  for(i <- 0 until memOut.elements.length){
    memOut.elements(i)._2<>lastStage.elements(i)._2
  }
  //memOut <> lastStage
}

class WB extends Component{
  val lastStage= new MEMOut().flip()

  val wbOut= master(new RegHeapWritePort)

  wbOut.writeAddr := lastStage.writeRegAddr
  wbOut.writeData := lastStage.writeData
  wbOut.writeEn := lastStage.writeReg

  def <>(regHeap: RegHeap)={
    wbOut <> regHeap.writePort
  }
}
