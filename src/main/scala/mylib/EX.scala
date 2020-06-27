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

  exOut.writeReg := lastStage.writeReg
  exOut.writeRegAddr := lastStage.writeRegAddr
  exOut.writeData :=0

  OpEnum.caculate(lastStage.op,lastStage.opSel,lastStage.opRnd1,lastStage.opRnd2,exOut.writeData)

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
