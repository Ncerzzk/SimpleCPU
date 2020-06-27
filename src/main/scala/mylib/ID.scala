package mylib

import spinal.core._
import spinal.lib.{master, slave}

object InstEnum extends SpinalEnum{  // 指令枚举
  val EXEORI = newElement()
  defaultEncoding = SpinalEnumEncoding("static"){
    EXEORI-> 0xD // 001101
  }
}

object OpEnum extends SpinalEnum{
  val LOGIC = newElement()
}

object OpLogic extends SpinalEnum{
  val OR = newElement()
}

class IDOut extends Bundle{
  val op = out Bits( 3 bits)     // 运算类型
  val opSel = out Bits(8 bits) //运算子类型
  val opRnd1 = out Bits(GlobalConfig.dataBitsWidth)
  val opRnd2 = out Bits(GlobalConfig.dataBitsWidth)
  val writeReg = out Bool
  val writeRegAddr = out Bits(log2Up(GlobalConfig.regNum) bits)
}

class ID extends Component{
  val regHeap = master(new RegHeapReadPort)

  val exBack = new EXOut().flip()
  val memBack = new MEMOut().flip()
  val wbBack = slave(new RegHeapWritePort)

  def <>(regs: RegHeap)={
    regHeap <> regs.readPort
  }

  def <>(ex:EX): Unit =exBack <> ex.exOut
  def <>(mem:MEM) = memBack <> mem.memOut
  def <>(wb:WB) = wbBack <> wb.wbOut


  val lastStage = new IFOut().flip()

  val idOut= new IDOut

  val op =lastStage.inst.takeHigh(6)
  val op2 = lastStage.inst(6 to 10)
  val op3 = lastStage.inst.take(6)
  val op4 = lastStage.inst(16 to 20)

  var imm = Bits(GlobalConfig.dataBitsWidth)
  when(idOut.op===OpEnum.LOGIC.asBits.resized) {
    //val imm = B(0, 16 bits) ## lastStage.inst.take(16) // 立即数
    imm = lastStage.inst.take(16).resize(GlobalConfig.dataBitsWidth)
  }otherwise{
    imm = lastStage.inst.take(16).asSInt.resize(GlobalConfig.dataBitsWidth).asBits
  }


  val reg1Addr = lastStage.inst(21 to 25)
  val reg2Addr = lastStage.inst(16 to 20)


  for(i <- idOut.elements){
    if(i._1 == "writeReg"){
      i._2 := False
    }
    else {
      i._2 := B(0)
    }
  }

  regHeap.readAddrs(0) :=reg1Addr
  regHeap.readAddrs(1) :=reg2Addr
  regHeap.readEns(0) := False
  regHeap.readEns(1) := False
  switch(op){
    is(InstEnum.EXEORI.asBits.resize(op.getWidth bits)){
      val targetRegAddr = lastStage.inst(16 to 20)
      idOut.writeReg := True
      idOut.op := OpEnum.LOGIC.asBits.resize(3 bits)
      idOut.opSel := OpLogic.OR.asBits.resize(8 bits)
      idOut.writeRegAddr := targetRegAddr
      regHeap.readEns(0) := True
      regHeap.readEns(1) := False
    }
  }
  var i = 0;
  for( rnd <- List(idOut.opRnd1,idOut.opRnd2)){
    when(regHeap.readEns(i)){
      rnd := regHeap.readAddrs(i).mux(
        exBack.writeRegAddr -> exBack.writeData,
        memBack.writeRegAddr -> memBack.writeData,
        wbBack.writeAddr -> wbBack.writeData,
        default ->regHeap.readDatas(i)
      )
    }otherwise{
      rnd := imm
    }
    i+=1
  }
  /*
  when(regHeap.readEns(0)){
    idOut.opRnd1 := regHeap.readAddrs(0).mux(
      exBack.writeRegAddr -> exBack.writeData,
      memBack.writeRegAddr -> memBack.writeData,
      default ->regHeap.readDatas(0)
    )
  }otherwise{
    idOut.opRnd1 := imm
  }

  when(regHeap.readEns(1)){
    idOut.opRnd2 := regHeap.readDatas(1)
  }otherwise{
    idOut.opRnd2 := imm
  }
*/

}


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

  switch(lastStage.opSel){
    is(OpLogic.OR.asBits.resize(lastStage.opSel.getWidth)){
      exOut.writeData := lastStage.opRnd1 | lastStage.opRnd2
    }
  }
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