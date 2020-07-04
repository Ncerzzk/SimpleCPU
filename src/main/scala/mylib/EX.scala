package mylib

import mylib.OPArith.{ADDU, MUL, MULU, SLT, SLTU, SUBU}
import spinal.core._
import spinal.lib.master


class EXOut extends Bundle{
  val writeReg = out Bool
  val writeRegAddr = out Bits(log2Up(GlobalConfig.regNum) bits)
  val writeData = out Bits(GlobalConfig.dataBitsWidth)
  val op = out Bits( 3 bits)     // 运算类型
  val opSel = out Bits(8 bits) //运算子类型

  val loadStoreAddr = out Bits(GlobalConfig.dataBitsWidth) // 用于存放load store指令中，计算完的地址
}

class EX extends Component{
  val lastStage= new IDOut().flip()
  val exOut = new EXOut
  val hi = Reg(Bits(GlobalConfig.dataBitsWidth)).init(0)
  val lo = Reg(Bits(GlobalConfig.dataBitsWidth)).init(0)
  exOut.writeReg := lastStage.writeReg
  exOut.writeRegAddr := lastStage.writeRegAddr
  exOut.writeData := 0
  exOut.loadStoreAddr := 0

  exOut.op := lastStage.op // 继续往Mem传
  exOut.opSel := lastStage.opSel

  //val LOGIC = for ((opsel,func) <- OPLogic.funcs)
    //yield opsel.asBits.resize(lastStage.opSel.getWidth)->func(lastStage.opRnd1,lastStage.opRnd2)
/*
  exOut.writeData := lastStage.op.mux(
    OpEnum.ALU.asBits.resize(lastStage.op.getWidth)->lastStage.opSel.muxList(ALU),
    OpEnum.LOGIC.asBits.resize(lastStage.op.getWidth)->lastStage.opSel.muxList(LOGIC)
  )
  */
  val inst=INST(lastStage.inst)
  when(lastStage.op === OpEnum.LOAD.asBits.resized || lastStage.op === OpEnum.STORE.asBits.resized){
    exOut.loadStoreAddr := (lastStage.opRnd1.asSInt + inst.immI.asSInt.resize(GlobalConfig.dataBitsWidth)).asBits
    exOut.writeData := lastStage.opRnd2 // writeData此时存放着store要写入内存的值，如果是load，则此值无意义

  }otherwise {
    for (i <- OpEnum.OPs) {
      when(lastStage.op === i._1.asBits.resize(lastStage.op.getWidth)) {
        for ((opsel, func) <- i._2.funcs) {
          when(lastStage.opSel === opsel.asBits.resize(lastStage.opSel.getWidth)) {
            if (opsel == OPArith.MUL || opsel == OPArith.MULU) {
              hi := func(lastStage.opRnd1, lastStage.opRnd2).takeHigh(GlobalConfig.dataBitsWidth.value)
              lo := func(lastStage.opRnd1, lastStage.opRnd2).take(GlobalConfig.dataBitsWidth.value)
            } else {
              exOut.writeData := func(lastStage.opRnd1, lastStage.opRnd2)
            }
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

  val ramPort = Ram.masterPort(GlobalConfig.ramRegNum)

  ramPort.writeData := B(0).resized
  ramPort.writeEn := False
  ramPort.addr := lastStage.loadStoreAddr.resized
  ramPort.operateType := Ram.RamOperateType.WORD

  memOut.writeReg := lastStage.writeReg
  memOut.writeRegAddr := lastStage.writeRegAddr
  memOut.writeData := lastStage.writeData
  when(lastStage.op === OpEnum.LOAD.asBits.resized){
    //ramPort.addr := lastStage.loadStoreAddr.resized
    val rawData = ramPort.readData
    // 这里判断一下读取数据的符号位如何拓展
    when(lastStage.opSel === OPLoad.LOADHWORD.asBits.resized){
      memOut.writeData := rawData.take(16).asSInt.resize(32 bits).asBits
    }elsewhen(lastStage.opSel === OPLoad.LOADBYTE.asBits.resized){
      memOut.writeData := rawData.take(8).asSInt.resize(32 bits).asBits
    }otherwise{
      memOut.writeData := ramPort.readData //默认都是无符号拓展，因此直接赋值即可
    }

    ramPort.operateType assignFromBits lastStage.opSel.mux(
      OPLoad.LOADWORD.asBits.resized  -> Ram.RamOperateType.WORD.asBits,
      OPLoad.LOADHWORD.asBits.resized  -> Ram.RamOperateType.HWORD.asBits,
      OPLoad.LOADHWORDU.asBits.resized  -> Ram.RamOperateType.HWORD.asBits,
      OPLoad.LOADBYTE.asBits.resized   -> Ram.RamOperateType.BYTE.asBits,
      OPLoad.LOADBYTEU.asBits.resized   -> Ram.RamOperateType.BYTE.asBits,
      default->Ram.RamOperateType.WORD.asBits
    )
  }elsewhen(lastStage.op===OpEnum.STORE.asBits.resized){
    ramPort.writeData := lastStage.writeData
    ramPort.writeEn := True
    ramPort.operateType assignFromBits lastStage.opSel.mux(
      OPStore.STOREBYTE.asBits.resized  -> Ram.RamOperateType.BYTE.asBits,
      OPStore.STOREHWORD.asBits.resized  -> Ram.RamOperateType.HWORD.asBits,
      OPStore.STOREWORD.asBits.resized  -> Ram.RamOperateType.WORD.asBits,
      default->Ram.RamOperateType.WORD.asBits
    )
  }
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
