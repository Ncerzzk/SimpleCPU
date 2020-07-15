package mylib

import spinal.core._
import spinal.lib.{master, slave}


class MEMOut extends Bundle{
  val writeReg = out Bool
  val writeRegAddr = out Bits(log2Up(GlobalConfig.regNum) bits)
  val writeData = out Bits(GlobalConfig.dataBitsWidth)

  val writeRegType = out(RegWriteType())
}

class MEM extends Component{
  val lastStage = new EXOut().flip()

  //val memOut = new MEMOut

  val ramPort = Ram.masterPort(GlobalConfig.ramRegNum)

  val memOut= master(new RegHeapWritePort)

  def <>(regHeap: RegHeap)={
    memOut <> regHeap.writePort
  }

  ramPort.writeData := B(0).resized
  ramPort.writeEn := False
  ramPort.addr := lastStage.loadStoreAddr.resized
  ramPort.operateType := Ram.RamOperateType.WORD

  memOut.writeEn := lastStage.writeReg
  memOut.writeAddr := lastStage.writeRegAddr
  memOut.writeData := lastStage.writeData
  memOut.writeType := lastStage.writeRegType  // 继续往WB传

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
