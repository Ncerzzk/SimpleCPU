package mylib

import mylib.OPArith.{ADDU, MUL, MULU, SLT, SLTU, SUBU}
import spinal.core._
import spinal.lib.{master, slave}
import spinal.lib.fsm._

class Divider(bitNum:Int) extends Component {

  val io = new Bundle{
    val dividend = in UInt(bitNum bits)
    val divisor = in UInt(bitNum bits)
    val en = in Bool
    val sign = in Bool

    val quotient = out UInt(bitNum bits)
    val remainder = out UInt (bitNum bits)
    val ok = out Bool
    val busy = out Bool
  }

  io.ok:=False
  io.quotient := U(0)
  io.remainder := U(0)
  io.busy := False

  val remQuoReg = Reg(UInt(bitNum*2 bits)).init(0)

  val fsm = new StateMachine{
    val idle:State = new State with EntryPoint{
      whenIsActive{
        when(io.en){
          goto(caculting)
        }
      }
    }
    val caculting:StateDelay = new StateDelay(bitNum+1){
      onEntry{
        remQuoReg := io.dividend.resized
      }
      whenCompleted{
        io.quotient := remQuoReg.asBits.take(bitNum).asUInt
        io.remainder := remQuoReg.asBits.takeHigh(bitNum).asUInt
        io.ok := True
        goto(idle)
      }
      onExit{
        remQuoReg := U(0)
      }
    }
    caculting.whenIsActive{
      val a = remQuoReg |<<1
      val high = a.asBits.takeHigh(bitNum).asUInt
      val subResult=high - io.divisor
      io.busy := True
      when(subResult.msb===False){
        remQuoReg := (subResult.asBits ## (a.asBits.take(bitNum) | B(1,bitNum bits))).asUInt
      }otherwise{
        remQuoReg := a
      }
    }
  }
}

class EXOut extends Bundle with DefaultValue {
  val writeReg = out Bool
  val writeRegAddr = out Bits(log2Up(GlobalConfig.regNum) bits)
  val writeData = out Bits(GlobalConfig.dataBitsWidth)
  val op = out Bits( 3 bits)     // 运算类型
  val opSel = out Bits(8 bits) //运算子类型

  val loadStoreAddr = out Bits(GlobalConfig.dataBitsWidth) // 用于存放load store指令中，计算完的地址

  val writeRegType = out(RegWriteType())          // 增加用于 Loadhi loadlo
}

class EX extends Component{
  val lastStage= new IDOut().flip()
  val exOut = new EXOut

  val backToID = new Bundle{
    val nowExOp = out Bits(lastStage.op.getBitsWidth bits)
    val nowExOpSel = out Bits(lastStage.opSel.getBitsWidth bits)
    val writeRegAddr = out Bits(lastStage.writeRegAddr.getBitsWidth bits)
  }

  val hi = Reg(Bits(GlobalConfig.dataBitsWidth)).init(0)
  val lo = Reg(Bits(GlobalConfig.dataBitsWidth)).init(0)

  val ex2memBack = new EXOut().flip()
  val regBack = slave(new RegHeapWritePort)
  val pcPort = master(new PCPort)
  val reqCTRL: StageCTRLReqBundle = master(new StageCTRLReqBundle)

  val oprnd1:Bits = Bits(GlobalConfig.dataBitsWidth)
  val oprnd2:Bits = Bits(GlobalConfig.dataBitsWidth)

  val divider = new Divider(GlobalConfig.dataBitsWidth.value)

  divider.io.dividend := oprnd1.asUInt
  divider.io.divisor := oprnd2.asUInt
  divider.io.en := lastStage.divEn
  divider.io.sign := False

  pcPort.setDefaultValue(pcPort.writeEN,pcPort.writeData)
  reqCTRL.req := StageCTRLReqEnum.NORMAL

  backToID.nowExOp := lastStage.op
  backToID.nowExOpSel := lastStage.opSel
  backToID.writeRegAddr := lastStage.writeRegAddr

  def <>(pc:PC): Unit  = pcPort <> pc.writePort
  def <>(ex2mem:Stage[EXOut]): Unit ={
    ex2mem.right <> ex2memBack
  }

  def <>(reg:RegHeap): Unit ={
    regBack <> reg.bypassBack
  }


  {
    import exOut._
    exOut.setDefaultValue(writeData,loadStoreAddr,writeRegType)
  }
  exOut.writeReg := lastStage.writeReg
  exOut.writeRegAddr := lastStage.writeRegAddr
  exOut.op := lastStage.op // 继续往Mem传
  exOut.opSel := lastStage.opSel



  oprnd1 := lastStage.opRnd1
  oprnd2 := lastStage.opRnd2
  when(lastStage.readEN0){
    when(ex2memBack.writeReg && ex2memBack.writeRegAddr===lastStage.readAddr0){
      oprnd1 := ex2memBack.writeData
    }elsewhen(regBack.writeEn && regBack.writeAddr===lastStage.readAddr0){
      oprnd1 := regBack.writeData
    }
  }

  when(lastStage.readEN1){
    when(ex2memBack.writeReg && ex2memBack.writeRegAddr===lastStage.readAddr1){
      oprnd2 := ex2memBack.writeData
    }elsewhen(regBack.writeEn && regBack.writeAddr===lastStage.readAddr1){
      oprnd2 := regBack.writeData
    }
  }

  val inst=INST(lastStage.inst)
  when(lastStage.op === OpEnum.LOAD.asBits.resized || lastStage.op === OpEnum.STORE.asBits.resized){
    exOut.loadStoreAddr := (oprnd1.asSInt + inst.immI.asSInt.resize(GlobalConfig.dataBitsWidth)).asBits
    exOut.writeData := oprnd2 // writeData此时存放着store要写入内存的值，如果是load，则此值无意义
    when(lastStage.opSel === OPLoad.LOADHI.asBits.resized){
      exOut.writeRegType := RegWriteType.HIGH_HALF
    }elsewhen(lastStage.opSel === OPLoad.LOADLO.asBits.resized){
      exOut.writeRegType := RegWriteType.LOW_HALF
    }elsewhen(lastStage.opSel === OPLoad.MFHI.asBits.resized){
      exOut.writeData := hi
    }elsewhen(lastStage.opSel === OPLoad.MFLO.asBits.resized){
      exOut.writeData := lo
    }elsewhen(lastStage.opSel === OPLoad.MTHI.asBits.resized){
      hi := oprnd1
    }elsewhen(lastStage.opSel === OPLoad.MTLO.asBits.resized){
      lo := oprnd1
    }
  }otherwise {
    for (i <- OpEnum.OPs) {
      when(lastStage.op === i._1.asBits.resize(lastStage.op.getWidth)) {
        for ((opsel, func) <- i._2.funcs) {
          when(lastStage.opSel === opsel.asBits.resize(lastStage.opSel.getWidth)) {
            if (opsel == OPArith.MUL || opsel == OPArith.MULU) {
              hi := func(oprnd1, oprnd2).asInstanceOf[Bits].takeHigh(GlobalConfig.dataBitsWidth.value)
              lo := func(oprnd1, oprnd2).asInstanceOf[Bits].take(GlobalConfig.dataBitsWidth.value)
            }else if(opsel == OPArith.DIV || opsel == OPArith.DIVU){
              when(divider.io.ok){
                hi := divider.io.quotient.asBits
                lo := divider.io.remainder.asBits
              }otherwise{
                reqCTRL.req := StageCTRLReqEnum.EXSTALL
              }
            }
            else if(i._1 == OpEnum.BRANCH) {
              when(func(oprnd1,oprnd2).asInstanceOf[Bool]){
                val target= (inst.immI.asSInt.resize(GlobalConfig.dataBitsWidth)+lastStage.pc.asSInt+1).asBits
                pcPort.JMP(target)
                reqCTRL.req := StageCTRLReqEnum.IFFLUSH
              }
            }else{
              exOut.writeData := func(oprnd1, oprnd2).asInstanceOf[Bits]
            }
          }
        }
      }
    }
  }


  //OpEnum.caculate(lastStage.op,lastStage.opSel,oprnd1,lastStage.opRnd2,exOut.writeData)

}


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
/*
class WB extends Component{
  val lastStage= new MEMOut().flip()

  val wbOut= master(new RegHeapWritePort)

  wbOut.writeAddr := lastStage.writeRegAddr
  wbOut.writeData := lastStage.writeData
  wbOut.writeEn := lastStage.writeReg
  wbOut.writeType := lastStage.writeRegType

  def <>(regHeap: RegHeap)={
    wbOut <> regHeap.writePort
  }
}
*/