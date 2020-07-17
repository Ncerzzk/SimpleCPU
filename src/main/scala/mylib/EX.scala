package mylib

import mylib.OPArith.{ADDU, MUL, MULU, SLT, SLTU, SUBU}
import spinal.core._
import spinal.lib.{master, slave}
import spinal.lib.fsm._


class EXOut extends Bundle with DefaultValue {
  val writeReg = out Bool
  val writeRegAddr = out Bits(log2Up(GlobalConfig.regNum) bits)
  val writeData = out Bits(GlobalConfig.dataBitsWidth)
  val op = out Bits( 3 bits)     // 运算类型
  val opSel = out Bits(8 bits) //运算子类型

  val loadStoreAddr = out Bits(GlobalConfig.dataBitsWidth) // 用于存放load store指令中，计算完的地址

  val writeRegType = out(RegWriteType())          // 增加用于 Loadhi loadlo

  val instIsDelaySlot = out Bool
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

  val nextInstIsDelaySlot = Reg(Bool).init(False).default(False) // 指示下一条指令是否是延时槽指令

  when(nextInstIsDelaySlot){
    exOut.instIsDelaySlot := True
  }otherwise{
    exOut.instIsDelaySlot := False
  }

  divider.io.dividend := oprnd1.asUInt
  divider.io.divisor := oprnd2.asUInt
  divider.io.en := lastStage.divEn
  divider.io.sign := False

  val divder_ok = Bool
  divder_ok :=divider.io.ok

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
              if(opsel == OPArith.DIV){
                divider.io.sign := True
              }
              when(divder_ok){
                lo := divider.io.quotient.asBits
                hi := divider.io.remainder.asBits
              }otherwise{
                reqCTRL.req := StageCTRLReqEnum.EXSTALL
              }
            }
            else if(i._1 == OpEnum.BRANCH) {
              when(func(oprnd1,oprnd2).asInstanceOf[Bool]){
                val target= (inst.immI.asSInt.resize(GlobalConfig.dataBitsWidth)+lastStage.pc.asSInt+1).asBits
                pcPort.JMP(target)
                reqCTRL.req := StageCTRLReqEnum.IFFLUSH
                nextInstIsDelaySlot := True // 指定下一条指令是延迟槽指令，在异常处理中会用到
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


