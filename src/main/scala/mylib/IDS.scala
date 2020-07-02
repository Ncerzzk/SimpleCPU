package mylib

import spinal.core._
import spinal.lib.{master, slave}

import scala.collection.mutable.ArrayBuffer

object Bin{

  def apply(src:String,res:Int=0):Int={
    src.length match {
      case 0|1 => res
      case _ => apply(src.substring(0,src.length-1), res + src.substring(0,1).toInt * (1 << src.length - 1))
    }
  }
}

object InstTypeEnum extends SpinalEnum{
  val R,I,J = newElement()
  // R型指令的高6位为0(有例外），靠低6位区分功能
  // I型指令直接靠高6位区分功能
}

object InstFUNCEnum extends SpinalEnum{ // 指令功能码枚举
  val AND,OR,XOR,NOR= newElement()
  val SLL,SRL,SRA,SLLV,SRLV,SRAV = newElement()
  val MOVN,MOVZ,MFHI,MFLO,MTHI,MTLO = newElement()
  val ADDU,SUBU,SLTU = newElement()

  val MULT,MULTU = newElement()
  defaultEncoding = SpinalEnumEncoding("static")(
    AND -> 0x24 ,
    OR -> 0x25,
    XOR ->0x26,
    NOR ->0x27,

    SLL->0x0,
    SRL->0x2,
    SRA->0x3,
    SLLV->0x4,
    SRLV->0x6,
    SRAV->0x7,

    MOVN->0xB,
    MOVZ->0xA,
    MFHI->0x10,
    MFLO->0x12,
    MTHI->0x11,
    MTLO->0x13,

    ADDU->0x21,
    SUBU->0x23,
    SLTU->0x2B,

    MULT->0x18,
    MULTU->0x19
  )
}

object InstOPEnum extends SpinalEnum{  // 指令操作码枚举
  val ORI,ANDI,XORI,ADDI,ADDIU,SLTI,SLTIU= newElement()
  val BEQ,BGTZ,BLEZ,BNE = newElement()
  defaultEncoding = SpinalEnumEncoding("static")(
    ORI -> 0xD ,// 001101
    ANDI -> 0xC,
    XORI ->0xE,
    ADDI ->0x8,
    ADDIU->0x9,
    SLTI -> 0xA,
    SLTIU->0xB,

    // 以下为分支语句
    BEQ->0x4,
    BGTZ->0x7,
    BLEZ->0x6,
    BNE->0x5
  )
}

object OpEnum extends SpinalEnum{
  val LOGIC,ALU = newElement()
  val OPs = List(
    LOGIC->OPLogic,
    ALU->OPArith
  )
}

trait withFuncs{
  val funcs:List[(SpinalEnumElement[_], (Bits, Bits) => Bits)]
}
object OPArith extends SpinalEnum with withFuncs {
  val ADDU,SUBU = newElement()
  val SLT,SLTU = newElement()
  val MULU,MUL = newElement()

  val funcs: List[(SpinalEnumElement[_], (Bits, Bits) => Bits)] = List(
    (ADDU,(a:Bits,b:Bits)=> (a.asUInt + b.asUInt).asBits),
    (SUBU,(a:Bits,b:Bits)=> (a.asUInt - b.asUInt).asBits),
    // SLTI => Source Less than Immediate
    (SLTU, (a:Bits, b:Bits)=> (a.asUInt < b.asUInt)?B(1,32 bits)|B(0)),
    (SLT, (a:Bits, b:Bits) => (a.asSInt < b.asSInt)?B(1,32 bits)|B(0)),

    (MULU,(a:Bits,b:Bits) => (a.asUInt*b.asUInt).asBits),
    (MUL,(a:Bits,b:Bits) => (a.asSInt*b.asSInt).asBits)
  )
}

object OPLogic extends SpinalEnum with withFuncs{
  val OR,AND,XOR = newElement()
  val funcs = List(
    (OR,(a:Bits,b:Bits)=> a | b),
    (AND,(a:Bits,b:Bits)=>a & b),
    (XOR,(a:Bits,b:Bits)=>a ^ b)
  )
}

class IDOut extends Bundle{
  val op = out Bits( 3 bits)     // 运算类型
  val opSel = out Bits(8 bits) //运算子类型
  val opRnd1 = out Bits(GlobalConfig.dataBitsWidth)
  val opRnd2 = out Bits(GlobalConfig.dataBitsWidth)
  val writeReg = out Bool
  val writeRegAddr = out Bits(log2Up(GlobalConfig.regNum) bits)
}


object IDS {
  def OPof(inst:Bits)= inst.takeHigh(6)
  def FUNCof(inst:Bits) = inst.take(6)
  def RSof(inst:Bits)=inst(21 to 25)
  def RTof(inst:Bits)=inst(16 to 20)

  def isJInst(inst:Bits): Bool = (OPof(inst)===B("6'b000010")) || (OPof(inst)=== B("6'b000011"))

  def isIRInst(inst:Bits):Bool = OPof(inst)=/=B(0,6 bits) && (~isJInst(inst))

  def isRInst(inst:Bits):Bool={
    val op = OPof(inst)
    val l = List(InstOPEnum.BEQ,InstOPEnum.BLEZ,InstOPEnum.BGTZ,InstOPEnum.BNE)
    var result :Bool = False
    val newL= for(i <- l) yield i.asBits.resize(op.getWidth) === op
    for(i <-newL){
      result = result|i
    }
    result
  }

  val instsI = List(
    new InstI(InstOPEnum.ORI,OpEnum.LOGIC,OPLogic.OR),
    new InstI(InstOPEnum.ANDI,OpEnum.LOGIC,OPLogic.AND),
    new InstI(InstOPEnum.XORI,OpEnum.LOGIC,OPLogic.XOR),
    new InstI(InstOPEnum.ADDIU,OpEnum.ALU,OPArith.ADDU),
    new InstI(InstOPEnum.SLTI,OpEnum.ALU,OPArith.SLT),
    new InstI(InstOPEnum.SLTIU,OpEnum.ALU,OPArith.SLTU)
  )

  val instsR = List(
    new InstR(InstFUNCEnum.AND,OpEnum.LOGIC,OPLogic.AND),
    new InstR(InstFUNCEnum.OR,OpEnum.LOGIC,OPLogic.OR),
    new InstR(InstFUNCEnum.ADDU,OpEnum.ALU,OPArith.ADDU),
    new InstR(InstFUNCEnum.SUBU,OpEnum.ALU,OPArith.SUBU),
    new InstR(InstFUNCEnum.SLTU,OpEnum.ALU,OPArith.SLTU),
    new InstR(InstFUNCEnum.MULT,OpEnum.ALU,OPArith.MUL),
    new InstR(InstFUNCEnum.MULTU,OpEnum.ALU,OPArith.MULU)
  )

  type getRsFuncType= Bits=>Bits
  type getRtFuncType= Bits=>Bits
  val reg0=()=>B(0,6 bits).clone()
  val instsB = List(
  // 指令OP，操作数1来源，操作数2来源，转移分支的条件
    (InstOPEnum.BEQ, (inst:Bits)=>RSof(inst),(inst:Bits)=>RTof(inst), (a:Bits,b:Bits)=> a === b),
    (InstOPEnum.BGTZ,(inst:Bits)=>RSof(inst),(inst:Bits)=>reg0(),     (a:Bits,b:Bits)=> a.asSInt > b.asSInt),
    (InstOPEnum.BLEZ,(inst:Bits)=>RSof(inst),(inst:Bits)=>reg0(),     (a:Bits,b:Bits)=> a.asSInt <= b.asSInt),
    (InstOPEnum.BNE,(inst:Bits)=>RSof(inst),(inst:Bits)=>RTof(inst),  (a:Bits,b:Bits)=> a =/= b)
  )
}

class InstI(s:SpinalEnumElement[_]*){  // I型指令类
  val arr= s.toList
  assert(arr.length==3)
  var instOP = arr(0).asBits   // 指令的指令码，与MIPS指令集相关
  val decodeOP = arr(1).asBits  // 译码后的指令，与CPU实现相关，即OpEnum中的值
  val decodeOPSel = arr(2).asBits // 译码后的指令子功能码，与CPU实现相关
}

class InstR(s:SpinalEnumElement[_]*){  // R型指令类
  val arr=s.toList
  assert(arr.length==3)
  val instFUNC = arr(0).asBits
  val decodeOP = arr(1).asBits
  val deCodeOpSel = arr(2).asBits
}

class ID extends Component{

  val regHeap: RegHeapReadPort = master(new RegHeapReadPort)

  val exBack: EXOut = new EXOut().flip()
  val memBack: MEMOut = new MEMOut().flip()
  val wbBack: RegHeapWritePort = slave(new RegHeapWritePort)

  val pcPort: PCPort = master(new PCPort)

  val reqCTRL: StageCTRLBundle = master(new StageCTRLBundle)

  def <>(regs: RegHeap): Unit = regHeap <> regs.readPort
  def <>(ex:EX): Unit =exBack <> ex.exOut
  def <>(mem:MEM): Unit  = memBack <> mem.memOut
  def <>(wb:WB): Unit  = wbBack <> wb.wbOut
  def <>(pc:PC): Unit  = pcPort <> pc.writePort


  val lastStage: IFOut = new IFOut().flip()

  val idOut= new IDOut

  //决定立即数的符号位拓展
  val imm:Bits = (idOut.op === OpEnum.LOGIC.asBits.resize(idOut.op.getWidth))?
    lastStage.inst.take(16).resize(GlobalConfig.dataBitsWidth)|
    lastStage.inst.take(16).asSInt.resize(GlobalConfig.dataBitsWidth).asBits

  idOut.elements.foreach(a=>{
    a._2 := (if(a._1 =="writeReg") False else B(0)) }
  )

  reqCTRL.stateOut := StageStateEnum.ENABLE

  pcPort.writeEN :=False
  pcPort.writeData := 0

  regHeap.readAddrs(0) := 0
  regHeap.readAddrs(1) := 0
  regHeap.readEns(0) := False
  regHeap.readEns(1) := False

  when(IDS.isIRInst(lastStage.inst)) {
    val targetReg = lastStage.inst(16 to 20)
    val sourceReg = lastStage.inst(21 to 25)
    val instOp = IDS.OPof(lastStage.inst)

    when(IDS.isRInst(lastStage.inst)){
      val offset = lastStage.inst.take(16)
      for(i <- IDS.instsB){
        when(instOp === i._1.asBits.resize(instOp.getWidth)){  // 确定了指令
          val rs = i._2(lastStage.inst)
          val rt = i._3(lastStage.inst)
          regHeap.readAddrs(0) := rs.resized
          regHeap.readAddrs(1) := rt.resized
          when(i._4(idOut.opRnd1,idOut.opRnd2)){
            val newPC = offset.asSInt.resize(GlobalConfig.dataBitsWidth)+lastStage.pc.asSInt+1
            pcPort.writeEN := True
            pcPort.writeData := newPC.asBits
          }
        }
      }
      //idOut.writeRegAddr := targetReg
      idOut.writeReg := False
      regHeap.readEns(0) := True
      regHeap.readEns(1) := True
    }otherwise{
      for (i <- IDS.instsI) {
        when(i.instOP.asBits.resize(instOp.getWidth) === instOp) {
          idOut.op := i.decodeOP.resized
          idOut.opSel := i.decodeOPSel.resized
        }
      }
      idOut.writeRegAddr := targetReg
      idOut.writeReg := True
      regHeap.readEns(0) := True
      regHeap.readEns(1) := False
      regHeap.readAddrs(0) := sourceReg
    }

  }elsewhen(IDS.isJInst(lastStage.inst)){
    val targetAddress = lastStage.inst.take(26)
    val newPC =  (lastStage.pc.asUInt+1).asBits.takeHigh(6) ## targetAddress
    pcPort.writeEN := True
    pcPort.writeData := newPC
    //reqCTRL.stateOut := StageStateEnum.FLUSH
/*
    when(IDS.OPof(lastStage.inst).take(1) === B(1,1 bit)){
      idOut.writeReg :=True
      idOut.writeRegAddr := (lastStage.pc.asUInt+1).asBits
    }
*/

  }otherwise{
    val targetReg= lastStage.inst(16 to 20)  //rt
    val sourceReg= lastStage.inst(21 to 25)  //rs
    val destinationReg = lastStage.inst(11 to 15)  //rd

    val FUNC = IDS.FUNCof(lastStage.inst)
    for(i<- IDS.instsR){
      when(FUNC === i.instFUNC.resized){
        idOut.op := i.decodeOP.resized
        idOut.opSel := i.deCodeOpSel.resized
      }
    }
    // TODO：
    // 有些指令如MOVN，最终未必会写入寄存器
    idOut.writeRegAddr := destinationReg
    idOut.writeReg := True
    regHeap.readEns(0) := True
    regHeap.readEns(1) := True
    regHeap.readAddrs(0) :=sourceReg
    regHeap.readAddrs(1) :=targetReg
  }


  var i = 0;
  for( rnd <- List(idOut.opRnd1,idOut.opRnd2)){
    // TODO:
    // 需要考虑，会有一些指令最后并没有写入寄存器，因此如果有这种情况，并不能使用这些指令的结果
    // 还要考虑，如果指令往$0写数据，那么这个数据也是不能用的
    when(regHeap.readEns(i)){
      rnd := regHeap.readDatas(i)
      when(exBack.writeReg && exBack.writeRegAddr===regHeap.readAddrs(i)){
        rnd := exBack.writeData
      }
      when(memBack.writeReg && memBack.writeRegAddr===regHeap.readAddrs(i)){
        rnd := memBack.writeData
      }
      when(wbBack.writeEn && wbBack.writeAddr===regHeap.readAddrs(i)){
        rnd := wbBack.writeData
      }
      /*
      rnd := regHeap.readAddrs(i).mux(
        exBack.writeRegAddr -> exBack.writeData,
        memBack.writeRegAddr -> memBack.writeData,
        wbBack.writeAddr -> wbBack.writeData,
        default ->regHeap.readDatas(i)
      )
       */
    }otherwise{
      rnd := imm
    }
    i+=1
  }

}

