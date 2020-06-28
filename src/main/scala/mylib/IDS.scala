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
    SRAV->0x7
  )
}

object InstOPEnum extends SpinalEnum{  // 指令操作码枚举
  val ORI,ANDI,XORI,ADDI,ADDIU,SLTI,SLTIU= newElement()

  defaultEncoding = SpinalEnumEncoding("static")(
    ORI -> 0xD ,// 001101
    ANDI -> 0xC,
    XORI ->0xE,
    ADDI ->0x8,
    ADDIU->0x9,
    SLTI -> 0xA,
    SLTIU->0xB
  )
}

object OpEnum extends SpinalEnum{
  val LOGIC,ALU = newElement()
  val funcs = List(
    (LOGIC,OPLogic.caculate _),
    (ALU,OPArith.caculate _)
  )

  def caculate(op:Bits,opsel:Bits,oprnd1:Bits,oprnd2:Bits,left:Bits): Unit ={
    for (i<- funcs){
      when(op===i._1.asBits.resized){
        i._2(opsel,oprnd1,oprnd2,left)
      }
    }
  }
}

trait OPWithFunc{
  val funcs:List[(SpinalEnumElement[_],(Bits,Bits)=>Bits)]

  def caculate(opsel:Bits,oprnd1:Bits,oprnd2:Bits,left:Bits)={
    for(i <- funcs){
      when(opsel === i._1.asBits.resized){
        left := i._2(oprnd1,oprnd2)
      }
    }
  }

}

object OPArith extends SpinalEnum with OPWithFunc{
  val ADDU,SUBU = newElement()
  val SLTI,SLTIU = newElement()

  val funcs = List(
    (ADDU,(a:Bits,b:Bits)=> (a.asUInt + b.asUInt).asBits),
    (SUBU,(a:Bits,b:Bits)=> (a.asUInt - b.asUInt).asBits),
    // SLTI => Source Less than Immediate
    (SLTIU,(a:Bits,b:Bits)=> (a.asUInt < b.asUInt)?B(1,32 bits)|B(0)),
    (SLTI,(a:Bits,b:Bits) => (a.asSInt < b.asSInt)?B(1,32 bits)|B(0))
  )
}

object OPLogic extends SpinalEnum with OPWithFunc {
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

  def getInstType(inst:Bits): SpinalEnumCraft[InstTypeEnum.type] = {
    (OPof(inst)===B(0,6 bits) || (OPof(inst)===B("6'b011100")))?
      InstTypeEnum.R | InstTypeEnum.I
  }

  val instsI = List(
    new InstI(InstOPEnum.ORI,OpEnum.LOGIC,OPLogic.OR),
    new InstI(InstOPEnum.ANDI,OpEnum.LOGIC,OPLogic.AND),
    new InstI(InstOPEnum.XORI,OpEnum.LOGIC,OPLogic.XOR),
    new InstI(InstOPEnum.ADDIU,OpEnum.ALU,OPArith.ADDU),
    new InstI(InstOPEnum.SLTI,OpEnum.ALU,OPArith.SLTI),
    new InstI(InstOPEnum.SLTIU,OpEnum.ALU,OPArith.SLTIU)
  )

  val instsR = List(
    new InstR(InstFUNCEnum.AND,OpEnum.LOGIC,OPLogic.AND),
    new InstR(InstFUNCEnum.OR,OpEnum.LOGIC,OPLogic.OR)
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

  val instFUNC = arr(0).asBits
  val decodeOP = arr(1).asBits
  val deCodeOpSel = arr(2).asBits
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

  //决定立即数的符号位拓展
  val imm:Bits = (idOut.op === OpEnum.LOGIC.asBits.resize(idOut.op.getWidth))?
    lastStage.inst.take(16).resize(GlobalConfig.dataBitsWidth)|
    lastStage.inst.take(16).asSInt.resize(GlobalConfig.dataBitsWidth).asBits


  for(i <- idOut.elements){
    if(i._1 == "writeReg"){
      i._2 := False
    }
    else {
      i._2 := B(0)
    }
  }

  regHeap.readAddrs(0) := 0
  regHeap.readAddrs(1) := 0
  regHeap.readEns(0) := False
  regHeap.readEns(1) := False

  when(IDS.getInstType(lastStage.inst)===InstTypeEnum.I){
    val targetReg= lastStage.inst(16 to 20)
    val sourceReg = lastStage.inst(21 to 25)
    val instOp = IDS.OPof(lastStage.inst)

    for (i<- IDS.instsI){
      when(i.instOP.asBits.resize(instOp.getWidth)===instOp){
        idOut.op := i.decodeOP.asBits.resized
        idOut.opSel := i.decodeOPSel.asBits.resized
      }
    }

    idOut.writeRegAddr := targetReg
    idOut.writeReg := True
    regHeap.readEns(0) := True
    regHeap.readEns(1) := False
    regHeap.readAddrs(0) :=sourceReg
  }elsewhen(IDS.getInstType(lastStage.inst)===InstTypeEnum.R){
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
    idOut.writeRegAddr := destinationReg
    idOut.writeReg := True
    regHeap.readEns(0) := True
    regHeap.readEns(1) := True
    regHeap.readAddrs(0) :=sourceReg
    regHeap.readAddrs(1) :=targetReg
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

}
