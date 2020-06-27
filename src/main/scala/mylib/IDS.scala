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
  // R型指令的高6位为0，靠低6位区分功能
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
  val EXEORI,EXEANDI,EXEXORI= newElement()

  defaultEncoding = SpinalEnumEncoding("static")(
    EXEORI -> 0xD ,// 001101
    EXEANDI -> 0xC,
    EXEXORI ->0xE
  )
}

object OpEnum extends SpinalEnum{
  val LOGIC,ALU = newElement()
  val funcs = List(
    (LOGIC,OPLogic.caculate _)
  )

  def caculate(op:Bits,opsel:Bits,oprnd1:Bits,oprnd2:Bits,left:Bits): Unit ={
    for (i<- funcs){
      when(op===i._1.asBits.resized){
        i._2(opsel,oprnd1,oprnd2,left)
      }
    }
  }
}
object OPArith extends SpinalEnum{
  val
}

object OPLogic extends SpinalEnum{
  val OR,AND,XOR = newElement()

  val funcs = List(
    (OR,(a:Bits,b:Bits)=> a | b),
    (AND,(a:Bits,b:Bits)=>a & b),
    (XOR,(a:Bits,b:Bits)=>a ^ b)
  )

  def caculate(opsel:Bits,oprnd1:Bits,oprnd2:Bits,left:Bits)={
    for(i <- funcs){
      when(opsel === i._1.asBits.resized){
        left := i._2(oprnd1,oprnd2)
      }
    }
  }

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
  def getInstType(inst:Bits): SpinalEnumCraft[InstTypeEnum.type] = (inst.takeHigh(6)===B(0,6 bits))?InstTypeEnum.R|InstTypeEnum.I
  //val temp =new Inst(InstEnum.EXEORI,OpEnum.LOGIC,OpLogic.OR)
  val instsI = List(
    new InstI(InstOPEnum.EXEORI,OpEnum.LOGIC,OPLogic.OR),
    new InstI(InstOPEnum.EXEANDI,OpEnum.LOGIC,OPLogic.AND),
    new InstI(InstOPEnum.EXEXORI,OpEnum.LOGIC,OPLogic.XOR)
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

  val op =lastStage.inst.takeHigh(6)
  val op2 = lastStage.inst(6 to 10)
  val op3 = lastStage.inst.take(6)
  val op4 = lastStage.inst(16 to 20)

  val imm = B(0,16 bits) ## lastStage.inst.take(16)    // 立即数
  lastStage.inst.resizeLeft(5)

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

  regHeap.readAddrs(0) := 0
  regHeap.readAddrs(1) := 0
  regHeap.readEns(0) := False
  regHeap.readEns(1) := False

  when(IDS.getInstType(lastStage.inst)===InstTypeEnum.I){
    var targetReg= lastStage.inst(16 to 20)
    var sourceReg = lastStage.inst(21 to 25)
    val instOp = lastStage.inst.takeHigh(6)

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
  }/*elsewhen(IDS.getInstType(lastStage.inst)===InstTypeEnum.R){
    var targetReg= lastStage.inst(16 to 20)
  }*/


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

