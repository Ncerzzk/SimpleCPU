//package mylib

import scala.collection.mutable

object interpreter {

  MIPSInst("add",     0x20,     FuncSpecial,      List(D,S,T)       )
  MIPSInst("addu",    0x21,     FuncSpecial,      List(D,S,T)       )
  MIPSInst("and",     0x24,     FuncSpecial,      List(D,S,T)       )

  MIPSInst("addi",    0x8,      OPSpecial,        List(T,S,I)       )
  MIPSInst("addiu",   0x9,      OPSpecial,        List(T,S,I)       )
  MIPSInst("andi",    0xc,      OPSpecial,        List(T,S,I)       )
  MIPSInst("ori",     0xD,      OPSpecial,        List(T,S,I)       )
  MIPSInst("SLTIU",   0xB,      OPSpecial,        List(T,S,I)       )
  MIPSInst("SLTI",    0xA,      OPSpecial,        List(T,S,I)       )
  MIPSInst("xori",    0xE,      OPSpecial,        List(T,S,I)       )


  MIPSInst("j",       0x2,      OPSpecial,        List(I2)          )
  MIPSInst("beq",     0x4,      OPSpecial,        List(S,T,I2)      )
  MIPSInst("bgtz",    0x7,      OPSpecial,        List(S,T,I2)      )    // 这个和别的汇编器可能不太一样
  MIPSInst("blez",    0x6,      OPSpecial,        List(S,T,I2)      )
  MIPSInst("bne",     0x5,      OPSpecial,        List(S,T,I2)      )

/*
  def apply(str:String): Iterator[String] ={
    for(line:String <- str.lines) yield singleInst(line)
  }
*/
  def regToNum(str:String): Int =str.replace("$","").trim.toInt

  def singleInst(str:String) ={
    val strSplits = str.split(",")
    val instName = strSplits(0).split(" ")(0)
    val argumentArr = for(i<-strSplits) yield {
      regToNum(i.replace(instName,""))
    }
    val inst = MIPSInst.all(instName.toLowerCase())
    assert(inst.oprndTypes.length==strSplits.length)
    var result:Int=0
    for(i <- inst.oprndTypes.indices){
      val oprndtype = inst.oprndTypes(i)
      val arg = argumentArr(i)
      result |=  (if (oprndtype.offset>=0) arg<<oprndtype.offset else arg >> -oprndtype.offset)
    }
    if(inst.OPorFUNC==OPSpecial){
      result |= inst.specialCode<<26
    }else{
      result |= inst.specialCode
    }
    result.toHexString
  }

  def main(args: Array[String]): Unit = {
    assert(singleInst("addu $1,$0,$2")=="20821")
    assert(singleInst("add $1,$0,$2")=="20820")
    assert(singleInst("addi $1,$0,1000")=="200103e8")
    assert(singleInst("addiu $1,$0,1000")=="240103e8")
    assert(singleInst("beq $1,$0,28")=="10200007")
  }
}


trait Special
object OPSpecial extends Special
object FuncSpecial extends Special


case class OPrndType(offset:Int,length:Int)
object D extends OPrndType(11,5)
object T extends OPrndType(16,5)
object S extends OPrndType(21,5)
object A extends OPrndType(6,5)
object I extends OPrndType(0,16)
object I2 extends OPrndType(-2,26)  //需要/4 因此>>2位


object MIPSInst{
  val all = mutable.HashMap[String,MIPSInst]()
}
case class MIPSInst(inst:String,specialCode:Int,OPorFUNC:Special,oprndTypes:List[OPrndType]){
  MIPSInst.all(inst) = this

  def isRInst= OPorFUNC == FuncSpecial
  def isJInst= inst.indexOf("j")==0
  def isBinst= inst.indexOf("b")==0
  def isIinst= OPorFUNC==OPorFUNC && (!isBinst) && (!isJInst)
}
