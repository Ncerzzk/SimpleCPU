package mylib

import spinal.core._

trait Instruction{
  //val insts = List[(Stageable[_ <: BaseType],Any)]()
  val insts:List[(MaskedLiteral, Seq[(Stageable[_ <: BaseType], Any)])]
  val actions:List[(Stageable[_ <: BaseType], Any)]

  def build(cpu: CPU2)={}
}

object Instructions{
  var instsAll = List[(MaskedLiteral, Seq[(Stageable[_ <: BaseType], Any)])]()


  def apply[T <: Instruction](cpu:CPU2,instructions:T*)= {
    for(i <- instructions) {
      i.build(cpu);
        instsAll ++= i.insts
      }
    instsAll
    }
}

object IInsts extends Instruction{
  import GlobalConfig._

  val actions: List[(Stageable[_ <: BaseType], Any)] = List[(Stageable[_ <: BaseType],Any)](
    WRITE_REG           -> True,
    READ_REG0            ->True,
    READ_REG1            ->False,
    OPRND1_SRC          ->OPrndSource.REG,
    OPRND2_SRC          ->OPrndSource.IMM
  )

  val insts: List[(MaskedLiteral, Seq[(Stageable[_ <: BaseType], Any)])] = List (
    ANDI->(actions++List(OP->OpEnum.LOGIC,OPSEL->OPLogic.AND)),
    ADDI->(actions++List(OP->OpEnum.ALU,OPSEL->OPArith.ADDU))
  )

  override def build(cpu: CPU2): Unit = {
    import cpu._
    import EX._
    EX plug new Area{
      for(i <- insts){
        i._1
      }
      when(input(OP)===OpEnum.ALU.asBits.resize(OP.getBitsWidth)){
        output(WRITE_REG_DATA) := (input(OPRND1).asUInt + input(OPRND2).asUInt).asBits
      }
    }
  }
}

object RInsts extends Instruction{
  import GlobalConfig._

  val actions: List[(Stageable[_ <: BaseType], Any)] = List[(Stageable[_ <: BaseType],Any)](
    WRITE_REG           -> True,
    READ_REG0            ->True,
    READ_REG1            ->True,
    OPRND1_SRC          ->OPrndSource.REG,
    OPRND2_SRC          ->OPrndSource.REG
  )

  val insts = List(
    ADD ->(actions++List(OP->OpEnum.ALU,OPSEL->OPArith.ADDU))
  )
}
