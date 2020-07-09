package mylib

import spinal.core._

import scala.collection.immutable
import scala.collection.immutable.HashMap
trait Actions
trait Arguments

object READ_REG0 extends Actions //  注意，如果READ_REGX为FALSE,则会默认使用Imm来替代(代码在ID的最后一部分）
object READ_REG1 extends Actions
object WRITE_REG extends Actions
object WRITE_REG_ADDR extends Actions
object READ_REG0_ADDR extends Actions
object READ_REG1_ADDR extends Actions
object INST_OP  extends Actions
object INST_OPSEL extends Actions
object BRANCH_CONDITION extends Actions
object BRANCH_OPRND2 extends Actions // 只需要设置2，因为OPRN1默认都是寄存器的值
object WRITE_PC extends Actions
object BRANCH_TARGET extends Actions
object IMMA_USE extends Actions    // 表示是否使用IMMA作为操作数2，移位运算用

object RS extends Arguments
object RT extends Arguments
object RD extends Arguments


object IMMJ_ABSOLUTE extends Arguments  // 这两个是跳转用
object IMMI_RELATIVE extends Arguments
object REG  extends Arguments
case class RAW_BITS(b:Bits) extends Arguments


object Insts{
  def ADD     = M"000000--_--------_--------_--100000"
  def ADDU    = M"000000--_--------_--------_--100001"
  def ADDI    = M"001000--_--------_--------_--------"
  def ADDIU   = M"001001--_--------_--------_--------"

  def AND     = M"000000--_--------_--------_--100100"
  def ANDI    = M"001100--_--------_--------_--------"

  def DIV     = M"000000--_--------_--------_--011010"
  def DIVU    = M"000000--_--------_--------_--011011"
  def MULT    = M"000000--_--------_--------_--011000"
  def MULTU   = M"000000--_--------_--------_--011001"

  def NOR     = M"000000--_--------_--------_--100111"
  def OR      = M"000000--_--------_--------_--100101"
  def ORI     = M"001101--_--------_--------_--------"

  def SLL     = M"000000--_--------_--------_--000000"
  def SLLV    = M"000000--_--------_--------_--000100"
  def SRA     = M"000000--_--------_--------_--000011"
  def SRAV    = M"000000--_--------_--------_--000111"

  def SRL     = M"000000--_--------_--------_--000010"
  def SRLV    = M"000000--_--------_--------_--000110"
  def SUB     = M"000000--_--------_--------_--100010"
  def SUBU    = M"000000--_--------_--------_--100011"
  def XOR     = M"000000--_--------_--------_--100110"

  def XORI    = M"001110--_--------_--------_--------"
  def LHI     = M"011001--_--------_--------_--------"
  def LLO     = M"011000--_--------_--------_--------"

  def SLT     = M"000000--_--------_--------_--101010"
  def SLTU    = M"000000--_--------_--------_--101001"
  def SLTI    = M"001010--_--------_--------_--------"
  def SLTIU   = M"001011--_--------_--------_--------"

  def BEQ     = M"000100--_--------_--------_--------"
  def BGTZ    = M"000111--_--------_--------_--------"
  def BLEZ    = M"000110--_--------_--------_--------"
  def BNE     = M"000101--_--------_--------_--------"

  def J       = M"000010--_--------_--------_--------"
  def JAL     = M"000011--_--------_--------_--------"
  def JALR    = M"000000--_--------_--------_--001001"
  def JR      = M"000000--_--------_--------_--001000"

  def LB      = M"100000--_--------_--------_--------"
  def LBU     = M"100100--_--------_--------_--------"
  def LH      = M"100001--_--------_--------_--------"
  def LHU     = M"100101--_--------_--------_--------"
  def LW      = M"100011--_--------_--------_--------"

  def SB      = M"101000--_--------_--------_--------"
  def SH      = M"101001--_--------_--------_--------"
  def SW      = M"101011--_--------_--------_--------"

  def MFHI    = M"000000--_--------_--------_--010000"
  def MFLO    = M"000000--_--------_--------_--010010"
  def MTHI    = M"000000--_--------_--------_--010001"
  def MTLO    = M"000000--_--------_--------_--010011"

  def TRAP    = M"011010--_--------_--------_--------"

  // 在object中，最好不要定义val，因为这样的话，该值的创立时间与object的创立时间相关
  // 而在spinahdl中，信号在什么时候被创建，跟能不能赋值息息相关
  // 很多因素会影响object的建立时间
  // 具体可看：
  // https://github.com/Ncerzzk/MyBlog/blob/master/articles/SpinalHDL中一个匪夷所思的报错.md

  def BActions=HashMap(
    READ_REG0 -> True,
    READ_REG1 -> True,
    READ_REG0_ADDR ->RS,
    READ_REG1_ADDR ->RT,
    BRANCH_TARGET -> IMMI_RELATIVE,
    BRANCH_OPRND2->1,  // 此处设置一个默认值，后面可被覆盖
    BRANCH_CONDITION-> null
  )
  def LActions: Map[Actions, Object] = immutable.HashMap (
    (READ_REG0 -> True),
    (READ_REG0_ADDR -> RS),
    (READ_REG1 -> False),
    (WRITE_REG -> True),
    (WRITE_REG_ADDR -> RT),
    (INST_OP -> OpEnum.LOAD)
  )
  def SActions=HashMap(
    READ_REG0 -> True,
    READ_REG1 -> True,
    READ_REG0_ADDR ->RS,
    READ_REG1_ADDR ->RT,
    WRITE_REG -> False,
    INST_OP -> OpEnum.STORE
  )
  def IActions=HashMap(
    WRITE_REG       ->  True,
    WRITE_REG_ADDR  ->  RT,
    READ_REG0       ->  True,
    READ_REG0_ADDR  ->  RS,
    READ_REG1       ->  False
  )
  def RActions=HashMap(
    WRITE_REG -> True,
    WRITE_REG_ADDR -> RD,
    READ_REG0 -> True,
    READ_REG1 -> True,
    READ_REG0_ADDR -> RS,
    READ_REG1_ADDR -> RT
  )

  def JActions=HashMap(
      BRANCH_TARGET -> IMMJ_ABSOLUTE,
      BRANCH_CONDITION -> ((a:Bits,b:Bits)=>True),
      WRITE_REG -> False
  )

  def IInsts= List(
    ORI ->(IActions++ HashMap(INST_OP-> OpEnum.LOGIC,INST_OPSEL->OPLogic.OR)),
    ANDI ->(IActions++ HashMap(INST_OP-> OpEnum.LOGIC,INST_OPSEL->OPLogic.AND)),
    XORI ->(IActions++ HashMap(INST_OP-> OpEnum.LOGIC,INST_OPSEL->OPLogic.XOR)),
    ADDIU ->(IActions++ HashMap(INST_OP-> OpEnum.ALU,INST_OPSEL->OPArith.ADDU)),
    ADDI ->(IActions++ HashMap(INST_OP-> OpEnum.ALU,INST_OPSEL->OPArith.ADD)),
    SLTI ->(IActions++ HashMap(INST_OP-> OpEnum.ALU,INST_OPSEL->OPArith.SLT)),
    SLTIU ->(IActions++ HashMap(INST_OP-> OpEnum.ALU,INST_OPSEL->OPArith.SLTU))
  )

  def RInsts=List(
    AND  -> (RActions ++ HashMap(INST_OP-> OpEnum.LOGIC,INST_OPSEL->OPLogic.AND)),
    OR   -> (RActions ++ HashMap(INST_OP-> OpEnum.LOGIC,INST_OPSEL->OPLogic.OR)),
    XOR   -> (RActions ++ HashMap(INST_OP-> OpEnum.LOGIC,INST_OPSEL->OPLogic.XOR)),
    NOR   -> (RActions ++ HashMap(INST_OP-> OpEnum.LOGIC,INST_OPSEL->OPLogic.NOR)),

    ADD -> (RActions ++ HashMap(INST_OP-> OpEnum.ALU,INST_OPSEL->OPArith.ADD)),
    SUB -> (RActions ++ HashMap(INST_OP-> OpEnum.ALU,INST_OPSEL->OPArith.SUB)),
    ADDU -> (RActions ++ HashMap(INST_OP-> OpEnum.ALU,INST_OPSEL->OPArith.ADDU)),
    SUBU -> (RActions ++ HashMap(INST_OP-> OpEnum.ALU,INST_OPSEL->OPArith.SUBU)),
    SLT ->  (RActions ++ HashMap(INST_OP-> OpEnum.ALU,INST_OPSEL->OPArith.SLT)),
    SLTU -> (RActions ++ HashMap(INST_OP-> OpEnum.ALU,INST_OPSEL->OPArith.SLTU)),
    MULT -> (RActions ++ HashMap(INST_OP-> OpEnum.ALU,INST_OPSEL->OPArith.MUL)),
    MULTU -> (RActions ++ HashMap(INST_OP-> OpEnum.ALU,INST_OPSEL->OPArith.MULU)),

    SLLV -> (RActions ++ HashMap(
      INST_OP-> OpEnum.LOGIC,
      INST_OPSEL->OPLogic.LEFT_SHIFT,
      READ_REG0_ADDR -> RT,
      READ_REG1_ADDR -> RS
    )),
    SRAV -> (RActions ++ HashMap(
      INST_OP-> OpEnum.LOGIC,
      INST_OPSEL->OPLogic.RIGHT_SHIFT_ARITH,
      READ_REG0_ADDR -> RT,
      READ_REG1_ADDR -> RS
    )),
    SRLV -> (RActions ++ HashMap(
      INST_OP-> OpEnum.LOGIC,
      INST_OPSEL->OPLogic.RIGHT_SHIFT_LOGIC,
      READ_REG0_ADDR -> RT,
      READ_REG1_ADDR -> RS
    )),

    SLL -> (RActions ++ HashMap(
      INST_OP-> OpEnum.LOGIC,
      INST_OPSEL->OPLogic.LEFT_SHIFT,
      READ_REG1 -> False,
      IMMA_USE -> True
    )),
    SRA -> (RActions ++ HashMap(
      INST_OP-> OpEnum.LOGIC,
      INST_OPSEL->OPLogic.RIGHT_SHIFT_ARITH,
      READ_REG1 -> False,
      IMMA_USE -> True
    )),
    SRL -> (RActions ++ HashMap(
      INST_OP-> OpEnum.LOGIC,
      INST_OPSEL->OPLogic.RIGHT_SHIFT_LOGIC,
      READ_REG1 -> False,
      IMMA_USE -> True
    ))

  )

  def JInsts= List(
      J->JActions,
      JAL->(JActions ++ HashMap(
        WRITE_REG->True,
        WRITE_REG_ADDR->RAW_BITS(B("32'd31"))
      )),
      JR->(JActions ++ HashMap(
        BRANCH_TARGET->REG,
        READ_REG0 -> True,
        READ_REG0_ADDR -> RS
      )),
      JALR->(JActions ++ HashMap(
        BRANCH_TARGET->REG,
        READ_REG0 -> True,
        READ_REG0_ADDR -> RS,
        WRITE_REG->True,
        WRITE_REG_ADDR->RAW_BITS(B("32'd31"))
      ))
  )

  def LInsts =List(
    LB->(LActions++ HashMap(INST_OPSEL->OPLoad.LOADBYTE)),
    LW->(LActions++ HashMap(INST_OPSEL->OPLoad.LOADWORD)),
    LH->(LActions++ HashMap(INST_OPSEL->OPLoad.LOADHWORDU)),

    MFHI->HashMap(
      INST_OP -> OpEnum.LOAD,
      INST_OPSEL -> OPLoad.MFHI,
      WRITE_REG -> True,
      WRITE_REG_ADDR -> RD
    ),

    MFLO->HashMap(
      INST_OP -> OpEnum.LOAD,
      INST_OPSEL -> OPLoad.MFLO,
      WRITE_REG -> True,
      WRITE_REG_ADDR -> RD
    ),

    MTHI->HashMap(
      INST_OP -> OpEnum.LOAD,
      INST_OPSEL -> OPLoad.MTHI,
      READ_REG0 -> True,
      READ_REG0_ADDR -> RS
    ),

    MTLO->HashMap(
      INST_OP -> OpEnum.LOAD,
      INST_OPSEL -> OPLoad.MTLO,
      READ_REG0 -> True,
      READ_REG0_ADDR -> RS
    ),

    LHI ->HashMap(    // 这两个指令比较特别，重新写
      INST_OP -> OpEnum.LOAD,
      INST_OPSEL -> OPLoad.LOADHI,
      WRITE_REG -> True,
      WRITE_REG_ADDR -> RT
    ),
    LLO ->HashMap(    // 这两个指令比较特别，重新写
      INST_OP -> OpEnum.LOAD,
      INST_OPSEL -> OPLoad.LOADLO,
      WRITE_REG -> True,
      WRITE_REG_ADDR -> RT
    )
  )

  def SInsts=List(
    SB->(SActions++HashMap(INST_OPSEL->OPStore.STOREBYTE)),
    SH->(SActions++HashMap(INST_OPSEL->OPStore.STOREHWORD)),
    SW->(SActions++HashMap(INST_OPSEL->OPStore.STOREWORD))
  )

  def BInsts=List(
    BEQ ->( BActions++HashMap(
      BRANCH_CONDITION -> ((a:Bits, b:Bits) => a === b               )
    )),
    BGTZ->(BActions ++ HashMap(
      BRANCH_CONDITION -> ((a:Bits, b:Bits)  =>a.asSInt > b.asSInt   ),
      BRANCH_OPRND2 -> 0
    )),
    BLEZ->(BActions ++ HashMap(
      BRANCH_CONDITION -> ((a:Bits, b:Bits)=> a.asSInt <= b.asSInt   ),
      BRANCH_OPRND2 -> 0
    )),
    BNE ->(BActions ++ HashMap(
      BRANCH_CONDITION -> ((a:Bits, b:Bits)=> a =/= b   )
    ))
  )

  def AllInsts= IInsts++RInsts++JInsts++LInsts++SInsts++BInsts

  def chooseSource[T <: Arguments](a:T,inst:INST)={
    a match {
      case RT => inst.rt
      case RS => inst.rs
      case RD => inst.rd
      case rawBits: RAW_BITS => rawBits.b.resize(inst.rt.getWidth)
      case _ => B(0)
    }
  }
}


