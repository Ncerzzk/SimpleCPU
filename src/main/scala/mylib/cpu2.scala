package mylib
import spinal.core._

import scala.collection.{immutable, mutable}

class CPU2 extends Component {

  val io = new Bundle{
    val inst = in Bits(GlobalConfig.dataBitsWidth)
    val romEn = out Bool
    val romAddr = out Bits( log2Up(GlobalConfig.instRomCellNum) bits)
  }
  val regFile = new RegHeap



  val IF = new StageArea{
    val pcArea =new PCArea
    io.romAddr := pcArea.pc.resized
    io.romEn := True

    output(INSTRUCTION) := io.inst
    output(PC2) := pcArea.pc
  }

  val ID = new StageArea{
    input(INSTRUCTION)
    input(PC2)

    regFile.readPort.readEns(0) :=insert(REG_HEAP_EN0)
    regFile.readPort.readEns(1) :=insert(REG_HEAP_EN1)
    regFile.readPort.readAddrs(0) :=insert(REG_HEAP_ADDR0)
    regFile.readPort.readAddrs(1) :=insert(REG_HEAP_ADDR1)
    insert(REG_HEAP_EN0):=False;insert(REG_HEAP_EN1):=False
    insert(REG_HEAP_ADDR0):=B(0);insert(REG_HEAP_ADDR1):=B(0)
    insert(REG_HEAP_DATA0):=regFile.readPort.readDatas(0)
    insert(REG_HEAP_DATA1):=regFile.readPort.readDatas(1)

    output(OPSEL,OPRND1,OPRND2,WRITE_REG_ADDR)
    output(OP)
    output(WRITE_REG_EN)
  }

  val EX = new StageArea{
    input(OP,OPSEL,OPRND1,OPRND2,WRITE_REG_ADDR)
    input(WRITE_REG_EN)

    output(WRITE_REG_EN)
    output(WRITE_REG_ADDR,WRITE_REG_DATA)
  }
  val stages = List(IF,ID,EX)
  for(i <- 0 until stages.length){
    if(i!=0){
      val stage = stages(i)
      val lastStage = stages(i-1)
      for((key,signal) <- stage.inputs){
        signal :=lastStage.output(key)
      }
    }
  }

  Instructions(this,IInsts,RInsts)
  new IDBasicArea().build(this)

}

import GlobalConfig._

// 以下是流水线间信号
object INSTRUCTION extends Stageable(Bits(dataBitsWidth))
object OPRND1 extends Stageable(Bits(dataBitsWidth))
object OPRND2 extends Stageable(Bits(dataBitsWidth))
object PC2 extends Stageable(Bits(dataBitsWidth))
object OPSEL extends Stageable(Bits( 4 bits))
object OP extends Stageable(Bits(log2Up(OpEnum.elements.length) bits))
object WRITE_REG_ADDR extends Stageable(Bits(log2Up(regNum) bits))
object WRITE_REG_EN extends Stageable(Bool)
object WRITE_REG_DATA extends Stageable(Bits(dataBitsWidth))

//以下是中间信号
object REG_HEAP_ADDR0 extends Stageable(Bits(log2Up(regNum) bits))
object REG_HEAP_ADDR1 extends Stageable(Bits(log2Up(regNum) bits))
object REG_HEAP_EN0 extends Stageable(Bool)
object REG_HEAP_EN1 extends Stageable(Bool)
object REG_HEAP_DATA0 extends Stageable(Bits(dataBitsWidth))
object REG_HEAP_DATA1 extends Stageable(Bits(dataBitsWidth))

// 以下是译码动作
object READ_REG0 extends Stageable(Bool)
object READ_REG1 extends Stageable(Bool)
object WRITE_REG extends Stageable(Bool)
object OPRND1_SRC extends Stageable(OPrndSource())
object OPRND2_SRC extends Stageable(OPrndSource())

object OPrndSource extends SpinalEnum{
  val IMM,REG =newElement()
}


class PCArea extends Area{
  val writeEN = Bool
  val writeData =  Bits(GlobalConfig.dataBitsWidth)
  val pc = Bits(GlobalConfig.dataBitsWidth)

  val pc_reg = Reg(UInt(GlobalConfig.dataBitsWidth)).init(U("32'h0"))
  pc := pc_reg.asBits

  when(writeEN){
    pc_reg := writeData.asUInt
  }otherwise {
    pc_reg := pc_reg + 1 // 每次取一条指令，一条指令4字节，因为Rom的地址以字为单位，因此这里+1而不是+4
  }

}

case class INST(bits:Bits){
  def op = bits.takeHigh(6)
  def func = bits.take(6)
  def rs:Bits=bits(21 to 25)
  def rt:Bits=bits(16 to 20)
  def rd=bits(11 to 15)
  def immI = bits.take(16)
  def immJ = bits.take(26)
  def raw = bits
}

