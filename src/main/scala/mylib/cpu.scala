package mylib

import spinal.core._
import spinal.lib._

object GlobalConfig{
  val ANDI = M"001100--_--------_--------_--------"
  val ADDI = M"001000--_--------_--------_--------"
  val ADD  = M"000000--_--------_--------_--100000"


  val dataBitsWidth= 32 bits
  val spmCellNum:Int = 4096/dataBitsWidth.value
  val regNum = 32
  val instRomCellNum:Int = 16

}

class Mut extends Component{
  val io=new Bundle{
    val data1 = in(SInt(32 bits))
    val data2 = in(SInt(32 bits))
    val outdata = out(Reg(UInt(64 bits)))
  }

  io.outdata := (io.data1 * io.data2).asUInt
}
class CPU extends Component  {

  val io = new Bundle{
    val inst = in Bits(GlobalConfig.dataBitsWidth)
    val romEn = out Bool
    val romAddr = out Bits( log2Up(GlobalConfig.instRomCellNum) bits)
  }
  val stageCTRL = new StageCTRL()
  val regs= new RegHeap(GlobalConfig.regNum)

  val pc_reg =new PC()
  io.romAddr := pc_reg.io.pc.resize(io.romAddr.getWidth)
  io.romEn := True


  val if2id = new Stage(new IFOut())
  val id = new ID()
  id <> regs
  id <> pc_reg
  if2id.left.pc := pc_reg.io.pc
  if2id.left.inst := io.inst
  if2id.right <> id.lastStage


  val id2ex = new Stage(new IDOut())
  val ex = new EX()
  id2ex <>(id.idOut,ex.lastStage)

  val ex2mem = new Stage(new EXOut())
  val mem = new MEM()
  ex2mem <>(ex.exOut,mem.lastStage)


  val mem2wb = new Stage(new MEMOut())
  val wb = new WB()
  mem2wb <> (mem.memOut,wb.lastStage)

  wb<>regs

  id <> ex
  id <> mem
  id <> wb

  stageCTRL <> List(if2id.ctrl,id2ex.ctrl,ex2mem.ctrl,mem2wb.ctrl)
  stageCTRL.reqFromID <> id.reqCTRL
}
class SOC2 extends Component{
  val cpu2 = new CPU2
}
class SOC extends Component {

  val cpu = new CPU
  val rom = new InstRom


  romInitTestBNOJ()
  rom.io.inst<> cpu.io.inst
  rom.io.en <> cpu.io.romEn
  rom.io.addr<> cpu.io.romAddr

  def romInitTestBack0gap()={
    val romInitVal=List(B(0),B("32'h34011100"),B("32'h34220011"))++List.fill(13)(B(0))
    rom.init(romInitVal)
  }

  def romInitTestBack1gap()={
    val romInitVal=List(B(0),B("32'h34011100"),B(0),B("32'h34220011"))++List.fill(12)(B(0))
    rom.init(romInitVal)
  }

  def romInitTestBack2gap()={
    val romInitVal=List(B(0),B("32'h34011100"),B(0),B(0),B("32'h34220011"))++List.fill(11)(B(0))
    rom.init(romInitVal)
  }

  def romInitTestAnd0gap()={
    val inits=List(B(0),B("32'h34011100"),B("32'h34220011"),B("32'h30410010"),B("32'h38221000"))
    val romInitVal=inits++List.fill(GlobalConfig.instRomCellNum-inits.length)(B(0))
    rom.init(romInitVal)
  }

  def romInitTestAdd0gap()={
    val inits=List(B(0),B("32'h34011100"),B("32'h24220064"))
    val romInitVal=inits++List.fill(GlobalConfig.instRomCellNum-inits.length)(B(0))
    rom.init(romInitVal)
  }

  def romInitTestADDSLTI()={
    /*
    addiu $2, $1, 0x1100
	  sltiu $3, $2, 0x1000
	  sltiu  $4, $2, 0x1101
     */
    val inits=List(B(0),B("32'h24221100"),B("32'h2c431000"),B("32'h2c441101"))
    val romInitVal=inits++List.fill(GlobalConfig.instRomCellNum-inits.length)(B(0))
    rom.init(romInitVal)
  }

  def romInitTestSUBSLTI(): Unit ={
    /*
    addiu $2, $1, 0xF100
	  sltiu $3, $2, 0xF101
	  slti  $4, $2, 0xE100
     */
    val inits=List(B(0),B("32'h2422f100"),B("32'h2c43f101"),B("32'h2844e100"))
    val romInitVal=inits++List.fill(GlobalConfig.instRomCellNum-inits.length)(B(0))
    rom.init(romInitVal)
  }

  def romInitTestANDOR(): Unit ={
    /*
    addiu $1, $0, 0x1100
    addiu $2, $0, 0x0111
    and   $3, $1 ,$2
    or    $4, $1, $2
     */
    val inits=List(B(0),B("32'h24011100"),B("32'h24020111"),B("32'h00221824"),B("32'h00222025"))
    val romInitVal=inits++List.fill(GlobalConfig.instRomCellNum-inits.length)(B(0))
    rom.init(romInitVal)
  }

  def romInitTestJ():Unit = {
    val inits=List(B(0),B("32'h24011100"),B("32'h08000005"),B("32'h24020111"),B("32'h00221824"),
      B("32'h00222025"))
    val romInitVal=inits++List.fill(GlobalConfig.instRomCellNum-inits.length)(B(0))
    rom.init(romInitVal)
  }

  def romInitTestB():Unit = {
    /*
    nop
    addiu $1, $0, 0x1100
    b 2
    addiu $2, $0, 0x0111
    and   $3, $1 ,$2
    or    $4, $1, $2
     */
    val inits=List(B(0),B("32'h24011100"),B("32'h10000002"),B("32'h24020111"),B("32'h00221824"),
      B("32'h00222025"))
    val romInitVal=inits++List.fill(GlobalConfig.instRomCellNum-inits.length)(B(0))
    rom.init(romInitVal)
  }

  def romInitTestBNOJ():Unit = {
    /*
	nop
	addiu $1, $0, 0x1100
	beq  $1,$0,20
	addiu $2, $0, 0x0111
	and   $3, $1 ,$2
	or    $4, $1, $2
     */
    val inits=List(B(0),B("32'h24011100"),B("32'h10200007"),B("32'h24020111"),B("32'h00221824"),
      B("32'h00222025"))
    val romInitVal=inits++List.fill(GlobalConfig.instRomCellNum-inits.length)(B(0))
    rom.init(romInitVal)
  }

}