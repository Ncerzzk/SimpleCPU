package mylib

import spinal.core._
import spinal.lib._

object GlobalConfig{
  val dataBitsWidth= 32 bits
  val spmCellNum:Int = 4096/dataBitsWidth.value
  val regNum = 32
  val instRomCellNum:Int = 16
  val ramRegNum = 64
}

object FlowStateEnum extends SpinalEnum(defaultEncoding = binarySequential) with NeedNBits{
  val NORMAL,REFRESH,DELAY = newElement()
}

class Mut extends Component{
  val io=new Bundle{
    val data1 = in(SInt(32 bits))
    val data2 = in(SInt(32 bits))
    val outdata = out(Reg(UInt(64 bits)))
  }

  io.outdata := (io.data1 * io.data2).asUInt
}


class CPU extends Component  with BusMasterContain {

  val ram =new Ram(GlobalConfig.ramRegNum)
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
  //id <> pc_reg
  if2id.left.pc := pc_reg.io.pc
  if2id.left.inst := io.inst
  if2id.right <> id.lastStage


  val id2ex = new Stage(new IDOut())
  val ex = new EX()
  id2ex <>(id.idOut,ex.lastStage)
  ex <> pc_reg

  val ex2mem = new Stage(new EXOut())
  val mem = new MEM()
  ex2mem <>(ex.exOut,mem.lastStage)

  ex <> ex2mem
  ex <> regs

  //val mem2wb = new Stage(new MEMOut())
  //val wb = new WB()
  //mem2wb <> (mem.memOut,wb.lastStage)
  mem<>regs
  //wb<>regs

  id <> ex
  //id <> mem
  //id <> wb

  stageCTRL <> List(pc_reg.ctrl,if2id.ctrl,id2ex.ctrl,ex2mem.ctrl)
  stageCTRL <> id
  stageCTRL <> ex

  ram.io <> mem.ramPort
}

class SOC extends Component {

  val cpu = new CPU
  val rom = new InstRom

  romInitTestDIV()
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
    /*
    ori $1,$0,0x1100   -> $1 = 0x1100
    ori $2,$1,0x11     -> $2 = 0x1111
    andi $1,$2,0x10    -> $1 = 0x0010
    xori $2,$1,0x1000  -> $2 = 0x1010
     */
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
    /*
0   nop
4   addiu $1, $0, 0x1100
8   j 20
12  addiu $2, $0, 0x0111
16  and   $3, $1 ,$2
20  or    $4, $1, $2
 */
    val inits=List(B(0),B("32'h24011100"),B("32'h08000005"),B("32'h24020111"),B("32'h00221824"),
      B("32'h00222025"))
    val romInitVal=inits++List.fill(GlobalConfig.instRomCellNum-inits.length)(B(0))
    rom.init(romInitVal)
  }

  def romInitTestB():Unit = {
    /*
    addiu $1, $0, 0x1100
    b 2
    addiu $2, $0, 0x0111
    and   $3, $1 ,$2
    or    $4, $1, $2
     */
    val inits=List(B("32'h24011100"),B("32'h10000002"),B("32'h24020111"),B("32'h00221824"),
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

  def romInitTestMUL():Unit = {
    /*
	nop
	addiu $1, $0, 0xFF01
	addiu $2,$0,0x7001

	mult  $1,$2
	multu $1,$2
     */
    val inits=List(B(0),B("32'h2401ff01"),B("32'h24027001"),B("32'h00220018"),B("32'h00220019"))
    val romInitVal=inits++List.fill(GlobalConfig.instRomCellNum-inits.length)(B(0))
    rom.init(romInitVal)
  }

  def romInitTestLoad():Unit = {
    /*
    nop
	LB $1,01($0)
	LH $2,02($0)
	LW $3,04($0)
     */
    val inits=List(B(0),B("32'h80010001"),B("32'h84020002"),B("32'h8c030004"))
    val romInitVal=inits++List.fill(GlobalConfig.instRomCellNum-inits.length)(B(0))
    rom.init(romInitVal)
  }

  def romInitTestStore():Unit = {
    /*
	addi $1,100
	addi $2,0xFF01
	addi $3,0x200

	sb $1,0($0)
	sh $2,2($0)
	sw $3,4($0)
	lw $4,0($0)
	lw $5,4($0)
     */
    val inits=List(B("32'h20210064"),B("32'h2042ff01"),B("32'h20630200"),
      B("32'ha0010000"),B("32'ha4020002"),B("32'hac030004"),
      B("32'h8c040000"),B("32'h8c050004")
    )
    val romInitVal=inits++List.fill(GlobalConfig.instRomCellNum-inits.length)(B(0))
    rom.init(romInitVal)
  }

  def romInitTestLoadADD():Unit = {
    /*
    nop
	LBU $1,01($0)
	addiu $2,$1,100
     */
    val inits=List(B(0),B("32'h90010001"),B("32'h24220064"))
    val romInitVal=inits++List.fill(GlobalConfig.instRomCellNum-inits.length)(B(0))
    rom.init(romInitVal)
  }

  def romInitTestDIVU():Unit={
    /*
    nop
    addiu $1,160
	  addiu $2,3
	  divu  $1,$2
     */
    val inits=List(B(0),B("32'h242100a0"),B("32'h24420003"),B("32'h0022001b"),B("32'h242100a0"))
    val romInitVal=inits++List.fill(GlobalConfig.instRomCellNum-inits.length)(B(0))
    rom.init(romInitVal)
  }

  def romInitTestDIV():Unit={
    /*
    nop
    addiu $1,160
	  addiu $2,3
	  divu  $1,$2
     */
    val inits=List(B(0),B("32'h2021ff60"),B("32'h24420003"),B("32'h0022001a"),B("32'h242100a0"))
    val romInitVal=inits++List.fill(GlobalConfig.instRomCellNum-inits.length)(B(0))
    rom.init(romInitVal)
  }
}