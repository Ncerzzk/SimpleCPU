package mylib

import spinal.core._
import spinal.lib._

object GlobalConfig{
  val dataBitsWidth= 32 bits
  val spmCellNum:Int = 4096/dataBitsWidth.value
  val regNum = 32
  val instRomCellNum:Int = 16

}

object FlowStateEnum extends SpinalEnum(defaultEncoding = binarySequential) with NeedNBits{
  val NORMAL,REFRESH,DELAY = newElement()
}




class SpmPort extends Bundle with IMasterSlave{
  val addr =  Bits(log2Up(GlobalConfig.spmCellNum) bits)
  val cs =  Bool
  val read_or_write =  Bool
  val write_data=  Bits(GlobalConfig.dataBitsWidth)
  val read_data =  Bits(GlobalConfig.dataBitsWidth)

  override def asMaster(): Unit = {
    out(addr,cs,read_or_write,write_data)
    in(read_data)
  }


}

class Spm(size :Int = 4096, width:Int = 32) extends Component {

  val ioA = slave(new SpmPort)
  val ioB= slave(new SpmPort)

  val mem= Mem(Bits(width bits),size/width)
  // AB端口读，B端口写

  ioA.read_data := mem.readSync(ioA.addr.asUInt,ioA.cs & ioA.read_or_write)
  ioB.read_data := mem.readSync(ioB.addr.asUInt,ioB.cs & ioB.read_or_write)

  mem.write(ioB.addr.asUInt,ioB.write_data,ioB.cs & ~ioB.read_or_write)

}

class BusInterface extends  Component{
  val asm_line= new Bundle{
    val stall = in Bool // 延迟信号
    val flush = in Bool // 刷新
    val busy = out Bool // 总线忙
  }

  val spm= new Bundle{
    val read_data = in Bits(32 bits)
    val addr = out Bits(7 bits)
    val cs = out Bool
    val read_or_write = out Bool
    val write_data = out Bits(32 bits)
  }

  val bus= new Bundle{
    val read_data = in Bits(32 bits)
    val ready = in Bool
    val grnt = in Bool
    val req = out Bool
    val addr = out Bits(32 bits)
    val cs = out Bool
    val read_or_write = out Bool
    val write_data = out Bits(32 bits)
  }
}

class CPU extends Component  with BusMasterContain {

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
  id2ex.left <> id.idOut
  id2ex.right <> ex.lastStage


  val ex2mem = new Stage(new EXOut())
  val mem = new MEM()
  ex2mem.left<>ex.exOut
  ex2mem.right<>mem.lastStage



  val mem2wb = new Stage(new MEMOut())
  val wb = new WB()
  mem2wb.left <> mem.memOut
  mem2wb.right<>wb.lastStage

  wb<>regs

  id <> ex
  id <> mem
  id <> wb

  stageCTRL <> List(if2id.ctrl,id2ex.ctrl,ex2mem.ctrl,mem2wb.ctrl)
  stageCTRL.reqFromID <> id.reqCTRL
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