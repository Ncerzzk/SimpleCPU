package mylib

import spinal.core._
class Rom(cellNum:Int=1024) extends Component with BusSlaveContain {

  protected val mem=Mem(Bits(GlobalConfig.dataBitsWidth),cellNum)
  protected val addressWdith=log2Up(cellNum) bits

  busSlave.data_out := mem.readSync(busSlave.address.asUInt.resize(addressWdith),busSlave.cs & busSlave.read_write )
  busSlave.ready := busSlave.cs
}

class Ram extends Rom{
  mem.write(busSlave.address.asUInt.resize(addressWdith),busSlave.data_in,busSlave.cs & (~busSlave.read_write))
}

class InstRom extends Component {
  val io = new Bundle{
    val en = in Bool
    val addr = in Bits(log2Up(GlobalConfig.instRomCellNum) bits)
    val inst = out Bits(GlobalConfig.dataBitsWidth)
  }
  protected val mem=Mem(Bits(GlobalConfig.dataBitsWidth),GlobalConfig.instRomCellNum)

  //mem.init(List.fill(16)(B("32'h34011100")))
  io.inst := mem.readSync(io.addr.asUInt,io.en)

  def init(a:Seq[Bits]) = mem.init(a)
}
