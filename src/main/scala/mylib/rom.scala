package mylib

import spinal.core._


class InstRom extends Component {
  val io = new Bundle{
    val en = in Bool
    val addr = in Bits(log2Up(GlobalConfig.instRomCellNum) bits)
    val inst = out Bits(GlobalConfig.dataBitsWidth)
  }
  protected val mem=Mem(Bits(GlobalConfig.dataBitsWidth),GlobalConfig.instRomCellNum)

  //mem.init(List.fill(16)(B("32'h34011100")))
  when(io.en){
    io.inst := mem.readAsync(io.addr.asUInt)
  }otherwise{
    io.inst := 0
  }

  def init(a:Seq[Bits]) = mem.init(a)
}
