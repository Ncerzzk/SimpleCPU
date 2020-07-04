package mylib

import spinal.core._

import scala.collection.immutable
import scala.tools.nsc.backend.jvm.BTypes


object Ram{

  object RamOperateType extends SpinalEnum{
    val BYTE,HWORD,WORD = newElement()
  }

  def slavePort(regNum:Int) = new Bundle{
    val addr = in Bits(log2Up(regNum*4) bits)
    val writeData = in Bits GlobalConfig.dataBitsWidth
    val writeEn = in Bool
    //val writeAddr = in Bits(log2Up(regNum*4) bits)
    val operateType = in (RamOperateType())
    val readData = out Bits GlobalConfig.dataBitsWidth
  }

  def masterPort(regNum:Int)=slavePort(regNum).flip()

}

class Ram(regNum:Int =64) extends Component{
  import Ram.RamOperateType._
  val io = Ram.slavePort(regNum)
  val mem = Mem(Bits(GlobalConfig.dataBitsWidth),regNum).init(List.fill(regNum)(B("32'hFFFF_FFFF")))

  val byteAddr = io.addr.take(2).asUInt
  val hwordAddr = io.addr(1).asUInt
  val addrDiv = (io.addr>>2).asUInt

  io.readData:= 0
  when(io.writeEn){
    switch(io.operateType){
      is(BYTE){
        val shiftBitNum=(byteAddr<<3)  // 如果是1则应移动8位 2->16位 ...
        val mask = B("32'hFF") |<< shiftBitNum
        val validData= io.writeData.take(8).resize(GlobalConfig.dataBitsWidth) |<< shiftBitNum
        mem(addrDiv):= validData | (~mask & mem(addrDiv))
      }
      is(HWORD){
        val shiftBitNum=hwordAddr<<4
        val mask = B("32'hFFFF") |<< shiftBitNum
        val validData= io.writeData.take(16).resize(GlobalConfig.dataBitsWidth) |<< shiftBitNum
        mem(addrDiv):= validData | (~mask & mem(addrDiv))
      }
      is(WORD){
        mem(addrDiv) := io.writeData
      }
    }
  }otherwise {
    switch(io.operateType){
      is(BYTE){
        io.readData := mem(addrDiv).subdivideIn(8 bits)(byteAddr).resized
      }
      is(HWORD){
        io.readData := mem(addrDiv).subdivideIn(16 bits)(hwordAddr).resized
      }
      is(WORD){
        io.readData := mem(addrDiv)
      }
    }
  }

//  mem(io.writeAddr.asUInt) := (mem(io.writeAddr.asUInt) & ~mask) | (io.writeData & mask)
  //val data = mem.readAsync((io.readAddr>>2).asUInt)


}
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
