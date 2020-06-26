package mylib
import spinal.core._
import spinal.lib.fsm._

object PriorityEncode{
  def apply(conds_bits: Bits,num:Int=0)(block:Int => Unit):Unit={
    if(num!=conds_bits.getBitsWidth){
      when(conds_bits(num)){
        block(num)
      }otherwise{
        apply(conds_bits,num+1)(block)
      }
    }
  }
}

object ParallelSelector{
  def apply(conds_bits:Bits)(block:Int=>Unit):Unit={
    for(i <- 0 until conds_bits.getWidth){
      when(conds_bits(i)){
        block(i)
      }
    }
  }
}

trait BusSlaveContain{
  class BusSlaveBundle(bitwidth:BitCount = 32 bits)extends Bundle{
    val data_in = in Bits (bitwidth)
    val data_out = out Bits (bitwidth)
    val read_write = in Bool
    val address = in Bits(bitwidth)
    val cs = in Bool
    val ready= out Bool
  }

  val busSlave = new BusSlaveBundle(GlobalConfig.dataBitsWidth)
}

class BusMasterBundle(bitwidth:BitCount = 32 bits) extends Bundle{
  val data_in = in Bits (bitwidth)
  val data_out = out Bits (bitwidth)
  val read_write = out Bool
  val address_store_out = out Bool  // 地址锁存信号

  val ready = in Bool

  val address_out = out Bits (bitwidth) // 连接到mux
  val req = out Bool
  val grnt = in Bool  // 总线许可
}

trait BusMasterContain{
  val busMaster = new BusMasterBundle(GlobalConfig.dataBitsWidth)
}

class BusControllerArea(flow_state:Bits,busMaster:BusMasterBundle) extends Area {
  val state_machine= new StateMachine {
    val state_idle : State = new State with EntryPoint {
      whenIsActive {
        when(
          busMaster.address_out.takeHigh(AddressDecodeEnum.needBits.value) =/= AddressDecodeEnum.SPM.asBits
            && flow_state =/= FlowStateEnum.REFRESH.asBits
        ) {
          when(busMaster.address_store_out) {
            goto(state_request)
          }
        }
        goto(state_delay)
      }
    }

    val state_request:State= new State {
      whenIsActive {
        when(busMaster.grnt) {
          goto(state_acess)
        }
      }
    }

    val state_acess :State = new State {
      whenIsActive {
        when(busMaster.ready) {
          when(flow_state === FlowStateEnum.DELAY.asBits) {
            goto(state_delay)
          } otherwise {
            goto(state_idle)
          }
        }
      }
    }

    val state_delay :State= new State {
      whenIsActive {
        when(flow_state =/= FlowStateEnum.DELAY.asBits) {
          goto(state_idle)

        }
      }
    }
  }
}



class BusSlaveDecoder(bitwidth:BitCount=32 bits,slaveNum:Int) extends  Component{
  val io= new Bundle{
    val addr= in Bits(bitwidth)
    val cs = out Bits(slaveNum bits)
  }
}



class BusArbiter(masterNum:Int) extends  Component{
  val io=new Bundle{
    val reqs = in(Bits(masterNum bits))
    val grnts = out(Reg(Bits(masterNum bits))) init(0)
  }

  io.grnts := 0
  // 固定优先级的仲裁，会导致饥饿
  PriorityEncode(io.reqs,0){i=>io.grnts(i):=True}

  def <>(that: List[BusMasterContain]):Unit={
    for(i <- that.indices){
      io.reqs(i) <> that(i).busMaster.req
      io.grnts(i) <> that(i).busMaster.grnt
    }
  }

  def <>(that : BusMasterMux)={
    that.io.grnts <> io.grnts
  }

}


class BusSlaveMux(num:Int,bits_width:BitCount =32 bits) extends  Component{
  val io= new Bundle{
    val data = in Vec(Bits(bits_width),num)
    val readys = in Vec(Bool,num)
    val sel = in Bits(log2Up(num) bits)

    val data_out = out Bits(bits_width)
    val ready_out = out Bool
  }

  io.data_out := io.data(io.sel.asUInt)
  io.ready_out := io.readys(io.sel.asUInt)

  def <>(slaves:List[BusSlaveContain],masters :List[BusMasterContain]):Unit={
    for (i <- slaves.indices){
      io.data(i) <> slaves(i).busSlave.data_out
      io.readys(i) <> slaves(i).busSlave.ready
    }
    for (i <- masters.indices){
      io.data_out <> masters(i).busMaster.data_in
      io.ready_out <> masters(i).busMaster.ready
    }
  }

}

trait NeedNBits extends SpinalEnum{
  def needBits : BitCount = log2Up(elements.length) bits
  // 此处不可定义为val，必须定义为函数
  // 否则当val定义时，elements的长度为0，导致此量永远为0 bits
}
// 地址译码器枚举
object AddressDecodeEnum extends SpinalEnum(defaultEncoding = binarySequential) with NeedNBits{
  val SPM,ROM,RAM = newElement()

}




class BusMasterMux(num:Int,bits_width:BitCount =32 bits) extends  Component{
  val io=new Bundle{
    val address=in Vec(Bits(bits_width),num)
    val data = in Vec(Bits(bits_width),num)
    val grnts = in Bits(num bits)

    val addr_out = out Bits(bits_width)
    val data_out = out Bits(bits_width)
  }

  io.addr_out := 0
  io.data_out := 0
  ParallelSelector(io.grnts){
    i=>
      io.data_out := io.data(i)
      io.addr_out := io.address(i)
  }

  def <>(that : BusArbiter):Unit={
    that <> this
  }


  def <>(slaves:List[BusSlaveContain],masters :List[BusMasterContain]):Unit={
    for (i <- slaves.indices){
      io.addr_out <> slaves(i).busSlave.address
      io.data_out <> slaves(i).busSlave.data_in
    }
    for (i <- masters.indices){
      masters(i).busMaster.address_out <> io.address(i)
      masters(i).busMaster.data_out <> io.data(i)
    }
  }

}