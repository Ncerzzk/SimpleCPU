package mylib

import spinal.core._
import spinal.lib.{IMasterSlave, master, slave}

class PCPort extends Bundle with IMasterSlave{
  val writeEN = Bool
  val writeData =  Bits(GlobalConfig.dataBitsWidth)

  override def asMaster(): Unit = {
    out(writeEN,writeData)
  }
}
class PC extends Component{
  val io= new Bundle{
    val pc = out Bits(GlobalConfig.dataBitsWidth)
  }
  val writePort: PCPort = slave(new PCPort)

  val pc_reg = Reg(UInt(GlobalConfig.dataBitsWidth)).init(U("32'h0"))
  io.pc := pc_reg.asBits

  when(writePort.writeEN){
    pc_reg := writePort.writeData.asUInt
  }otherwise {
    pc_reg := pc_reg + 1 // 每次取一条指令，一条指令4字节，因为Rom的地址以字为单位，因此这里+1而不是+4
  }
}

object StageStateEnum extends SpinalEnum(binaryOneHot){
  val FLUSH,STALL,ENABLE = newElement()
}

class StageCTRLBundle extends Bundle with IMasterSlave{
  val stateOut = StageStateEnum()

  override def asMaster(): Unit = {
    out(stateOut)
  }
}


class StageCTRL extends Component{
  val slaves = Vec(master(new StageCTRLBundle()),4)
  val reqFromID= slave(new StageCTRLBundle())

  def <>(a:List[StageCTRLBundle]):Unit={
    for(i <- 0 until 4){
      a(i) <> slaves(i)
    }
  }

  slaves.foreach(s=>s.stateOut:=StageStateEnum.ENABLE)
  when(reqFromID.stateOut === StageStateEnum.FLUSH){
    slaves(0).stateOut := StageStateEnum.FLUSH
  }
}

class Stage[T <: Bundle](gen: => T) extends Component{
  val left:T= gen.flip()
  val ctrl: StageCTRLBundle = slave(new StageCTRLBundle)
  val right:T= createOutPort(left)

  def <>(l:T,r:T):Unit={
    l<>left
    r<>right
  }

  def createOutPort(inBundle:Bundle):T= {
    new Bundle {
      for(i <- inBundle.elements){
        val a =out (Reg(i._2.clone()))
        a match{
          case s:Bits => s.init(0)
          case b:Bool => b.init(False)
        }
        valCallbackRec(a,i._1)
        when(ctrl.stateOut===StageStateEnum.ENABLE){
          a := i._2
        }otherwise{
          a match {
            case s:Bits => s:=B(0)
            case b:Bool => b:=False
          }
        }

      }
    }
  }.asInstanceOf[T]
}

class IFOut extends Bundle{
  val pc = out Bits(GlobalConfig.dataBitsWidth)
  val inst = out Bits(GlobalConfig.dataBitsWidth)
}



