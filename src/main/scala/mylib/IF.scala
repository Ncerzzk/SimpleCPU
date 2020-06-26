package mylib

import spinal.core._
import spinal.lib.master

class PC extends Component{
  val io= new Bundle{
    val pc = out Bits(GlobalConfig.dataBitsWidth)
  }

  val pc_reg = Reg(UInt(GlobalConfig.dataBitsWidth)).init(0)
  io.pc := pc_reg.asBits
  pc_reg := pc_reg + 1  // 每次取一条指令，一条指令4字节，因为Rom的地址以字为单位，因此这里+1而不是+4
}

class Stage[T <: Bundle](gen: => T) extends Component{
  val left:T= gen.flip()
  val right = createOutPort(left)
  def createOutPort(inBundle:Bundle)= {
    new Bundle {
      for(i <- inBundle.elements){
        val a =out (Reg(i._2.clone()))
        a match{
          case s:Bits => s.init(0)
          case b:Bool => b.init(False)
        }
        valCallbackRec(a,i._1)
        a := i._2
      }
    }
  }
}

class IFOut extends Bundle{
  val pc = out Bits(GlobalConfig.dataBitsWidth)
  val inst = out Bits(GlobalConfig.dataBitsWidth)
}



