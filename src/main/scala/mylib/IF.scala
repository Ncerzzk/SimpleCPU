package mylib

import spinal.core._
import spinal.lib.{IMasterSlave, master, slave}

class PCPort extends Bundle with IMasterSlave with DefaultValue{
  val writeEN = Bool
  val writeData =  Bits(GlobalConfig.dataBitsWidth)

  override def asMaster(): Unit = {
    out(writeEN,writeData)
  }

  def JMP(target:Bits): Unit ={
    writeEN := True
    writeData := target.resized
  }
}
class PC extends Component{
  val io= new Bundle{
    val pc = out Bits(GlobalConfig.dataBitsWidth)
  }
  val writePort: PCPort = slave(new PCPort)
  val ctrl = slave(new StageCTRLBundle)

  val pc_reg = Reg(UInt(GlobalConfig.dataBitsWidth)).init(U("32'h0"))
  io.pc := pc_reg.asBits

  when(writePort.writeEN){
    pc_reg := writePort.writeData.asUInt
  }elsewhen(ctrl.stateOut === StageStateEnum.ENABLE){
    pc_reg := pc_reg + 1 // 每次取一条指令，一条指令4字节，因为Rom的地址以字为单位，因此这里+1而不是+4
  }elsewhen (ctrl.stateOut === StageStateEnum.FLUSH){
    pc_reg := U(0)
  }otherwise { // STALL
    pc_reg := pc_reg
  }
}

object StageStateEnum extends SpinalEnum(binaryOneHot){
  val FLUSH,STALL,ENABLE = newElement()
}

object StageCTRLReqEnum extends SpinalEnum{
  val NORMAL = newElement()
  val IFSTALL,IDSTALL,EXSTALL,MEMSTALL=newElement()
  val IFFLUSH,IDFLUSH,EXFLUSH=newElement()
}

class StageCTRLReqBundle extends Bundle with IMasterSlave{
  val req = StageCTRLReqEnum()

  override def asMaster(): Unit = {
    out (req)
  }
}
class StageCTRLBundle extends Bundle with IMasterSlave{
  val stateOut = StageStateEnum()

  override def asMaster(): Unit = {
    out(stateOut)
  }
}


class StageCTRL extends Component{
  val slaves = Vec(master(new StageCTRLBundle()),4)
  val reqFromID= slave(new StageCTRLReqBundle())
  val reqFromEX= slave(new StageCTRLReqBundle())

  def <>(a:List[StageCTRLBundle]):Unit={
    // 顺序应该是 PC，IF2ID,ID2EX,EX2MEM
    // 不能放错误顺序
    for(i <- a.indices){
      a(i) <> slaves(i)
    }
  }

  def <>(ex:EX)=reqFromEX <> ex.reqCTRL
  def <>(id:ID)=reqFromID <> id.reqCTRL

  slaves.foreach(s=>s.stateOut:=StageStateEnum.ENABLE)
  val req = (reqFromEX.req === StageCTRLReqEnum.NORMAL) ?reqFromID.req | reqFromEX.req
  when(req === StageCTRLReqEnum.IFFLUSH){
    slaves(1).stateOut := StageStateEnum.FLUSH
  }elsewhen(req === StageCTRLReqEnum.IDSTALL){
    slaves(0).stateOut := StageStateEnum.STALL
    slaves(1).stateOut := StageStateEnum.STALL
    slaves(2).stateOut := StageStateEnum.FLUSH
  }
}

class Stage[T <: Bundle](gen: => T) extends Component{
  val left:T= gen.flip()
  val ctrl = slave(new StageCTRLBundle)
  val right:T= createOutPort(left)

  def <>(l:T,r:T):Unit={
    l<>left
    r<>right
  }

  def getInitData[T <: Data](data:T):Data={
    data match {
      case a:Bits => B(0)
      case b:Bool => False
      case e:SpinalEnumCraft[_] => e.spinalEnum.elements(0)
    }
  }
  def defaultData[T <: Data](data:T): Unit={
    data match {
      case v:Vec[_] => v.foreach(a=>initData(a))
      case a => a:= getInitData(a).asInstanceOf[T]
    }
  }
  def initData[T <: Data](data:T): Unit ={
    data match {
      case v:Vec[_] => v.foreach(a=>initData(a))
      case a => a.init(getInitData(a).asInstanceOf[T])
    }
  }

  def createOutPort(inBundle:Bundle):T= {
    new Bundle {
      for(i <- inBundle.elements){

        val a =out (Reg(i._2.clone()))
        initData(a)
        /*
        a match{
          case s:Bits => s.init(0)
          case b:Bool => b.init(False)
          case e:SpinalEnumCraft[_] =>
            val a =e.spinalEnum.elements(0)
            e.asInstanceOf[e.spinalEnum.C].init(a)
        }

         */
        valCallbackRec(a,i._1)
        when(ctrl.stateOut===StageStateEnum.ENABLE){
          a := i._2
        }elsewhen(ctrl.stateOut === StageStateEnum.FLUSH){
          defaultData(a)
        }otherwise{   //Stall
          // 啥都不干，锁存
        }

      }
    }
  }.asInstanceOf[T]
}

class IFOut extends Bundle{
  val pc = out Bits(GlobalConfig.dataBitsWidth)
  val inst = out Bits(GlobalConfig.dataBitsWidth)
}



