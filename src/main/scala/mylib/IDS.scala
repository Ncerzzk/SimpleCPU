package mylib

import spinal.core._
import spinal.lib.{master, slave}

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

object Bin{

  def apply(src:String,res:Int=0):Int={
    src.length match {
      case 0|1 => res
      case _ => apply(src.substring(0,src.length-1), res + src.substring(0,1).toInt * (1 << src.length - 1))
    }
  }
}

object InstTypeEnum extends SpinalEnum{
  val R,I,J = newElement()
  // R型指令的高6位为0(有例外），靠低6位区分功能
  // I型指令直接靠高6位区分功能
}


object OpEnum extends SpinalEnum{
  val LOGIC,ALU,LOAD,STORE = newElement()

  val OPs = List(
    LOGIC->OPLogic,
    ALU->OPArith
    // load sotre不在这里，因为load在EX中特殊处理了
  )
}

trait withFuncs{
  val funcs:List[(SpinalEnumElement[_], (Bits, Bits) => Bits)]
}
object OPArith extends SpinalEnum with withFuncs {
  val ADDU,SUBU = newElement()
  val SLT,SLTU = newElement()
  val MULU,MUL = newElement()
  val ADD,SUB =newElement()

  val funcs: List[(SpinalEnumElement[_], (Bits, Bits) => Bits)] = List(
    (ADDU,(a:Bits,b:Bits)=> (a.asUInt + b.asUInt).asBits),
    (SUBU,(a:Bits,b:Bits)=> (a.asUInt - b.asUInt).asBits),
    (ADD,(a:Bits,b:Bits)=> (a.asSInt + b.asSInt).asBits),
    (SUB,(a:Bits,b:Bits)=> (a.asSInt - b.asSInt).asBits),
    // SLTI => Source Less than Immediate
    (SLTU, (a:Bits, b:Bits)=> (a.asUInt < b.asUInt)?B(1,32 bits)|B(0)),
    (SLT, (a:Bits, b:Bits) => (a.asSInt < b.asSInt)?B(1,32 bits)|B(0)),

    (MULU,(a:Bits,b:Bits) => (a.asUInt*b.asUInt).asBits),
    (MUL,(a:Bits,b:Bits) => (a.asSInt*b.asSInt).asBits)
  )
}

object OPLogic extends SpinalEnum with withFuncs{
  val OR,AND,XOR,NOR = newElement()
  val LEFT_SHIFT = newElement()
  val RIGHT_SHIFT_LOGIC,RIGHT_SHIFT_ARITH = newElement()
  val funcs = List(
    (OR,(a:Bits,b:Bits)=>   a | b),
    (AND,(a:Bits,b:Bits)=>  a & b),
    (XOR,(a:Bits,b:Bits)=>  a ^ b),
    (NOR,(a:Bits,b:Bits)=> ~(a | b)),
    (LEFT_SHIFT,(a:Bits,b:Bits) => a |<< b.asUInt),   // 左移只有一种
    (RIGHT_SHIFT_LOGIC,(a:Bits,b:Bits) => a |>> b.asUInt),
    (RIGHT_SHIFT_ARITH,(a:Bits,b:Bits) => a >> b.asUInt)
  )
}

object OPLoad extends SpinalEnum {
  val LOADBYTE,LOADHWORD,LOADWORD=newElement() // 这四个指令从某个内存地址读出数据，写入某个寄存器
  val LOADBYTEU,LOADHWORDU=newElement()
  val LOADHI,LOADLO=newElement()   // 这两个指令是写入某个寄存器的高16位或者低16位（数据来源是立即数）
  val MFHI,MFLO = newElement()
  val MTLO,MTHI = newElement()
}
object OPStore extends SpinalEnum{
  val STOREBYTE,STOREHWORD,STOREWORD=newElement()
}

class IDOut extends Bundle{
  val op = out Bits( 3 bits)     // 运算类型
  val opSel = out Bits(8 bits) //运算子类型
  val opRnd1 = out Bits(GlobalConfig.dataBitsWidth)
  val opRnd2 = out Bits(GlobalConfig.dataBitsWidth)
  val writeReg = out Bool
  val writeRegAddr = out Bits(log2Up(GlobalConfig.regNum) bits)
  val inst = out Bits(GlobalConfig.dataBitsWidth)
  // 为了load store指令，将当前指令继续往下传
  // 为什么呢？以store指令为例，它需要计算储存地址（寄存器rs的值+imm)，还需要读出寄存器rt的值（这个值之后要写入内存）
  // 如果我们将计算地址的过程放在EX，则ID阶段需要提供的两个操作数，一个是寄存器rs中的值，一个是imm
  // 那么rt的值就没地方放了。
  // 因此现在修改为ID阶段提供 rs 和rt 的值，EX阶段自己通过inst来获取imm，计算出地址。

}

case class INST(bits:Bits){
  //import InstOPEnum._
  def op = bits.takeHigh(6)
  def func = bits.take(6)
  def rs:Bits=bits(21 to 25)
  def rt:Bits=bits(16 to 20)
  def rd=bits(11 to 15)
  def immI = bits.take(16)
  def immJ = bits.take(26)
  def immA = bits(6 to 10)
  def raw = bits
  def OPMatters : Bool = op =/=B(0,6 bits)
  def FUNCMatters :Bool = ~OPMatters
  def isJInst:Bool = op===B("6'b000010") || op=== B("6'b000011")
  def isIBInst:Bool = op=/=B(0,6 bits) && (~isJInst)
  /*
  def isLInst:Bool = isXInst(IDS.instsL)
  def isSInst:Bool = isXInst(IDS.instsS)
  def isLSInst:Bool = isXInst(IDS.instsLoadStore)
  */

  def isXInst(a:immutable.Seq[(SpinalEnumElement[_],(_))]):Bool={
    var result :Bool = False
    val instsList = for(i<- a) yield i._1
    val newL = for(i<-instsList) yield i.asBits.resize(op.getWidth) === op
    for(i <-newL){
      result = result | i
    }
    result
  }
}


class ID extends Component{

  val regHeap: RegHeapReadPort = master(new RegHeapReadPort)

  val exBack: EXOut = new EXOut().flip()
  val memBack: MEMOut = new MEMOut().flip()
  val wbBack: RegHeapWritePort = slave(new RegHeapWritePort)

  val pcPort: PCPort = master(new PCPort)

  val reqCTRL: StageCTRLBundle = master(new StageCTRLBundle)

  def <>(regs: RegHeap): Unit = regHeap <> regs.readPort
  def <>(ex:EX): Unit =exBack <> ex.exOut
  def <>(mem:MEM): Unit  = memBack <> mem.memOut
  def <>(wb:WB): Unit  = wbBack <> wb.wbOut
  def <>(pc:PC): Unit  = pcPort <> pc.writePort

  val lastStage: IFOut = new IFOut().flip()
  val idOut= new IDOut

  def JMP(target:Bits)={
    //val newPC = inst.immI.asSInt.resize(GlobalConfig.dataBitsWidth)+lastStage.pc.asSInt+1
    pcPort.writeEN := True
    pcPort.writeData := target.resized
  }

  val use_imma = False

  def doDecode(instsList:immutable.Seq[(MaskedLiteral, Map[Actions, _])]) ={
    for (i <- instsList){
      when(inst.raw === i._1){
        for((action,argument) <- i._2){
          if(action == READ_REG0){
            regHeap.readEns(0) := argument.asInstanceOf[Bool]
          }else if(action == READ_REG1){
            regHeap.readEns(1) := argument.asInstanceOf[Bool]
          }else if(action == WRITE_REG){
            idOut.writeReg := argument.asInstanceOf[Bool]
          }else if(action == WRITE_REG_ADDR){
            idOut.writeRegAddr := Insts.chooseSource(argument.asInstanceOf[Arguments],inst)
          }else if(action == READ_REG0_ADDR){
            regHeap.readAddrs(0) := Insts.chooseSource(argument.asInstanceOf[Arguments],inst)
          }else if(action == READ_REG1_ADDR){
            regHeap.readAddrs(1) := Insts.chooseSource(argument.asInstanceOf[Arguments],inst)
          }else if(action == INST_OP){
            idOut.op := argument.asInstanceOf[SpinalEnumElement[_]].asBits.resized
          }else if(action == INST_OPSEL){
            idOut.opSel := argument.asInstanceOf[SpinalEnumElement[_]].asBits.resized
          }else if(action == BRANCH_CONDITION){
            val oprnd2 = if(i._2.getOrElse(BRANCH_OPRND2,1)==0) B("32'h0") else idOut.opRnd2
            val JMPOrNot: Bool = argument.asInstanceOf[(Bits,Bits)=>Bool](idOut.opRnd1,oprnd2)
            when(JMPOrNot){
              val target:Bits = (i._2(BRANCH_TARGET) match{
                case IMMJ_ABSOLUTE =>  (lastStage.pc.asUInt+1).asBits.takeHigh(6) ## inst.immJ
                case IMMI_RELATIVE => (inst.immI.asSInt.resize(GlobalConfig.dataBitsWidth)+lastStage.pc.asSInt+1).asBits
                case REG => idOut.opRnd1
                case _ => B(0).resize(GlobalConfig.dataBitsWidth)
              })
              JMP(target)
            }
          }else if(action == IMMA_USE){
              use_imma := True
          }
        }
      }
    }
  }

  idOut.elements.foreach(a=>{
    a._2 := (if(a._1 =="writeReg") False else B(0)) }
  )

  reqCTRL.stateOut := StageStateEnum.ENABLE

  pcPort.writeEN :=False
  pcPort.writeData := 0

  regHeap.readAddrs(0) := 0
  regHeap.readAddrs(1) := 0
  regHeap.readEns(0) := False
  regHeap.readEns(1) := False


  val inst = INST(lastStage.inst)
  //决定立即数的符号位拓展
  // TODO:
  //  符号位拓展就这么简单决定是不妥的。比如load指令里也有可能无符号拓展和有符号拓展，得根据每个指定设置
  //  咳，这里目前还是可以的。之前看错了。load指令中关于无符号、有符号的拓展是从内存读出的时候进行的，而不是对这个立即数的拓展，load中这个立即数
  //  应该是有符号拓展
  val imm:Bits = (idOut.op === OpEnum.LOGIC.asBits.resize(idOut.op.getWidth))?
    inst.immI.resize(GlobalConfig.dataBitsWidth)|
    inst.immI.asSInt.resize(GlobalConfig.dataBitsWidth).asBits

  doDecode(Insts.AllInsts)


  var i = 0;
  for( rnd <- List(idOut.opRnd1,idOut.opRnd2)){
    // TODO:
    // 需要考虑，会有一些指令最后并没有写入寄存器，因此如果有这种情况，并不能使用这些指令的结果
    // 还要考虑，如果指令往$0写数据，那么这个数据也是不能用的
    when(regHeap.readEns(i)){
      rnd := regHeap.readDatas(i)
      when(exBack.writeReg && exBack.writeRegAddr===regHeap.readAddrs(i)){
        rnd := exBack.writeData
      }elsewhen(memBack.writeReg && memBack.writeRegAddr===regHeap.readAddrs(i)) {
        rnd := memBack.writeData
      }elsewhen(wbBack.writeEn && wbBack.writeAddr===regHeap.readAddrs(i)){
        rnd := wbBack.writeData
      }
    }otherwise{
      when(use_imma){
        rnd := inst.immA.resized
      }otherwise{
        rnd := imm
      }

    }
    i+=1
  }

}

