package mylib

import spinal.core.{Area, Bits, Bool, Data, SpinalEnumElement, when}

class IDBasicArea {
  def build(cpu: CPU2): Unit ={
    import cpu._

    ID plug new Area{
      import ID._
      val inst = INST(input(INSTRUCTION))
      def getOprnd[T <: Data]( a: SpinalEnumElement[OPrndSource.type],which: Stageable[_]): Bits ={
        if(a==OPrndSource.REG){
          which match {
            case OPRND1_SRC =>insert(REG_HEAP_DATA0)
            case OPRND2_SRC =>insert(REG_HEAP_DATA1)
          }
        }else{
          inst.immI.resize(GlobalConfig.dataBitsWidth)
        }
      }
      for (i <- Instructions.instsAll){
        when(i._1 === inst.raw){
          for((actionName,actionValue) <- i._2){
            actionName match{
              case WRITE_REG => output(WRITE_REG_EN):= actionValue.asInstanceOf[Bool]
              case READ_REG0 => insert(REG_HEAP_EN0) := actionValue.asInstanceOf[Bool]
              case READ_REG1 => insert(REG_HEAP_EN1) := actionValue.asInstanceOf[Bool]
              case OP => output(OP) := actionValue.asInstanceOf[SpinalEnumElement[_]].asBits.resized
              case OPSEL => output(OPSEL) :=actionValue.asInstanceOf[SpinalEnumElement[_]].asBits.resized
              case OPRND1_SRC => output(OPRND1):= getOprnd(actionValue.asInstanceOf[SpinalEnumElement[OPrndSource.type]],OPRND1_SRC)
              case OPRND2_SRC => output(OPRND2):=getOprnd(actionValue.asInstanceOf[SpinalEnumElement[OPrndSource.type]],OPRND2_SRC)
              case _ =>
            }
          }
        }
      }


    }
  }

}
