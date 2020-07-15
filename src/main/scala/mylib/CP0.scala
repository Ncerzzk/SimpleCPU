package mylib

import spinal.core._

import scala.collection.immutable.HashMap

class STATUS extends Bits{
  setWidth(GlobalConfig.dataBitsWidth.value)
  def CU = takeHigh(4)
  def RP = this(27)
  def RE=this(25)
  def BEV = this(22)
  def TS= this(21)
  def SR = this(20)
  def NMI = this(19)
  def IM = this(8 to 15)
  def UM = this(4)
  def ERL=this(2)
  def EXL = this(1)
  def IE=this(0)
}

class CAUSE extends Bits{
  setWidth(GlobalConfig.dataBitsWidth.value)
  def BD = this(31)
  def CE = this(28,2 bits)
  def DC = this(27)
  def PCI = this(26)
  def IV = this(23)
  def WP= this(22)
  def HIP=this(10 to 15)
  def SIP=this(8 to 9)
  def EXCCODE=this(2 to 6)
}

class CP0 extends Component {
  val Counter = Reg(Bits(GlobalConfig.dataBitsWidth)).init(0)
  val Status = Reg(new STATUS).init(0)
  val Cause = Reg(new CAUSE).init(0)


}
