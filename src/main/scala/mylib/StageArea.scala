package mylib

import spinal.core._

import scala.collection.mutable

class StageArea extends Area{
  val ctrl = StageStateEnum()
  val inputs = mutable.HashMap[Stageable[Data],Data]()
  val inserts = mutable.HashMap[Stageable[Data],Data]()
  val outputs = mutable.HashMap[Stageable[Data],Data]()

  val inputsDefault   = mutable.HashMap[Stageable[Data],Data]()
  val outputsDefault  = mutable.HashMap[Stageable[Data],Data]()

  def plug(area:Area): Unit ={
    area.setCompositeName(this,getName()).reflectNames()
  }

  def input[T <: Data](keys:Stageable[T]*):Unit = {keys.foreach(input)}
  def output[T <: Data](keys:Stageable[T]*) :Unit= {keys.foreach(output)}
  def input[T <: Data ](key:Stageable[T])= {
    inputs.getOrElseUpdate(key.asInstanceOf[Stageable[Data]],outsideCondScope {
      val input= key()
      //inputsDefault(key.asInstanceOf[Stageable[Data]]) = inputDefault
      //input := inputDefault
      input.asInstanceOf[Data].setPartialName(this,"in_"+key.getName())
    }).asInstanceOf[T]
  }
  def insert[T <: Data](key : Stageable[T]) : T = {
    inserts.getOrElseUpdate(key.asInstanceOf[Stageable[Data]],outsideCondScope(
      key().setPartialName(this,"insert_"+key.getName()))
    ).asInstanceOf[T]
  }
  def output[T <: Data](key : Stageable[T]) : T = {
    outputs.getOrElseUpdate(key.asInstanceOf[Stageable[Data]],outsideCondScope{
      val output =Reg(key())
      output match{
        case bit:Bits=>bit.init(B(0))
        case bool:Bool=>bool.init(False)
        case _ =>
      }
      output.setPartialName(this, "out_"+key.getName())
    }).asInstanceOf[T]
  }

  def outsideCondScope[T](that : => T) : T = {
    val body = Component.current.dslBody
    body.push()
    val swapContext = body.swap()
    val ret = that
    body.pop()
    swapContext.appendBack()
    ret
  }
}

class Stageable[T <: Data](_dataType : => T) extends HardType[T](_dataType) with Nameable{
  def dataType = apply()
  setWeakName(this.getClass.getSimpleName.replace("$",""))
}

