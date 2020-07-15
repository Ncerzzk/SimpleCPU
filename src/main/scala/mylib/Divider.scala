package mylib

import spinal.core._
import spinal.lib.fsm._


class Divider(bitNum:Int) extends Component {
  val io = new Bundle with DefaultValue {
    val dividend = in UInt(bitNum bits)
    val divisor = in UInt(bitNum bits)
    val en = in Bool
    val sign = in Bool

    val quotient = out UInt(bitNum bits)
    val remainder = out UInt (bitNum bits)
    val ok = out Bool
    val busy = out Bool
  }

  io.setDefaultValue(io.ok,io.quotient,io.remainder,io.busy)

  val divisorReg = Reg(in UInt(bitNum bits))   // 除数寄存器
  val dividendMsb = Reg(Bool).init(False)
  val remQuoReg = Reg(UInt(bitNum*2 bits)).init(0)
  val resultSign = Reg(Bool).init(False)

  val fsm = new StateMachine{
    val idle:State = new State with EntryPoint{
      whenIsActive{
        when(io.en){
          goto(caculting)
        }
      }
    }
    val caculting:StateDelay = new StateDelay(bitNum+1){
      onEntry{
        when(io.sign){
          divisorReg := io.divisor.asSInt.abs
          remQuoReg := io.dividend.asSInt.abs.resized
          resultSign := io.dividend.msb ^ io.divisor.msb
          dividendMsb := io.dividend.msb
        }otherwise{
          remQuoReg := io.dividend.resized
          divisorReg := io.divisor
          resultSign := False
          dividendMsb := False
        }

      }
      whenCompleted{
        when(resultSign){
          io.quotient := (~remQuoReg.asBits.take(bitNum)).asUInt + U(1)
        }otherwise {
          io.quotient := remQuoReg.asBits.take(bitNum).asUInt
        }

        when(dividendMsb){
          io.remainder := (~remQuoReg.asBits.takeHigh(bitNum)).asUInt+U(1)
        }otherwise{
          io.remainder := remQuoReg.asBits.takeHigh(bitNum).asUInt
        }

        io.ok := True
        goto(idle)
      }
      onExit{
        remQuoReg := U(0)
      }
    }
    caculting.whenIsActive{
      val a = remQuoReg |<<1
      val high = a.asBits.takeHigh(bitNum).asUInt
      val subResult=high - divisorReg
      io.busy := True
      when(subResult.msb===False){
        remQuoReg := (subResult.asBits ## (a.asBits.take(bitNum) | B(1,bitNum bits))).asUInt
      }otherwise{
        remQuoReg := a
      }
    }
  }
}
