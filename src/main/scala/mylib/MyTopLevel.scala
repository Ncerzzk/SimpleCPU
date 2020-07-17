/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

package mylib

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.util.Random

//Hardware definition

class TopLevel extends Component {
  val io = new Bundle{
    val result = out Bool
  }

  val fsm = new StateMachine{
    val counter = Reg(UInt(8 bits)) init (0)
    io.result := False

    val stateA : State = new State with EntryPoint{
      whenIsActive (goto(stateB))
    }
    val stateB : State = new State{
      onEntry(counter := 0)
      whenIsActive {
        counter := counter + 1
        when(counter === 4){
          goto(stateC)
        }
      }
      onExit(io.result := True)
    }
    val stateC : State = new State{
      whenIsActive (goto(stateA))
    }
  }
}

class MyTop2 extends Component{
  setName("top_module",Nameable.USER_SET)
  val io = new Bundle{
    val q=out(Reg(UInt(4 bits))) init(1)
    q.setName("q")
  }

  when(io.q === 10){
    io.q := 1
  }otherwise{
    io.q := io.q+1
  }
}

//Define a custom SpinalHDL configuration with synchronous reset instead of the default asynchronous one. This configuration can be resued everywhere
object MySpinalConfig extends SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC))

//Generate the MyTopLevel's Verilog using the above custom configuration.
object MyTopLevelVerilogWithCustomConfig {
  def main(args: Array[String]) {
    MySpinalConfig.generateVerilog(new SOC).printPruned()

    //SpinalVerilog(new MyTop2)
    //MySpinalConfig.generateVerilog(new MyTopLevel)
  }
}

object GenDivder {
  def main(args: Array[String]) {
    MySpinalConfig.generateVerilog(new CP0).printPruned()

    //SpinalVerilog(new MyTop2)
    //MySpinalConfig.generateVerilog(new MyTopLevel)
  }
}