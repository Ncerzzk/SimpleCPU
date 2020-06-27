package mylib

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.util.Random

object RomTest {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new InstRom).doSim{ dut =>
      //Simulation code here
      dut.clockDomain.forkStimulus(period = 10)

      for(i <- 0 to 3){
        dut.io.addr #= i
        dut.io.en #= true
        dut.clockDomain.waitSampling()
      }
    }
  }
}

object SocTestBasic{
  def main(args:Array[String]):Unit = {
    SimConfig.withWave.compile(new SOC()).doSim{dut=>
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.deassertReset()

      dut.clockDomain.assertReset()
      sleep(10)
      dut.clockDomain.risingEdge()
      sleep(10)
      dut.clockDomain.deassertReset()
      dut.clockDomain.fallingEdge()
      for(i <- 0 to 10){
        dut.clockDomain.waitSampling()
      }

    }
  }
}


