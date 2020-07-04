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
object MUTTest{
  def main(args:Array[String]):Unit = {
    SimConfig.withWave.compile(new Mut()).doSim{dut=>
      dut.clockDomain.forkStimulus(period = 10)
      val test=List(
        ((-10),(-100000)),
        ((10),(1000000)),
        ((10),(1000000)),
        ((-10),(1000000)),
          (0,0)

      )
      for(i <- test){
        dut.io.data1 #= i._1
        dut.io.data2 #= i._2
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
      for(i <- 0 to 20){
        dut.clockDomain.waitSampling()
      }

    }
  }
}


