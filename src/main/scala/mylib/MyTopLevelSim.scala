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

object SocTest{
  def main(args:Array[String]):Unit = {
    SimConfig.withWave.compile(new SOC).doSim{dut=>
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



//MyTopLevel's testbench
/*
object MyTopLevelSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new TopLevel){dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      var modelState = 0
      for(idx <- 0 to 99){
        //Drive the dut inputs with random values
        dut.io.cond0 #= Random.nextBoolean()
        dut.io.cond1 #= Random.nextBoolean()

        //Wait a rising edge on the clock
        dut.clockDomain.waitRisingEdge()

        //Check that the dut values match with the reference model ones
        val modelFlag = modelState == 0 || dut.io.cond1.toBoolean
        assert(dut.io.state.toInt == modelState)
        assert(dut.io.flag.toBoolean == modelFlag)

        //Update the reference model value
        if(dut.io.cond0.toBoolean) {
          modelState = (modelState + 1) & 0xFF
        }
      }
    }
  }
}*/
