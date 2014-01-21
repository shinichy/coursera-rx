package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal == false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal == true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal == true, "or 3")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal == false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal == true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal == true, "or 3")
  }

  test("multiAndGate 2 example") {
    val in1, in2, out = new Wire
    andGate(Seq(in1, in2), out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal == false)

    in1.setSignal(true)
    run

    assert(out.getSignal == false)

    in2.setSignal(true)
    run

    assert(out.getSignal == true)
  }
  test("multiAndGate 3 example") {
    val in1, in2, in3, out = new Wire
    andGate(Seq(in1, in2, in3), out)
    in1.setSignal(false)
    in2.setSignal(false)
    in3.setSignal(false)
    run

    assert(out.getSignal == false)

    in1.setSignal(true)
    run

    assert(out.getSignal == false)

    in2.setSignal(true)
    run

    assert(out.getSignal == false)

    in3.setSignal(true)
    run

    assert(out.getSignal == true)
  }

  test("multiAndGate 4 example") {
    val in1, in2, in3, in4, out = new Wire
    andGate(Seq(in1, in2, in3, in4), out)
    in1.setSignal(false)
    in2.setSignal(false)
    in3.setSignal(false)
    in4.setSignal(false)
    run

    assert(out.getSignal == false)

    in1.setSignal(true)
    run

    assert(out.getSignal == false)

    in2.setSignal(true)
    run

    assert(out.getSignal == false)

    in3.setSignal(true)
    run

    assert(out.getSignal == false)

    in4.setSignal(true)
    run

    assert(out.getSignal == true)
  }

  test("multiAndGate 5 example") {
    val in1, in2, in3, in4, in5, out = new Wire
    andGate(Seq(in1, in2, in3, in4, in5), out)
    in1.setSignal(false)
    in2.setSignal(false)
    in3.setSignal(false)
    in4.setSignal(false)
    in5.setSignal(false)
    run

    assert(out.getSignal == false)

    in1.setSignal(true)
    run

    assert(out.getSignal == false)

    in2.setSignal(true)
    run

    assert(out.getSignal == false)

    in3.setSignal(true)
    run

    assert(out.getSignal == false)

    in4.setSignal(true)
    run

    assert(out.getSignal == false)

    in5.setSignal(true)
    run

    assert(out.getSignal == true)
  }

  test("demux n 0 example") {
    val in = new Wire
    val n = 0
    val controlWires = (0 until n) map (_ => new Wire)
    val outWires = (0 until math.pow(2, n).toInt) map (_ => new Wire)
    demux(in, controlWires.toList, outWires.toList)

    in.setSignal(false)
    run

    assert(outWires(0).getSignal == false)

    in.setSignal(true)
    run

    assert(outWires(0).getSignal == true)
  }

  test("demux n 1 example") {
    val in = new Wire
    val n = 1
    val controlWires = (0 until n) map (_ => new Wire)
    val outWires = (0 until math.pow(2, n).toInt) map (_ => new Wire)
    demux(in, controlWires.toList, outWires.toList)

    in.setSignal(false)
    controlWires(0).setSignal(false)
    run

    assert(outWires(0).getSignal == false)
    assert(outWires(1).getSignal == false)

    in.setSignal(false)
    controlWires(0).setSignal(true)
    run

    assert(outWires(0).getSignal == false)
    assert(outWires(1).getSignal == false)

    in.setSignal(true)
    controlWires(0).setSignal(false)
    run

    assert(outWires(0).getSignal == false)
    assert(outWires(1).getSignal == true)

    in.setSignal(true)
    controlWires(0).setSignal(true)
    run

    assert(outWires(0).getSignal == true)
    assert(outWires(1).getSignal == false)
  }

  test("demux n 2 example") {
    val in = new Wire
    val n = 2
    val controlWires = (0 until n) map (_ => new Wire)
    val outWires = (0 until math.pow(2, n).toInt) map (_ => new Wire)
    demux(in, controlWires.toList, outWires.toList)

    in.setSignal(false)
    controlWires(0).setSignal(false)
    controlWires(1).setSignal(false)
    run

    assert(outWires(0).getSignal == false)
    assert(outWires(1).getSignal == false)
    assert(outWires(2).getSignal == false)
    assert(outWires(3).getSignal == false)

    in.setSignal(true)
    run

    assert(outWires(0).getSignal == false)
    assert(outWires(1).getSignal == false)
    assert(outWires(2).getSignal == false)
    assert(outWires(3).getSignal == true)

    controlWires(0).setSignal(false)
    controlWires(1).setSignal(true)
    run

    assert(outWires(0).getSignal == false)
    assert(outWires(1).getSignal == false)
    assert(outWires(2).getSignal == true)
    assert(outWires(3).getSignal == false)

    controlWires(0).setSignal(true)
    controlWires(1).setSignal(false)
    run

    assert(outWires(0).getSignal == false)
    assert(outWires(1).getSignal == true)
    assert(outWires(2).getSignal == false)
    assert(outWires(3).getSignal == false)

    controlWires(0).setSignal(true)
    controlWires(1).setSignal(true)
    run

    assert(outWires(0).getSignal == true)
    assert(outWires(1).getSignal == false)
    assert(outWires(2).getSignal == false)
    assert(outWires(3).getSignal == false)
  }

  test("demux n 3 example") {
    val in = new Wire
    val n = 3
    val controlWires = (0 until n) map (_ => new Wire)
    val outWires = (0 until math.pow(2, n).toInt) map (_ => new Wire)
    demux(in, controlWires.toList, outWires.toList)

    in.setSignal(true)

    controlWires(0).setSignal(false)
    controlWires(1).setSignal(false)
    controlWires(2).setSignal(false)
    run

    assert(outWires(0).getSignal == false)
    assert(outWires(1).getSignal == false)
    assert(outWires(2).getSignal == false)
    assert(outWires(3).getSignal == false)
    assert(outWires(4).getSignal == false)
    assert(outWires(5).getSignal == false)
    assert(outWires(6).getSignal == false)
    assert(outWires(7).getSignal == true)

    controlWires(0).setSignal(false)
    controlWires(1).setSignal(false)
    controlWires(2).setSignal(true)
    run

    assert(outWires(0).getSignal == false)
    assert(outWires(1).getSignal == false)
    assert(outWires(2).getSignal == false)
    assert(outWires(3).getSignal == false)
    assert(outWires(4).getSignal == false)
    assert(outWires(5).getSignal == false)
    assert(outWires(6).getSignal == true)
    assert(outWires(7).getSignal == false)

    controlWires(0).setSignal(false)
    controlWires(1).setSignal(true)
    controlWires(2).setSignal(false)
    run

    assert(outWires(0).getSignal == false)
    assert(outWires(1).getSignal == false)
    assert(outWires(2).getSignal == false)
    assert(outWires(3).getSignal == false)
    assert(outWires(4).getSignal == false)
    assert(outWires(5).getSignal == true)
    assert(outWires(6).getSignal == false)
    assert(outWires(7).getSignal == false)

    controlWires(0).setSignal(false)
    controlWires(1).setSignal(true)
    controlWires(2).setSignal(true)
    run

    assert(outWires(0).getSignal == false)
    assert(outWires(1).getSignal == false)
    assert(outWires(2).getSignal == false)
    assert(outWires(3).getSignal == false)
    assert(outWires(4).getSignal == true)
    assert(outWires(5).getSignal == false)
    assert(outWires(6).getSignal == false)
    assert(outWires(7).getSignal == false)

    controlWires(0).setSignal(true)
    controlWires(1).setSignal(false)
    controlWires(2).setSignal(false)
    run

    assert(outWires(0).getSignal == false)
    assert(outWires(1).getSignal == false)
    assert(outWires(2).getSignal == false)
    assert(outWires(3).getSignal == true)
    assert(outWires(4).getSignal == false)
    assert(outWires(5).getSignal == false)
    assert(outWires(6).getSignal == false)
    assert(outWires(7).getSignal == false)

    controlWires(0).setSignal(true)
    controlWires(1).setSignal(false)
    controlWires(2).setSignal(true)
    run

    assert(outWires(0).getSignal == false)
    assert(outWires(1).getSignal == false)
    assert(outWires(2).getSignal == true)
    assert(outWires(3).getSignal == false)
    assert(outWires(4).getSignal == false)
    assert(outWires(5).getSignal == false)
    assert(outWires(6).getSignal == false)
    assert(outWires(7).getSignal == false)

    controlWires(0).setSignal(true)
    controlWires(1).setSignal(true)
    controlWires(2).setSignal(false)
    run

    assert(outWires(0).getSignal == false)
    assert(outWires(1).getSignal == true)
    assert(outWires(2).getSignal == false)
    assert(outWires(3).getSignal == false)
    assert(outWires(4).getSignal == false)
    assert(outWires(5).getSignal == false)
    assert(outWires(6).getSignal == false)
    assert(outWires(7).getSignal == false)

    controlWires(0).setSignal(true)
    controlWires(1).setSignal(true)
    controlWires(2).setSignal(true)
    run

    assert(outWires(0).getSignal == true)
    assert(outWires(1).getSignal == false)
    assert(outWires(2).getSignal == false)
    assert(outWires(3).getSignal == false)
    assert(outWires(4).getSignal == false)
    assert(outWires(5).getSignal == false)
    assert(outWires(6).getSignal == false)
    assert(outWires(7).getSignal == false)
  }
}
