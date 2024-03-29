package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal

  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def connection(input: Wire, output: Wire)  {
    def connectionAction() {
      output.setSignal(input.getSignal)
    }

    input addAction connectionAction
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire): Unit = {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  def andGate(inWires: Seq[Wire], output: Wire): Unit = {
    require(inWires.size > 1)

    if (inWires.size == 2) {
      andGate(inWires(0), inWires(1), output)
    } else {
      val newOutWire = new Wire
      andGate(inWires(0), inWires(1), newOutWire)
      val lastInWire = inWires.last
      val tmpOutput = inWires.drop(2).dropRight(1).foldLeft(newOutWire) { (outWire, inWire) =>
        val tmpOutput = new Wire
        andGate(outWire, inWire, tmpOutput)
        tmpOutput
      }
      andGate(tmpOutput, lastInWire, output)
    }
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig)}
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val notIn1, notIn2, notOut = new Wire
    inverter(a1, notIn1)
    inverter(a2, notIn2)
    andGate(notIn1, notIn2, notOut)
    inverter(notOut, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    if (c.isEmpty) {
      connection(in, out(0))
      return
    }

    val notAndControlWires = c map { controlWire =>
      val not = new Wire
      inverter(controlWire, not)
      (not, controlWire)
    }

    val outIndeces = for (i <- 0 until out.size) yield i
    permute(notAndControlWires, outIndeces.toList.reverse, List(in))

    def permute(notAndControl: List[(Wire, Wire)], outIndeces: List[Int], inList: List[Wire]): Unit = {
      notAndControl.headOption match {
        case Some((not, in)) =>
          val (leftIndeces, rightIndeces) = outIndeces.splitAt(outIndeces.size / 2)
          permute(notAndControl.drop(1), leftIndeces, not :: inList)
          permute(notAndControl.drop(1), rightIndeces, in :: inList)
        case _ =>
          require(outIndeces.size == 1)
          andGate(inList, out(outIndeces.head))
      }
    }
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
