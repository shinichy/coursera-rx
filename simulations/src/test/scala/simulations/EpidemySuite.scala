package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EpidemySuite extends FunSuite {

  test("prevalence rate") {
    val prevalenceRate = 0.01

    val es = new EpidemySimulator
    val numInfected = es.persons.count(_.infected)

    assert(numInfected == es.SimConfig.population * prevalenceRate,
      "prevalence rate should be 0.01"
    )
  }

  test("moveTop") {
    val es = new EpidemySimulator
    val person = new es.Person(0)
    person.row = 1
    person.col = 1
    val (row, col) = person.newPosAfterMoveTop
    person.row = row
    person.col = col
    assert(row == 0 && col == 1)

    val (row2, col2) = person.newPosAfterMoveTop
    assert(row2 == es.SimConfig.roomRows - 1 && col2 == 1)
  }

  test("moveDown") {
    val es = new EpidemySimulator
    val person = new es.Person(0)
    person.row = es.SimConfig.roomRows - 2
    person.col = 1
    val (row, col) = person.newPosAfterMoveDown
    person.row = row
    person.col = col
    assert(row == es.SimConfig.roomRows - 1 && col == 1)

    val (row2, col2) = person.newPosAfterMoveDown
    assert(row2 == 0 && col2 == 1)
  }

  test("moveLeft") {
    val es = new EpidemySimulator
    val person = new es.Person(0)
    person.row = 1
    person.col = 1
    val (row, col) = person.newPosAfterMoveLeft
    person.row = row
    person.col = col
    assert(row == 1 && col == 0)

    val (row2, col2) = person.newPosAfterMoveLeft
    assert(row2 == 1 && col2 == es.SimConfig.roomColumns - 1)
  }

  test("moveRight") {
    val es = new EpidemySimulator
    val person = new es.Person(0)
    person.row = 1
    person.col = es.SimConfig.roomColumns - 2
    val (row, col) = person.newPosAfterMoveRight
    person.row = row
    person.col = col
    assert(row == 1 && col == es.SimConfig.roomColumns - 1)

    val (row2, col2) = person.newPosAfterMoveRight
    assert(row2 == 1 && col2 == 0)
  }

  test("a person moves to one of their neighbouring rooms (left, right, up, down) within the next 5 days. A person moves repeatedly until he is dead") {
    val es = new EpidemySimulator

    val chosenOne = es.persons.head

    var (row, col) = (chosenOne.row, chosenOne.col)

    var testDays = 6

    while (!es.agenda.isEmpty && es.agenda.head.time < testDays) {
      es.next
    }

    assert(!(chosenOne.row == row && chosenOne.col == col))

    row = chosenOne.row
    col = chosenOne.col

    testDays = 11

    while (!es.agenda.isEmpty && es.agenda.head.time < testDays) {
      es.next
    }

    assert(!(chosenOne.row == row && chosenOne.col == col))
  }

  test("A person avoids rooms with sick or dead (visibly infectious) people") {
    val es = new EpidemySimulator {
      lazy val person: Person = new Person(0) {
        row = 1
        col = 1
      }

      lazy val sickPerson: Person = new Person(1) {
        sick = true
        row = 0
        col = 1
      }

      lazy val deadPerson: Person = new Person(2) {
        dead = true
        row = 1
        col = 2
      }

      override lazy val persons: List[this.type#Person] = List(person, sickPerson, deadPerson)
    }

    val person: es.Person = new es.Person(0)
    person.row = 1
    person.col = 1

    val (row, col) = person.newPosAfterMoveTop
    assert(es.existsVisiblyInfectiousPersonInRoom(row, col))

    val (row2, col2) = person.newPosAfterMoveRight
    assert(es.existsVisiblyInfectiousPersonInRoom(row2, col2))

    val (row3, col3) = person.newPosAfterMoveLeft
    assert(!es.existsVisiblyInfectiousPersonInRoom(row3, col3))
  }

  test("dead person stays dead") {
    val es = new EpidemySimulator

    val chosenOne = es.persons.head
    chosenOne.infected = true
    chosenOne.sick = true
    chosenOne.dead = true
    chosenOne.immune = false

    val (row, col) = (chosenOne.row, chosenOne.col)

    val testDays = 100

    while (!es.agenda.isEmpty && es.agenda.head.time < testDays) {
      es.next

      assert(chosenOne.dead == true, "Dead person should keep dead state")
      assert(chosenOne.infected == true, "Dead person keeps infected")
      assert(chosenOne.immune == false, "Dead person cannot become immune")
      assert(chosenOne.sick == true, "Dead person keeps sick")
      assert(chosenOne.col == col && chosenOne.row == row, "Dead person cannot move")
    }
  }

  test("no longer visibly infectious after immune") {
    val immuneTime = 16

    val es = new EpidemySimulator

    val chosenOne = es.persons.head
    chosenOne.infect()

    while (!es.agenda.isEmpty && es.agenda.head.time < immuneTime + 1) {
      es.next
    }

    assert(!chosenOne.isVisiblyInfectious)
  }

  test("life cycle") {
    val es = new EpidemySimulator {
      SimConfig.dieRate = 1
    }

    val incubationTime = 6
    val dieTime = 14
    val immuneTime = 16
    val healTime = 18

    val prevalenceRate = 0.01
    val transRate = 0.4
    val dieRate = 1

    val infectedPerson = (es.persons.find {
      _.infected
    }).get

    //before incubation time
    while (es.agenda.head.time < incubationTime) {
      assert(infectedPerson.infected == true, "Infected person keeps infected in 6 days")
      assert(infectedPerson.sick == false, "Infected person does not get sick in 6 days")
      assert(infectedPerson.immune == false, "Infected person cannot become immune in 6 days")
      assert(infectedPerson.dead == false, "Infected person does not die in 6 days")
      es.next
    }

    //incubation time has passed, there should be an event for getting sick
    assert(es.agenda.head.time == incubationTime, "You should set a 'sick' event after incubation time")
    while (es.agenda.head.time == incubationTime) es.next
    assert(infectedPerson.sick == true, "Infected person should become sick after 6 days")

    //wait for dieTime
    while (es.agenda.head.time < dieTime) {
      assert(infectedPerson.infected == true, "Sick person keeps infected")
      assert(infectedPerson.sick == true, "Sick person keeps sick before turning immune")
      assert(infectedPerson.immune == false, "Sick person is not immune")
      assert(infectedPerson.dead == false, "Sick person does not die before 14 infected days")
      es.next
    }

    assert(es.agenda.head.time == dieTime, "You should set a 'die' event (decides with a probability 25% whether the person dies) after 14 days")
    while (es.agenda.head.time == dieTime) es.next
    assert(infectedPerson.dead == true)

    //wait for immuneTime
    while (es.agenda.head.time < immuneTime) {
      assert(infectedPerson.infected == true, "Dead person keeps infected")
      assert(infectedPerson.sick == true)
      assert(infectedPerson.immune == false)
      assert(infectedPerson.dead == true)
      es.next
    }
    assert(es.agenda.head.time == immuneTime)
    while (es.agenda.head.time == immuneTime) es.next
    assert(infectedPerson.immune == true)
    assert(infectedPerson.sick == false)
    assert(infectedPerson.dead == false)

    //wait for healTime
    while (es.agenda.head.time < healTime) {
      assert(infectedPerson.infected == true, "Immune person keeps infected")
      assert(infectedPerson.sick == false)
      assert(infectedPerson.immune == true)
      assert(infectedPerson.dead == false)
      es.next
    }
    assert(es.agenda.head.time == healTime)
    while (es.agenda.head.time == healTime) es.next
    assert(
      infectedPerson.infected == false &&
        infectedPerson.sick == false &&
        infectedPerson.dead == false &&
        infectedPerson.immune == false
    )
  }

  test("transmissibility rate") {
    var infectedTimes = 0
    for (i <- 0 to 100) {
      val es = new EpidemySimulator {
        SimConfig.transmissibilityRate = 1
      }

      val healthyPerson = (es.persons find {
        p => !p.infected
      }).get
      es.persons.withFilter(p => p != healthyPerson) foreach {
        _.infected = true
      }

      while (es.agenda.head.time < 6) es.next

      infectedTimes = infectedTimes + (if (healthyPerson.infected) 1 else 0)
    }
    assert(infectedTimes > 0, "A person should get infected according to the transmissibility rate when he moves into a room with an infectious person")
  }

}