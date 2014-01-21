package simulations

import math.random

class EpidemySimulator extends Simulator {
  final val TOP = 0
  final val DOWN = 1
  final val LEFT = 2
  final val RIGHT = 3

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val prevalenceRate = 0.01
    var transmissibilityRate = 0.4
    var dieRate = 0.25
    val takeAirplaneRate = 0.00
    val mobility = 5
    val givenVaccinesRate = 0.00

    // to complete: additional parameters of simulation
  }

  import SimConfig._

  lazy val persons: List[Person] = {
    val persons = {for (i <- 0 until population) yield new Person(i)}.toList
    persons.take((persons.size * givenVaccinesRate).toInt) foreach (_.givenVaccines = true)
    persons
  } // to complete: construct list of persons

  def findHealthyPerson(): Person = {
    val person = persons(randomBelow(persons.size))
    if (!person.infected) person else findHealthyPerson()
  }

  def existsVisiblyInfectiousPersonInRoom(row: Int, col: Int): Boolean = {
    existsSomePersonInRoom(row, col, _.isVisiblyInfectious)
  }
  def existsInfectiousPersonInRoom(row: Int, col: Int): Boolean = {
    existsSomePersonInRoom(row, col, _.infected == true)
  }

  private def existsSomePersonInRoom(row: Int, col: Int, condition: Person => Boolean): Boolean = {
    persons.exists(p => p.isSameRoom(row, col) && condition(p))
  }

  def movableDirections(p: Person): List[Int] = {
    List(TOP, DOWN, LEFT, RIGHT) filter { direction =>
      val (newRow, newCol) = direction match {
        case TOP => p.newPosAfterMoveTop
        case DOWN => p.newPosAfterMoveDown
        case LEFT => p.newPosAfterMoveLeft
        case RIGHT => p.newPosAfterMoveRight
      }
      !existsVisiblyInfectiousPersonInRoom(newRow, newCol)
    }
  }

  // infect persons with prevalenceRate
  for (i <- 0 until (persons.size * prevalenceRate).toInt) {
    val person = findHealthyPerson()
    person.infect()
  }

  class Person (val id: Int) {
    final val periodBeforeSick = 6
    final val periodBeforeDead = 14
    final val periodBeforeImmune = 16
    final val periodBeforeHealthyAgain = 18

    var infected = false
    var sick = false
    var immune = false
    var dead = false
    var givenVaccines = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    moveRepeatedly()

    //
    // to complete with simulation logic
    //
    def moveDelay: Int = {
      val delay = randomBelow(mobility) + 1
      delay
//      if (infected) delay * 2 else delay
    }

    def newPosAfterMoveTop = {
      val newRow = if (row - 1 < 0) roomRows - 1 else row - 1
      (newRow, col)
    }

    def newPosAfterMoveDown = {
      val newRow = if (row + 1 > roomRows - 1) 0 else row + 1
      (newRow, col)
    }

    def newPosAfterMoveLeft = {
      val newCol = if (col - 1 < 0) roomColumns - 1 else col - 1
      (row, newCol)
    }

    def newPosAfterMoveRight = {
      val newCol = if (col + 1 > roomColumns - 1) 0 else col + 1
      (row, newCol)
    }

    def randomlyMove(): Boolean = {
      val movableDirectionList: List[Int] = movableDirections(this)
      if (dead || movableDirectionList.isEmpty) return false

      val (newRow, newCol) = movableDirectionList(randomBelow(movableDirectionList.size)) match {
        case TOP => newPosAfterMoveTop
        case DOWN => newPosAfterMoveDown
        case LEFT => newPosAfterMoveLeft
        case RIGHT => newPosAfterMoveRight
      }
      row = newRow
      col = newCol
      true
    }

    def randomlyMoveWithAirTraffic(): Boolean = {
      row = randomBelow(roomRows)
      col = randomBelow(roomColumns)
      true
    }

    def infect() = {
      if (!givenVaccines) {
        infected = true
        afterDelay(periodBeforeSick)(gotSick())
        afterDelay(periodBeforeDead)(die())
        afterDelay(periodBeforeImmune)(gotImmune())
        afterDelay(periodBeforeHealthyAgain)(gotHealthy())
      }
    }

    def gotSick() = sick = true

    def die() = {
      if (random < SimConfig.dieRate) dead = true
    }

    def gotImmune() = {
      immune = true
      sick = false
      dead = false
    }

    def gotHealthy() = {
      infected = false
      sick = false
      immune = false
      dead = false
    }

    def moveRepeatedly(): Unit = {
      afterDelay(moveDelay) {
        val success = if (random < takeAirplaneRate) randomlyMoveWithAirTraffic() else randomlyMove()
        if (success && !infected && !immune && existsInfectiousPersonInRoom(row, col) && random < transmissibilityRate) {
          infect()
        }
        moveRepeatedly()
      }
    }

    def isVisiblyInfectious = sick || dead

    def isSameRoom(aRow: Int, aCol: Int) = row == aRow && col == aCol
  }
}
