package it.unibo.pps.polyglot.a01a

import it.unibo.pps.polyglot.a01a.Logics
import it.unibo.pps.polyglot.a01a.Logics.Result
import it.unibo.pps.polyglot.a01a.Logics.Result.*
import it.unibo.pps.util.Sequences.*
import it.unibo.pps.util.Sequences.Sequence.*

import scala.util.Random
/*
trait LogicsTrait:
  def fieldSize: Int
  def boatLenght: Int

  enum Result:
    case HIT, MISS, WON, LOST
    //case MISS
    //case WON
    //case LOST

  def hit(raw: Int, col: Int): Result

object LogicTrait:
  // Factory method for creating an empty  instance
  def apply(fieldSize: Int, boatLenght: Int): LogicsTrait =
    LogicsImpl(fieldSize, boatLenght)
//end LogicTrait
*/

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
case class LogicsImpl(val fieldSize: Int, val boatLenght: Int) extends Logics:
  import LogigsTrait.Result.*
  //assert(fieldSize != 5)

  private final val MAX_ATTEMPTS: Int = 10
  private final val MAX_BOAT_FIRST_COLUMN: Int = boatLenght - 1
  private final val boatRow: Int = randomCoordinate(0)
  private final val boatFirstColumn: Int = randomCoordinate(MAX_BOAT_FIRST_COLUMN)
  private final val boatLastColumn: Int = boatFirstColumn + boatLenght
  private var attempts: Int = 0
  private var hits: Int = 0

  def apply(size: Int, boat: Int): Logics =
    LogicsImpl(fieldSize, boatLenght)

  private def randomCoordinate(offset: Int): Int =
    Random.nextInt(fieldSize - offset)

  def hit(row: Int, col: Int): Result =
    attempts = attempts + 1
    if row == boatRow && col >= boatFirstColumn && col <= boatLastColumn then
      hits = hits + 1
      if hits == boatLenght then
        Result.WON
      else if attempts == MAX_ATTEMPTS then
        Result.LOST
      else
        Result.HIT
    else if attempts == MAX_ATTEMPTS then
      Result.LOST
    else
      Result.MISS

@main def tryLogicsImpl: Unit = {
  val li: LogicsImpl = LogicsImpl(10, 4)
  val li1 = li(5,3) // utilizza apply() definita nella case class e non considera assert
  println("nop")
}