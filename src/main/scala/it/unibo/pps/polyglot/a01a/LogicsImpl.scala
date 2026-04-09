package it.unibo.pps.polyglot.a01a

import scala.util.Random

trait LogicsTrait:
  def fieldSize: Int
  def boatLenght: Int
  def hit(raw: Int, col: Int): LogicsTrait.Result

object LogicsTrait:
  enum Result:
    case HIT, MISS, WON, LOST

  // Factory method for creating an empty  instance
  def apply(fieldSize: Int, boatLenght: Int): LogicsTrait =
    LogicsImpl(fieldSize, boatLenght)

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
  case class LogicsImpl(val fieldSize: Int, val boatLenght: Int) extends LogicsTrait:
    private final val MAX_ATTEMPTS: Int = 10
    private final val MAX_BOAT_FIRST_COLUMN: Int = boatLenght - 1
    private final val boatRow: Int = randomCoordinate(0)
    private final val boatFirstColumn: Int = randomCoordinate(MAX_BOAT_FIRST_COLUMN)
    private final val boatLastColumn: Int = boatFirstColumn + boatLenght
    private var attempts: Int = 0
    private var hits: Int = 0

    def apply(size: Int, boat: Int): LogicsTrait = LogicsImpl(fieldSize, boatLenght)

    private def randomCoordinate(offset: Int): Int =
      Random.nextInt(fieldSize - offset)

    def hit(row: Int, col: Int): LogicsTrait.Result =
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

@main def tryLogicsImpl: Unit =
  //val li = LogicsTrait(10,4)
  println("nop")