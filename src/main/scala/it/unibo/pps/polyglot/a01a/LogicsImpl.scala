package it.unibo.pps.polyglot.a01a

import it.unibo.pps.polyglot.a01a.Logics
import it.unibo.pps.polyglot.a01a.Logics.Result
import it.unibo.pps.util.Sequences.*
import it.unibo.pps.util.Sequences.Sequence.*
//import scala.annotation.tailrec
import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
case class LogicsImpl(private val fieldSize: Int, private val boatLenght: Int) extends Logics:
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

@main def tryLogicsImpl: Unit =
  println("nop")