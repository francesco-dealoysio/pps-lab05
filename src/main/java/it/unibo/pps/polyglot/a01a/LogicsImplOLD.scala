package it.unibo.pps.polyglot.a01a
//import it.unibo.pps.polyglot.a01a.Logics.Result
import it.unibo.pps.util.Sequences.*
import it.unibo.pps.util.Sequences.Sequence.*
import scala.annotation.tailrec
import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
case class LogicsImplOld(private val fieldSize: Int, private val boatLenght: Int) extends LogicsScala:
  private final val MAX_ATTEMPTS: Int = 10
  private final val MAX_BOAT_FIRST_COLUMN: Int = boatLenght - 1
  //private var boatPosition: Sequence[(Int, Int)] = boatDeploy
  private final val boatRow: Int = randomCoordinate(0)
  private final val boatFirstColumn: Int = randomCoordinate(MAX_BOAT_FIRST_COLUMN)
  private final val boatLastColumn: Int = boatFirstColumn + boatLenght
  private var attempts: Int = 0
  private var hits: Int = 0

  def apply(size: Int, boat: Int): LogicsScala =
    LogicsImplOld(fieldSize, boatLenght)
/*
  def boatDeploy: Sequence[(Int, Int)] =
    val originRow: Int = randomCoordinate(0)
    val originColumn: Int = randomCoordinate(MAX_BOAT_FIRST_COLUMN)
    @tailrec
    def inner(n: Int, seq: Sequence[(Int, Int)]): Sequence[(Int, Int)] = n match
      case n if n > 0 => inner(n - 1, Cons((originRow, originColumn + boatLenght - n), seq))
      case _ => seq
    inner(boatLenght, Nil())
*/
  private def randomCoordinate(offset: Int): Int =
    Random.nextInt(fieldSize - offset)

  def hit(row: Int, col: Int): Result =
    attempts = attempts + 1
    if row == boatRow && col >= boatFirstColumn && col <= boatLastColumn then
      hits = hits + 1
      if hits == boatLenght then
        monitor
        Result.WON
      else if attempts == MAX_ATTEMPTS then
        monitor
        Result.LOST
      else
        monitor
        Result.HIT
    else if attempts == MAX_ATTEMPTS then
      monitor
      Result.LOST
    else
      monitor
      Result.MISS

  def monitor: Unit =
    println(s"Size of field: $fieldSize")
    println(s"Boat lenght..: $boatLenght")
    println(s"Boat position: ($boatRow, $boatFirstColumn)")
    println(s"Max attemps..: $MAX_ATTEMPTS")
    println(s"Attemps......: $attempts")
    println(s"Hits.........: $hits")

@main def tryLogicsImplOld: Unit =
  val logic = LogicsImplOld(10, 4)
  println(logic.monitor)
