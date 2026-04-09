package it.unibo.pps.polyglot.a01b

import it.unibo.pps.polyglot.OptionToOptional
import it.unibo.pps.polyglot.a01b.Logics

import scala.jdk.javaapi.OptionConverters
import scala.util.Random
import java.util.Optional

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
/*
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:
 
  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    OptionToOptional(ScalaOptional.Empty()) // Option => Optional converter

  def won = false
*/

import it.unibo.pps.util.Sequences.*
import it.unibo.pps.util.Sequences.Sequence.*

class LogicsImpl(size: Int, mines: Int) extends Logics:

  private var mineSet: Sequence[(Int, Int)] = generateMines(mines, Sequence.empty)
  private var discovered: Sequence[(Int, Int)] = Sequence.empty
  private var lost: Boolean = false

  private def randomCell(): (Int, Int) =
    (Random.nextInt(size), Random.nextInt(size))

  private def generateMines(n: Int, acc: Sequence[(Int, Int)]): Sequence[(Int, Int)] =
    if n <= 0 then acc
    else
      val cell = randomCell()
      if acc.contains(cell) then
        generateMines(n, acc)
      else
        generateMines(n - 1, Cons(cell, acc))

  private def count(sequence: Sequence[(Int, Int)]): Int = sequence match
    case Cons(_, tail) => 1 + count(tail)
    case Nil() => 0

  private def isInside(x: Int, y: Int): Boolean =
    x >= 0 && x < size && y >= 0 && y < size

  private def countAdjacentMines(x: Int, y: Int): Int =
    var result = 0
    var dx = -1
    while dx <= 1 do
      var dy = -1
      while dy <= 1 do
        if !(dx == 0 && dy == 0) then
          val nx = x + dx
          val ny = y + dy
          if isInside(nx, ny) && mineSet.contains((nx, ny)) then
            result = result + 1
        dy = dy + 1
      dx = dx + 1
    result

  override def hit(x: Int, y: Int): Optional[Integer] =
    val cell = (x, y)
    if lost || discovered.contains(cell) then
      Optional.empty()
    else if mineSet.contains(cell) then
      lost = true
      Optional.empty()
    else
      discovered = Cons(cell, discovered)
      Optional.of(Integer.valueOf(countAdjacentMines(x, y)))

  override def won(): Boolean =
    !lost && count(discovered) == size * size - mines