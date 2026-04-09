package it.unibo.pps.polyglot.a01b

import java.util.Optional
import scala.util.Random
import it.unibo.pps.util.Sequences.*
import it.unibo.pps.util.Sequences.Sequence.*

trait ScalaLogics:
  def hit(x: Int, y: Int): Optional[Integer]
  def won(): Boolean

class LogicsImpl(size: Int, mines: Int) extends Logics, ScalaLogics:

  private val mineSet: Sequence[(Int, Int)] = generateMines(mines, Sequence.empty)
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
    case Nil()         => 0
    case Cons(_, tail) => 1 + count(tail)

  private def isInside(x: Int, y: Int): Boolean =
    x >= 0 && x < size && y >= 0 && y < size

  private def countAdjacentMines(x: Int, y: Int): Int =
    countNeighbours(x, y, -1, -1)

  private def countNeighbours(x: Int, y: Int, dx: Int, dy: Int): Int =
    if dx > 1 then 0
    else if dy > 1 then countNeighbours(x, y, dx + 1, -1)
    else
      val nx = x + dx
      val ny = y + dy
      val current =
        if (dx == 0 && dy == 0) || !isInside(nx, ny) || !mineSet.contains((nx, ny)) then 0
        else 1
      current + countNeighbours(x, y, dx, dy + 1)

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