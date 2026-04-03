package it.unibo.pps.polyglot.a05b

import it.unibo.pps.polyglot.a05b.Logics

import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
case class LogicsImpl(private val size: Int) extends Logics:
  private val originX: Int = randomCoordinate
  private val originY: Int = randomCoordinate
  private val maxTick: Int = getMaxTick
  private var currentTick: Int = 0

  def apply(size: Int): Logics =
    LogicsImpl(size)

  override def tick(): Unit =
    currentTick = currentTick + 1

  override def isOver: Boolean =
    currentTick > maxTick

  override def hasElement(x: Int, y: Int): Boolean =
    val dx = math.abs(x - originX)
    val dy = math.abs(y - originY)

    x == originX && dy <= currentTick ||
    y == originY && dx <= currentTick ||
    dx == dy && dx <= currentTick

  private def randomCoordinate: Int =
    1 + Random.nextInt(size - 2)

  private def getMaxTick: Int =
    val n: Int = Math.abs(size - originY - 1)
    val s: Int = originY
    val e: Int = Math.abs(size - originX - 1)
    val w: Int = originX
    math.min(n, math.min(s, math.min(w,e)))

@main def tryLogicsImpl(): Unit =
  println("nop")
