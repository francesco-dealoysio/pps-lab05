package it.unibo.pps.ex

import it.unibo.pps.util.Sequences.*
import it.unibo.pps.util.Sequences.Sequence.*

object sameCategory:

  private def allSameCategory(courses: Sequence[Course], cat: String): Boolean = courses match
    case Cons(head, tail) => head.category == cat && allSameCategory(tail, cat)
    case Nil()            => true

  def unapply(courses: Sequence[Course]): Option[String] =
    courses match
      case Cons(head, tail) =>
        val cat = head.category
        if allSameCategory(tail, cat) then Some(cat) else None
      case Nil()            => None


/* Solo per test
@main def testSameCategory(): Unit =
  val c1 = Course("SCALA01", "Scala", "Prof A", "Programming")
  val c2 = Course("PYTHON01", "Python", "Prof B", "Programming")
  val c3 = Course("DESIGN01", "Design", "Prof C", "Design")

  val same = Sequence(c1, c2)
  val different = Sequence(c1, c3)

  same match
    case sameCategory(cat) =>
      println(s"$same have same category $cat")
    case _ =>
      println(s"$same have different categories")

  different match
    case sameCategory(cat) =>
      println(s"$different have same category $cat")
    case _ =>
      println(s"$different have different categories")
      */