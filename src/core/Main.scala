package core

import scalaz._
import Scalaz._

object Main extends App {
  import τ._

  val i = I ∙ I
  val c = K ∙ I ∙ 1 ∙ 2
  val f = S ∙ I ∙ I ∙ (S ∙ I ∙ I)  // fixpoint
  val rev = S ∙ (K ∙ (S ∙ I)) ∙ K ∙ 1 ∙ 2

  rev.reduceList.reverse foreach println

  println (1 :: 2 :: 3 :: Nil ∋ 3)
  println ((3 ≠ 2) ∧ ⊥)
  println(1 :: 2 :: 3 :: Nil ∀ (4 >))
}

sealed abstract class τ {
  import τ._

  def reduce: τ = this match {
    case I ∙ x => x
    case K ∙ x ∙ y => x
    case S ∙ x ∙ y ∙ z => x ∙ z ∙ (y ∙ z)
    case x ∙ y => x.reduce ∙ y.reduce
    case _ => this
  }

  def reduceList(ts: List[τ]): List[τ] = {
    val r = ts.head.reduce
    val newTs = r :: ts
    if (ts ∋ r) newTs else reduceList(newTs)
  }

  def reduceList: List[τ] = reduceList(List(this))
  def simplify = reduceList.head
  def ∙(x: τ) = new ∙(this, x)

  override def toString = this match {
    case S => "S"
    case K => "K"
    case I => "I"
    case x ∙ (k @ (_∙_)) => x + " ∙ (" + k + ")"
    case x ∙ y => x + " ∙ " + y
    case Var(x) => x.toString
  }
}

object τ {
  implicit def AnyToVar[T](s: T): Var[T] = new Var(s)

  lazy val S = new S_()
  lazy val K = new K_()
  lazy val I = new I_()
}

case class S_() extends τ
case class K_() extends τ
case class I_() extends τ
case class Var[T](s: T) extends τ
case class ∙(x: τ, y: τ) extends τ