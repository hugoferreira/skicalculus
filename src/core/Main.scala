package core

object Main extends App {
  import Term._

  val i = I ∙ I
  val c = K ∙ I ∙ 1 ∙ 2
  val f = S ∙ I ∙ I ∙ (S ∙ I ∙ I)  // fixpoint

  c.reduceList.reverse foreach println
  f.reduceList.reverse foreach println
}

sealed abstract class Term {
  import Term._

  def reduce: Term = this match {
    case I ∙ x => x
    case K ∙ x ∙ y => x
    case S ∙ x ∙ y ∙ z => x ∙ z ∙ (y ∙ z)
    case x ∙ y => x.reduce ∙ y.reduce
    case _ => this
  }

  def reduceList(ts: List[Term]): List[Term] = {
    val r = ts.head.reduce
    val newTs = r :: ts
    if (ts contains r) newTs else reduceList(newTs)
  }

  def reduceList: List[Term] = reduceList(List(this))
  def simplify = reduceList.head
  def ∙(x: Term) = new ∙(this, x)

  override def toString = this match {
    case S => "S"
    case K => "K"
    case I => "I"
    case x ∙ (k @ (y ∙ z)) => x + " ∙ (" + k + ")"
    case x ∙ y => x + " ∙ " + y
    case Var(x) => x.toString
  }
}

object Term {
  implicit def AnyToVar[T](s: T): Var[T] = new Var(s)

  lazy val S = new S_()
  lazy val K = new K_()
  lazy val I = new I_()
}

case class S_() extends Term
case class K_() extends Term
case class I_() extends Term
case class Var[T](s: T) extends Term
case class ∙(x: Term, y: Term) extends Term