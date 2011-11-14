package core

object Main extends App {
  import Term._

  val i = I ∙ I
  var c: Term = K ∙ I ∙ 1 ∙ 2
  println(c.reduceall)
}

sealed abstract class Term {
  import Term._

  def reduce: Term = this match {
    case I ∙ x => x
    case K ∙ x ∙ y => x
    case S ∙ x ∙ y ∙ z => x ∙ z ∙ (y ∙ z)
    case ∙(x, y) => x.reduce ∙ y.reduce
    case _ => this
  }

  def reduceall: Term = {
    def rec(ts: List[Term]): Term = ts match {
      case t1 :: t2 :: _  if t1 == t2 => t1
      case t :: _ => rec(t.reduce :: ts)
    }

    rec(List(this))
  }

  def ∙(x: Term) = new ∙(this, x)
}

object Term {
  implicit def AnyToVar[T](s: T): Var[T] = new Var(s)

  lazy val S = new S_()
  lazy val K = new K_()
  lazy val I = new I_()
}

case class S_() extends Term { override def toString = "S"}
case class K_() extends Term { override def toString = "K"}
case class I_() extends Term { override def toString = "I"}
case class Var[T](s: T) extends Term { override def toString = s.toString }
case class ∙(x: Term, y: Term) extends Term { override def toString = "(" + x + " ∙ " + y + ")" }