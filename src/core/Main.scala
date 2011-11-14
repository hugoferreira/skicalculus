package core

object Main extends App {
  import Term._

  val i = I ∙ I
  val c = K ∙ I ∙ "a" ∙ "b"
  println(c)
  println(c.reduce)
  println(c.reduce.reduce)
  println(c.reduce.reduce.reduce)
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

  def ∙(x: Term) = new ∙(this, x)
}

object Term {
  implicit def stringToVar(s: String): Var = Var(s)

  lazy val S = new S_()
  lazy val K = new K_()
  lazy val I = new I_()
}

case class S_() extends Term { override def toString = "S"}
case class K_() extends Term { override def toString = "K"}
case class I_() extends Term { override def toString = "I"}
case class Var(s: String) extends Term { override def toString = s }
case class ∙(x: Term, y: Term) extends Term { override def toString = "∙(" + x + ", " + y + ")" }