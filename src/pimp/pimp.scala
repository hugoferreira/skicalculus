package pimp

class AnyW(a: Any) {
  def ∈[T](xs: Seq[T]): Boolean = xs contains a
  def ≠[T](b: Any): Boolean = a != b
}

class BoolW(a: Boolean) {
  def ∨(b: Boolean) = a || b
  def ∧(b: Boolean) = a && b
}

object Pimp {
  implicit def anyW[T](xs: Any) = new AnyW(xs)
  implicit def boolW[T](xs: Boolean) = new BoolW(xs)

  def ¬[T](b: Boolean): Boolean = !b
  val ⊤ = true
  val ⊥ = false
}