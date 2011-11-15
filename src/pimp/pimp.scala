package pimp

class AnyW(a: Any) {
  def ∈[T](xs: Seq[T]): Boolean = xs contains a
  def ≠[T](b: Any): Boolean = a != b
}

class BoolW(a: Boolean) {
  def ∨(b: Boolean) = a || b
  def ∧(b: Boolean) = a && b
}

class IterableW[T](a: Iterable[T]) {
  def ∀(f: T => Boolean) = a forall f
}

object Pimp {
  implicit def anyW(xs: Any) = new AnyW(xs)
  implicit def boolW(xs: Boolean) = new BoolW(xs)
  implicit def iterableW[T](xs: Iterable[T]) = new IterableW(xs)

  def ¬[T](b: Boolean): Boolean = !b
  val ⊤ = true
  val ⊥ = false
}