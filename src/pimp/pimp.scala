package pimp

class AnyW(elem: Any) {
  def ∈[T](xs: Seq[T]): Boolean = { xs contains elem }
}

object Pimp {
  implicit def anyW[T](xs: Any) = new AnyW(xs)
}