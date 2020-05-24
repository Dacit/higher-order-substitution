import scala.language.implicitConversions

trait T0
object T0 {
  def apply(): T0 = Alpha
  case object Alpha extends T0 { override def toString: String = "α" }
  case object Beta extends T0 { override def toString: String = "β" }
  case object Gamma extends T0 { override def toString: String = "γ" }
}
case class Type(args: Vector[Type], ret: T0) {
  def isElementary: Boolean = args.isEmpty

  override def toString: String = {
    val h = if (isElementary) "" else s"${args.map(_.innerToString).mkString(", ")} → "
    s"$h$ret"
  }

  private def innerToString: String = {
    if (isElementary) toString else s"($toString)"
  }
}
object Type {
  implicit def apply(e: T0): Type = Type(Vector.empty, e)
}
object Fn {
  // C-tors
  def apply(arg: Type, moreArgs: Type*): Type = {
    val args = arg +: moreArgs
    if (!args.last.isElementary) {
      throw new IllegalArgumentException("Last type must be elementary!")
    }
    Type(args.dropRight(1).toVector, args.last.ret)
  }
}
