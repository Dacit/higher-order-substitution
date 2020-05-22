sealed trait Type {
  override def toString: String =
    this match {
      case Type.E(t) => t.toString
      case Type.Fn(args, res) => s"${args.map(_.innerToString).mkString(", ")} → ${res.innerToString}"
    }
  private def innerToString: String =
    this match {
      case Type.E(t) => t.toString
      case t: Type.Fn => s"($t)"
    }
}

object Type {

  trait T0
  object T0 {
    case object Alpha extends T0 { override def toString: String = "α" }
    case object Beta extends T0 { override def toString: String = "β" }
    case object Gamma extends T0 { override def toString: String = "γ" }
  }
  case class E(t: T0) extends Type
  case class Fn(args: Vector[Type], res: Type) extends Type
  object Fn {
    def apply(arg1: Type, arg2: Type, args: Type*): Fn = {
      val allArgs = arg1 +: arg2 +: args

      Fn(allArgs.dropRight(1).toVector, allArgs.last)
    }
  }
}
