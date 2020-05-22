sealed trait Type {
  def pretty: String = this match {
    case Type.E(t) => t.pretty
    case Type.Fn(args, res) => s"${args.map(_.innerPretty).mkString(", ")} → ${res.innerPretty}"
  }
  private def innerPretty: String = this match {
    case Type.E(t) => t.pretty
    case t: Type.Fn => s"(${t.pretty})"
  }
}

object Type {
  trait T0 {
    def pretty: String
  }
  object T0 {
    case object Alpha extends T0 { override def pretty: String = "α" }
    case object Beta extends T0 { override def pretty: String = "β" }
    case object Gamma extends T0 { override def pretty: String = "γ" }
  }
  case class E(t: T0) extends Type
  case class Fn(args: Vector[Type], res: Type) extends Type
}
