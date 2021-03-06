import scala.math.max

import Head._
import Tree.SubstPair

case class Abs(typ: Type, display: Option[String] = None)

case class Variable(name: String, typ: Type)
object Variable {
  def fresh(prefix: String, typ: Type, vars: Set[Variable]): String = {
    var i = -1
    var name = prefix
    while (vars.contains(Variable(name, typ))) {
      i += 1
      name = s"${prefix}_$i"
    }
    name
  }
}

case class Term(binder: Vector[Abs], head: Head, args: Vector[Term], ret: T0) {
  // Pretty-printing
  override def toString: String = pretty(Vector.empty)

  private def pretty(v: Vector[Option[String]]): String = {
    val vNew = v ++ binder.map(_.display)
    val b = if (binder.isEmpty) "" else s"λ${binder.indices.reverse.map(Bound(_).pretty(vNew)).mkString(" ")}. "
    val e = if (args.isEmpty) "" else s"(${args.map(_.pretty(vNew)).mkString(", ")})"

    s"$b${head.pretty(vNew)}$e"
  }

  // Checks
  def isRigid: Boolean = head.isRigid

  // Type inference
  def typ: Type = Type(binder.map(_.typ), ret)
  def headType: Type = Type(args.map(_.typ), ret)

  // Free variables
  def free: Set[Variable] =
    this.head match {
      case Free(name) => args.flatMap(_.free).toSet + Variable(name, headType)
      case _ => args.flatMap(_.free).toSet
    }

  // Substitution
  def subst(p: SubstPair): Term =
    head match {
      case Free(name) if name == p.v.name && headType == p.v.typ =>
        // First substitute in variables
        val args = this.args.map(_.subst(p))
        // Then build new term
        applySubst(p.e, args)
      case _ => copy(args = args.map(_.subst(p)))
    }

  private def applySubst(subst: Term, substArgs: Vector[Term]): Term = {
    val m = subst.binder.length
    val p1 = args.length
    val k = max(0, m - p1)

    val (argsInner, argsOuter) = substArgs.splitAt(m)

    this.copy(
      binder = binder ++ subst.binder.drop(p1),
      head = subst.head,
      args = subst.args.map(_.substBounds(k, argsInner, k)) ++ argsOuter
    )
  }

  private def substBounds(start: Int, range: Vector[Term], shift: Int): Term = {
    if (range.isEmpty) {
      this
    } else {
      val n = binder.length
      head match {
        case Bound(idx) if idx - n >= start && idx - n < start + range.length =>
          val subst = range((range.length - 1 + start) - (idx - n))
          val args = this.args.map(_.substBounds(start + n, range, shift + n))

          applySubst(subst.shiftBounds(shift), args)

        case _ => copy(args = args.map(_.substBounds(start + n, range, shift + n)))
      }
    }
  }

  private def shiftBounds(shift: Int, outer: Int = 0): Term = {
    if (shift == 0) {
      this
    } else {
      copy(
        head = head match {
          case Bound(idx) if idx >= outer + binder.length => Bound(idx + shift)
          case h => h
        },
        args = args.map(_.shiftBounds(shift, outer + binder.length))
      )
    }
  }
}
object Term {
  // C-tors
  def apply(head: Head): Term = Term(head, T0())
  def apply(head: Head, ret: T0): Term = Term(Vector.empty, head, ret)
  def apply(n: Int, head: Head): Term = Term(Vector.fill(n)(Abs(T0())), head, T0())
  def apply(n: Int, head: Head, ret: T0): Term = Term(Vector.fill(n)(Abs(T0())), head, ret)
  def apply(binder: Vector[Abs], head: Head): Term = Term(binder, head, T0())
  def apply(binder: Vector[Abs], head: Head, args: Vector[Term]): Term = Term(binder, head, args, T0())
  def apply(binder: Abs, head: Head, args: Vector[Term]): Term = Term(Vector(binder), head, args, T0())
  def apply(binder: Vector[Abs], head: Head, args: Term): Term = Term(binder, head, args, T0())
  def apply(binder: Abs, head: Head): Term = Term(Vector(binder), head, T0())
  def apply(binder: Vector[Abs], head: Head, ret: T0): Term = Term(binder, head, Vector.empty, ret)
  def apply(binder: Abs, head: Head, ret: T0): Term = Term(Vector(binder), head, ret)
  def apply(head: Head, args: Vector[Term]): Term = Term(head, args, T0())
  def apply(head: Head, args: Term): Term = Term(head, Vector(args), T0())
  def apply(head: Head, args: Vector[Term], ret: T0): Term = Term(Vector.empty, head, args, ret)
  def apply(head: Head, args: Term, ret: T0): Term = Term(head, Vector(args), ret)
  def apply(n: Int, head: Head, args: Vector[Term]): Term = Term(Vector.fill(n)(Abs(T0())), head, args, T0())
  def apply(n: Int, head: Head, args: Term): Term = Term(Vector.fill(n)(Abs(T0())), head, Vector(args), T0())
  def apply(n: Int, head: Head, args: Vector[Term], ret: T0): Term =
    Term(Vector.fill(n)(Abs(T0())), head, args, ret)
  def apply(n: Int, head: Head, args: Term, ret: T0): Term =
    Term(Vector.fill(n)(Abs(T0())), head, Vector(args), ret)
  def apply(binder: Abs, head: Head, args: Vector[Term], ret: T0): Term = Term(Vector(binder), head, args, ret)
  def apply(binder: Vector[Abs], head: Head, args: Term, ret: T0): Term = Term(binder, head, Vector(args), ret)
  def apply(binder: Abs, head: Head, args: Term, ret: T0): Term = Term(Vector(binder), head, Vector(args), ret)
}

sealed trait Head {
  def pretty(v: Vector[Option[String]]): String =
    this match {
      case Const(name) => s"c_$name"
      case Free(name) => s"f_$name"
      case Bound(idx) =>
        val vIdx = v.length - 1 - idx
        v(vIdx).map("b_" + _).getOrElse(s"x$vIdx")
    }
  def isRigid: Boolean =
    this match {
      case _: Const | _: Bound => true
      case _: Free => false
    }
}
object Head {
  case class Const(name: String) extends Head
  case class Free(name: String) extends Head
  case class Bound(idx: Int) extends Head
}
