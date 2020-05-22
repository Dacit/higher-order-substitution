import scala.math.max

import Head._
import Tree.SubstPair
import Type._

case class Abs(typ: Type, display: Option[String] = None)

case class Variable(name: String, typ: Type)

case class Term(binder: Vector[Abs], head: Head, args: Vector[Term], ret: E) {
  // Pretty-printing
  override def toString: String = prettyInternal(Vector.empty)

  private def innerPretty(v: Vector[Option[String]]): String = {
    if (binder.isEmpty && args.isEmpty) prettyInternal(v) else s"(${prettyInternal(v)})"
  }

  private def prettyInternal(v: Vector[Option[String]]): String = {
    val vNew = v ++ binder.map(_.display)
    val b = if (binder.isEmpty) "" else s"λ${binder.indices.reverse.map(Bound(_).pretty(vNew)).mkString(" ")}. "

    s"$b${head.pretty(vNew)}${args.map(t => s" ${t.innerPretty(vNew)}").mkString}"
  }

  // Checks
  def isRigid: Boolean = head.isRigid

  // Type inference
  def typ: Type = {
    if (binder.isEmpty) ret else Fn(binder.map(_.typ), ret)
  }

  def headType: Type = {
    if (args.isEmpty) ret else Fn(args.map(_.typ), ret)
  }

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
        val args = this.args.map(_.subst(p))
        substitute(p.e, args)
      case _ => copy(args = args.map(_.subst(p)))
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

          substitute(subst.shiftBounds(shift), args)

        case _ => copy(args = args.map(_.substBounds(start + n, range, shift + n)))
      }
    }
  }

  private def substitute(subst: Term, substArgs: Vector[Term]): Term = {
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
  def apply(head: Head, ret: E): Term = new Term(Vector.empty, head, Vector.empty, ret)
  def apply(binder: Vector[Abs], head: Head, ret: E): Term = new Term(binder, head, Vector.empty, ret)
  def apply(head: Head, args: Vector[Term], ret: E): Term = new Term(Vector.empty, head, args, ret)
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
