import Head._
import Tree.SubstPair
import scala.math.max

import Type.{E, Fn}

case class Abs(typ: Type, display: Option[String] = None)

case class Variable(name: String, typ: Type)

case class Term(binder: Vector[Abs], head: Head, args: Vector[Term], ret: E) {
  // Pretty-printing
  def pretty: String = prettyInternal(Vector.empty)
  private def innerPretty(v: Vector[Option[String]]): String = {
    if (binder.isEmpty && args.isEmpty) prettyInternal(v)
    else s"(${prettyInternal(v)})"
  }
  private def prettyInternal(v: Vector[Option[String]]): String = {
    val vNew = v ++ binder.map(_.display)
    val b =
      if (binder.isEmpty) {
        ""
      } else {
        s"λ${binder.indices.reverse.map(Bound(_).pretty(vNew)).mkString(" ")}. "
      }
    s"$b${head.pretty(vNew)}${args.map(t => s" ${t.innerPretty(vNew)}").mkString}"
  }

  // Checks
  def isRigid: Boolean = head.isRigid

  // Type inference
  def typ: Type = if (binder.isEmpty) ret else Fn(binder.map(_.typ), ret)
  def headType: Type = head match {
    case Bound(idx) => binder(binder.length - 1 - idx).typ
    case _: Const | _: Free =>
      if (args.isEmpty) ret else Fn(args.map(_.typ), ret)
  }

  // Free variables
  def free: Set[Variable] =
    this.head match {
      case Free(name) => args.flatMap(_.free).toSet + Variable(name, headType)
      case _          => args.flatMap(_.free).toSet
    }

  // Substitutuion
  def subst(p: SubstPair): Term = head match {
    case Free(name) if name == p.v =>
      // Replace head f in ('u_1...u_n* f e1_1...e1_p1) by new term ('v_1...v_m* g e2_1...e2_p2):
      // 'u_1...u_n* ('v_1...v_m* g e2_1...e2_p2) e1_1...e1_p1
      // becomes (by beta-reduction):
      // 'u_1...u_n v_(m-p1+1)...v_m* g e2_1[e1_i/v_i]...e2_p2[e1_i/v_i] e1_(m+1)...e1_p1
      val n = binder.length
      val m = p.e.binder.length
      val p1 = this.args.length

      // range [m, m-p1[
      val args = p.e.args
        .map(_.substBound(m - p1, this.args.take(m))) ++ this.args.drop(m)

      Term(??? /*n + max(0, m - p1)*/, p.e.head, args.map(_.subst(p)), ret)
    case _ => copy(args = args.map(_.subst(p)))
  }
  private def substBound(start: Int, range: Vector[Term]): Term = {
    if (range.isEmpty) {
      this
    } else {
      val n = this.binder.length
      this.head match {
        case Bound(idx)
            if idx - start - n >= 0 && idx - start - n < range.length =>
          val r = range(idx - start - n)
          val m = r.binder.length
          val p1 = this.args.length

          val e1 = this.args.map(_.substBound(start + n, range))
          val args = r.args.map(_.substBound(m - p1, e1.take(m))) ++ e1.drop(m)

          Term(??? /*n + max(0, m - p1)*/, r.head, args, ret)
        case _ => copy(args = args.map(_.substBound(start + n, range)))
      }
    }
  }
}

sealed trait Head {
  def pretty(v: Vector[Option[String]]): String = this match {
    case Const(name) => s"c_$name"
    case Free(name)  => s"f_$name"
    case Bound(idx) =>
      val vIdx = v.length - 1 - idx
      v(vIdx).map("b_" + _).getOrElse(s"x$vIdx")
  }
  def isRigid: Boolean = this match {
    case _: Const | _: Bound => true
    case _: Free             => false
  }
}
object Head {
  case class Const(name: String) extends Head
  case class Free(name: String) extends Head
  case class Bound(idx: Int) extends Head
}
