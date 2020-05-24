import Head.{Bound, Const, Free}
import Tree._

object Algorithm {
  private def SIMPLStep1(N: Set[Disagreement]): Option[Set[Disagreement]] = {
    // Find rigid/rigid pairs
    val (rigidRigid, nPrime) = N.partition(d => d.e1.isRigid && d.e2.isRigid)
    if (rigidRigid.isEmpty) {
      Some(nPrime)
    } else {
      val exploded = rigidRigid.flatMap { d =>
        // Binder does not need to be checked --
        // in beta-eta-normal form, if types are equal, so is the number of binders.
        if (d.e1.head != d.e2.head) {
          return None
        } else {
          val binder = d.e1.binder
          // Same number of args as head is the same, and type must be the same
          d.e1.args.zip(d.e2.args).map {
            case (ei1, ei2) =>
              Disagreement(ei1.copy(binder = binder ++ ei1.binder), ei2.copy(binder = binder ++ ei2.binder))
          }
        }
      }
      // Re-do step on exploded formulas
      SIMPLStep1(exploded.toSet).map(_ ++ nPrime)
    }
  }

  private def SIMPLStep2(N: Set[Disagreement]): Set[Disagreement] = {
    // Swap rigid/flexible pairs
    val (toFlip, ok) = N.partition(d => d.e1.isRigid && !d.e2.isRigid)
    ok ++ toFlip.map(d => Disagreement(d.e2, d.e1))
  }

  private def SIMPLStep3(N: Set[Disagreement]): Node = {
    if (N.exists(_.e2.isRigid)) {
      TreeNode(N)
    } else {
      // Only flex-flex pairs
      val free = N.flatMap(d => d.e1.free ++ d.e2.free).toSet

      val subst = free.map {
        case v @ Variable(_, Type(args, ret)) =>
          v -> Term(args.map(Abs(_)), Free(Variable.fresh("h", Type(Vector.empty, ret), free)), ret)
      }.toMap

      Leaf(S(subst))
    }
  }

  def SIMPL(N: Set[Disagreement]): Node =
    SIMPLStep1(N) match {
      case None => Leaf(F)
      case Some(n) => SIMPLStep3(SIMPLStep2(n))
    }

  private def MATCH_imitate(e1: Term, f: Free, e2: Term, c: Const, v: Set[Variable]): SubstPair = {
    val f_typ = e1.headType

    val Es = e2.args.zipWithIndex.map { case (e2_i, i) => buildH_iTerm(i, e2_i.typ, f_typ.args, v) }

    SubstPair(Variable(f.name, f_typ), Term(f_typ.args.map(Abs(_)), c, Es, f_typ.ret))
  }

  private def buildH_iTerm(i: Int, targetType: Type, wTypes: Vector[Type], v: Set[Variable]): Term = {
    val h = Free(Variable.fresh(s"h$i", Type(wTypes ++ targetType.args, targetType.ret), v))

    val ws = buildBounds(wTypes, targetType.args.length)
    val vs = buildBounds(targetType.args)

    Term(targetType.args.map(Abs(_)), h, ws ++ vs)
  }

  private def buildBounds(ts: Vector[Type], offset: Int = 0): Vector[Term] = {
    val n = ts.length

    ts.zipWithIndex.map {
      case (alpha, i) =>
        Term(alpha.args.map(Abs(_)), Bound(n - 1 - i + offset + alpha.args.length), buildBounds(alpha.args), alpha.ret)
    }
  }

  private def MATCH_project(e1: Term, f: Free, e2: Term, v: Set[Variable]): Iterator[SubstPair] = {
    Iterator.range(0, e1.args.length).map { i =>
      val p1 = e1.args.length
      val f_typ = e1.headType

      val w_iType = f_typ.args(p1 - 1 - i)
      val Es = w_iType.args.zipWithIndex.map { case (gamma_j, j) => buildH_iTerm(j, gamma_j, f_typ.args, v) }

      SubstPair(Variable(f.name, f_typ), Term(f_typ.args.map(Abs(_)), Bound(i), Es, f_typ.ret))
    }
  }

  def MATCH(e1: Term, e2: Term, v: Set[Variable]): Iterator[SubstPair] = {
    (e1.head, e2.head) match {
      case (f: Free, c: Const) => Iterator(MATCH_imitate(e1, f, e2, c, v)) ++ MATCH_project(e1, f, e2, v)
      case (f: Free, _) => MATCH_project(e1, f, e2, v)
      case _ => throw new IllegalArgumentException("Not a flex-rigid pair")
    }
  }
}
