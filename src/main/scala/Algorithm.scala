import Head.{Bound, Const, Free}
import Tree._
import Type.{E, Fn}

object Algorithm {
  private def SIMPLStep1(N: Set[Disagreement]): Option[Set[Disagreement]] = {
    // Find rigid/rigid pairs
    val (rigidRigid, nPrime) = N.partition(d => d.e1.isRigid && d.e2.isRigid)
    if (rigidRigid.isEmpty) {
      Some(nPrime)
    } else {
      val exploded = rigidRigid.flatMap { d =>
        if (d.e1.head != d.e2.head || d.e1.binder.map(_.typ) != d.e2.binder.map(_.typ)) {
          return None
        } else {
          val binder = d.e1.binder
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
        case v @ Variable(_, Fn(args, res)) => v -> Term(args.map(Abs(_)), Free(Variable.fresh("h", res, free)), res)
        case v @ Variable(_, e: E) => v -> Term(Free(Variable.fresh("h", e, free)), e)
      }.toMap

      Leaf(S(subst))
    }
  }

  def SIMPL(N: Set[Disagreement]): Node =
    SIMPLStep1(N) match {
      case None => Leaf(F)
      case Some(n) => SIMPLStep3(SIMPLStep2(n))
    }

  private def MATCH_imitate(
      n1: Int,
      f: Free,
      e1: Vector[Term],
      n2: Int,
      c: Const,
      e2: Vector[Term],
      V: Set[Variable]): Iterator[SubstPair] = {
    val hs = ???
    ///val E = hs.map(i => Term(0, )???)
    //Iterator(SubstPair(f.name, Term(e1.length, c, ???, ???)))
    ???
  }

  private def MATCH_project(
      n1: Int,
      f: Free,
      e1: Vector[Term],
      n2: Int,
      b: Bound,
      e2: Vector[Term],
      V: Set[Variable]): Iterator[SubstPair] = ???

  def MATCH(e1: Term, e2: Term, v: Set[Variable]): Iterator[SubstPair] = {
    (e1.head, e2.head) match {
      case (f: Free, c: Const) =>
        MATCH_imitate(e1.binder.length, f, e1.args, e2.binder.length, c, e2.args, v)
      case (f: Free, b: Bound) =>
        MATCH_project(e1.binder.length, f, e1.args, e2.binder.length, b, e2.args, v)
      case _ => throw new IllegalArgumentException("Not a flex-rigid pair")
    }
  }
}
