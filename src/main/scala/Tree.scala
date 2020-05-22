object Tree {
  case class Disagreement(e1: Term, e2: Term) {
    def subst(p: SubstPair): Disagreement =
      Disagreement(e1.subst(p), e2.subst(p))
  }
  case class SubstPair(v: Variable, e: Term)
  type Substitution = Map[Variable, Term]

  sealed trait Res
  case object F extends Res
  case class S(subst: Substitution) extends Res

  sealed trait Node
  case class Leaf(r: Res) extends Node
  case class TreeNode(reduced: Set[Disagreement]) extends Node
}
