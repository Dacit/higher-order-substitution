import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.ExecutionContext.Implicits.global

import Tree.{Disagreement, F, Node, Res, S, SubstPair, Substitution, TreeNode}
import Algorithm._

object Traversal {
  // Tree traversal helper + heuristic
  case class Edge(subst: SubstPair, to: Node)

  def children(n: TreeNode): Iterator[Edge] = {
    // TODO heuristic
    val d = n.reduced.find(_.e2.isRigid).get

    MATCH(d.e1, d.e2, d.e1.free ++ d.e2.free).map { substPair =>
      Edge(substPair, SIMPL(n.reduced.map(_.subst(substPair))))
    }
  }

  // Traversals
  def bfs(init: Node): Res = {
    case class BFSNode(n: Node, substitution: Substitution)

    val q = mutable.Queue(BFSNode(init, Map.empty))
    while (q.nonEmpty) {
      q.dequeue() match {
        case BFSNode(Tree.Leaf(F), _) =>
        case BFSNode(Tree.Leaf(S(last)), subst) => S(last ++ subst)
        case BFSNode(t: TreeNode, subst) =>
          q ++= children(t).map(c => BFSNode(c.to, subst + (c.subst.v -> c.subst.e)))
      }
    }
    F
  }

  // Top-level
  def unify(e1: Term, e2: Term): Option[Res] = {
    Option(
      Await.result(
        Future {
          val init = SIMPL(Set(Disagreement(e1, e2)))
          bfs(init)
        },
        Duration(5L, SECONDS)
      )
    )
  }
}
