import Head.{Bound, Const, Free}
import Tree.{Disagreement, F, Leaf, S, TreeNode}
import Type.E
import Type.T0.Alpha
import org.scalatest.{FunSuite, Inside, Matchers}

class AlgorithmTest extends FunSuite with Matchers with Inside {
  test("SIMPL paper example (1)") {
    val N = Set(
      Disagreement(
        Term(Const("A"), Vector(Term(1, Const("B"), Vector(Term(Free("x")), Term(Bound(0)))), Term(Const("C")))),
        Term(
          Const("A"),
          Vector(Term(1, Const("B"), Vector(Term(Free("y")), Term(Bound(0)))), Term(Free("f"), Term(Const("C"))))
        )
      )
    )
    inside(Algorithm.SIMPL(N)) {
      case TreeNode(s) =>
        s should contain theSameElementsAs List(
          Disagreement(Term(1, Free("x")), Term(1, Free("y"))),
          Disagreement(Term(Free("f"), Term(Const("C"))), Term(Const("C")))
        )
    }
  }

  test("SIMPL paper example (2)") {
    val N = Set(
      Disagreement(
        Term(Const("A"), Term(Abs(E(Alpha), Some("u")), Const("B"), Vector(Term(Free("x")), Term(Bound(0))))),
        Term(Const("A"), Term(Abs(E(Alpha), Some("v")), Const("B"), Vector(Term(Free("y")), Term(Bound(0)))))
      )
    )
    inside(Algorithm.SIMPL(N)) {
      case Leaf(S(s)) =>
        s should contain(Variable("x", E(Alpha)) -> Term(Free("h0")))
        s should contain(Variable("y", E(Alpha)) -> Term(Free("h0")))
    }
  }

  test("SIMPL paper example (3)") {
    val N = Set(
      Disagreement(
        Term(2, Const("A"), Vector(Term(Bound(1)), Term(1, Bound(1)))),
        Term(2, Const("A"), Vector(Term(Bound(1)), Term(1, Bound(2))))
      )
    )
    Algorithm.SIMPL(N) should equal(Leaf(F))
  }
}
