import Head.{Bound, Const, Free}
import T0.Alpha
import Tree.{Disagreement, F, Leaf, S, SubstPair, TreeNode}
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
        Term(Const("A"), Term(Abs(Alpha, Some("u")), Const("B"), Vector(Term(Free("x")), Term(Bound(0))))),
        Term(Const("A"), Term(Abs(Alpha, Some("v")), Const("B"), Vector(Term(Free("y")), Term(Bound(0)))))
      )
    )
    inside(Algorithm.SIMPL(N)) {
      case Leaf(S(s)) =>
        s should contain(Variable("x", Alpha) -> Term(Free("h")))
        s should contain(Variable("y", Alpha) -> Term(Free("h")))
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

  test("MATCH paper example") {
    // Terms need to be in beta-eta normal form. We use default type (Alpha) instead of gamma.
    val e1 = Term(Free("f"), Vector(Term(2, Free("x"), Vector(Term(Bound(1)), Term(Bound(0)))), Term(Const("C"))))
    val e2 = Term(Const("A"), Term(Const("B")))
    val v = Set(Variable("x", Fn(Alpha, Alpha, Alpha)), Variable("f", Fn(Fn(Alpha, Alpha, Alpha), Alpha, Alpha)))

    val res = Algorithm.MATCH(e1, e2, v).toList

    res should have size 3
    val f = Variable("f", Fn(Fn(Alpha, Alpha, Alpha), Alpha, Alpha))
    val t1 = Term(
      Vector(Abs(Fn(Alpha, Alpha, Alpha)), Abs(Alpha)),
      Const("A"),
      Term(Free("h0"), Vector(Term(2, Bound(3), Vector(Term(Bound(1)), Term(Bound(0)))), Term(Bound(0))))
    )
    val t2 = Term(
      Vector(Abs(Fn(Alpha, Alpha, Alpha)), Abs(Alpha)),
      Bound(1),
      Vector(
        Term(Free("h0"), Vector(Term(2, Bound(3), Vector(Term(Bound(1)), Term(Bound(0)))), Term(Bound(0)))),
        Term(Free("h1"), Vector(Term(2, Bound(3), Vector(Term(Bound(1)), Term(Bound(0)))), Term(Bound(0))))
      )
    )
    val t3 = Term(Vector(Abs(Fn(Alpha, Alpha, Alpha)), Abs(Alpha)), Bound(0))
    res should contain allElementsOf List(SubstPair(f, t1), SubstPair(f, t2), SubstPair(f, t3))
  }
}
