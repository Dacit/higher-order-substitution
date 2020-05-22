import Head.{Bound, Const, Free}
import Type.{E, Fn}
import Type.T0._
import org.scalatest.{FunSuite, Matchers}

class TermTest extends FunSuite with Matchers {
  test("pretty printing") {
    val g = Term(Vector.empty, Free("g"), Vector.empty, E(Beta))
    val c0 = Term(
      Vector(Abs(E(Alpha)), Abs(E(Alpha)), Abs(E(Alpha))),
      Bound(4),
      Vector(g),
      E(Gamma)
    )
    val c1 = Term(Vector.empty, Free("f"), Vector.empty, E(Alpha))
    val C = Term(Vector.empty, Const("C"), Vector(c0, c1), E(Beta))
    val t = Term(
      Vector(Abs(Fn(Vector(E(Beta)), E(Gamma)), Some("y")), Abs(E(Alpha))),
      Bound(1),
      Vector(C),
      E(Gamma)
    )

    t.pretty should equal("λb_y x1. b_y (c_C (λx2 x3 x4. b_y f_g) f_f)")
  }

  test("is rigid") {
    Term(Vector.empty, Free("g"), Vector.empty, E(Alpha)).isRigid should be(
      false
    )
    Term(Vector(Abs(E(Alpha))), Bound(0), Vector.empty, E(Alpha)).isRigid should be(
      true
    )
    Term(Vector.empty, Const("C"), Vector.empty, E(Alpha)).isRigid should be(
      true
    )
  }

  test("type inference") {
    val t = Term(
      Vector(Abs(E(Alpha)), Abs(E(Beta))),
      Const("C"),
      Vector(Term(Vector(Abs(E(Gamma))), Bound(1), Vector.empty, E(Beta))),
      E(Gamma)
    )

    t.typ should equal(Fn(Vector(E(Alpha), E(Beta)), E(Gamma)))
    t.headType should equal(Fn(Vector(Fn(Vector(E(Gamma)), E(Beta))), E(Gamma)))
  }

  test("free variables") {
    val t = Term(
      Vector.empty,
      Free("f"),
      Vector(
        Term(Vector.empty, Free("g"), Vector.empty, E(Alpha)),
        Term(Vector.empty, Const("C"), Vector.empty, E(Alpha)),
        Term(Vector.empty, Free("f"), Vector.empty, E(Beta))
      ),
      E(Gamma)
    )
    t.free should contain theSameElementsAs List(
      Variable("f", E(Beta)),
      Variable("f", Fn(Vector(E(Alpha), E(Alpha), E(Beta)), E(Gamma))),
      Variable("g", E(Alpha))
    )
  }

  test("substitution") {}
}
