import Head.{Bound, Const, Free}
import Tree.SubstPair
import Type.{E, Fn}
import Type.T0._
import org.scalatest.{FunSuite, Matchers}

class TermTest extends FunSuite with Matchers {
  test("pretty printing") {
    val g = Term(Free("g"), E(Beta))
    val c0 = Term(Vector(Abs(E(Alpha)), Abs(E(Alpha)), Abs(E(Alpha))), Bound(4), Vector(g), E(Gamma))
    val c1 = Term(Free("f"), E(Alpha))
    val C = Term(Const("C"), Vector(c0, c1), E(Beta))
    val t = Term(Vector(Abs(Fn(E(Beta), E(Gamma)), Some("y")), Abs(E(Alpha))), Bound(1), Vector(C), E(Gamma))

    t.toString should equal("λb_y x1. b_y (c_C (λx2 x3 x4. b_y f_g) f_f)")
  }

  test("is rigid") {
    Term(Free("g"), E(Alpha)).isRigid should be(false)
    Term(Vector(Abs(E(Alpha))), Bound(0), E(Alpha)).isRigid should be(true)
    Term(Const("C"), E(Alpha)).isRigid should be(true)
  }

  test("type inference") {
    val t = Term(
      Vector(Abs(E(Alpha)), Abs(E(Beta))),
      Const("C"),
      Vector(Term(Vector(Abs(E(Gamma))), Bound(1), E(Beta))),
      E(Gamma)
    )

    t.typ should equal(Fn(E(Alpha), E(Beta), E(Gamma)))
    t.headType should equal(Fn(Fn(E(Gamma), E(Beta)), E(Gamma)))
  }

  test("free variables") {
    val t = Term(
      Free("f"),
      Vector(
        Term(Free("g"), E(Alpha)),
        Term(Const("C"), E(Alpha)),
        Term(Free("f"), E(Beta))
      ),
      E(Gamma)
    )
    t.free should contain theSameElementsAs List(
      Variable("f", E(Beta)),
      Variable("f", Fn(Vector(E(Alpha), E(Alpha), E(Beta)), E(Gamma))),
      Variable("g", E(Alpha))
    )
  }

  test("full substitution") {
    val t = Term(Free("f"), E(Alpha))
    val s = Term(Vector(Abs(E(Beta))), Const("C"), Vector(Term(Bound(0), E(Gamma))), E(Alpha))

    val res = t.subst(SubstPair(Variable("f", E(Alpha)), s))

    res should equal(s)
  }

  test("complex substitution") {
    val t = Term(
      Vector(Abs(E(Alpha)), Abs(E(Beta))),
      Free("f"),
      Vector(
        Term(Const("C"), E(Beta)),
        Term(Free("f"), Vector(Term(Bound(0), E(Beta)), Term(Bound(1), E(Alpha))), E(Alpha))
      ),
      E(Alpha)
    )
    val s = Term(
      Vector(Abs(E(Beta)), Abs(E(Alpha))),
      Const("D"),
      Vector(
        Term(
          Vector(Abs(Fn(E(Alpha), E(Beta), E(Gamma)))),
          Bound(0),
          Vector(Term(Bound(1), E(Alpha)), Term(Bound(2), E(Beta))),
          E(Gamma)
        )
      ),
      E(Alpha)
    )

    val res = t.subst(SubstPair(Variable("f", Fn(E(Beta), E(Alpha), E(Alpha))), s))

    val expected = Term(
      Vector(Abs(E(Alpha)), Abs(E(Beta))),
      Const("D"),
      Vector(
        Term(
          Vector(Abs(Fn(E(Alpha), E(Beta), E(Gamma)))),
          Bound(0),
          Vector(
            Term(
              Const("D"),
              Vector(
                Term(
                  Vector(Abs(Fn(E(Alpha), E(Beta), E(Gamma)))),
                  Bound(0),
                  Vector(Term(Bound(3), E(Alpha)), Term(Bound(2), E(Beta))),
                  E(Gamma)
                )
              ),
              E(Alpha)
            ),
            Term(Const("C"), E(Beta))
          ),
          E(Gamma)
        )
      ),
      E(Alpha)
    )
    res should equal(expected)
  }
}
