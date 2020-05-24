import Head.{Bound, Const, Free}
import T0._
import Tree.SubstPair
import org.scalatest.{FunSuite, Matchers}

class TermTest extends FunSuite with Matchers {
  test("pretty printing") {
    val C = Term(Const("C"), Vector(Term(3, Bound(4), Term(Free("g"))), Term(Free("f"))))
    val t = Term(Vector(Abs(Fn(Beta, Gamma), Some("y")), Abs(Alpha)), Bound(1), C)

    t.toString should equal("λb_y x1. b_y(c_C(λx2 x3 x4. b_y(f_g), f_f))")
  }

  test("is rigid") {
    Term(Free("g")).isRigid should be(false)
    Term(1, Bound(0)).isRigid should be(true)
    Term(Const("C")).isRigid should be(true)
  }

  test("type inference") {
    val t = Term(Vector(Abs(Alpha), Abs(Beta)), Const("C"), Term(Abs(Gamma), Bound(1), Beta), Gamma)

    t.typ should equal(Fn(Alpha, Beta, Gamma))
    t.headType should equal(Fn(Fn(Gamma, Beta), Gamma))
  }

  test("free variables") {
    val t = Term(
      Free("f"),
      Vector(
        Term(Free("g"), Alpha),
        Term(Const("C"), Alpha),
        Term(Free("f"), Beta)
      ),
      Gamma
    )
    t.free should contain theSameElementsAs List(
      Variable("f", Beta),
      Variable("f", Fn(Alpha, Alpha, Beta, Gamma)),
      Variable("g", Alpha)
    )
  }

  test("full substitution") {
    val t = Term(Free("f"), Alpha)
    val s = Term(Vector(Abs(Beta)), Const("C"), Term(Bound(0), Gamma), Alpha)

    val res = t.subst(SubstPair(Variable("f", Alpha), s))

    res should equal(s)
  }

  test("complex substitution") {
    val t = Term(
      Vector(Abs(Alpha), Abs(Beta)),
      Free("f"),
      Vector(
        Term(Const("C"), Beta),
        Term(Free("f"), Vector(Term(Bound(0), Beta), Term(Bound(1), Alpha)), Alpha)
      ),
      Alpha
    )
    val s = Term(
      Vector(Abs(Beta), Abs(Alpha)),
      Const("D"),
      Vector(
        Term(
          Vector(Abs(Fn(Alpha, Beta, Gamma))),
          Bound(0),
          Vector(Term(Bound(1), Alpha), Term(Bound(2), Beta)),
          Gamma
        )
      ),
      Alpha
    )

    val res = t.subst(SubstPair(Variable("f", Fn(Beta, Alpha, Alpha)), s))

    val expected = Term(
      Vector(Abs(Alpha), Abs(Beta)),
      Const("D"),
      Vector(
        Term(
          Vector(Abs(Fn(Alpha, Beta, Gamma))),
          Bound(0),
          Vector(
            Term(
              Const("D"),
              Vector(
                Term(
                  Vector(Abs(Fn(Alpha, Beta, Gamma))),
                  Bound(0),
                  Vector(Term(Bound(3), Alpha), Term(Bound(2), Beta)),
                  Gamma
                )
              ),
              Alpha
            ),
            Term(Const("C"), Beta)
          ),
          Gamma
        )
      ),
      Alpha
    )
    res should equal(expected)
  }
}
