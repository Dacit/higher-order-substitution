import T0._
import org.scalatest.{FunSuite, Matchers}

class TypeTest extends FunSuite with Matchers {
  test("pretty-printing") {
    val t = Fn(Alpha, Fn(Alpha, Beta), Gamma)

    t.toString should equal("α, (α → β) → γ")
  }

  test("elementary") {
    Type(T0()).isElementary should be(true)
    Fn(T0(), T0()).isElementary should be(false)
  }
}
