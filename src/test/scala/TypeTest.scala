import Type.T0._
import Type._
import org.scalatest.{FunSuite, Matchers}

class TypeTest extends FunSuite with Matchers {
  test("pretty-printing") {
    val t = Fn(E(Alpha), Fn(E(Alpha), E(Beta)), E(Gamma))

    t.toString should equal("α, (α → β) → γ")
  }
}
