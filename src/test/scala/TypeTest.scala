import Type.T0._
import Type._
import org.scalatest.{FunSuite, Matchers}

class TypeTest extends FunSuite with Matchers {
  test("pretty-printing") {
   val t = Fn(Vector(E(Alpha), Fn(Vector(E(Alpha)), E(Beta))), E(Gamma))

   t.pretty should equal ("α, (α → β) → γ")
  }
}
