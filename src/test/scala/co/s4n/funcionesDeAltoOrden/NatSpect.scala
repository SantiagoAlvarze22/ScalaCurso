
package co.s4n.funcionesDeAltoOrden

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NatSpec extends AnyFlatSpec with Matchers {
  "La suma de (Suc(Suc(Cero))) + Suc(Suc(Cero))" should "es Suc(Suc(Suc(Suc(Cero))))" in {
    Nat.addNat(Suc(Suc(Cero)),Suc(Suc(Cero))) shouldEqual  Suc(Suc(Suc(Suc(Cero))))
  }

  "el producto de (Suc(Suc(Cero))) + Suc(Suc(Cero))" should "es Suc(Suc(Suc(Suc(Cero))))" in {
    Nat.prodNat(Suc(Suc(Cero)),Suc(Suc(Cero))) shouldEqual  Suc(Suc(Suc(Suc(Cero))))
  }

}