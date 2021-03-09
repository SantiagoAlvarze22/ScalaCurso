package co.s4n.tallerM1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NatSpec extends AnyFlatSpec with Matchers {
  "Nat se puede contruir" should "como Cero" in {
    val cero = Cero
    cero shouldEqual Cero
  }

  "Nat se puede construir" should "como uno" in {
    val uno = Suc(Cero)
    uno shouldEqual Suc(Cero)
  }

  "Nat se puede contruir" should "como dos" in {
    val dos = Suc(Suc(Cero))
    dos shouldEqual Suc(Suc(Cero))
  }

  "La transformación de Nat a entero" should "debe ser 0 es Cero" in {
    val cero = Cero
    Nat.fromNatToInt(Cero) shouldEqual 0
  }

  "La transformación de Nat a entero" should "dede ser 1 si Suc(Cero)" in {
    val uno = Suc(Cero)
    Nat.fromNatToInt(uno) shouldEqual 1
  }

  "La transformación de Nat a entero" should "dede ser 6 si Suc(Suc(Suc(Suc(Suc(Suc(Cero))))))" in {
    val uno = Suc(Suc(Suc(Suc(Suc(Suc(Cero))))))
    Nat.fromNatToInt(uno) shouldEqual 6
  }

  "La transformación de entero a Nat" should "debe ser Suc(Suc(Suc(Suc(Suc(Suc(Cero)))))) si es 6" in {
    val int = 6
    Nat.fromIntToNat(int) shouldEqual Suc(Suc(Suc(Suc(Suc(Suc(Cero))))))
  }

  "La transformación de entero a Nat" should "debe ser Suc(Suc(Suc(Suc(Suc(Suc(Suc(Suc(Suc(Suc(Cero)))))))))) si es 10" in {

    val int = 10
    Nat.fromIntToNat(int) shouldEqual Suc(Suc(Suc(Suc(Suc(Suc(Suc(Suc(Suc(Suc(Cero))))))))))
  }
}