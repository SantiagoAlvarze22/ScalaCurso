package co.s4n.tallerM0
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.math.Pi

class EjercicioSpec extends AnyFlatSpec with Matchers {
  //TEST EJERCICIO1
  "Area de un triángulo rectángulo con altura 10 y base 20" should "debe ser igual a 100" in {
    Ejercicio.areaTrianguloRectangulo(10,20) shouldEqual 100
  }

  //Test EJercicio2
  "El área de un circulo con radio de 5" should "debe ser igual a 78.54" in{
    Ejercicio.areaDeunCirculo(5) shouldEqual 5*5*Pi
  }

  //Test Ejercicio3
  "EL salario de x persona con 200000 devengado y 20000 de deducciones" should "debe ser igual a $180000" in {
    Ejercicio.calSalario(200000,20000) shouldEqual 180000
  }

  //Test Ejercicio4
  "EL salario de x persona con 300000 devengado y 30000 de deducciones y 10% del total devengado en bono" should "debe ser igual a $300000" in{
    Ejercicio.calSalarioBono(300000,30000) shouldEqual 300000
  }

  //Test Ejercicio 5
  "El salario compuesto con calSalario con 400000 devengado y 30000 de deducciones " should "debe ser igual a $370000" in {
    Ejercicio.compSalarioDeSalario shouldEqual 370000
  }

  "El salario compuesto de x persona con calSalario con 500000 devengado y 30000 de deducciones " should "debe ser igual a $520000" in {
    Ejercicio.compSalarioDeBono shouldEqual 520000
  }

  //Test ejercicio7
  "EL salario de x persona con 400000 devengado y 50000 de deducciones y 5% del total devengado en bono" should "debe ser igual a $370000" in {
    Ejercicio.calSalario5(400000, 50000) shouldEqual 370000
  }
  //Test ejercicio8
  "EL salario de x persona con 500000 devengado y 90000 de deducciones y 20% del total devengado en bono" should "debe ser igual a $510000" in {
    Ejercicio.calSalario20(500000, 90000) shouldEqual 510000
  }

  //TEst ejercicio 10
  "Salario calculado por la funcion calSalario, y como parametro la función calSalarioBonoClausura, 5% adicional del salario devengado en bono como variable externa, 500000 de salario devengado y 45200 de deducciones" should "debe ser igual a $479800" in {
    Ejercicio.compSalario(Ejercicio.calSalarioBonoClausura,500000,45200) shouldEqual 479800
  }

  //Test ejercicio11
  "Salario calculado con 500000 devengado, 45200 de deducción y 15% de bono" should "debe ser igual a %529800" in {
    Ejercicio.calSalario15(500000,45200) shouldEqual 529800
  }
  //Test ejercicio12
  "Salario calculado con 500000 devengado, 45200 de deducción y 100% de bono" should "debe ser igual a %1154800" in {
    Ejercicio.calSalario100(600000,45200) shouldEqual 1154800
  }

  //Test ejercicio15
  "El resultado del factorial de 5" should "debe ser 120" in{
    Ejercicio.factorial(5) shouldEqual 120
  }

  //Test ejercicio16
  "El resultado de la serie fibonacci 5 " should "debe ser 5" in{
    Ejercicio.fibonacci(5) shouldEqual 5
  }

  //Test ejercicio17
  "El resultado del factorial Recursivo de 5" should "debe ser 120" in{
    Ejercicio.factorialR(5) shouldEqual 120
  }
}