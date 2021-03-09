package co.s4n.tallerM1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class ListSpec extends AnyFlatSpec with Matchers{

  //Test ejercicio1
  "La longitud de la lista (1,2,3,4,5)"  should "5" in {
    val lst1 = Lista(1,2,3,4,5)
    Lista.length(lst1) shouldEqual 5
  }

  //Test ejercicio2
  "La cola de la lista (1,2,3,4,5) " should "(2,3,4,5)" in {
    val lst1 = Lista(1,2,3,4,5)
    Lista.tail(lst1) shouldEqual Const(2,Const(3,Const(4,Const(5,Nil))))
  }

  //Test ejercicio3
  "El head de la lista (1,2,3,4,5) " should "1" in {
    val lst1 = Lista(1,2,3,4,5)
    Lista.head(lst1) shouldEqual 1
  }

  //Test ejercicio4
  "EL resultado booleano de la siguiente lista (false,true,false,true)" should "false" in {
    val lst1 = Lista(false,true,false,true)
    Lista.and(lst1) shouldEqual false
  }

  //Test Ejercicio5
  "El resultado booleano de la siguiente (false,true,false,true)" should "true" in{
    val lst1 = Lista(false,true,false,false)
    Lista.or(lst1) shouldEqual true
  }

  //Test ejercicio6
  "EL numero maximo de la lista (1,2,3,45,6) es: " should "45" in {
    val lst1 = Lista(1,2,3,45,6)
    Lista.max((lst1)) shouldEqual 45
  }

  //Test ejercicio7
  "EL numero minimo de la lista (1,2,-3,45,6) es:" should "3" in {
    val lst =Lista(1L,2L,-3L,45L,6L)
    Lista.min((lst)) shouldEqual -3
  }

  //Test ejercicio8
  "EL numero minimo y maximo de la lista (1,2,-3,45,6) es " should "(45,-3)" in {
    val lst =Lista(1.0,3.0,0.0,45.0)
    Lista.minMax(lst) shouldEqual (45.0,0.0)
  }
}