package co.s4n.funcionesDeAltoOrden

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListSpec extends AnyFlatSpec with Matchers {
  //"The Hello object" should "say hello" in {
  //funcionesDeAltoOrden.append shouldEqual "hello"
  //}

  //Test ejercicio1
  "Los primeros 3 elementos de la lista (1,2,3,4,5)" should "son (1,2,3)" in {
    val lst1 = List(1, 2, 3, 4, 5)
    List.take(3, lst1) shouldEqual List(1, 2, 3)
  }

  //Test ejercico2
  "la (1,2,3,4,5) aplicandole la funcion init" should "son (1,2,3,4)" in {
    val lst1 = List(1, 2, 3, 4, 5)
    List.init(lst1) shouldEqual List(1, 2, 3, 4)
  }

  //Test ejercicio3
  "la (1,2,3,4,5) aplicandole la funcion split(2,lst)" should "debe ser (1,2,3,4)" in {
    val lst1 = List(1, 2, 3, 4, 5)
    List.split(2, lst1) shouldEqual(Const(1, Const(2, Nil)), Const(3, Const(4, Const(5, Nil))))
  }

  //Test Ejercicio 4
  "La lst1 =  List(1,2,3,4,5), lst2=  List(1,2,3,4,5) aplicandole la funcion zip(lst1,lst2)" should "debe ser Const((1,1),Const((2,2),Const((3,3),Const((4,4),Const((5,5),Nil)))))" in {
    val lst1 = List(1, 2, 3, 4, 5)
    val lst2 = List(1, 2, 3, 4, 5)
    List.zip(lst1, lst2) shouldEqual Const((1, 1), Const((2, 2), Const((3, 3), Const((4, 4), Const((5, 5), Nil)))))
  }

  //Test ejercicio5
  "La lista List((1,\"a\"),(2,\"b\"),(3,\"c\")) aplicandole la función unzip" should "debe ser Const((1,a),Const((2,b),Const((3,c),Nil)))" in {
    val lst3 = List((1, 4), (2, 5), (3, 6))
    List.unzip(lst3) shouldEqual(Const(1, Const(2, Const(3, Nil))), Const(4, Const(5, Const(6, Nil))))
  }

  //Test Ejercicio6
  "La lista List(1,2,3,4,5) en reverse " should "es Const(5,Const(4,Const(3,Const(2,Const(1,Nil)))))" in {
    val lst1 = List(1, 2, 3, 4, 5)
    List.reverse(lst1) shouldEqual Const(5, Const(4, Const(3, Const(2, Const(1, Nil)))))
  }

  //Test ejercicio7
  "La lista List(1,2,3,4,5) aplicandole la función interperse List.intersperse(\"H\",lst2)" should "es  Const(1,Const(H,Const(2,Const(H,Const(3,Const(H,Const(4,Const(H,Const(5,Nil)))))))))" in {
    val lst2 = List(1, 2, 3, 4, 5)
    List.intersperse(1, lst2) shouldEqual Const(1, Const(1, Const(2, Const(1, Const(3, Const(1, Const(4, Const(1, Const(5, Nil)))))))))
  }

  //Test ejercicio8
  "La lista resultante de la concatenación entre List(1,2,3,4,5) y List(1,2,3,4,5) " should "debe ser  Const(1,Const(2,Const(3,Const(4,Const(5,Const(1,Const(2,Const(3,Const(4,Const(5,Nil))))))))))" in {
    val lst1 = List(1, 2, 3, 4, 5)
    val lst2 = List(1, 2, 3, 4, 5)
    List.concat(lst1, lst2) shouldEqual Const(1, Const(2, Const(3, Const(4, Const(5, Const(1, Const(2, Const(3, Const(4, Const(5, Nil))))))))))
  }

  //Test ejercicio13
  "si paso Nil y const" should "" in {
    List.foldRight(List(9L, 6L, 7L), Nil: List[Long])(Const(_, _)) shouldEqual Const(9, Const(6, Const(7, Nil)))

  }


  //Test ejercicio 14
  "La longitud de la lista (1,2,3,4,5) calculado " should "debe ser 5" in {
    val lst1 = List(1, 2, 3, 4, 5)
    List.lengthR(lst1) shouldEqual 5
  }

  //Test ejercicio 15
  "El resultado de la list(true,false,true)" should "debe ser false" in {
    val lst1 = List(true, false, true)
    List.andR(lst1) shouldEqual false
  }

  "El resultado de la list(true,true,true)" should "debe ser true" in {
    val lst1 = List(true, true, true)
    List.andR(lst1) shouldEqual true
  }

  //Test ejercicio 16
  "El resultado de la takeWhile(list1)(_<4)" should "debe ser Const(1,Const(2,Const(3,Nil))) " in {
    val list1 = List(1, 2, 3, 4, 5)
    List.takeWhile(list1)(_ < 4) shouldEqual Const(1, Const(2, Const(3, Nil)))
  }

  //Test Ejercicio17
  "El resultado de la filter(list1)(_<4)" should "debe ser Const(1,Const(2,Const(3,Nil))) " in {
    val list1 = List(1, 2, 3, 4, 5)
    List.filterR(list1)(_ < 4) shouldEqual Const(1, Const(2, Const(3, Nil)))
  }

  //Test Ejercicio18
  "El resultado de unZipR(List((1,2),(2,3),(3,4))) " should "debe ser (Const(1,Const(2,Const(3,Nil))),Const(2,Const(3,Const(4,Nil))))" in {
    val list1 = List((1, 2), (2, 3), (3, 4))
    List.unZipR(list1) shouldEqual(Const(1, Const(2, Const(3, Nil))), Const(2, Const(3, Const(4, Nil))))
  }

  //Test ejercicio19
  "El resultado de LengthL(1,2,3,4,5)" should "debe ser 5" in {
    val list1 = List(1, 2, 3, 4, 56)
    List.lengthL(list1) shouldEqual 5
  }


  //Test20
  "El resultado de la list(true,true,true,true)" should "debe ser true" in {
    val lst2 = List(true, true, true, true)
    List.andL(lst2) shouldEqual true
  }

  //Testt21
  "El resultado de la takeWhileL(list1)(_>1)" should "debe ser Const(2,Const(3,Const(4,Nil))) " in {
    val list1 = List(1, 2, 3, 4)
    List.takeWhileL(list1)(_ > 1) shouldEqual Const(2, Const(3, Const(4, Nil)))
  }

  //Test22
  "El resultado de la filterL(list1)(_>1)" should "debe ser Const(2,Const(3,Const(4,Nil))) " in {
    val list1 = List(1, 2, 3, 4)
    List.filterL(list1)(_ > 1) shouldEqual Const(2, Const(3, Const(4, Nil)))
  }

  //Test Ejercicio23
  "El resultado de unZipL(List((1,2),(2,3),(3,4))) " should "debe ser (Const(1,Const(2,Const(3,Nil))),Const(2,Const(3,Const(4,Nil))))" in {
    val list1 = List((1, 2), (2, 3), (3, 4))
    List.unZipL(list1) shouldEqual(Const(1, Const(2, Const(3, Nil))), Const(2, Const(3, Const(4, Nil))))
  }
}