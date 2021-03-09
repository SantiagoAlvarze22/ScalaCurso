package co.s4n.EjerciciosScala

class EjercicioTaller {
  //EJercicio1
  def myLast[A](lst:List[A]):A = lst match {
    case x :: Nil => x
    case x :: xs => myLast(xs)
  }

  //EJercico 2
  def myButLast[A](lst:List[A]):A = lst match {
    case x :: y :: Nil => x
    case x :: xs => myButLast(xs)
  }

  //Ejercicio 3
  def elementAt[A](lst:List[A], n:Int):A = (lst,n) match {
    case (x :: xs, 1) => x
    case ( _ :: xs, n) => elementAt(xs, n-1)
  }

  //Ejercicio 4
  def myLength[A](lst:List[A]):Int = lst match{
    //case x::Nil => 0
    case Nil => 0
    case x :: xs => 1 + myLength(xs)
  }

  //Ejercicio4.1
  def myLengthR[A](lst:List[A]):Int = {
          def myLengthRe[A](lst:List[A],n:Int):Int= lst match{
            case Nil => n
            case x :: xs => myLengthRe(xs,n+1)
          }
    myLengthRe(lst,0)
  }

  //Ejercicio 5
  def myReverse[A](lst:List[A]): List[A] = lst match{
    case Nil => Nil
    case (x :: xs) => myReverse(xs) ::: List(x)
  }

  //Ejercicio6
  def isPalindrome[A](lst:List[A]):Boolean = lst match {
    case Nil => true
    case x :: Nil => true
    /*case x :: y :: Nil => x == y
    case x :: y :: z :: Nil => x == z
    case w :: x :: y :: z :: Nil => w==z && x==y
    case w :: x :: y :: z :: t :: Nil => w==t && x == z */
    case x :: xs => (x == xs.last && isPalindrome(xs.init))
  }

  //Ejercicio8
  def compress[A](lst:List[A]):List[A] = lst match {
    case Nil => Nil
  }

  def failingFn(i:Int):Int = {
    //val y: Int = throw new Exception("Fail!")
    try {
      val x = 43 + i
      val y: Int = throw new Exception("Fail!")
      x+y
    }
    catch {case e: Exception => 43}
  }
}

//Ejercicio 7
/*sealed trait NestedList[+A]
//object Nil
case class Elem[A](elem:A) extends NestedList[A]

case class Lista[A](lista: List[NestedList[A]]) extends NestedList[A]*/
