package co.s4n.funcionesDeAltoOrden

import scala.annotation.tailrec
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Const[+A](h: A,t:List[A]) extends List[A]

object List{
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail: _*))
  }

  def const[A](h:A,t:List[A]):List[A] = Const(h,t)

  def length[A](lst:List[A]):Int = lst match {
    case Nil => 0
    case Const(h,t) => 1 + length(t)
  }

  def addEnd[A](lst:List[A], elem:A):List[A] = lst match {
    case Nil => Const(elem, Nil)
    case Const(h, t) => Const(h, addEnd(t, elem))
  }

  //Ejercicio 1
  def take[A](n:Int,lst:List[A]):List[A] = {
    def takeR[A](n: Int, lst: List[A], accu: List[A]): List[A] = (n, lst) match {
      case (0, Const(h,t)) => accu
      case (n, Const(h, t)) => takeR(n - 1, t, addEnd(accu,h))
      case (n, Nil) => Nil
    }
    takeR(n, lst, Nil)
  }

  //Ejercico2
  def init[A](lst:List[A]):List[A] = {
    def initR[A](lst:List[A], accu:List[A]):List[A] = lst match{
      case Nil => Nil
      case Const(h, Nil) => Nil
      case Const(h,t) => Const(h,initR(t,accu))
    }
    initR(lst,Nil)
  }

  //Ejercicio3
  def split[A](n:Int,lst:List[A]):(List[A],List[A]) = {
    def splitR[A](n:Int,lst:List[A], accu:List[A]):(List[A],List[A])= (n,lst) match{
      case (n, Nil) => (accu,lst)
      case (0, Const(h,t)) => (accu,lst)
      case (n, Const(h, t)) => splitR(n-1,t, addEnd(accu,h))
    }
    splitR(n,lst,Nil)
  }

  //Ejercicio4
  def zip[A,B](lst1:List[A], lst2:List[B]):List[(A,B)] = (lst1,lst2) match{
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Const(h1,t1), Const(h2,t2)) => Const((h1,h2), zip(t1,t2))
  }

  //Ejercicio5
  def unzip[A,B](lst:List[(A,B)]):(List[A],List[B]) = {
    def unzipR[A,B](lst:List[(A,B)],lst1:List[A],lst2:List[B]):(List[A],List[B])= lst match{
      case Nil => (lst1,lst2)
      case Const(h,t) => unzipR(t, addEnd(lst1,h._1), addEnd(lst2,h._2))
    }
    unzipR(lst,Nil,Nil)
  }

  //Ejercio 6
  def reverse[A](lst:List[A]):List[A] = {
    def reverseR[A](n: Int, lst: List[A], accu: List[A]): List[A] = (n, lst) match {
      case (0, Nil) => accu
      case (n, Const(h, t)) => reverseR(n - 1, t, Const(h,accu))
    }
    reverseR(length(lst), lst, Nil)
  }

  //Ejercicio7
  def intersperse[A](elem:A,lst:List[A]):List[A] = {
    def intersperse[A](elem:A,lst:List[A],accu:List[A]):List[A] = lst match {
      case Nil => accu
      case Const(h,Nil) => addEnd(accu,h)
      case Const(h,t)=> intersperse(elem, t, addEnd(addEnd(accu,h),elem))
    }
    intersperse(elem,lst,Nil)
  }

  //Ejercicio8
  def concat[A](lst1: List[A], lst2: List[A]): List[A] = (lst1, lst2) match {
    case (Nil, Nil) => Nil
    case (lst1, Nil) => lst1
    case (Nil, lst2) => lst2
    case (Const(h, t), lst2) => Const(h, concat(t, lst2))
  }

  //Ejercicio13
  def foldRight[A,B](as:List[A],z:B)(f:(A,B) => B):B= as match{
    case Nil => z
    case Const(h,t) => f(h,foldRight(t,z)(f))
  }

  def foldLeft[A,B](lst: List[A], z: B)(f: (B,A) => B): B = lst match {
    case Const(h, t) => foldLeft(t, f(z, h))(f)
    case Nil => z
  }


  //ejercicio14
  def lengthR[A](lst:List[A]):Int = foldRight(lst,0)((_,y) => y+1)

  //ejercicio15
  def andR(lst:List[Boolean]):Boolean = foldRight(lst,true)(_&&_)

  //Ejercicio16
  def takeWhile[A](lst:List[A])(p:A => Boolean):List[A] =  foldRight(lst,Nil:List[A])((h,t) => if(p(h)) Const(h,t) else Nil)

  //Ejercicio17
  def filterR[A](lst:List[A])(p:A => Boolean): List[A] = foldRight(lst,Nil:List[A])((h,t) => if (p(h)) Const(h,t) else t)

  //Ejercicio18
  def unZipR[A,B](lst:List[(A,B)]):(List[A],List[B]) = foldRight(lst, (Nil,Nil):(List[A],List[B]))((h,t) =>(Const(h._1,t._1),Const(h._2,t._2)))


  //Ejercicio19
  def lengthL[A](lst:List[A]): Int = foldLeft(lst,0)((y,_) => y+1)

  //Ejercicio20
  def andL(lst:List[Boolean]):Boolean = foldLeft(lst,true)((_&&_))

  //Ejercicio21
  def takeWhileL[A](lst:List[A])(p:A=>Boolean):List[A] = foldLeft(lst,Nil:List[A])((t,h) => if (p(h)) addEnd(t,h) else t)

  //EJercicio22
  def filterL[A](lst:List[A])(p:A=>Boolean):List[A] = foldLeft(lst,Nil:List[A])((t,h)=>if (p(h)) addEnd(t,h) else t)

  //Ejercicio23
  def unZipL[A,B](lst:List[(A,B)]):(List[A],List[B]) = foldLeft(lst,(Nil,Nil):(List[A],List[B]))((t,h) => (addEnd(t._1,h._1),addEnd(t._2,h._2)))
}