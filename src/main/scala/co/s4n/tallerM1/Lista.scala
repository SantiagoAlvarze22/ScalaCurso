package co.s4n.tallerM1

import scala.annotation.tailrec
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Const[+A](h: A,t:List[A]) extends List[A]

object Lista {
  //A* seq[A]
  def apply[A](as: A*) : List[A] = {
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail:_*))
  }

  //Ejercicio1
  def length[A](lst:List[A]):Int = lst match {
    case Nil => 0
    case Const(h,t) => 1 + length(t)
  }


  //Ejercicio2
  def tail[A](lst:List[A]):List[A] = lst match {
    case Nil => Nil
    //case Const(h,_) => Nil
    case Const(h,t) => t
  }

  //Ejercicio3
  def head[A](lst:List[A]):A = lst match {
    //case Nil => Nil
    case Const(h,Nil) => h
    case Const(h,t) => h
  }

  //Ejercicio4
  def and(lst:List[Boolean]):Boolean = lst match{
    case Nil => true
    case Const(h,t) => h && and(t)
  }

  //Ejercicio5
  def or(lst:List[Boolean]):Boolean = lst match {
    case Nil => false
    case Const(h,t) => h || or(t)
  }

  //Ejercicio6
  def max(lst:List[Int]):Int ={
    @tailrec
    def maxR(lst:List[Int],max:Int):Int = lst match {
      case Nil=> max
      case Const(h,t) => if(h > max) maxR(t,h) else maxR(t,max)
    }
    maxR(tail(lst),head(lst))
  }

  //EJercicio 7
  def min(lst:List[Long]):Long={
    @tailrec
    def minR(lst:List[Long],min:Long):Long = lst match {
      case Nil => min
      case Const(h,t) => if(h < min) minR(t,h) else minR(t,min)
    }
    minR(tail(lst),head(lst))
  }

  //Ejercicio 8
  def minMax(lst:List[Double]):(Double,Double) = {
    def tmp(lst:List[Double], tp:(Double, Double)):(Double, Double) = lst match {
      case Nil => tp
      case Const(h,t) => tmp(t, (if (h > tp._1) h else tp._1, if (h < tp._2) h else tp._2))
    }
    tmp(lst,(head(lst), head(lst)))
  }


}