package co.s4n.tallerM0

import scala.math.Pi

object Ejercicio{
  //Ejercicio1
  def areaTrianguloRectangulo(a: Int, b: Int): Int = (a * b) / 2

  //Ejercicio2
  val areaDeunCirculo = (r:Int) => r*r*Pi

  //Ejercicio3
  val calSalario = (dv:Double,dd:Double) => dv - dd

  //Ejercicio4
  val calSalarioBono = (dv:Double, dd:Double) => {dv*1.10 -dd}

  //Ejercicio5
  def compSalario(f:(Double,Double) => Double, a:Double,b:Double) =f(a,b)

  val compSalarioDeSalario = compSalario(calSalario,400000,30000)
  val compSalarioDeBono = compSalario(calSalarioBono,500000,30000)

  //Ejercicio6
  def genCalSalarioBono( bono : Double ) : ( Double , Double ) => Double = _ * bono - _

  //Ejercicio7
  val calSalario5 = genCalSalarioBono(1.05)


  //Ejercicio8
  val calSalario20 = genCalSalarioBono(1.20)

  //Ejercicio9
  val bono = 1.05
  val calSalarioBonoClausura = (dv:Double,dd:Double) => (dv*bono) -dd

  //Ejercicio10

  //Ejercicio11
  val tmp = genCalSalarioBono(_)

  val calSalario15 = tmp(1.15)

  //Ejercicio12

  val tmp2 = genCalSalarioBono(_)

  val calSalario100= tmp(2.00)

  //Ejercicio 15
  def factorial(n:Int):Int =n match{
    case 0 => 1
    case 1 => 1
    case _ => n* factorial(n-1)
  }

  //Ejercicio16
  def fibonacci(x:Int):Int =x match{
    case 0 => 0
    case 1 => 1
    case _ => fibonacci(x-1) + fibonacci(x-2)
  }

  //Ejercicio 17
  def factorialR(n:Int):Int = {
    @annotation.tailrec
    def factorial2(n: Int, a: Int): Int = n match {
      case 0 => a
      case 1 => a
      case n => factorial2(n - 1, n * a)
    }
    factorial2(n,1)
  }

}