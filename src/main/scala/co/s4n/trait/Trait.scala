package co.s4n.`trait`
import scala.math.Pi

//Ejercicio1
trait Felino{
  def color():String
  def sonido():String
}

class Leon(val tamañoMelan:Int) extends Felino {
  def color():String = "Verde"
  def sonido():String = "Meow"
}

class Tigre extends Felino {
  def color():String = "amarillo"
  def sonido():String = "Grrr"
}

class Jaguar extends Felino {
  def color():String = "Negro"
  def sonido():String = "Meow"
}

class Gato(val comida:String) extends Felino{
  def color():String = "Gris"
  def sonido():String = "Meow"
}

//Ejercicio2

sealed trait Forma{
  def tamaño():Int
  def perimetro():Double
  def area():Double
}


//Ejercicio3
trait Rectangular extends Forma{}

case class Circulo(radio:Double) extends Forma{
  def tamaño():Int = 0
  def perimetro():Double = 2*Pi* radio
  def area():Double =  Pi * radio * radio
}

case class Rectangulo(lado1:Double, lado2:Double) extends Rectangular{
  def tamaño():Int = 4
  def perimetro():Double = 2*lado1 + 2 * lado2
  def area():Double = lado1 * lado2
}

case class Cuadrado(lado:Double) extends Rectangular{
  def tamaño():Int = 4
  def perimetro():Double = lado * 4
  def area():Double = lado * lado
}

//4
object Draw{
  def apply(forma:Forma) = forma match{
    case Circulo(n) => println(s"Un circulo de radio $n cm")
    case Cuadrado(n) => println(s"Un cuadrado de ancho $n cm y largo $n cm")
    case Rectangulo(n,l) => println(s"Un rectangulo de ancho $n cm y largo $l cm")
  }
}

//Ejercjcio 5
class Color{
  val R:String = "Rojo"
  val G:String = "Amarillo"
  val B:String = "Rosa"
}


