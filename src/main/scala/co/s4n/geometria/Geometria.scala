package co.s4n.geometria

import scala.math.Pi

class Forma {
  def area : Double = 0.0
}

class Rectangulo(val ancho:Double, val altura:Double) extends Forma{
  override def area: Double = ancho*altura
}

class Circulo(val radio:Double) extends Forma{
  override def area: Double = Pi + radio * radio
}