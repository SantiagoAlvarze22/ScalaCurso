package co.s4n.tallerM1

sealed trait Nat
case object Cero extends Nat
case class Suc(nat: Nat) extends Nat

object Nat {
  //Ejrcicio 9
  val uno = Suc(Cero)
  val dos = Suc(uno)

  //Ejerciio 10
  def fromNatToInt(nat: Nat): Int = nat match {
    case Cero => 0
    case Suc(n) => 1 + Nat.fromNatToInt(n)
  }

  //Ejercicio 11
  def fromIntToNat(int:Int): Nat = int match {
    case 0 => Cero
    case int => Suc(fromIntToNat(int - 1))
  }
}