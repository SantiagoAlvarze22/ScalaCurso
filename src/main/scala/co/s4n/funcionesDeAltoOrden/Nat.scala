package co.s4n.funcionesDeAltoOrden

sealed trait Nat
case object Cero extends Nat
case class Suc(nat: Nat) extends Nat

object Nat {
  //Ejrcicio 9
  val uno = Suc(Cero)
  val dos = Suc(uno)

  def fromNatToInt(nat: Nat): Int = nat match {
    case Cero => 0
    case Suc(nat) => 1 + fromNatToInt(nat)
  }

  def fromIntToNat(int: Int): Nat = int match {
    case 0 => Cero
    case n => Suc(fromIntToNat(n - 1))
  }

  //Ejercico 9
  def addNat(nat1: Nat,nat2:Nat):Nat = (nat1,nat2) match {
    case (Cero,Cero) => Cero
    case (Cero,Suc(nat2)) => Suc(nat2)
    case (Suc(nat1), Cero) => Suc(nat1)
    case (nat1,nat2) => fromIntToNat(fromNatToInt(nat1)+ fromNatToInt(nat2))
  }

  //Ejercicio 10
  def prodNat(nat1: Nat,nat2:Nat):Nat = (nat1,nat2) match {
    case (Cero,Cero) => Cero
    case (Cero,Suc(nat2)) => Cero
    case (Suc(nat1), Cero) => Cero
    case (nat1,nat2) => fromIntToNat(fromNatToInt(nat1)* fromNatToInt(nat2))
  }
}