
package co.s4n.funcionesDeAltoOrden

import scala.annotation.tailrec
sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A],right: Tree[A]) extends Tree[A]

object tree {
  def size[A](tree:Tree[A]):Int = tree match {
    case Leaf(_) => 1
    case Branch(left,right) => size(left) + size(right) +1
  }

  def depth[A](tree:Tree[A]):Int ={
    tree match{
      case Leaf(_) => 1
      case Branch(left,right) => 1+math.max(depth(left),depth((right)))
    }

  }
}