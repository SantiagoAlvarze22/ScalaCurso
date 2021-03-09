package co.s4n.ListaScala

class Lista{

  def subs[A](lst:List[A]):List[List[A]] = {
      val a = lst match{
        case Nil => List()
        case head::tail => subs(tail).map(head:: _) ::: subs(tail)
      }
    a.sortBy(_.length)
  }

  def barajar[A](e:A,lst:List[A]):List[List[A]] = lst match{
    case Nil => List(List(e))
    case x :: xs => (e ::(x :: xs)) :: (barajar(e,xs)).map(x:: _)
  }

  def perms[A](lst:List[A]):List[List[A]] = lst match {
    case Nil => List(Nil)
    case x :: xs => (perms(xs)).flatMap(barajar(x,_))
  }

  /*def permutaciones[A](lst:List[A]):List[List[A]] = lst match{
    case Nil => List(Nil)
    case x :: Nil => List(List(x))
    case y :: tail => List
    case y:: x :: Nil => List(List(x,y),List(y,x))
    case z:: y :: x :: Nil => List(List(x,y,z),List(x,z,y),List(y,x,z),List(y,z,x),List(z,x,y),List(z,y,x))
  }*/


}

  /*
def subs[A](lst:List[A]):List[List[A]] = lst match {
  case Nil => List(Nil)
  case h :: rest => subs(rest) ::: subs(rest).map(h :: _)
}

  def subs[A](list:List[A]):List[List[A]] = {
  @tailrec
  def subsAux[A](list:List[A],iterator:Int,aux:List[List[A]]):List[List[A]] = iterator match {
  case -1 => aux
  case n => subsAux(list,n-1, list.combinations(n).toList ::: aux)
}
  subsAux(list,list.length,Nil)
}


  def subs[A](lst: List[A]): List[List[A]] = {
  @tailrec
  def subsAux(lstO: List[A], acum: List[List[A]]): List[List[A]] = lstO match {
  case Nil => acum
  case x :: xs => subsAux(xs, acum ::: (acum.map((x :: _))) )
}
  subsAux(lst, Nil::Nil)
}
*/
