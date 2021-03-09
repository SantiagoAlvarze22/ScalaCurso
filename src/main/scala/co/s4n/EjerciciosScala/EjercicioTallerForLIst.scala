package co.s4n.EjerciciosScala

object EjercicioTallerForList {
  /*def myLastFor[A](lst:List[A]):List[A] = for {
    x <- lst

    if(x == lst(lst.length -1))

  } yield x*/

  /*def myLastFor1[A](lst:List[A]):Option[A] = {
    def mylastaux(lst:List[A]):List[A] = for {
      x <- lst
      if(x == lst.last)
    } yield x
    lst match {
      case Nil => None
      case x => Some(mylastaux(x).head)
    }
  }*/


  /*def myButLastFor[A](lst:List[A]):List[A]= for {
    x <- lst
    if(x == lst.init.last)
  }yield x*/

  /*def myLast2 [A] (lst:List[A]): A = {
    val a = for {
      x <- 0 to lst.length
      if (x == lst.length-1)
    } yield x
    lst(a.head)
  }*/

  //Longitud de una Lista con for
  def lengthFor[A](lst:List[A]):Int = (for {
    _ <- lst
  } yield ((a:Int) => a+1)).foldLeft(0)((e,f)=> f(e))

  //for LIst[Int => Int]

  //último elemento de una lista con For
  def myLast[A](lst:List[A]):A=(for {
    xi <- lst
  }yield ((a:A)=> xi)).foldLeft(lst.head)((e,f) => f(e))

  //for List[A =>A]

  //Primer elemento de una lista con For
  def myHead[A](lst:List[A]):A = (for {
    xi <- lst
  }yield((a:A) => xi)).foldRight(lst.head)((f,e)=> f(e))

  //Encontrar el kth elemento en una lista
  def mykthElem[A](k:Int,lst:List[A]):Option[A] = (for{
    xi <- lst
  }yield ((t:(Int,Option[A]))=> (t._2) match {
    case None => if(t._1 == k) (t._1,Some(xi)) else (t._1+1,None)
    case Some(x) => (t._1,Some(x))
  })).foldLeft((0,None:Option[A]))((e,f) => f(e))._2

  /*def mykthElem1[A](k:Int,lst:List[A]):Option[A] = (for{
    xi <- lst
  }yield ((t:(Int,Option[A]))=> (t._2) match {
    case None => if(t._1 == k) (t._1,Some(xi)) else (t._1+1,None)
    case Some(x) => (t._1,Some(x))
  })).foldLeft((0,None:Option[A]))((e,f) => f(e))._2.getOrElse(-1) */

  def copyReal[A](lst:List[A]):List[A]= lst match {
    case x :: xs => x :: copyReal(xs)
    case Nil => Nil
  }

  def copyFor[A](lst:List[A]) = (for {
    xi <- lst
  }yield ((xs:List[A]) => xi :: xs)).foldRight(Nil:List[A])((f,e)=> f(e))



}


