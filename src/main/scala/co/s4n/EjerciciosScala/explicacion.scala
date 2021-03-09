package co.s4n.EjerciciosScala

object explicacion {
  //NUeva opcion de filtro
  def filtro(f:(Int,Int)=>Boolean,a:Int,b:Int):Int = if (f(a,b)) a else b


  /*1. Siempre tener en cuenta que tipo de datos estamos manejando ADENTRO de una lista, hay veces que pueden ser tuplas o varios tipos de datos dentro de una lista.
  2. Tener el cuenta el nombre al cual le estamos asignando al match case
  3. Sempre crear los casos por defecto */

  val n = List(1,2,3,4)
  val res = n.foldLeft(0)((m,n) => m+n)

  def concatedMessage(lst:List[String]) = lst.fold(""){(z,i)=> s"fold val1 $z val2 $i"}

  def makeINt(s:String):Option[Int] = {
    try {
      Some(s.trim.toInt)
    }catch {
      case e:Exception => None
    }
  }/*

  val x = makeINt("1")
  val y =makeINt("2")

  val sum = x match{
    case None => {
      y match{
        case None => {
          0
        }
        case Some(i) => {
          i
        }
      }
    }
    case Some(i) => {
      y match {
        case None => {
          i
        }
        case Some(j) => {
          i+j
        }
      }
    }
  }*/
}
