package co.s4n.editorial

class Libro(val titulo:String,val autor:String,val ref:String ="Ficcion"){
  def nombre = s"$titulo $autor $ref"
  def this(titulo:String, autor:String) = this(titulo,autor,"ficcion")

  //def setRef(ref:String):Libro={
    //new Libro(this.titulo,this.autor,ref)
  //}
}