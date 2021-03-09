package co.s4n.tallerClases

import scala.math.Pi

object comp {
  def cuadrado(lado:Float)={
    lado * lado
  }
  def cubo(lado:Float)={
    cuadrado(lado) * lado
  }
}

object comp2 {
  def cuadrado(lado:Long)={
    lado * lado
  }
  def cubo(lado:Long)={
    cuadrado(lado) * lado
  }
}

object prueba{
  def x = {
    println("x")
    1
  }
  def y = {
    println("y")
  x+2
  }
  def z = {
    println("z")
    x
    x + "c"
  }
  //Ejecutado en REPL val res1: String = 41c
}

//Ejercicio4

class Gato(val nombre:String,val color:String,val comida:String){
  def presentacion = s"$nombre $color $comida"
}

object gatos{
  val gatoIO = new Gato("IO","Fawn","Churrus")
  val gatoMake = new Gato("Make","Red","Leche")
  val fatoDocker= new Gato("Docker","Blue","Cuido")
}

//Ejercicio5
object VentaDeChurrus{
  def despachar(obj:Gato): Boolean ={
    if (obj.comida == "Churrus") true else false
  }
}

//ejercicio6
class Conductor(val nombre:String, val apellido:String, val totalCarreras: Int, val carrerasTerminadas:Int){
  def getNombre():String = this.nombre
  def getApellido():String = this.apellido
  def getTotalCarreras():Int = this.totalCarreras
  def getCarrerasTerminadas():Int = this.carrerasTerminadas
  def getCarrerasNoTerminadas():Int = getTotalCarreras() - getCarrerasTerminadas()
}

class Escuderia(val nombre:String, val conductor:Conductor) {
  def getNombre():String = this.nombre
  def getconductor(): Conductor = this.conductor
  //override def toString():String = s"value: $value step: $step"
}

//val conductor1 = new Conductor("Santiago","Alvarez",12,125)
//val escuderia1 = new Escuderia("Santiago",conductor1)

//Ejercicio 7


class Contador(val contador:Int){
  def incr():Contador = new Contador(contador+1)
  def decr():Contador = new Contador(contador-1)
  def ajuste(sum:Sumador):Contador = new Contador(sum.adicionar(contador))
}

//Ejercicio8
class Contador2(val contador:Int){
  def incr(cont:Int = 1):Contador2 = new Contador2(contador+cont)
  def decr(cont:Int = 1):Contador2 = new Contador2(contador-cont)
}

//Ejercicio9
class Sumador(monto : Int) {
  def adicionar(valor: Int) = valor + monto
}

//Ejercicio 10
class Persona(val nombre:String, val apellido:String){
  def nombreF = s"$nombre $apellido"
}

object Persona{
  def apply(nombre:String) = {
    val n = nombre.split(" ")
    new Persona (n(0),n(1))
  }
}

class Director (
  val nombre:String,
  val apellido:String,
  val nacimiento:Int){
  def nombreF:String = s"$nombre $apellido"
  def copy(nombre:String = this.nombre,
          apellido:String = this.apellido, nacimiento:Int = nacimiento): Director = new Director(nombre,apellido,nacimiento)
}

class Pelicula (val nombre: String, val presentacion:Int, val rangoIMDB: Double, val director: Director){
  def directorEdad = presentacion - director.nacimiento
  def esDirigidaPor(director: Director) = this.director == director
  def copy(
          nombre:String = this.nombre,
          presentacion:Int = this.presentacion,
          rangoIMDB:Double = this.rangoIMDB,
          director: Director = this.director):Pelicula = new Pelicula(nombre, presentacion,rangoIMDB,director)
}

object Director{
  def apply(nombre:String,apellido:String,nacimiento:Int) = {
    new Director(nombre,apellido,nacimiento)
  }

  def esMayor(dir1: Director, dir2: Director) = {
    if(dir1.nacimiento > dir2.nacimiento) dir2 else dir1
  }
}

object Pelicula{
  def apply(nombre: String, presentacion:Int, rangoIMDB: Double, director: Director) ={
    new Pelicula(nombre: String, presentacion:Int, rangoIMDB: Double, director: Director)
  }

  def mejorCalificada(pel1:Pelicula, pel2:Pelicula) = {
    if(pel1.rangoIMDB > pel2.rangoIMDB) pel1 else pel2
  }

  def mayorDirectorEnElTiempo(pel1:Pelicula,pel2:Pelicula) = {
    if(pel1.directorEdad > pel2.directorEdad) pel1.director.nombre else pel2.director.nombre
  }
}