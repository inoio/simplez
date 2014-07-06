package scalatypes

import id._
import std._
import int._
import list._
import syntax._

object Test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  import Kleisli._
	
	case class Config(x : Int, y: Int)
	
	val c = Config(4,2)                       //> c  : scalatypes.Test.Config = Config(4,2)
	
	def getTuple() : Reader[Config, (Int, Int) ]= Reader { c: Config => (c.x, c.y) }
                                                  //> getTuple: ()scalatypes.Reader[scalatypes.Test.Config,(Int, Int)]
	def product(t: (Int, Int)) : Int = t._1 * t._2
                                                  //> product: (t: (Int, Int))Int
	
	
	
	val r  = for {
		z <- getTuple() >=> kleisli[Id, (Int,Int) , Int] { product _}
	} yield z                                 //> r  : scalatypes.Kleisli[scalatypes.Id,scalatypes.Test.Config,Int] = scalatyp
                                                  //| es.Kleisli$$anon$1@2ef9b8bc
	
	println(r.run(c))                         //> 8
}