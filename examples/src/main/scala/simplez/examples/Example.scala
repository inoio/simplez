package simplez.examples

import java.util.Properties

object KleisliExample extends App {
  import simplez._
  import id._

  case class Config(name: String, a: Properties, b: Properties)

  def doSomethingWithConfig(anotherString: String) = Reader[Config, Unit] {
    config =>
      println(config.name + anotherString)
  }

  def doSomethingWithA(foo: String) = Reader[Properties, Unit] {
    a =>
      println("In A : " + a.getProperty(foo))
  }

  def doSomethingWithB(foo: String) = Reader[Properties, Unit] {
    b =>
      println("In B : " + b.getProperty(foo))
  }

  def combine() =


        doSomethingWithConfig("hello") >=> (doSomethingWithA("FooA").local[Config](config => config.a)) >=> doSomethingWithB("Foob").local[Config](config => config.b)


  }

  combine().run(Config("dude", new Properties(), new Properties()))

}
