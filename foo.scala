package argonaut.doc

import scalaz._, Scalaz._
import argonaut._, Argonaut._

case class MyData(val x : Int) {
  val T = 3
}

object MyData {
  // Recall, can either do by encode/decode,
  // or by way of codec
  implicit def MyDataCodecJson: CodecJson[MyData] =
    casecodec1(MyData.apply, MyData.unapply)("x")

  def dump(d : MyData) : String = {
    d.asJson.spaces2
  }

  def load(s : String) : Option[MyData] = {
    s.decodeOption[MyData]
  }
}




object Example {
  def main(args : Array[String]) : Unit = {
    val d = MyData(5)
    println("Dumping myData")
    val s = MyData.dump(d)
    println(s)
    println("Load myData")
    val j = MyData.load(s)
    println(j)
    println
  }
}