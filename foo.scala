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


object ArgUtils {
  // simple things are fine,
  // but for containers (and e.g. arrays??)
  // it's not so clear what needs to happen.

  implicit def OptionEncodeJson : EncodeJson[Option[String]] =
    EncodeJson((op : Option[String]) =>
//      op match {
//        case None => jEmptyObject
//        case Some(s) => ("some" := s) ->: jEmptyObject
//      }
      ("some" :=? op) ->?: jEmptyObject)

  implicit def PersonDecodeJson: DecodeJson[Option[String]] =
    DecodeJson(c =>
      // ().as[T] is pretty clever
      (c --\ "some").as[Option[String]]
    )
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

    val o1 = Some(3)
    println("Dumping Opt1")
    val s2 = o1.asJson.spaces2
    val j2 = s2.decodeOption[Option[String]]
    println("load Opt1")
    println(j2)
    println
  }
}