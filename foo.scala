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


case class Annoying(val dat : Either[String, MyData])

object Annoying {
  implicit def AnnoyingEncodeJson : EncodeJson[Annoying] =
    EncodeJson((op : Annoying) =>
      op.dat match {
        case Left(msg) => ("message" := msg) ->: jEmptyObject
        case Right(d) => ("data" := d) ->: jEmptyObject
      })

  implicit def AnnoyingDecodeJson: DecodeJson[Annoying] =
    DecodeJson(c => {
      val mDecode = (c --\ "message").as[String].toOption
      val dDecode = (c --\ "data").as[MyData].toOption

      (mDecode, dDecode) match {
        case (Some(msg), _) => {
          DecodeResult.ok(Annoying(Left(msg)))
        }
        case (_, Some(dat)) => {
          DecodeResult.ok(Annoying(Right(dat)))
        }
        case (_, _) => {
          DecodeResult.fail("", c.history)
        }
      }
    })
}

object Foo extends App {
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
  println(s2)
  val j2 = s2.decodeOption[Option[String]]
  println("load Opt1")
  println(j2)
  println

  val an1 = Annoying(Left("ABC"))
  println("Annoying")
  val s3 = an1.asJson.spaces2
  println(s3)
  val j3 = s3.decodeOption[Annoying]
  println("load Annoying")
  println(j3)
  println

  println("what happens w/ error?")
  val j4 = "{}".decodeOption[Annoying]
  println(j4)
  println

  val an2 = Annoying(Right(d))
  println("Annoying 2")
  val s5 = an2.asJson.spaces2
  println(s5)
  val j5 = s5.decodeOption[Annoying]
  println("load Annoying")
  println(j5)
  println

    // Not surprising, but can't do for Left/Right.
//    val e1 = Left()
//    println("Dumping Either1")
//    val s3 = e1.asJson.spaces2
//    println(s3)
}
