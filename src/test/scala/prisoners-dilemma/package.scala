package prisoners_dilemma

import org.scalacheck._

object Package {
  val move: Gen[Move] = Gen.oneOf(Cooperate, Defect)
  implicit val arbMoves: Arbitrary[Move] = Arbitrary(move)

  def someOf[T](n: Gen[Int], g: Gen[T]): Gen[Seq[T]] =
    for { len <- n
          list <- Gen.listOfN(len, g)
    } yield list

  def overAndOverForever[T](seq: Seq[T]): Stream[T] =
     Stream.continually(seq).flatten
}
