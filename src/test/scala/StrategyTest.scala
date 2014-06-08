package prisoners_dilemma

import org.scalacheck._

object StrategyGen {
  import Gen._

  def streamOfN[T](n: Int, g: Gen[T]): Gen[Stream[T]] = Gen.containerOfN[Stream, T](n, g)
  //val randomStrategy: Gen[Strategy] =


}

object StrategyProperties extends Properties("Various known strategies") {
  import Prop._
  import RuleGenerators._
  import StrategyGen._



  property("Two random streams of moves are played out") = forAll(Gen.posNum[Int]) { (turns: Int) =>
    val moveStreamGenerator = streamOfN(turns, RuleGenerators.move)
    forAll(moveStreamGenerator, moveStreamGenerator) { (stream1: Stream[Move], stream2: Stream[Move]) =>
      val s1 = Strategy.fromStream(stream1)
      val s2 = Strategy.fromStream(stream2)
      Strategy.moves(s1, s2).take(turns).toSeq =? stream1.zip(stream2).take(turns)
    }
  }

  //property("Sucker always says true") = { (opponent: Strategy)
  //}

}
