package prisoners_dilemma

import org.scalacheck._

object StrategyGen {
  import Gen._

  def streamOfN[T](n: Int, g: Gen[T]): Gen[Stream[T]] = Gen.containerOfN[Stream, T](n, g)
  //val randomStrategy: Gen[Strategy] =

  def infiniteStream[T](g: Gen[T]) = const("poo") map {
    (_) => {
      def streaminate(gen: Gen[T]): Stream[Option[T]] =
        gen.sample #:: streaminate(gen)

      streaminate(g).flatten
    }
  }

  val strategyGen:Gen[Strategy] = infiniteStream(RuleGenerators.move) map (Strategy.fromStream(_)) map (Strategy.recording(_))

  implicit val arbStrategy = Arbitrary(strategyGen)

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

  property("The sucker always cooperates") = forAll(strategyGen, Gen.posNum[Int]) { (opponent: Strategy, turns: Int) =>
    val allMoves:Stream[MoveSet] = Strategy.moves(Strategy.sucker, opponent).take(turns)
    val myMoves = allMoves.map (_._1)
    myMoves.forall(_ == Cooperate) :| s"The sucker defected OMG!! Here's the story: ${allMoves.toList}"
  }


}
