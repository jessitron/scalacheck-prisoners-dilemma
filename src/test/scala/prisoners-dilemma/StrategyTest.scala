package prisoners_dilemma

import org.scalacheck._

object StrategyGen {
  import Gen._

  def streamOfN[T](n: Int, g: Gen[T]): Gen[Stream[T]] = Gen.containerOfN[Stream, T](n, g)

  def infiniteStream[T](g: Gen[T]) = const("poo") map {
    (_) => {
      def streaminate(gen: Gen[T]): Stream[Option[T]] =
        gen.sample #:: streaminate(gen)

      streaminate(g).flatten
    }
  }

  val strategyGen:Gen[RoundStrategy] = infiniteStream(Package.move) map (RoundStrategy.fromStream(_)) map (RoundStrategy.recording(_))

  val strategizerGen: Gen[Strategizer] = strategyGen.map(Strategizer.thatDoes(_))

  implicit val arbStrategy = Arbitrary(strategizerGen)

}

object StrategyProperties extends Properties("Various known strategies") {
  import Prop._
  import RuleGenerators._
  import StrategyGen._

  property("Two random streams of moves are played out") = forAll(Gen.posNum[Int])
  { (turns: Int) =>
    val moveStreamGenerator = streamOfN(turns, Package.move)
    forAll(moveStreamGenerator, moveStreamGenerator) {
      (stream1: Stream[Move], stream2: Stream[Move]) =>
        val s1 = RoundStrategy.fromStream(stream1)
        val s2 = RoundStrategy.fromStream(stream2)
        RoundStrategy.moves(s1, s2).take(turns).toSeq =? stream1.zip(stream2).take(turns)
    }
  }

  property("The sucker always cooperates") =
    forAll(strategyGen, Gen.posNum[Int]) {
      (opponent: RoundStrategy, turns: Int) =>
        val allMoves:Stream[MoveSet] = RoundStrategy.moves(birds.StandardBirds.SUCKER, opponent).take(turns)
        val myMoves = allMoves.map (_._1)

        myMoves.forall(_ == Cooperate) :|
           s"The sucker defected OMG!! Here's the story: ${allMoves.toList}"
  }

  // TODO: Move titForTat over to birds package, and move both these tests too

  def play(turns: Int, one: RoundStrategy,
    other: RoundStrategy): (Seq[Move],Seq[Move]) =
      RoundStrategy.moves(one, other).take(turns).unzip
  property("Tit for Tat copies the prior move, and starts with Cooperate") =
    forAll(strategyGen, Gen.posNum[Int]) {
      (opponent:RoundStrategy, turns: Int) =>
      val (myMoves, theirMoves) = play(turns,
                                       RoundStrategy.titForTat,
                                       opponent)
      val myFirstMove = myMoves.head
      val myOtherMoves = myMoves.tail
      val theirPriorMoves = theirMoves.take(turns - 1)

      (myFirstMove =? Cooperate) &&
      (myOtherMoves =? theirPriorMoves)

    }


}
