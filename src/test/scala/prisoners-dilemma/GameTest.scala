package prisoners_dilemma

import org.scalacheck._
import scala.concurrent.duration._

object GameGen {
  import Gen._
  import StrategyGen._

  val playerGen =  for {
    name <- Gen.alphaStr
    strategy <- strategyGen
  } yield Player(name, strategy)

  implicit val arbPlayer = Arbitrary(playerGen)

  val someTurns: Gen[Int] = Gen.choose(0, 100)

  val reasonableTimeLimit: Gen[Duration] = Gen.choose(100, 1500).map(m => m.millis)
  implicit val arbDuration = Arbitrary(reasonableTimeLimit)

  val someSlowPlayers: Gen[Seq[Player]] = for {
    n <- Gen.choose(2, 20)
    list <- listOfN(n, playerGen) // TODO: slowPlayerGen
  } yield list
}

object BigGameTest extends Properties("A free-for-all") {
  import Prop._
  import GameGen._
  import RuleGenerators._

  val fudge = 500.millis

  def eachPlayerGetsAResult(players: Seq[Player], results: Seq[Outcome]) :Prop =
    (players.length =? results.length) :| "Wrong number of results" &&
    Prop.all(players.map(p => results.exists(_.player == p) :| "No results for $p"):_*)

  property("All games end within the time limit") =
    forAll(ruleGen(20) :| "Rules", someSlowPlayers :| "Slow players", reasonableTimeLimit) {
        (rules: Rules, players: Seq[Player], timeLimit: Duration) =>

      val timer = new Timer()
      val output = Game.eachOnEach(rules)(players, timeLimit)
      val timeTaken = timer.check
      (timeTaken <= (timeLimit + fudge)) :| s"$timeTaken was longer than $timeLimit" &&
      eachPlayerGetsAResult(players, output)
    }

}

class Timer {
  def now = new java.util.Date().getTime
  val start = now
  def check: Duration = (now - start).millis
}

object GameTest extends Properties("An iterated game of Prisoners Dilemma") {

  import GameGen._
  import RuleGenerators._
  import Prop._

  property("The results are fairly reported") =
    forAll(playerGen, playerGen, ruleGen(100), someTurns) {
    (p1: Player, p2: Player, rules: Rules, turns: Int) =>

      val (outcome1, outcome2) = Game.oneOnOne(rules, turns)(p1, p2)

      (outcome1.myMoves == outcome2.opponentMoves && outcome2.myMoves == outcome2.opponentMoves)
      (outcome1.player == p1 && outcome2.player == p2) &&
      (outcome1.opponent == p2 && outcome2.opponent == p1)

  }

  property("The results are consistent with the rules") = forAll {
    (p1: Player, p2: Player) =>
      forAll(someTurns) { turns =>
        val reasonableScore = if (turns == 0) Int.MaxValue else (Int.MaxValue / (turns * 2))
        forAll(ruleGen(reasonableScore)) { rules =>

      val (outcome1, outcome2) = Game.oneOnOne(rules, turns)(p1, p2)

      val minPossibleScore = rules.suckersPenalty * turns
      val maxPossibleScore = rules.temptationToDefect * turns
      val maxGlobalScore = rules.rewardForMutualCooperation * turns * 2

      val allOutcomes = Seq(outcome1, outcome2)

      val totalScore = allOutcomes.map(_.score).sum

      val eachScoreIsReasonable: Prop =
        Prop.all(Seq(outcome1, outcome2).map(_.score).map(score =>
            (score <= maxPossibleScore && score >= minPossibleScore) :|
            s"Score of $score is impossible"
          ): _*)
      val totalScoreIsReasonable: Prop = ((totalScore) <= maxGlobalScore) :|
                   s"Total score of ${totalScore} is impossible"

      (eachScoreIsReasonable && totalScoreIsReasonable) :|
            s"Outcomes:\n$outcome1\n$outcome2 "
    }
   }
  }


  property("The player that defects the most wins") = forAll {
    (p1: Player, p2: Player) =>
      forAll(someTurns) { turns =>
        val reasonableScore = if (turns == 0) Int.MaxValue else (Int.MaxValue / (turns * 2))
        forAll(ruleGen(reasonableScore)) { rules =>

          val (outcome1, outcome2) = Game.oneOnOne(rules, turns)(p1, p2)
          val allOutcomes = Seq(outcome1, outcome2)

          val maxDefections = allOutcomes.maxBy(_.myMoves.count(_ == Defect))
          val maxScore = allOutcomes.maxBy(_.score)

          (maxDefections == maxScore) :|
                    "Wait, how did you score higher without defecting?" :|
                    s"Outcomes:\n$outcome1\n$outcome2 "
    }
   }
  }

}
