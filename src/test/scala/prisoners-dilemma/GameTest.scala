package prisoners_dilemma

import akka.actor.ActorSystem
import org.scalacheck._
import scala.concurrent.duration._

object GameGen {
  import Gen._
  import StrategyGen._
  import NameGenerator._

  val playerGen =  for {
    name <- pronounceableStr
    strategy <- strategizerGen
  } yield Player(name, strategy)

  implicit val arbPlayer = Arbitrary(playerGen)

  val someTurns: Gen[Int] = Gen.choose(0, 100)

  val reasonableTimeLimit: Gen[FiniteDuration] = Gen.choose(200, 1500).map(m => m.millis)
  implicit val arbDuration = Arbitrary(reasonableTimeLimit)

  val somePlayers: Gen[Seq[Player]] = for {
    n <- Gen.choose(3, 20)
    list <- listOfN(n, playerGen)
  } yield list

}

object Pretties {
  // if this works, move it
  import org.scalacheck.util.Pretty
  implicit def prettyRules(t: Rules) = Pretty { p => s"T = ${t.temptationToDefect}\n" +
    s"       R = ${t.rewardForMutualCooperation}\n" +
    s"       P = ${t.punishmentForMutualDefection}\n" +
    s"       S = ${t.suckersPenalty}\n"
  }

  implicit def instructionSet(t: Seq[Instructions]) = Pretty { params =>
    "[" + t.map {
      case MakeAMove => "M"
      case Failinate => "F"
      case Wait(d) => d.toMillis.toString
    }.mkString(",") + "]"
  }
  implicit def multipleBirdInstructions(t: Seq[Seq[Instructions]]) = Pretty {
    params =>
      t.zipWithIndex.map {
        case (instrs, i) => s"Bird $i: " + instructionSet(instrs)(params)
      }.mkString("\n")
  }

  implicit def slowPlayer(t: SlowTestPlayer) = Pretty { p =>
    t.wrapped.toString +
    s" who always waits ${t.alwaysWaitTime}" +
    s" and has these bird instructions: \n" + multipleBirdInstructions(t.birdInstructions)(p)
  }

  implicit def playerSeq(t: Seq[SlowTestPlayer]) = Pretty { params =>
    s"${t.size} players:\n " +
    t.map(pl => slowPlayer(pl)(params)).mkString("\n ")
  }
}
import Pretties._

object BigGameTest2 extends Properties("A free-for-all") {
  import Prop._
  import GameGen._
  import RuleGenerators._
  import akka.actor._
  import PropForEach.forEach


  val fudge = 300.millis

  val customConf = com.typesafe.config.ConfigFactory.parseString("""
      akka {
        log-dead-letters = 0
      }
      """)

  property("suckers never win") = forAll(
    SlowTestPlayer.someSlowPlayers,
    ruleGen(100),
    reasonableTimeLimit) {
      (birds: Seq[SlowTestPlayer], rules: Rules, timeLimit: FiniteDuration) =>

      (birds.length >= 3) ==> { // Don't shrink too far
        println("Checking that suckers never win.")
        val suckers = birds.filter(_.alwaysCooperates)
        (suckers.nonEmpty) ==> {
          println(s"There are ${suckers.length} suckers  ${birds.length} competitors time limit: $timeLimit")

          val result = Game.eachOnEach(rules)(actorSystem, birds.map(_.player), timeLimit)
          classify(result.isLeft, "sad", "happy") {
          result match {
            case Right(happy) => {
               val output = happy.scores

              def score(p: TestPlayer) = {
               output.find(ao => ao.player == p.player).get.score
              }
              val maxScore = output.map(_.score).max

              forEach[TestPlayer](suckers,
               s => score(s) >= maxScore, // THIS SHOULD FAIL
               s => s"Sucker $s won with ${score(s)} points")
            }
            case Left(sad) =>
              println(sad)
              // dang, too slow or something
              Prop.undecided
          }
         }

        }
     }
   }


  // I should do this in ScalaTest so that I can shut this down
  val actorSystem = ActorSystem("big-game-test", customConf)
}

object BigGameTest extends Properties("A free-for-all") {
  import Prop._
  import GameGen._
  import RuleGenerators._
  import akka.actor._
  import PropForEach.forEach

  val fudge = 300.millis

  val customConf = com.typesafe.config.ConfigFactory.parseString("""
      akka {
        log-dead-letters = 0
      }
      """)

  // I should do this in ScalaTest so that I can shut this down
  val actorSystem = ActorSystem("big-game-test", customConf)

  def eachPlayerGetsAResult(players: Seq[Player], results: Seq[AggregateOutcome]) :Prop =
    (players.length =? results.length) :| "Wrong number of results" &&
    Prop.all(players.map(p => results.exists(_.player == p) :| "No results for $p"):_*)

 property("All games end within the time limit") =
  forAll(ruleGen(20) :| "Rules", SlowTestPlayer.someSlowPlayers :| "Players", reasonableTimeLimit :| "Time limit") {
  (rules: Rules, testPlayers: Seq[SlowTestPlayer], timeLimit: FiniteDuration) =>
      (testPlayers.length >= 3) ==> { // Don't shrink too far
   classify(testPlayers.size < 10, "small", "large") {

     val players = testPlayers.map(_.player)

    val timer = new Timer()
    val output = Game.eachOnEach(rules)(actorSystem, players, timeLimit)

    val timeTaken = timer.check
    val timeOver = timeTaken - timeLimit

    classify (timeOver < (fudge/2), "comfortable", "barely") {
       (timeTaken <= (timeLimit + fudge)) :| s"$timeTaken was longer than $timeLimit" &&
       (output match {
         case Right(EachOnEachOutcome(output, actorRef)) =>
          actorRef.isTerminated :| "Actor shut down" &&
          eachPlayerGetsAResult(players, output)
         case Left(_) =>
          Prop.passed
       })
    }
   }
   }
  }

}

class Timer {
  def now = new java.util.Date().getTime
  val start = now
  def check: Duration = (now - start).millis
}

object EachOnEachEasyTest extends Properties("Not too many at once") {
  import Prop._
  val customConf = com.typesafe.config.ConfigFactory.parseString("""
      akka {
        log-dead-letters = 0
      } """)

  val actorSystem = ActorSystem("big-game-test", customConf)

  property("minimal test") = {

    val alwaysWaitTime = 100.millis
    val timeLimit = 200.millis
    val rules = Rules(5,3,1,0)
    val numPlayers = 3

    val players = for {
      i <- 1 to numPlayers
    } yield
      (SlowTestPlayer(ConstantTestPlayer("Sucker #" + i, Cooperate), Seq(), Seq(Seq((MakeAMove))), alwaysWaitTime).initSomeNewGames(2))


    val output = Game.eachOnEach(rules)(actorSystem, players.map(_.player), timeLimit)

    // the most turns a player could make
    // number of matches
    val maxScorePerTurn = rules.temptationToDefect
    val maxConceivablePoints = (timeLimit / alwaysWaitTime) * matches(numPlayers) * maxScorePerTurn
    val happyOutput = output.right.get

    PropForEach.forEach[AggregateOutcome](happyOutput.scores,
      outcome => outcome.score <= maxConceivablePoints,
      outcome => s"Wait! But ${outcome.player} got ${outcome.score} points but anything over $maxConceivablePoints is ridiculous."
    )
  }

  def matches(numBirds: Int) = combinations(numBirds, 2)

  def combinations(n: Int, k: Int) :Int = if (k == 0) 1 else {
    n * combinations(n-1,k-1) / k
  }

  property("combinations works") = {
    combinations(2, 2) == 1 &&
    combinations(3, 2) == 3 &&
    combinations(20,2) == 190
  }
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
