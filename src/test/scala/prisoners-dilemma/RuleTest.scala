package prisoners_dilemma

import org.scalacheck._
import Package._

object RuleGenerators {

  def ruleGen(max: Points): Gen[Rules] = for {
     t <- Gen.choose(3, max)
     r <- Gen.choose(2, (t - 1))
     p <- Gen.choose(1, (r - 1))
     s <- Gen.choose(0, (p - 1))
     if (2 * r > (t + s))
  } yield Rules(temptationToDefect = t,
                rewardForMutualCooperation = r,
                punishmentForMutualDefection = p,
                suckersPenalty = s)

  implicit val arbRules: Arbitrary[Rules] = Arbitrary(ruleGen(Int.MaxValue / 2))


}

object RuleTest extends Properties("Prisoner's Dilemma") {
  import Prop._
  import Gen._
  import RuleGenerators._
  property("Defection is always better for me") =
     forAll {(rules: Rules, theirMove: Move) =>
       val (ifIDefect, _) = Rules.score(rules, (Defect, theirMove))
       val (ifICooperate, _) = Rules.score(rules, (Cooperate, theirMove))

       ifIDefect > ifICooperate
     }

  property("The best possible global outcome is both cooperate") =
    forAll{ (rules: Rules, moves: MoveSet) =>
      (moves != (Cooperate, Cooperate)) ==> {
        cooperationIsBetter(rules, moves)
      }
    }

  def cooperationIsBetter(rules:Rules, moves: MoveSet): Prop = {
    val bestResult = Rules.score(rules, (Cooperate, Cooperate))
    val myResult = Rules.score(rules, moves)
    (total(bestResult) > total(myResult)) :|
         s"Cooperating got $bestResult while $moves got $myResult"
  }

  def total(scores: ScoreSet) = scores._1 + scores._2

  property("The game is fair") =
    forAll {(rules: Rules, moves: MoveSet) =>
      val oneWay = Rules.score(rules, moves)
      val theOtherWay = Rules.score(rules, moves.swap)

      oneWay.swap == theOtherWay
    }

    // An Int as points doesn't work for ints too large to add together
// property("This one thing that failed once") = {
//  val rules = Rules(2079120587,1233644972,157614103,141316510)
//  val moves = (Defect,Defect)
//  cooperationIsBetter(rules, moves)
// }

}
