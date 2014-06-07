package prisoners_dilemma

import org.scalacheck._

object RuleGenerators {

  val ruleGen: Gen[Rules] = for {
     t <- Gen.choose(0, 10)
     r <- Gen.choose(0, 10)
     p <- Gen.choose(0, 10)
     s <- Gen.choose(0, 10)
     if (t > r) // todo: check against
     if (r > p)
     if (p > s)
     if (2 * r > (t + s))
  } yield Rules(t, r, p, s)

  implicit val arbRules: Arbitrary[Rules] = Arbitrary(ruleGen)

  val move: Gen[Move] = Gen.oneOf(Cooperate, Defect)
  implicit val arbMoves: Arbitrary[Move] = Arbitrary(move)
}

object GameTest extends Properties("Prisoners Dilemma") {
  import Prop._
  import RuleGenerators._
  property("Defection is always better for me") =
     forAll {(rules: Rules, theirMove: Move) =>
       val ifIDefect = Rules.score(rules, (Defect, theirMove))._1
       val ifICooperate = Rules.score(rules, (Cooperate, theirMove))._1

       ifIDefect > ifICooperate
     }

}
