package prisoners_dilemma

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import Package._

trait TestPlayer {
  def name: Name
  def strategy: RoundStrategy

  def alwaysCooperates: Boolean
  def startsFriendly: Boolean
  def isTitForTat: Boolean = false

  val player = Player(name, new Strategizer {
    def newGame() = strategy
  })

}


case class ConstantTestPlayer(name: Name, move: Move) extends TestPlayer {
  def alwaysCooperates = move == Cooperate
  def startsFriendly = alwaysCooperates
  def strategy = new RoundStrategy {
     val currentMove = move
     def next(x: Move) = this

     override def toString = s"always $move"
  }
}

case class ConsistentTestPlayer(
  name: Name,
  firstMove: Move,
  moveFunc: Move => Move) extends TestPlayer {
  def startsFriendly = firstMove == Cooperate
  def alwaysCooperates = startsFriendly &&
    moveFunc(Cooperate) == Cooperate &&
    moveFunc(Defect) == Cooperate
  override def isTitForTat = startsFriendly &&
    moveFunc(Cooperate) == Cooperate &&
    moveFunc(Defect) == Defect

  private def nextStrategy(nextMove: Move):RoundStrategy =
    new RoundStrategy {
    val currentMove = nextMove
    def next(o: Move) = nextStrategy(moveFunc(o))
  }

  def strategy = nextStrategy(firstMove)
}


object TestPlayer {
  val nameGen = MyLittleGennies.pronounceableStr
  val constantMovePlayer: Gen[TestPlayer] =
    for {
      name <- nameGen
      move <- Arbitrary.arbitrary[Move]
    } yield ConstantTestPlayer(name, move)

  val consistentMovePlayer: Gen[TestPlayer] =
    for {
      name <- nameGen
      firstMove <- Arbitrary.arbitrary[Move]
      moveFunc <- Arbitrary.arbitrary[Move => Move]
    } yield ConsistentTestPlayer(name, firstMove, moveFunc)

  val someStandardPlayer: Gen[TestPlayer] =
    Gen.oneOf(constantMovePlayer, consistentMovePlayer)

  val someStandardPlayers: Gen[Seq[TestPlayer]] =
    someOf(Gen.choose(3, 20), someStandardPlayer)


}
