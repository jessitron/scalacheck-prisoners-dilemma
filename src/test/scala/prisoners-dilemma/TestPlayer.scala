package prisoners_dilemma

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import Package._
import scala.concurrent.duration._

trait TestPlayer {
  def name: Name
  def strategy: RoundStrategy

  def alwaysCooperates: Boolean
  def startsFriendly: Boolean
  def isTitForTat: Boolean = false

  lazy val player = Player(name, new Strategizer {
    def newGame() = strategy
  })

}

sealed trait Instructions
case object MakeAMove extends Instructions
case object Failinate extends Instructions
case class Wait(d: Duration) extends Instructions

case class SlowStrategy(inner: RoundStrategy,
  instructions: Seq[Instructions],
  alwaysWaitTime: FiniteDuration) extends RoundStrategy
{
  private val doThese = overAndOverForever(instructions).iterator
   val currentMove = inner.currentMove
   def next(m: Move) = {
     Thread.sleep(alwaysWaitTime.toMillis)
     doThese.next() match {
       case MakeAMove => inner.next(m)
       case Failinate => throw new Exception("bird poop")
       case Wait(d) => Thread.sleep(d.toMillis); next(m)
     }
   }

  override def toString() = s"repeat these instructions: $instructions and wraps strategy: $inner"
}

case class SlowTestPlayer(wrapped: TestPlayer,
  newPlayerInstructions: Seq[Instructions],   // not yet used
  birdInstructions: Seq[Seq[Instructions]],
  alwaysWaitTime: FiniteDuration) extends TestPlayer {
  def name: Name = wrapped.name
  def strategy = ???
  def alwaysCooperates = wrapped.alwaysCooperates
  def startsFriendly = wrapped.startsFriendly
  override def isTitForTat = wrapped.isTitForTat

  private[this] def nextBird() = this.synchronized { birdIter.next() }

  override lazy val player = Player(name, new Strategizer {
    val myStrategization = nextBird()()
    def newGame() = myStrategization
    override def toString() = s" who likes to $myStrategization"
  })

  // I'm being strange here to make this thread-safe. Have a better idea?
  var birdIter: Iterator[() => SlowStrategy] = null

  // This is because I can't figure out how to make NewGame be thread-safe and still pull from one stream of instructions sets
  // there must be a better way
  def initSomeNewGames(numBirds:Int) = {
    // TODO: use instructions to make this sometimes slow
    val birdInstructionIterator = overAndOverForever(birdInstructions.toStream).iterator
    val requestedBirds = for {
      i <- 1 to numBirds
    } yield {
      val p = SlowStrategy(wrapped.strategy, birdInstructionIterator.next(), alwaysWaitTime)
      () => p
    }
    birdIter = requestedBirds.iterator
    this
  }
}

object SlowTestPlayer {
  import Gen._
  import scala.concurrent.duration._

  val waitTime: Gen[Wait] = choose(10,500).map(_.millis).map(Wait(_))
  val oneInstruction: Gen[Instructions] = oneOf(const(MakeAMove), const(Failinate), waitTime)
  val someInstructions: Gen[Seq[Instructions]] = Package.someOf(choose(1, 100), oneInstruction)
  val multipleBirdInstructions = Package.someOf(choose(1,20), someInstructions)
  val timeToWaitEveryMove: Gen[FiniteDuration] = Gen.choose(50, 100).map(_.millis)

  private val slowPlayer: Gen[SlowTestPlayer] = for {
    player <- TestPlayer.someStandardPlayer
    newPlayerInstructions <- someInstructions
    birdInstructions <- multipleBirdInstructions
    alwaysWaitTime <- timeToWaitEveryMove
  } yield (SlowTestPlayer(player, newPlayerInstructions, birdInstructions, alwaysWaitTime))

  val someSlowPlayers: Gen[Seq[SlowTestPlayer]] =
    for {
      n <- choose(3, 20)
      ps <- listOfN(n, slowPlayer)
    } yield {
      ps.foreach { p => p.initSomeNewGames(n)}
      ps
    }
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
  val nameGen = NameGenerator.pronounceableStr
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
