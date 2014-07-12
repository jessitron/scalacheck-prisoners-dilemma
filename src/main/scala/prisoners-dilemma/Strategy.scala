package prisoners_dilemma

trait Strategizer {
  def newGame(): RoundStrategy
}

trait RoundStrategy {
  val currentMove: Move
  def next(opponentMove: Move): RoundStrategy

  override def toString: String = s"Next I will $currentMove."
}

object RoundStrategy {
  def moves(p1: RoundStrategy, p2: RoundStrategy): Stream[MoveSet] =
    (p1.currentMove, p2.currentMove) #:: moves(p1.next(p2.currentMove), p2.next(p1.currentMove))

  class HistoricalRecord(wrapped: RoundStrategy, history: Seq[Move]) extends RoundStrategy {
    val currentMove = wrapped.currentMove
    def next(m: Move) = new HistoricalRecord(wrapped.next(m), history :+ currentMove)

    override def toString = s"About to $currentMove. From beginning of time: $history"
  }

  def recording(s: RoundStrategy):RoundStrategy = new HistoricalRecord(s, Seq())

  def fromStream(moves: Stream[Move]): RoundStrategy = moves match {
    case head #:: tail => new RoundStrategy {
      val currentMove = head
      def next(x: Move) = fromStream(tail)
    }
  }


  def chooseBasedOnTheirMove(myMove: Move, choice: Move => Move):RoundStrategy =
    new RoundStrategy {
      val currentMove = myMove
      def next(theirMove: Move) =
        chooseBasedOnTheirMove(choice(theirMove), choice)
    }

  val titForTat: RoundStrategy = chooseBasedOnTheirMove(Cooperate, (m) => m)
}

object Strategizer {
  def thatDoes(stuff: RoundStrategy) = new Strategizer {
    def newGame() = stuff
  }
}
