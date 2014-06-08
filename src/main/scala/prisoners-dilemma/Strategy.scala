package prisoners_dilemma

trait Strategy {
  val currentMove: Move
  def next(opponentMove: Move): Strategy

  override def toString: String = s"Next I will $currentMove."
}

object Strategy {
  def moves(p1: Strategy, p2: Strategy): Stream[MoveSet] =
    (p1.currentMove, p2.currentMove) #:: moves(p1.next(p2.currentMove), p2.next(p1.currentMove))

  def stringCatting(s: Strategy, log: String): Strategy = new Strategy {
     val currentMove = s.currentMove
     def next(opponentMove: Move) = stringCatting(s.next(opponentMove), (s"did $currentMove; "))

     override def toString: String = log + s.toString
  }

  def fromStream(moves: Stream[Move]): Strategy = moves match {
    case head #:: tail => new Strategy {
      val currentMove = head
      def next(x: Move) = stringCatting(fromStream(tail), s" about to $currentMove")

      override def toString = "Reading from a stream." + super.toString
    }
  }

  val sucker: Strategy = new Strategy {
     val currentMove = Defect
     def next(x: Move) = this
  }

}
