package prisoners_dilemma

trait Strategy {
  val currentMove: Move
  def next(opponentMove: Move): Strategy
}

object Strategy {
  def moves(p1: Strategy, p2: Strategy): Stream[MoveSet] =
    (p1.currentMove, p2.currentMove) #:: moves(p1.next(p2.currentMove), p2.next(p1.currentMove))

  def fromStream(moves: Stream[Move]): Strategy = moves match {
    case head #:: tail => new Strategy {
      val currentMove = head
      def next(x: Move) = fromStream(tail)
    }
  }

}
