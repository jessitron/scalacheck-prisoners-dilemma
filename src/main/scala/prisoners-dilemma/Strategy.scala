package prisoners_dilemma

trait Strategy {
  val currentMove: Move
  def next(opponentMove: Move): Strategy

  override def toString: String = s"Next I will $currentMove."
}

object Strategy {
  def moves(p1: Strategy, p2: Strategy): Stream[MoveSet] =
    (p1.currentMove, p2.currentMove) #:: moves(p1.next(p2.currentMove), p2.next(p1.currentMove))

  def fromStream(moves: Stream[Move],paren:String = "Reading from a stream. "): Strategy = moves match {
    case head #:: tail => new Strategy {
      val currentMove = head
      def next(x: Move) = fromStream(tail,toString)

      override def toString = paren+s",$currentMove"
    }
  }

  val sucker: Strategy = new Strategy {
     val currentMove = Defect
     def next(x: Move) = this

     override def toString = "Sucker!!"
  }

}
