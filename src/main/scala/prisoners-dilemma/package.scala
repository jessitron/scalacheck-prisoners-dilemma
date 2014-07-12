

package object prisoners_dilemma {
  type Points = Int

  sealed trait Move
  case object Defect extends Move
  case object Cooperate extends Move

  type MoveSet = (Move, Move)
  type ScoreSet = (Points, Points)

  type Name = String
}
