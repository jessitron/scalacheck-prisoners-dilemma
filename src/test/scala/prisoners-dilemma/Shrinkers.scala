package prisoners_dilemma

import org.scalacheck.Shrink
import org.scalacheck.Shrink.shrink

object Shrinkers {

  implicit val instructionSetShrink: Shrink[Seq[Instructions]] = Shrink {
        // The goal is to remove failures and waits
    instrs =>
      breakOn((i: Instructions) => i != MakeAMove, instrs) match {
        case None => Stream.empty[Seq[Instructions]]
        case Some(Bits(before, breaking, after)) =>
          allMovesIsOneMove(before ++ after) #:: shrink(after).map(before ++ _)
      }
  }
  def allMovesIsOneMove(in: Seq[Instructions]): Seq[Instructions] =
    if (in.forall(_ == MakeAMove)) Seq(MakeAMove) else in

  case class Bits[T](before: Seq[T], breaking: T, after: Seq[T])
  def breakOn[T](pred: T => Boolean, ts: Seq[T]): Option[Bits[T]] = {
    def recur(beforeBackwards: Seq[T], stillLooking: Seq[T]): Option[Bits[T]] = stillLooking match {
      case Seq() => None
      case Seq(head, tail@ _*) if (pred(head)) => Some(Bits(beforeBackwards.reverse, head, tail))
      case Seq(head, tail@ _*) => recur(head +: beforeBackwards, tail)
    }
    recur(Seq(), ts)
  }

  implicit val testPlayerShrink: Shrink[SlowTestPlayer] = Shrink {
    case SlowTestPlayer(inner, na, birdInstrs, waitTime ) =>
      Stream.concat[SlowTestPlayer](
         shrink(birdInstrs).filter(_.nonEmpty) map (SlowTestPlayer(inner, na, _, waitTime)) // simplify instructions
      )
  }
}
