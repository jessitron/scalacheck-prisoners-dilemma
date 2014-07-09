package prisoners_dilemma

import scala.concurrent._
import duration._
import java.util.concurrent.atomic.AtomicReference

object TestStrategy {

  trait Instruction[T]
  case class WhatYouNeed[T](m: T) extends Instruction[T]
  case class WaitAndThen(d: Duration) extends Instruction[Nothing]
  case object Failinate extends Instruction[Nothing]

  type RoundInstruction = Instruction[Move]

  class TestRound(thisMove: Move, future: Seq[RoundInstruction]) extends RoundStrategy {

    var instructions = new AtomicReference(future)

    def currentMove = thisMove
    def next(oppMove: Move) = follow(instructions.get,
      (m, rest) => TestRound(m, rest)
      (is) => instructions.set(is))
  }


  private def follow[T, R](instr: Seq[Instruction[T]],
                           finish: (T, Seq[Instruction[T]]) => R,
                           beforeException: Seq[Instruction[T]] => Unit): R = {
        if (instr.isEmpty) throw new Exception("End of instruction set")
        instr.head match {
         case WhatYouNeed(m) => finish(m, instr.tail)
         case WaitAndThen(d) => blocking {
           Thread.sleep(d.toMillis)
          }
          follow(instr.tail, remainder)
         case Failinate =>
          beforeException(instr.tail)
          // can't recurse when I'm about to...
          throw new Exception("POOOOOOOOP")
       }
      }

      type StrategizerInstruction = Instruction[RoundInstruction]
  class TestStrategizer(stuff: Seq[StrategizerInstruction]) extends Strategizer {

    var instructionSeqs = new AtomicReference(stuff)

     def newGame() = {
       val myInstructions = instructionSeqs.wait....
       val result = follow(instructionSeqs.get,
         (newGame, remainder) => {
           instructionSeqs.set(remainder)
           newGame
         }
         instructionSeqs.set _)

       result
     }
  }

}
