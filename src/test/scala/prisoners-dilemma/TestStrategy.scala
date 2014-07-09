package prisoners_dilemma

import scala.concurrent._
import duration._
import java.util.concurrent.atomic.AtomicReference

object TestStrategy {

  trait Instruction
  case class TakeMove(m: Move) extends Instruction
  case class WaitAndThen(d: Duration) extends Instruction
  case object Failinate extends Instruction


  class TestRound(thisMove: Move, future: Seq[Instruction]) extends RoundStrategy {

    var instructions = new AtomicReference(future)

    def currentMove = thisMove
    def next(oppMove: Move) = follow(instructions.get, (is) => instructions.set(is))
  }

  private def follow(instr: Seq[Instruction], remainder: Seq[Instruction] => Unit): TestRound= {
        if (instr.isEmpty) throw new Exception("End of instruction set")
        instr.head match {
         case TakeMove(m) => new TestRound(m, instr.tail)
         case WaitAndThen(d) => blocking {
           Thread.sleep(d.toMillis)
          }
          follow(instr.tail, remainder)
         case Failinate =>
          remainder(instr.tail)
          // can't recurse when I'm about to...
          throw new Exception("POOOOOOOOP")
       }
      }

  class TestStrategizer(stuff: Seq[Seq[Instruction]]) extends Strategizer {

    var instructionSeqs = new AtomicReference(stuff)

     def newGame() = {
       if (instructionSeqs.isEmpty) {
         throw new Exception("No more instruction sets")
       }
       val thisStuff = instructionSeqs.head

       val result = follow(thisStuff,
         // in case of exception, I want to re-use the rest of this instruction sequence
         is => instructionSeqs.set(is :: instructionSeqs.tail ))

       instructionSeqs.set(instructionSeqs.tail)
       result
     }
  }

}
