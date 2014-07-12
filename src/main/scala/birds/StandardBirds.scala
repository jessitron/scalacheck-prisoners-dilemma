package birds

import prisoners_dilemma._

object StandardBirds {

  val SUCKER: RoundStrategy = new RoundStrategy {
     val currentMove = Cooperate
     def next(x: Move) = this

     override def toString = "Sucker!!"
  }

}
