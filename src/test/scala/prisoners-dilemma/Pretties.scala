package prisoners_dilemma


object Pretties {
  // if this works, move it
  import org.scalacheck.util.Pretty
  implicit def prettyRules(t: Rules) = Pretty { p => s"T = ${t.temptationToDefect}\n" +
    s"       R = ${t.rewardForMutualCooperation}\n" +
    s"       P = ${t.punishmentForMutualDefection}\n" +
    s"       S = ${t.suckersPenalty}\n"
  }

  implicit def instructionSet(t: Seq[Instructions]) = Pretty { params =>
    "[" + t.map {
      case MakeAMove => "M"
      case Failinate => "F"
      case Wait(d) => d.toMillis.toString
    }.mkString(",") + "]"
  }
  implicit def multipleBirdInstructions(t: Seq[Seq[Instructions]]) = Pretty {
    params =>
      t.zipWithIndex.map {
        case (instrs, i) => s"Bird $i: " + instructionSet(instrs)(params)
      }.mkString("\n")
  }

  implicit def slowPlayer(t: SlowTestPlayer) = Pretty { p =>
    t.wrapped.toString +
      s" who always waits ${t.alwaysWaitTime}" +
      s" and has these bird instructions: \n" + multipleBirdInstructions(t.birdInstructions)(p)
  }

  implicit def playerSeq(t: Seq[SlowTestPlayer]) = Pretty { params =>
    s"${t.size} players:\n " +
      t.map(pl => slowPlayer(pl)(params)).mkString("\n ")
  }
}
