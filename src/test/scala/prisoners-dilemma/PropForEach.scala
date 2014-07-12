package prisoners_dilemma

import org.scalacheck.Prop

object PropForEach {

  def constantly[I, R](result: R): I => R = (i:I) => result
  def emptyString: Any => String = constantly("")

  def forEach[T](thing: Seq[T], property: T => Prop,
    message: T => String = emptyString): Prop = {
    Prop.all(thing.map{a =>
        val p = property(a)
        if (message(a).nonEmpty)
          p :| message(a)
        else p} :_*)
  }
}


