package prisoners_dilemma

import org.scalacheck.Gen

object NameGenerator {
  val VOWELS = Seq('A','E','I','O','U')
  def isVowel(c:Char) = VOWELS.contains(c.toUpper)

  def fixQ(c: Char) = c match {
    case 'Q' | 'q' => c + "u"
    case other => other.toString
  }

  val consonant: Gen[String] =
    Gen.oneOf('b' to 'z').suchThat(!isVowel(_)).map(fixQ)
  val vowel: Gen[String] =
    Gen.oneOf(VOWELS).map(_.toLower).map(_.toString)

  val syllable = for {
    c1 <- consonant
    v  <- vowel
    c2 <- consonant.suchThat(! _.startsWith("q"))
  } yield c1 + v + c2

  val pronounceableStr = for {
    n <- Gen.choose(1,4)
    parts <- Gen.listOfN(n, syllable)
  } yield parts.mkString("").capitalize
}
