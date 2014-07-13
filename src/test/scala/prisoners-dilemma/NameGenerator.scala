package prisoners_dilemma

import org.scalacheck.Gen

object NameGenerator {
  val VOWELS = Seq('A','E','I','O','U')
  def isVowel(c:Char) = VOWELS.contains(c.toUpper)

  val CONSONANTS = ('B' to 'Z').filterNot(isVowel).filterNot(_ == 'Q')

  val consonant: Gen[Char] =
    Gen.oneOf(CONSONANTS)
  val vowel: Gen[Char] =
    Gen.oneOf(VOWELS)

  val syllable = for {
    c1 <- consonant
    v  <- vowel
    c2 <- consonant
  } yield c1.toString + v + c2

  val pronounceableStr = for {
    n <- Gen.choose(1,4)
    parts <- Gen.listOfN(n, syllable)
  } yield parts.mkString("").toLowerCase.capitalize
}
