package prisoners_dilemma

class MyLittleGennies {
  val VOWELS = Seq('A','E','I','O','U')
    def isVowel(c:Char) = VOWELS.contains(c.toUpper)

      def fixQ(c: Char) = c match {
           case 'Q' || 'q' => c + "u"
           case other => other.toString
        }

        val consonant: Gen[String] = Gen.oneOf('B' to 'Z').suchThat(!isVowel(_)).map(fixQ)
          val vowel = Gen.oneOf(VOWELS).map(_.toLower).map(_.toString)

            val syllable = {
                c1 <- consonant
                v  <- vowel
                c2 <- consonant
              } yield c1 + v + c2

              val pronounceableStr = for {
                   n <- Gen.choose(1,4)
                   parts <- Gen.listOfN(n, syllable)
                } yield parts.mkString("")
}
