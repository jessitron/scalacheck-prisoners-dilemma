name := "prisoners-dilemma-scala"

version := "0.1"

scalaVersion := "2.11.1"

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
                             //"org.scalatest"  % "scalatest_2.11"  % "2.2.0"  % "test",
                   "org.scalacheck" % "scalacheck_2.11" % "1.11.4" % "test",
                   "com.typesafe.akka" % "akka-actor_2.11" % "2.3.3"          ,
                   "com.typesafe.akka" % "akka-testkit_2.11" % "2.3.3" % "test"
                           )
