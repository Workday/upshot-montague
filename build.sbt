name := "montague"

version := "0.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "commons-lang" % "commons-lang" % "2.6",
  "org.scala-lang" % "scala-reflect" % "2.10.4",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)

excludeFilter := {
  val sqlOld = (baseDirectory.value / "src/main/scala/com/workday/montague/sql_old").getCanonicalPath
  new SimpleFileFilter(_.getCanonicalPath startsWith sqlOld)
}
