name := "driver-trips"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "net.sourceforge.htmlunit" % "htmlunit" % "2.21",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test" exclude("org.scala-lang", "scala-reflect")
)

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"