
name := "99probs"

version := "0.0.1"

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.8.1" % "test",
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
  "org.mockito" % "mockito-core" % "1.8.5" % "test"
)

unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_))

unmanagedSourceDirectories in Test <<= (scalaSource in Test)(Seq(_))

//parallelExecution in Test := false

scalacOptions ++= Seq("-deprecation", "-unchecked")

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

