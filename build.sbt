import AssemblyKeys._

assemblySettings


name := "proggen"

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-deprecation", "-feature")


EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE16)

EclipseKeys.withSource := true

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource


resolvers += "Typesafe Repository (Snapshots)" at "http://repo.typesafe.com/typesafe/snapshots/"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"


libraryDependencies ++= Seq(
//	"org.scalatest" %% "scalatest" % "1.9.1" % "test",
	"junit" % "junit" % "4.11" % "test"
)


mainClass in assembly := Some("TBD")




