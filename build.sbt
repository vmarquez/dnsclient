name := "DNSClient"

version := ".1-SNAPSHOT"

scalaVersion := "2.11.8"

resolvers ++= Seq("Sonatype Nexus releases" at "https://oss.sonatype.org/content/repositories/releases", "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/")

libraryDependencies ++= Seq("org.scalaz" %%  "scalaz-core" % "7.2.10", 
                          "org.scodec" %% "scodec-protocols" % "0.12.0",
                          "io.netty" % "netty-example" % "4.0.0.Final")
 

initialCommands in console := "import scalaz._;import Scalaz._;import scala.concurrent.Future; import scala.reflect.runtime.universe.reify; import scala.concurrent.ExecutionContext.Implicits.global;"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:higherKinds")
