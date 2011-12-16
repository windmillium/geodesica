name := "Geodesica Engine"

version := "0.01.00"

scalaVersion := "2.9.1"

checksums := Nil

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Akka Repository" at "http://akka.io/repository/"

scalacOptions += "-deprecation"

libraryDependencies += "se.scalablesolutions.akka" % "akka-actor" % "1.2"

libraryDependencies += "se.scalablesolutions.akka" % "akka-camel" % "1.2"

libraryDependencies += "org.apache.camel" % "camel-jetty" % "2.7.0"

libraryDependencies += "cc.spray.json" %% "spray-json" % "1.0.1"

libraryDependencies += "cc.spray" % "spray-server" % "0.8.0"

libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "latest.integration"

libraryDependencies += "net.liftweb" %% "lift-json" % "2.4-M5"
