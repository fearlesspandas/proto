name := "Typical"

version := "1.0"


scalaVersion := "2.12.10"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

initialCommands in console := """
                                |println("helloFromConsoleDataSETTTTT")
                                |import Typical.core.dataset._
                                |import Typical.core.grammar._
                                |import test.runner._
                                |import test._
                                |import test.Account._
    """.trim.stripMargin
//libraryDependencies += "dev.zio" %% "zio" % "1.0.5"
//
//libraryDependencies += "com.algorand" % "algosdk" % "1.4.0"