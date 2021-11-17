
ThisBuild / organization := "org.statefreak"
ThisBuild / version      := "1.0-SNAPSHOT"

name := "typical"


scalaVersion := "2.12.10"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

// https://mvnrepository.com/artifact/org.scala-lang/scala-reflect
// https://mvnrepository.com/artifact/org.scala-lang/scala-reflect
libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value


initialCommands in console := """
                                |import Typical.core.dataset._
                                |import Typical.core.grammar._
                                |import test.runner._
                                |import test._
                                |import test.Account._
                                |import GrowAccounts._
                                |import Consumption._
                                |import SpendWildly._
                                |import Property._
                                |import Date._
                                |import AccountRates._
                                |import Income._
                                |println("Done Importing...Press enter to continue")
    """.trim.stripMargin
//libraryDependencies += "dev.zio" %% "zio" % "1.0.5"
//
//libraryDependencies += "com.algorand" % "algosdk" % "1.4.0"