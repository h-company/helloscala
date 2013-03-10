import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "helloplay"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    //    anorm
    "com.github.seratch" %% "scalikejdbc" % "[1.4,)",
    "com.github.seratch" %% "scalikejdbc-play-plugin" % "[1.4,)",
    "com.github.seratch" %% "scalikejdbc-interpolation" % "[1.4,)",
    "org.scala-tools.time" % "time_2.9.1" % "0.5",
    "jp.t2v" %% "play21.auth" % "0.6",
    "org.slf4j" % "slf4j-simple" % "[1.7,)"
  )

  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
  )

}
