name := "HbcTestTask"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "4.2.0" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")