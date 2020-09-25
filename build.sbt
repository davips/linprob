name := "tupi"

version := "0.1"

scalaVersion := "2.13.3"

//libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.2.2"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies += "mysql" % "mysql-connector-java" % "8.0.21"

libraryDependencies += "com.github.haifengl" %% "smile-scala" % "2.5.3" //ML stuff

libraryDependencies += "io.spray" %%  "spray-json" % "1.3.5"

libraryDependencies += "org.json4s" %% "json4s" % "3.2.11"

libraryDependencies ++= Seq( //numeric stuff
  // Last stable release
  "org.scalanlp" %% "breeze" % "1.1",

  // Native libraries are not included by default. add this if you want them
  // Native libraries greatly improve performance, but increase jar sizes.
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  "org.scalanlp" %% "breeze-natives" % "1.1",

  // The visualization library is distributed separately as well.
  // It depends on LGPL code
  "org.scalanlp" %% "breeze-viz" % "1.1"
)