addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.3.0")

addSbtPlugin("io.spray" % "sbt-revolver" % "0.7.2")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.13.0")

libraryDependencies ++= Seq(
  "net.virtual-void"  %% "json-lenses"  % "0.6.0",
  "io.spray"          %% "spray-json"   % "1.3.2")
