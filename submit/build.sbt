//  NOTE(arjun): When last checked, Spray did not support 2.11
scalaVersion := "2.10.4"

scalacOptions ++=
  Seq("-deprecation",
      "-unchecked",
      "-feature",
      "-Xfatal-warnings")


// The JavaFX initialization must only run once per JVM instance. Without fork,
// running twice in the same SBT instance will fail.
fork := true

// Prevents epic screw ups when redirecting to UMass.
javaOptions += "-Djsse.enableSNIExtension=false"

resolvers += "spray repo" at "http://repo.spray.io"

libraryDependencies ++=
  Seq("io.spray" %%  "spray-json" % "1.2.6",
      "org.scala-lang.modules" %% "scala-async" % "0.9.1",
      "net.databinder.dispatch" %% "dispatch-core" % "0.11.1")