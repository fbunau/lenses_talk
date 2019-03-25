name := "lenses_talk"

version := "1.0"

scalacOptions += "-Ypartial-unification"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")

scalaVersion := "2.12.8"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.2.0"
libraryDependencies += "com.github.julien-truffaut" %%  "monocle-core"  % "1.5.0"
libraryDependencies += "com.github.julien-truffaut" %%  "monocle-macro"  % "1.5.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"