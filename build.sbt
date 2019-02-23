name := "dublin-scala-user-group"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.typelevel" %% "cats-mtl-core" % "0.4.0"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0"

libraryDependencies += "org.typelevel" %% "cats-effect" % "1.2.0"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")