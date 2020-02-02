import sbt.Keys.libraryDependencies

initialCommands in console :=
  """import pw._0x9.iz._
    |import Stage._
    |""".stripMargin

lazy val commonSettings = Seq(
  organization := "pw._0x9",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.13.1",
  test in assembly := {}
)

lazy val assemblySettings = Seq(
  assemblyMergeStrategy in assembly := {
    case PathList(ps @ _*) if ps.last endsWith ".xml" => MergeStrategy.first
    case PathList(ps @ _*) if ps.last endsWith ".class" => MergeStrategy.first
    case PathList(ps @ _*) if ps.last endsWith ".properties" => MergeStrategy.first
    case x =>
      val oldStrategy = (assemblyMergeStrategy in assembly).value
      oldStrategy(x)
  }
)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(name := "iz")

lazy val lib = (project in file("lib"))
  .settings(commonSettings: _*)
  .settings(assemblySettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2-core" % "4.6.0" % "test",
      "com.typesafe.akka" %% "akka-actor" % "2.6.3"
    ),
    scalacOptions ++= Seq("-language:postfixOps"),
    scalacOptions in Test ++= Seq("-Yrangepos"),
    assemblyJarName in assembly := s"iz-${name.value}-${version.value}.jar"
  )

lazy val fx = (project in file("fx"))
  .settings(commonSettings: _*)
  .settings(assemblySettings: _*)
  .settings(
    fork in run := true,
    // Add dependency on ScalaFX library
    libraryDependencies += "org.scalafx" %% "scalafx" % "12.0.2-R18",
    libraryDependencies ++= {
      // Determine OS version of JavaFX binaries
      lazy val osName = System.getProperty("os.name") match {
        case n if n.startsWith("Linux")   => "linux"
        case n if n.startsWith("Mac")     => "mac"
        case n if n.startsWith("Windows") => "win"
        case _ => throw new Exception("Unknown platform!")
      }

      // Add dependency on JavaFX libraries, OS dependent
      lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
      javaFXModules.map( m =>
        "org.openjfx" % s"javafx-$m" % "12.0.2" classifier osName
      )
    },
    assemblyJarName in assembly := s"iz-${name.value}-${version.value}.jar"
  )
  .dependsOn(lib)
