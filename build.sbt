import Dependencies._

lazy val root = (project in file("."))
  .settings(
    inThisBuild(List(
      organization := "com.ruchij",
      scalaVersion := "2.12.4"
    )),
    name := "jenny-subtree-scala",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      pegdown % Test
    )
  )

coverageEnabled := true

testOptions in Test +=
  Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/test-results")