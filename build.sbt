import play.sbt.routes.RoutesKeys
import scoverage.ScoverageKeys

val appName = "transit-movements-trader-message-generator"

lazy val microservice = Project(appName, file("."))
  .enablePlugins(PlayScala, SbtDistributablesPlugin)
  .disablePlugins(
    JUnitXmlReportPlugin
  ) //Required to prevent https://github.com/scalatest/scalatest/issues/1427
  .settings(commonSettings)
  .settings(scalacSettings)
  .settings(scoverageSettings)
  .settings(SbtDistributablesPlugin.publishingSettings)
  .settings(
    libraryDependencies ++= AppDependencies.compile ++ AppDependencies.test,
    RoutesKeys.routesImport ++= Seq("models._")
  )

lazy val commonSettings = Def.settings(
  majorVersion := 0,
  scalaVersion := "2.13.8"
)

lazy val scalacSettings = Def.settings(
  // Disable warnings arising from generated routing code
  // scalacOptions += "-Wconf:src=routes/.*:silent",
  // Disable fatal warnings and warnings from discarding values
  scalacOptions ~= {
    opts =>
      opts.filterNot(Set("-Xfatal-warnings", "-Ywarn-value-discard"))
  }
)

lazy val scoverageSettings = Def.settings(
  Test / parallelExecution := false,
  ScoverageKeys.coverageMinimumStmtTotal := 90,
  ScoverageKeys.coverageFailOnMinimum := true,
  ScoverageKeys.coverageHighlighting := true,
  ScoverageKeys.coverageExcludedPackages := Seq(
    "<empty>",
    "Reverse.*",
    ".*(config|views.*)",
    ".*(BuildInfo|Routes).*"
  ).mkString(";"),
  ScoverageKeys.coverageExcludedFiles := Seq(
    "<empty>",
    "Reverse.*",
    ".*BuildInfo.*",
    ".*javascript.*",
    ".*Routes.*",
    ".*GuiceInjector"
  ).mkString(";")
)
