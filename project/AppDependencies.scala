import play.core.PlayVersion
import sbt._

object AppDependencies {

  val compile: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"          %% "bootstrap-backend-play-28" % "7.12.0",
    "uk.gov.hmrc"          %% "play-nunjucks"             % "0.41.0-play-28",
    "org.typelevel"        %% "cats-core"                 % "2.6.1",
    "org.scalacheck"       %% "scalacheck"                % "1.17.0",
    "io.chrisdavenport"    %% "cats-scalacheck"           % "0.3.0",
    "io.github.wolfendale" %% "scalacheck-gen-regexp"     % "0.1.3"
  )

  val test: Seq[ModuleID] = Seq(
    "org.scalatestplus"      %% "scalacheck-1-15"    % "3.2.11.0",
    "org.scalatestplus.play" %% "scalatestplus-play" % "5.1.0",
    "com.vladsch.flexmark"    % "flexmark-all"       % "0.36.8"
  ).map(_ % Test)
}
