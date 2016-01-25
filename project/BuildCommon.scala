import sbt.Keys._
import sbt._

import scalariform.formatter.preferences._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import de.heikoseeberger.sbtheader.HeaderPattern
import de.heikoseeberger.sbtheader.HeaderPlugin
import de.heikoseeberger.sbtheader.HeaderKey.headers

import scala.{ Console => C }

object BuildCommon extends AutoPlugin {
  override def requires = plugins.JvmPlugin && SbtScalariform && HeaderPlugin
  override def trigger = allRequirements

  def baseSettings = Seq(
    organization    := "fail.sauce",
    version         := "0.0.0",
    scalaVersion    := "2.11.7",
    scalacOptions   ++= Seq("-deprecation", "-feature", "-unchecked", "-encoding", "utf8"),
    javacOptions    ++= Seq("-encoding", "UTF-8", "-Xlint:-options"),
    headers         <<= (name, version) { (name, version) => Map(
      "scala" -> (
        HeaderPattern.cStyleBlockComment,
        """|/*
           | * Copyright 2016 Andy Scott
           | *
           | * Licensed under the Apache License, Version 2.0 (the "License");
           | * you may not use this file except in compliance with the License.
           | * You may obtain a copy of the License at
           | *
           | *    http://www.apache.org/licenses/LICENSE-2.0
           | *
           | * Unless required by applicable law or agreed to in writing, software
           | * distributed under the License is distributed on an "AS IS" BASIS,
           | * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
           | * See the License for the specific language governing permissions and
           | * limitations under the License.
           | */
           |
           |""".stripMargin)
    )}
  )

  def formatSettings = SbtScalariform.scalariformSettings ++ Seq(
    ScalariformKeys.preferences := ScalariformKeys.preferences.value
      .setPreference(RewriteArrowSymbols, true)
      .setPreference(AlignParameters, true)
      .setPreference(AlignSingleLineCaseStatements, true)
      .setPreference(DoubleIndentClassDeclaration, true)
      .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
      .setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, true)
  )

  def miscSettings = Seq(
    shellPrompt := { s => s"${C.BLUE}${Project.extract(s).currentProject.id}>${C.RESET} " }
  )

  override def projectSettings =
    baseSettings ++
    formatSettings ++
    miscSettings

  object autoImport {
    def compilelibs(deps: ModuleID*) = deps map (_ % "compile")
    def testlibs(deps: ModuleID*) = deps map (_ % "test")
  }
}
