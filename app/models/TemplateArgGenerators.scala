/*
 * Copyright 2022 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package models

import cats.implicits.catsSyntaxTuple2Semigroupal
import org.scalacheck.Gen
import org.scalacheck.cats.implicits.genInstances
import play.api.libs.json.JsObject

abstract class TemplateArgGenerators {
  type ArgGen = Gen[JsObject]

  def alphaNum(maxLen: Int, minLen: Int = 1) = for {
    len <- Gen.choose(minLen, maxLen)
    str <- Gen.stringOfN(len, Gen.alphaNumChar)
  } yield str

  def alphaNumCapital(maxLen: Int, minLen: Int = 1) = for {
    len <- Gen.choose(minLen, maxLen)
    str <- Gen.stringOfN(len, Gen.alphaNumChar)
  } yield str.toUpperCase()

  def alpha(maxLen: Int, minLen: Int = 1) = for {
    len <- Gen.choose(minLen, maxLen)
    str <- Gen.stringOfN(len, Gen.alphaChar)
  } yield str

  def alphaNumCSV(maxLen: Int, maxListLength: Int, minLen: Int = 1): Gen[String] =
    Gen.oneOf(Seq("15501523082774,15501523082773"))

  def alphaNumExactly(len: Int) =
    Gen.stringOfN(len, Gen.alphaNumChar)

  def alphaExactly(len: Int) =
    alpha(len, len)

  def decimalNumber(totalDigits: Int, fractionDigits: Int) =
    (num1(totalDigits - fractionDigits), num(fractionDigits)).mapN(_ + "." + _)

  def num(len: Int) =
    Gen.stringOfN(len, Gen.numChar)

  val indicator = Gen.oneOf("0", "1")

  def num1(len: Int) =
    if (len <= 0)
      Gen.const("")
    else if (len <= 1)
      Gen.choose(1, 9).map(_.toString)
    else
      for {
        initChar  <- Gen.choose(1, 9).map(_.toString)
        restChars <- Gen.stringOfN(len - 1, Gen.numChar)
      } yield initChar + restChars

  def leftPad(str: String, len: Int, paddingChar: Char) =
    str.reverse.padTo(len, paddingChar).reverse
}
