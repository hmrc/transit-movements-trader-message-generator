/*
 * Copyright 2021 HM Revenue & Customs
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

import org.scalacheck.Gen
import play.api.libs.json.JsObject

abstract class TemplateArgGenerators {
  type ArgGen = Gen[JsObject]

  def alphaNum(maxLen: Int) = for {
    len <- Gen.choose(1, maxLen)
    str <- Gen.stringOfN(len, Gen.alphaNumChar)
  } yield str

  def alphaNumExactly(len: Int) =
    Gen.stringOfN(len, Gen.alphaNumChar)

  def alphaExactly(len: Int) =
    Gen.stringOfN(len, Gen.alphaChar)

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
