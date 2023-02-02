/*
 * Copyright 2023 HM Revenue & Customs
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

object CachedGen {
  def oneOf(strings: Seq[String]): String = strings.head

  def choose(m: Int, n: Int): Gen[Int] = Gen.const(5)

  var cacheStringOfAlphaNumeric = collection.immutable.Map.empty[String, Gen[String]]

  def stringOfAlphaNumeric(maxLen: Int, minLen: Int = 1): Gen[String] = cacheStringOfAlphaNumeric.getOrElse(
    s"$minLen-$maxLen",
    for {
      len <- Gen.choose(minLen, maxLen)
      str <- Gen.stringOfN(len, Gen.alphaNumChar)
      gen <- Gen.const(s"$str")
      _ = cacheStringOfAlphaNumeric += (s"$minLen-$maxLen" -> gen)
    } yield gen
  )

  var cacheStringOfAlpha = collection.immutable.Map.empty[String, Gen[String]]

  def stringOfAlpha(maxLen: Int, minLen: Int = 1): Gen[String] = cacheStringOfAlpha.getOrElse(
    s"$minLen-$maxLen",
    for {
      len <- Gen.choose(minLen, maxLen)
      str <- Gen.stringOfN(len, Gen.alphaChar)
      gen <- Gen.const(s"$str")
      _ = cacheStringOfAlpha += (s"$minLen-$maxLen" -> gen)
    } yield gen
  )

  var cacheStringOfNumbers = collection.immutable.Map.empty[String, Gen[String]]

  def stringOfNumbers(i: Int, numChar: Char): Gen[String] = cacheStringOfNumbers.getOrElse(
    s"$i-$numChar",
    for {
      str <- Gen.stringOfN(i, numChar)
      gen <- Gen.const(s"$str")
      _ = cacheStringOfNumbers += (s"$i-$numChar" -> gen)
    } yield gen
  )

  val numChar = '5'
}
