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
import play.api.mvc.PathBindable

import scala.collection.immutable.ListSet

sealed abstract class MessageType(
  val rootNode: String,
  val templateFile: String,
  val templateArgGen: Gen[JsObject]
) extends Product
    with Serializable

object MessageType {
  case object CC007A
      extends MessageType("CC007A", "cc007a.njk", Phase4Generators.cc007aGen)
  case object CC007C
      extends MessageType("CC007C", "cc007c.njk", Phase5Generators.cc007cGen)

  case object CC015B
      extends MessageType("CC015B", "cc015b.njk", Phase4Generators.cc015bGen)
  case object CC015C
      extends MessageType("CC015C", "cc015c.njk", Phase5Generators.cc015cGen)

  case object CC044A
      extends MessageType("CC044A", "cc044a.njk", Phase4Generators.cc044aGen)
  case object CC044C
      extends MessageType("CC044C", "cc044c.njk", Phase5Generators.cc044cGen)

  val values: Set[MessageType] = ListSet(
    CC007A,
    CC007C,
    CC015B,
    CC015C,
    CC044A,
    CC044C
  )

  implicit val messageTypePathBindable: PathBindable[MessageType] = new PathBindable.Parsing(
    parse = rootNode => values.find(_.rootNode == rootNode).get,
    serialize = _.rootNode,
    (rootNode, _) =>
      s"${rootNode} is not a valid message type. Known message types: ${values.map(_.rootNode).mkString(", ")}"
  )
}
