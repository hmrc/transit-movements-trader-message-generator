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
  case object CC004C extends MessageType("CC004C", "cc004c.njk", Phase5Generators.cc004cGen)

  case object CC007A extends MessageType("CC007A", "cc007a.njk", Phase4Generators.cc007aGen)
  case object CC007C extends MessageType("CC007C", "cc007c.njk", Phase5Generators.cc007cGen)

  case object CC009C extends MessageType("CC009C", "cc009c.njk", Phase5Generators.cc009cGen)

  case object CC013C extends MessageType("CC013C", "CC013C.njk", Phase5Generators.cc013cGen)

  case object CC014C extends MessageType("CC014C", "cc014c.njk", Phase5Generators.cc014cGen)

  case object CC015B extends MessageType("CC015B", "cc015b.njk", Phase4Generators.cc015bGen)
  case object CC015C extends MessageType("CC015C", "cc015c.njk", Phase5Generators.cc015cGen)

  case object CD034A extends MessageType("CD034A", "cd034a.njk", Phase4Generators.cd034aGen)
  case object CD037A extends MessageType("CD037A", "cd037a.njk", Phase4Generators.cd037aGen)

  case object CC044A extends MessageType("CC044A", "cc044a.njk", Phase4Generators.cc044aGen)
  case object CC044C extends MessageType("CC044C", "cc044c.njk", Phase5Generators.cc044cGen)

  val values: Set[MessageType] = ListSet(
    CC004C,
    CC007A,
    CC007C,
    CC015B,
    CC015C,
    CC044A,
    CC044C,
    CD034A,
    CD037A,
    CC013C,
    CC009C,
    CC014C
  )

  implicit val messageTypePathBindable: PathBindable[MessageType] = new PathBindable.Parsing(
    parse = rootNode => values.find(_.rootNode == rootNode).get,
    serialize = _.rootNode,
    (rootNode, _) => s"$rootNode is not a valid message type. Known message types: ${values.map(_.rootNode).mkString(", ")}"
  )
}
