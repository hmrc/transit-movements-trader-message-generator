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

package models.error

import models.MessageType
import play.api.http.Status.INTERNAL_SERVER_ERROR
import play.api.http.Status.NOT_ACCEPTABLE

case class ConversionError (statusCode: Int, message: String)

object ConversionError {
  def unexpected(message: String) =
    ConversionError(INTERNAL_SERVER_ERROR, s"An unexpected error occurred: $message")

  def noConversionExists(messageType: MessageType) =
    ConversionError(NOT_ACCEPTABLE, s"The message type ${messageType.rootNode} cannot be converted to Json")

}
