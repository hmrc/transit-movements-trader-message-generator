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

package controllers

import connector.ConverterConnector
import models.MessageType
import play.api.http.HeaderNames
import play.api.http.MimeTypes
import play.api.mvc.ControllerComponents
import uk.gov.hmrc.nunjucks.NunjucksRenderer
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class MessageController @Inject() (
  renderer: NunjucksRenderer,
  converterConnector: ConverterConnector,
  cc: ControllerComponents
)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  def generateMessage(`type`: MessageType) = Action.async { implicit request =>
    val templateArgs = `type`.templateArgGen.sample

    templateArgs
      .map { args =>
        renderer.render(`type`.templateFile, args)
      }
      .map { result =>
        request.headers.get(HeaderNames.ACCEPT) match {
          case Some(MimeTypes.JSON) =>
            result
              .flatMap(x => converterConnector.convert(`type`, x.body).value)
              .map {
                  case Right(source) => Ok.chunked(source)
                  case Left(err)     => Status(err.statusCode)(err.message)
              }
          case _ => result.map(Ok(_))
        }
      }
      .getOrElse {
        Future.successful(InternalServerError("Unable to generate template arguments"))
      }
  }
}
