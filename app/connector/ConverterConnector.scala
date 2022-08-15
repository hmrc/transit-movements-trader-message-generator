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

package connector

import akka.stream.Materializer
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Source
import akka.util.ByteString
import cats.data.EitherT
import com.google.inject.Inject
import config.AppConfig
import models.MessageType
import models.error.ConversionError
import play.api.Logging
import play.api.http.HeaderNames
import play.api.http.MimeTypes
import play.api.http.Status.OK
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.http.StringContextOps
import uk.gov.hmrc.http.client.HttpClientV2

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class ConverterConnector @Inject()(appConfig: AppConfig, httpClient: HttpClientV2)(implicit ec: ExecutionContext, materializer: Materializer) extends Logging {

  private def converterUrl(messageType: String) =
    s"${appConfig.converterUrl}/transit-movements-converter/messages/$messageType"

  def convert(messageType: MessageType, input: String): EitherT[Future, ConversionError, Source[ByteString, _]] = {
    EitherT(
      Future.successful(
        messageType.converterType.toRight(ConversionError.noConversionExists(messageType))
      )
    )
    .flatMap {
      messageType =>
        EitherT(
          httpClient
            .post(url"${converterUrl(messageType)}")(HeaderCarrier())
            .setHeader(HeaderNames.CONTENT_TYPE -> MimeTypes.XML)
            .setHeader(HeaderNames.ACCEPT -> MimeTypes.JSON)
            .withBody(input)
            .stream[HttpResponse]
            .flatMap {
              response =>
                response.status match {
                  case OK => Future.successful(Right(response.bodyAsSource))
                  case _ =>
                    response
                      .bodyAsSource
                      .reduce(_ ++ _)
                      .map(_.utf8String)
                      .runWith(Sink.head)
                      .map {
                        result =>
                          logger.error(s"Failed to convert: $result, status code ${response.status}")
                          Left(ConversionError.unexpected("Internal service error"))
                      }
                }
            }
        )
    }
  }

}
