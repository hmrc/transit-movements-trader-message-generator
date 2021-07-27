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

import cats.syntax.all._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import play.api.libs.json.Json
import wolfendale.scalacheck.regexp.RegexpGen

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object Phase5Generators extends TemplateArgGenerators {
  val dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss")
  val dateFormatter = DateTimeFormatter.ISO_LOCAL_DATE

  def messageFieldsGen(messageType: String): ArgGen = for {
    messageSender         <- alphaNum(35)
    messageRecipient      <- alphaNum(35)
    dateTime              <- arbitrary[LocalDateTime].map(_.withYear(2021))
    messageIdentification <- alphaNum(35)
    correlationIdentifier <- alphaNum(35)
  } yield Json.obj(
    "messageSender"         -> messageSender,
    "messageRecipient"      -> messageRecipient,
    "dateTime"              -> dateTimeFormatter.format(dateTime),
    "messageIdentification" -> messageIdentification,
    "messageType"           -> messageType,
    "correlationIdentifier" -> correlationIdentifier
  )

  val cc007cGen: ArgGen = for {
    SynIdeMES1    <- Gen.const("UNOC")
    SynVerNumMES2 <- Gen.const("3")
  } yield Json.obj(
    "SynIdeMES1"    -> SynIdeMES1,
    "SynVerNumMES2" -> SynVerNumMES2
  )

  val transitOperation08FieldsGen: ArgGen = for {
    LRN                       <- alphaNum(22)
    declarationType           <- alphaNum(5)
    additionalDeclarationType <- alphaExactly(1)
    TIRCarnetNumber <- RegexpGen.from(
      "([1-9][0-9]{0,6}|(1[0-9]|2[0-4])[0-9]{0,6}|25000000|(X[A-Z]|[A-Z]X)(2[5-9]|[3-9][0-9]|[1-9][0-9][0-9])[0-9]{6})"
    )
    security                         <- num(1)
    reducedDatasetIndicator          <- indicator
    specificCircumstanceIndicator    <- alphaNum(3)
    communicationLanguageAtDeparture <- alphaExactly(2)
    bindingItinerary                 <- indicator
    date                             <- arbitrary[LocalDate].map(_.withYear(2021))
  } yield Json.obj(
    "LRN"                              -> LRN,
    "declarationType"                  -> declarationType,
    "additionalDeclarationType"        -> additionalDeclarationType,
    "TIRCarnetNumber"                  -> TIRCarnetNumber,
    "security"                         -> security,
    "reducedDatasetIndicator"          -> reducedDatasetIndicator,
    "specificCircumstanceIndicator"    -> specificCircumstanceIndicator,
    "communicationLanguageAtDeparture" -> communicationLanguageAtDeparture,
    "bindingItinerary"                 -> bindingItinerary,
    "date"                             -> dateFormatter.format(date)
  )

  val addressFieldsGen: ArgGen = for {
    streetAndNumber <- alphaNum(70)
    postcode        <- alphaNum(17)
    city            <- alphaNum(35)
    country         <- alphaExactly(2)
  } yield Json.obj(
    "streetAndNumber" -> streetAndNumber,
    "postcode"        -> postcode,
    "city"            -> city,
    "country"         -> country
  )

  val postcodeAddressFieldsGen: ArgGen = for {
    houseNumber <- alphaNum(17)
    postcode    <- alphaNum(17)
    country     <- alphaExactly(2)
  } yield Json.obj(
    "houseNumber" -> houseNumber,
    "postcode"    -> postcode,
    "country"     -> country
  )

  val contactPersonFieldsGen: ArgGen = for {
    name         <- alphaNum(70)
    phoneNumber  <- alphaNum(35)
    eMailAddress <- (alphaNum(120), alphaNum(120)).mapN(_ + "@" + _ + ".com")
  } yield Json.obj(
    "name"         -> name,
    "phoneNumber"  -> phoneNumber,
    "eMailAddress" -> eMailAddress
  )

  val guarantee03FieldsGen: ArgGen = for {
    guaranteeType           <- alphaNum(1)
    otherGuaranteeReference <- alphaNum(35)
    GRN                     <- RegexpGen.from("[0-9]{2}[A-Z]{2}[A-Z0-9]{12}[0-9]([A-Z][0-9]{6})?")
    accessCode              <- alphaNum(4)
    amountToBeCovered       <- (num1(16), num(2)).mapN(_ + "." + _)
    currency                <- alphaExactly(3)
  } yield Json.obj(
    "guaranteeType"           -> guaranteeType,
    "otherGuaranteeReference" -> otherGuaranteeReference,
    "GRN"                     -> GRN,
    "accessCode"              -> accessCode,
    "amountToBeCovered"       -> amountToBeCovered,
    "currency"                -> currency
  )

  val transportEquipment02FieldsGen: ArgGen = for {
    containerIdentificationNumber <- alphaNum(17)
    numberOfSeals                 <- num1(4)
    declarationGoodsItemNumber    <- num(5)
  } yield Json.obj(
    "containerIdentificationNumber" -> containerIdentificationNumber,
    "numberOfSeals"                 -> numberOfSeals,
    "declarationGoodsItemNumber"    -> declarationGoodsItemNumber
  )

  val locationOfGoods01FieldsGen: ArgGen = for {
    typeOfLocation            <- alphaExactly(1)
    qualifierOfIdentification <- alphaExactly(1)
    authorisationNumber       <- alphaNum(35)
    additionalIdentifier      <- alphaNum(4)
    UNLocode                  <- alphaNum(17)
    latitude                  <- RegexpGen.from("""[+-]?([0-8]?[0-9]\.[0-9]{5,7}|90.000000?0?)""")
    longitude                 <- RegexpGen.from("""[+-]?((0?[0-9]?|1[0-7])[0-9]\.[0-9]{5,7}|180.000000?0?)""")
  } yield Json.obj(
    "typeOfLocation"            -> typeOfLocation,
    "qualifierOfIdentification" -> qualifierOfIdentification,
    "authorisationNumber"       -> authorisationNumber,
    "additionalIdentifier"      -> additionalIdentifier,
    "UNLocode"                  -> UNLocode,
    "latitude"                  -> latitude,
    "longitude"                 -> longitude
  )

  val activeBorderTransportMeans01FieldsGen: ArgGen = for {
    customsOfficeAtBorderReferenceNumber <- alphaNumExactly(8)
    conveyanceReferenceNumber            <- alphaNum(17)
  } yield Json.obj(
    "customsOfficeAtBorderReferenceNumber" -> customsOfficeAtBorderReferenceNumber,
    "conveyanceReferenceNumber"            -> conveyanceReferenceNumber
  )

  val additionalInformation02FieldsGen: ArgGen = for {
    code <- alphaNumExactly(5)
    text <- alphaNum(512)
  } yield Json.obj(
    "code" -> code,
    "text" -> text
  )

  val packaging02FieldsGen: ArgGen = for {
    typeOfPackages   <- alphaNumExactly(2)
    numberOfPackages <- num(8)
    shippingMarks    <- alphaNum(512)
  } yield Json.obj(
    "typeOfPackages"   -> typeOfPackages,
    "numberOfPackages" -> numberOfPackages,
    "shippingMarks"    -> shippingMarks
  )

  val houseConsignment04FieldsGen: ArgGen = for {
    packaging02Fields              <- packaging02FieldsGen
    countryOfDispatch              <- alphaExactly(2)
    countryOfDestination           <- alphaExactly(2)
    goodsItemNumber                <- num(5)
    declarationGoodsItemNumber     <- num(5)
    declarationType                <- alphaNum(5)
    descriptionOfGoods             <- alphaNum(512)
    cusCode                        <- alphaNumExactly(9)
    harmonizedSystemSubHeadingCode <- alphaNumExactly(6)
    combinedNomenclatureCode       <- alphaNumExactly(2)
    UNNumber                       <- alphaNumExactly(4)
    measurementUnitAndQualifier    <- alphaNum(4)
    quantity                       <- (num1(10), num(6)).mapN(_ + "." + _)
  } yield packaging02Fields ++ Json.obj(
    "countryOfDispatch"              -> countryOfDispatch,
    "countryOfDestination"           -> countryOfDestination,
    "goodsItemNumber"                -> goodsItemNumber,
    "declarationGoodsItemNumber"     -> declarationGoodsItemNumber,
    "declarationType"                -> declarationType,
    "descriptionOfGoods"             -> descriptionOfGoods,
    "cusCode"                        -> cusCode,
    "harmonizedSystemSubHeadingCode" -> harmonizedSystemSubHeadingCode,
    "combinedNomenclatureCode"       -> combinedNomenclatureCode,
    "UNNumber"                       -> UNNumber,
    "measurementUnitAndQualifier"    -> measurementUnitAndQualifier,
    "quantity"                       -> quantity
  )

  val consignment14FieldsGen: ArgGen = for {
    transportEquipment02Fields         <- transportEquipment02FieldsGen
    locationOfGoods01Fields            <- locationOfGoods01FieldsGen
    activeBorderTransportMeans01Fields <- activeBorderTransportMeans01FieldsGen
    additionalInformation02Fields      <- additionalInformation02FieldsGen
    houseConsignment04Fields           <- houseConsignment04FieldsGen
    countryOfDispatch                  <- alphaExactly(2)
    countryOfDestination               <- alphaExactly(2)
  containerIndicator                 <- num(1)
    inlandModeOfTransport              <- num(1)
    modeOfTransportAtTheBorder         <- num(1)
    grossMass                          <- (num1(10), num(6)).mapN(_ + "." + _)
    netMass                            <- (num1(10), num(6)).mapN(_ + "." + _)
    supplementaryUnits                 <- (num1(10), num(6)).mapN(_ + "." + _)
    referenceNumberUCR                 <- alphaNum(35)
    role02                             <- alphaExactly(3)
    typeOfIdentification               <- alphaNumExactly(2)
    nationality                        <- alphaExactly(2)
    complementOfInformation            <- alphaNum(35)
    methodOfPayment                    <- alphaExactly(1)
    location                           <- alphaNum(35)
    documentLineItemNumber             <- num(5)
  } yield transportEquipment02Fields ++ locationOfGoods01Fields ++ activeBorderTransportMeans01Fields ++ additionalInformation02Fields ++ houseConsignment04Fields ++ Json
    .obj(
      "countryOfDispatch"          -> countryOfDispatch,
      "countryOfDestination"       -> countryOfDestination,
      "containerIndicator"         -> containerIndicator,
      "inlandModeOfTransport"      -> inlandModeOfTransport,
      "modeOfTransportAtTheBorder" -> modeOfTransportAtTheBorder,
      "grossMass"                  -> grossMass,
      "netMass"                    -> netMass,
      "supplementaryUnits"         -> supplementaryUnits,
      "referenceNumberUCR"         -> referenceNumberUCR,
      "role02"                     -> role02,
      "typeOfIdentification"       -> typeOfIdentification,
      "nationality"                -> nationality,
      "complementOfInformation"    -> complementOfInformation,
      "methodOfPayment"            -> methodOfPayment,
      "location"                   -> location,
      "documentLineItemNumber"     -> documentLineItemNumber
    )

  val cc015cGen: ArgGen = for {
    messageFields                 <- messageFieldsGen("CC015C")
    transitOperation08Fields      <- transitOperation08FieldsGen
    addressFields                 <- addressFieldsGen
    postcodeAddressFields         <- postcodeAddressFieldsGen
    contactPersonFields           <- contactPersonFieldsGen
    guarantee03Fields             <- guarantee03FieldsGen
    consignment14Fields           <- consignment14FieldsGen
    sequenceNumber                <- num(5)
    type01                        <- alphaNum(4)
    type02                        <- alphaNumExactly(4)
    referenceNumber01             <- RegexpGen.from("[A-Z]{2}[A-Z0-9]{6}")
    referenceNumber02             <- alphaNum(35)
    referenceNumber03             <- alphaNum(70)
    identificationNumber01        <- alphaNum(17)
    identificationNumber02        <- alphaNum(35)
    TIRHolderIdentificationNumber <- alphaNum(17)
    status02                      <- num(1)
    identifier                    <- alphaNum(20)
  } yield messageFields ++ transitOperation08Fields ++ addressFields ++ postcodeAddressFields ++ contactPersonFields ++ guarantee03Fields ++ consignment14Fields ++ Json
    .obj(
      "sequenceNumber"                -> sequenceNumber,
      "type01"                        -> type01,
      "type02"                        -> type02,
      "referenceNumber01"             -> referenceNumber01,
      "referenceNumber02"             -> referenceNumber02,
      "referenceNumber03"             -> referenceNumber03,
      "identificationNumber01"        -> identificationNumber01,
      "identificationNumber02"        -> identificationNumber02,
      "TIRHolderIdentificationNumber" -> TIRHolderIdentificationNumber,
      "status02"                      -> status02,
      "identifier"                    -> identifier
    )

  val cc044cGen: ArgGen = for {
    SynIdeMES1    <- Gen.const("UNOC")
    SynVerNumMES2 <- Gen.const("3")
  } yield Json.obj(
    "SynIdeMES1"    -> SynIdeMES1,
    "SynVerNumMES2" -> SynVerNumMES2
  )

}
