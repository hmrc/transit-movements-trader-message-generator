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

import cats.syntax.all._
import models.Phase5Generators.ArgGen
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import play.api.data.Forms.date
import play.api.libs.json.Json
import wolfendale.scalacheck.regexp.RegexpGen

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object Phase5Generators extends TemplateArgGenerators {
  val dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss")
  val dateFormatter     = DateTimeFormatter.ISO_LOCAL_DATE

  def messageFieldsGen(messageType: String): ArgGen = for {
    messageRecipient      <- alphaNum(35)
    dateTime              <- arbitrary[LocalDateTime].map(_.withYear(2022))
    messageIdentification <- alphaNum(35)
    correlationIdentifier <- alphaNum(35)
  } yield Json.obj(
    "messageRecipient"       -> messageRecipient,
    "preparationDateAndTime" -> dateTimeFormatter.format(dateTime),
    "messageIdentification"  -> messageIdentification,
    "messageType"            -> messageType,
    "correlationIdentifier"  -> correlationIdentifier
  )

  val cc007cGen: ArgGen = for {
    synIdeMES1    <- Gen.const("UNOC")
    synVerNumMES2 <- Gen.const("3")
  } yield Json.obj(
    "SynIdeMES1"    -> synIdeMES1,
    "SynVerNumMES2" -> synVerNumMES2
  )

  val TransitOperationType06: ArgGen = for {
    lrn                       <- alphaNum(22)
    declarationType           <- alphaNum(5)
    additionalDeclarationType <- alphaExactly(1)
    tirCarnetNumber <- RegexpGen.from(
      "([1-9][0-9]{0,6}|(1[0-9]|2[0-4])[0-9]{0,6}|25000000|(X[A-Z]|[A-Z]X)(2[5-9]|[3-9][0-9]|[1-9][0-9][0-9])[0-9]{6})"
    )
    presentationOfTheGoodsDateAndTimeContentType <- arbitrary[LocalDateTime].map(_.withYear(2022))
    security                                     <- num(1)
    reducedDatasetIndicator                      <- indicator
    specificCircumstanceIndicator                <- alphaNum(3)
    communicationLanguageAtDeparture             <- alphaExactly(2)
    bindingItinerary                             <- indicator
    limitDate                                    <- arbitrary[LocalDate].map(_.withYear(2022))
  } yield Json.obj(
    "LRN"                       -> lrn,
    "declarationType"           -> declarationType,
    "additionalDeclarationType" -> additionalDeclarationType,
    "TIRCarnetNumber"           -> tirCarnetNumber,
    "presentationOfTheGoodsDateAndTime" -> dateTimeFormatter
      .format(presentationOfTheGoodsDateAndTimeContentType),
    "security"                         -> security,
    "reducedDatasetIndicator"          -> reducedDatasetIndicator,
    "specificCircumstanceIndicator"    -> specificCircumstanceIndicator,
    "communicationLanguageAtDeparture" -> communicationLanguageAtDeparture,
    "bindingItinerary"                 -> bindingItinerary,
    "limitDate"                        -> dateFormatter.format(limitDate)
  )

  val transitOperationType04: ArgGen = for {
    lrn <- alphaNum(22)
    mrn <- RegexpGen.from(
      "([2][4-9]|[3-9][0-9])[A-Z]{2}[A-Z0-9]{12}[J-M][0-9]"
    ) //used new MRN structure from the xsd (max and min length is 18)
    declarationType           <- alphaNum(5)
    additionalDeclarationType <- alphaExactly(1)
    tirCarnetNumber <- RegexpGen.from(
      "([1-9][0-9]{0,6}|(1[0-9]|2[0-4])[0-9]{0,6}|25000000|(X[A-Z]|[A-Z]X)(2[5-9]|[3-9][0-9]|[1-9][0-9][0-9])[0-9]{6})"
    )
    presentationOfTheGoodsDateAndTimeContentType <- arbitrary[LocalDate].map(_.withYear(2022))
    security                                     <- num(1)
    reducedDatasetIndicator                      <- indicator
    specificCircumstanceIndicator                <- alphaNum(3)
    communicationLanguageAtDeparture             <- alphaExactly(2)
    bindingItinerary                             <- indicator
    amendmentTypeFlag                            <- indicator
    limitDateContentType                         <- arbitrary[LocalDate].map(_.withYear(2022))
  } yield Json.obj(
    "LRN"                       -> lrn,
    "MRN"                       -> mrn,
    "declarationType"           -> declarationType,
    "additionalDeclarationType" -> additionalDeclarationType,
    "TIRCarnetNumber"           -> tirCarnetNumber,
    "presentationOfTheGoodsDateAndTime" -> dateFormatter
      .format(presentationOfTheGoodsDateAndTimeContentType),
    "security"                         -> security,
    "reducedDatasetIndicator"          -> reducedDatasetIndicator,
    "specificCircumstanceIndicator"    -> specificCircumstanceIndicator,
    "communicationLanguageAtDeparture" -> communicationLanguageAtDeparture,
    "bindingItinerary"                 -> bindingItinerary,
    "amendmentTypeFlag"                -> amendmentTypeFlag,
    "limitDate"                        -> dateFormatter.format(limitDateContentType)
  )

  val authorisationType03: ArgGen = for {
    sequenceNumber               <- num(5)
    typeContentType04            <- alphaNum(4)
    referenceNumberContentType03 <- alphaNum(35)
  } yield Json.obj(
    "sequenceNumber"               -> sequenceNumber,
    "type"                         -> typeContentType04,
    "referenceNumberAuthorisation" -> referenceNumberContentType03
  )

  val CustomsOfficeOfDepartureType03: ArgGen = for {
    referenceNumberContentType05 <- RegexpGen.from("""[A-Z]{2}[A-Z0-9]{6}""")
  } yield Json.obj(
    "referenceNumberDeparture" -> referenceNumberContentType05
  )

  val CustomsOfficeOfDestinationDeclaredType01 = CustomsOfficeOfDepartureType03

  val CustomsOfficeOfTransitDeclaredType03: ArgGen = for {
    sequenceNumber                         <- num(5)
    referenceNumberContentType05           <- RegexpGen.from("[A-Z]{2}[A-Z0-9]{6}")
    arrivalDateAndTimeEstimatedContentType <- arbitrary[LocalDateTime].map(_.withYear(2022))
  } yield Json.obj(
    "sequenceNumber"         -> sequenceNumber,
    "referenceNumberTransit" -> referenceNumberContentType05,
    "arrivalDateAndTimeEstimated" -> dateTimeFormatter
      .format(arrivalDateAndTimeEstimatedContentType)
  )

  val CustomsOfficeOfExitForTransitDeclaredType02: ArgGen = for {
    sequenceNumber               <- num(5)
    referenceNumberContentType05 <- RegexpGen.from("[A-Z]{2}[A-Z0-9]{6}")
  } yield Json.obj(
    "sequenceNumber"         -> sequenceNumber,
    "referenceNumberTransit" -> referenceNumberContentType05
  )

  val contactPersonFieldsGen: ArgGen = for {
    nameContentType02         <- alphaNum(70)
    phoneNumberContentType02  <- alphaNum(35)
    eMailAddressContentType02 <- (alphaNum(120), alphaNum(120)).mapN(_ + "@" + _ + ".com")
  } yield Json.obj(
    "name"         -> nameContentType02,
    "phoneNumber"  -> phoneNumberContentType02,
    "eMailAddress" -> eMailAddressContentType02
  )

  val RepresentativeType05: ArgGen = for {
    identificationNumberContentType01 <- alphaNum(17)
    statusContentType03               <- num(1)
    contactPersonType05               <- contactPersonFieldsGen
  } yield contactPersonType05 ++ Json.obj(
    "identificationNumberRepresentative01" -> identificationNumberContentType01,
    "status"                               -> statusContentType03
  )

  val addressFieldsGen: ArgGen = for {
    streetAndNumberContentType02 <- alphaNum(70)
    postcodeContentType02        <- alphaNum(17)
    cityContentType03            <- alphaNum(35)
    countryContentType           <- alphaExactly(2)
  } yield Json.obj(
    "streetAndNumber" -> streetAndNumberContentType02,
    "postcode"        -> postcodeContentType02,
    "city"            -> cityContentType03,
    "country"         -> countryContentType
  )

  val HolderOfTheTransitProcedureType14: ArgGen = for {
    identificationNumberContentType01        <- alphaNum(17)
    tirHolderIdentificationNumberContentType <- alphaNum(17)
    nameContentType02                        <- alphaNum(70)
    addressType17                            <- addressFieldsGen
    contactPersonType05                      <- contactPersonFieldsGen
  } yield addressType17 ++ contactPersonType05 ++ Json.obj(
    "identificationNumberTransitProcedure01" -> identificationNumberContentType01,
    "TIRHolderIdentificationNumber"          -> tirHolderIdentificationNumberContentType,
    "name"                                   -> nameContentType02
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

  val GuaranteeReferenceType03: ArgGen = for {
    sequenceNumberContentType    <- num(5)
    grnContentType               <- RegexpGen.from("[0-9]{2}[A-Z]{2}[A-Z0-9]{12}[0-9]([A-Z][0-9]{6})?")
    accessCodeContentType02      <- alphaNum(4)
    amountToBeCoveredContentType <- decimalNumber(16, 2)
    currencyContentType          <- alphaExactly(3)
  } yield Json.obj(
    "sequenceNumber"    -> sequenceNumberContentType,
    "GRN"               -> grnContentType,
    "accessCode"        -> accessCodeContentType02,
    "amountToBeCovered" -> amountToBeCoveredContentType,
    "currency"          -> currencyContentType
  )

  val GuaranteeType01: ArgGen = for {
    sequenceNumberContentType            <- num(5)
    guaranteeTypeContentType             <- alphaNum(1)
    otherGuaranteeReferenceContentType02 <- alphaNum(35)
    guaranteeReferenceType03             <- GuaranteeReferenceType03
  } yield guaranteeReferenceType03 ++ Json.obj(
    "sequenceNumber"          -> sequenceNumberContentType,
    "guaranteeType"           -> guaranteeTypeContentType,
    "otherGuaranteeReference" -> otherGuaranteeReferenceContentType02
  )

  val GuaranteeType02: ArgGen = for {
    sequenceNumberContentType            <- num(5)
    guaranteeTypeContentType02           <- alphaNumCapital(1)
    otherGuaranteeReferenceContentType02 <- alphaNum(35)
    guaranteeReferenceType03             <- GuaranteeReferenceType03
  } yield guaranteeReferenceType03 ++ Json.obj(
    "sequenceNumber"          -> sequenceNumberContentType,
    "guaranteeType02"         -> guaranteeTypeContentType02,
    "otherGuaranteeReference" -> otherGuaranteeReferenceContentType02
  )

  val guarantee03FieldsGen: ArgGen = for {
    sequenceNumberContentType <- num(5)
    guaranteeType             <- alphaNum(1)
    otherGuaranteeReference   <- alphaNum(35)
    grn                       <- RegexpGen.from("[0-9]{2}[A-Z]{2}[A-Z0-9]{12}[0-9]([A-Z][0-9]{6})?")
    accessCode                <- alphaNum(4)
    amountToBeCovered         <- (num1(16), num(2)).mapN(_ + "." + _)
    currency                  <- alphaExactly(3)
  } yield Json.obj(
    "sequenceNumber"          -> sequenceNumberContentType,
    "guaranteeType"           -> guaranteeType,
    "otherGuaranteeReference" -> otherGuaranteeReference,
    "GRN"                     -> grn,
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

  val CustomsOfficeType02: ArgGen = for {
    referenceNumberContentType05 <- RegexpGen.from("[A-Z]{2}[A-Z0-9]{6}")
  } yield Json.obj(
    "referenceNumberCustoms" -> referenceNumberContentType05
  )

  val GNSSType: ArgGen = for {
    latitudeContentType <- RegexpGen.from("""[+-]?([0-8]?[0-9]\.[0-9]{5,7}|90.000000?0?)""")
    longitudeContentType <- RegexpGen.from(
      """[+-]?((0?[0-9]?|1[0-7])[0-9]\.[0-9]{5,7}|180.000000?0?)"""
    )
  } yield Json.obj(
    "latitude"  -> latitudeContentType,
    "longitude" -> longitudeContentType
  )

  val EconomicOperatorType03: ArgGen = for {
    identificationNumberContentType01 <- alphaNum(17)
  } yield Json.obj(
    "identificationNumber01" -> identificationNumberContentType01
  )

  val AddressType14 = addressFieldsGen

  val PostcodeAddressType02: ArgGen = for {
    houseNumberContentType02 <- alphaNum(17)
    postcodeContentType02    <- alphaNum(17)
    countryContentType       <- alphaExactly(2)
  } yield Json.obj(
    "houseNumber" -> houseNumberContentType02,
    "postcode"    -> postcodeContentType02,
    "country"     -> countryContentType
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
    unnumber                       <- alphaNumExactly(4)
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
    "UNNumber"                       -> unnumber,
    "measurementUnitAndQualifier"    -> measurementUnitAndQualifier,
    "quantity"                       -> quantity
  )

  val CarrierType04: ArgGen = for {
    identificationNumberContentType02 <- alphaNum(17)
    contactPersonType05               <- contactPersonFieldsGen

  } yield contactPersonType05 ++ Json.obj(
    "identificationNumberCarrier" -> identificationNumberContentType02
  )

  val ConsignorType07: ArgGen = for {
    identificationNumberContentType01 <- alphaNum(17)
    nameContentType02                 <- alphaNum(70)
    addressType17                     <- addressFieldsGen
    contactPersonType05               <- contactPersonFieldsGen
  } yield addressType17 ++ contactPersonType05 ++ Json.obj(
    "identificationNumberConsignor" -> identificationNumberContentType01,
    "name"                          -> nameContentType02
  )

  val ConsigneeType05: ArgGen = for {
    identificationNumberContentType01 <- alphaNum(17)
    nameContentType02                 <- alphaNum(70)
    addressType17                     <- addressFieldsGen
  } yield addressType17 ++ Json.obj(
    "identificationNumberConsignee" -> identificationNumberContentType01,
    "name"                          -> nameContentType02
  )

  val AdditionalSupplyChainActorType: ArgGen = for {
    sequenceNumberContentType         <- num(5)
    roleContentType01                 <- alphaExactly(3)
    identificationNumberContentType02 <- alphaNum(17)
  } yield Json.obj(
    "sequenceNumber"                       -> sequenceNumberContentType,
    "role"                                 -> roleContentType01,
    "identificationNumberAdditionalSupply" -> identificationNumberContentType02
  )

  val SealType05: ArgGen = for {
    sequenceNumberContentType <- num(5)
    identifierContentType02   <- alphaNum(20)
  } yield Json.obj(
    "sequenceNumber" -> sequenceNumberContentType,
    "identifier"     -> identifierContentType02
  )

  val GoodsReferenceType02: ArgGen = for {
    sequenceNumberContentType               <- num(5)
    declarationGoodsItemNumberContentType01 <- num(5)
  } yield Json.obj(
    "sequenceNumber"             -> sequenceNumberContentType,
    "declarationGoodsItemNumber" -> declarationGoodsItemNumberContentType01
  )

  val TransportEquipmentType06: ArgGen = for {
    sequenceNumberContentType                <- num(5)
    containerIdentificationNumberContentType <- alphaNum(17)
    numberOfSealsContentType                 <- num(4)
    sealType05                               <- SealType05
    goodsReferenceType02                     <- GoodsReferenceType02
  } yield sealType05 ++ goodsReferenceType02 ++ Json.obj(
    "sequenceNumber"                -> sequenceNumberContentType,
    "containerIdentificationNumber" -> containerIdentificationNumberContentType,
    "numberOfSeals"                 -> numberOfSealsContentType
  )

  val ContactPersonType06 = contactPersonFieldsGen

  val LocationOfGoodsType05: ArgGen = for {
    typeOfLocationContentType            <- alphaExactly(1)
    qualifierOfIdentificationContentType <- alphaExactly(1)
    authorisationNumberContentType02     <- alphaNum(35)
    additionalIdentifierContentType02    <- alphaNum(4)
    unlocodeContentType                  <- alphaNum(17)
    customsOfficeType02                  <- CustomsOfficeType02
    gnssType                             <- GNSSType
    economicOperatorType03               <- EconomicOperatorType03
    addressType14                        <- AddressType14
    postcodeAddressType02                <- PostcodeAddressType02
    contactPersonType06                  <- ContactPersonType06

  } yield customsOfficeType02 ++ gnssType ++ economicOperatorType03 ++ addressType14 ++ postcodeAddressType02 ++ contactPersonType06 ++ Json
    .obj(
      "typeOfLocation"            -> typeOfLocationContentType,
      "qualifierOfIdentification" -> qualifierOfIdentificationContentType,
      "authorisationNumber"       -> authorisationNumberContentType02,
      "additionalIdentifier"      -> additionalIdentifierContentType02,
      "UNLocode"                  -> unlocodeContentType
    )

  val DepartureTransportMeansType03: ArgGen = for {
    sequenceNumberContentType         <- num(5)
    typeOfIdentificationContentType   <- num(2)
    identificationNumberContentType06 <- alphaNum(35)
    nationalityContentType            <- alphaExactly(2)
  } yield Json.obj(
    "sequenceNumber"                           -> sequenceNumberContentType,
    "typeOfIdentification"                     -> typeOfIdentificationContentType,
    "identificationNumberDepartureTransport03" -> identificationNumberContentType06,
    "nationality"                              -> nationalityContentType
  )

  val CountryOfRoutingOfConsignmentType01: ArgGen = for {
    sequenceNumberContentType <- num(5)
    countryContentType        <- alphaExactly(2)
  } yield Json.obj(
    "sequenceNumber" -> sequenceNumberContentType,
    "country"        -> countryContentType
  )

  val ActiveBorderTransportMeansType02: ArgGen = for {
    sequenceNumberContentType                       <- num(5)
    customsOfficeAtBorderReferenceNumberContentType <- alphaNum(8)
    typeOfIdentificationContentType                 <- num(2)
    identificationNumberContentType06               <- alphaNum(35)
    nationalityContentType                          <- alphaExactly(2)
    conveyanceReferenceNumberContentType02          <- alphaNum(17)
  } yield Json.obj(
    "sequenceNumber"                       -> sequenceNumberContentType,
    "customsOfficeAtBorderReferenceNumber" -> customsOfficeAtBorderReferenceNumberContentType,
    "typeOfIdentification"                 -> typeOfIdentificationContentType,
    "identificationNumberActiveBorder"     -> identificationNumberContentType06,
    "nationality"                          -> nationalityContentType,
    "conveyanceReferenceNumber"            -> conveyanceReferenceNumberContentType02
  )

  val PlaceOfLoadingType03: ArgGen = for {
    unlocodeContentType   <- alphaNum(17)
    countryContentType    <- alphaExactly(2)
    locationContentType01 <- alphaNum(35)
  } yield Json.obj(
    "UNLocode" -> unlocodeContentType,
    "country"  -> countryContentType,
    "location" -> locationContentType01
  )

  val PlaceOfUnloadingType01 = PlaceOfLoadingType03

  val PreviousDocumentType09: ArgGen = for {
    sequenceNumberContentType            <- num(5)
    typeContentType02                    <- alphaNum(4)
    referenceNumberContentType04         <- alphaNum(70)
    complementOfInformationContentType02 <- alphaNum(35)
  } yield Json.obj(
    "sequenceNumber"                  -> sequenceNumberContentType,
    "type"                            -> typeContentType02,
    "referenceNumberPreviousDocument" -> referenceNumberContentType04,
    "complementOfInformation"         -> complementOfInformationContentType02
  )

  val SupportingDocumentType05: ArgGen = for {
    sequenceNumberContentType            <- num(5)
    typeContentType02                    <- alphaNum(4)
    referenceNumberContentType04         <- alphaNum(70)
    documentLineItemNumberContentType    <- num(5)
    complementOfInformationContentType02 <- alphaNum(35)
  } yield Json.obj(
    "sequenceNumber"                    -> sequenceNumberContentType,
    "type"                              -> typeContentType02,
    "referenceNumberSupportingDocument" -> referenceNumberContentType04,
    "documentLineItemNumber"            -> documentLineItemNumberContentType,
    "complementOfInformation"           -> complementOfInformationContentType02
  )

  val TransportDocumentType04: ArgGen = for {
    sequenceNumberContentType    <- num(5)
    typeContentType02            <- alphaNum(4)
    referenceNumberContentType04 <- alphaNum(70)
  } yield Json.obj(
    "sequenceNumber"                   -> sequenceNumberContentType,
    "type"                             -> typeContentType02,
    "referenceNumberTransportDocument" -> referenceNumberContentType04
  )

  val AdditionalReferenceType06 = TransportDocumentType04

  val AdditionalInformationType03: ArgGen = for {
    sequenceNumberContentType <- num(5)
    codeContentType03         <- alphaNum(5)
    textContentType02         <- alphaNum(512)
  } yield Json.obj(
    "sequenceNumber" -> sequenceNumberContentType,
    "code"           -> codeContentType03,
    "text"           -> textContentType02
  )

  val TransportChargesType: ArgGen = for {
    methodOfPaymentContentType <- alphaExactly(1)
  } yield Json.obj(
    "methodOfPayment" -> methodOfPaymentContentType
  )

  val DepartureTransportMeansType05: ArgGen = for {
    sequenceNumberContentType         <- num(5)
    typeOfIdentificationContentType   <- num(2)
    identificationNumberContentType06 <- alphaNum(35)
    nationalityContentType            <- alphaExactly(2)
  } yield Json.obj(
    "sequenceNumber"                           -> sequenceNumberContentType,
    "typeOfIdentification"                     -> typeOfIdentificationContentType,
    "identificationNumberDepartureTransport05" -> identificationNumberContentType06,
    "nationality"                              -> nationalityContentType
  )

  val PreviousDocumentType10 = PreviousDocumentType09

  val CommodityCodeType02: ArgGen = for {
    harmonizedSystemSubHeadingCodeContentType <- alphaNum(6)
    combinedNomenclatureCodeContentType02     <- alphaNum(2)
  } yield Json.obj(
    "harmonizedSystemSubHeadingCode" -> harmonizedSystemSubHeadingCodeContentType,
    "combinedNomenclatureCode"       -> combinedNomenclatureCodeContentType02
  )

  val DangerousGoodsType01: ArgGen = for {
    sequenceNumberContentType <- num(5)
    unnumberContentType       <- alphaNum(4)
  } yield Json.obj(
    "sequenceNumber" -> sequenceNumberContentType,
    "UNNumber"       -> unnumberContentType
  )

  val GoodsMeasureType02: ArgGen = for {
    grossMassContentType02        <- decimalNumber(16, 6)
    netMassContentType            <- decimalNumber(16, 6)
    supplementaryUnitsContentType <- decimalNumber(16, 6)
  } yield Json.obj(
    "grossMass"          -> grossMassContentType02,
    "netMass"            -> netMassContentType,
    "supplementaryUnits" -> supplementaryUnitsContentType
  )

  val CommodityType06: ArgGen = for {
    descriptionOfGoodsContentType02 <- alphaNum(512)
    cusCodeContentType              <- alphaNum(9)
    commodityCodeType02             <- CommodityCodeType02
    dangerousGoodsType01            <- DangerousGoodsType01
    goodsMeasureType02              <- GoodsMeasureType02
  } yield commodityCodeType02 ++ dangerousGoodsType01 ++ goodsMeasureType02 ++ Json.obj(
    "descriptionOfGoodsCommodity" -> descriptionOfGoodsContentType02,
    "cusCode"                     -> cusCodeContentType
  )

  val PackagingType03: ArgGen = for {
    sequenceNumberContentType     <- num(5)
    typeOfPackagesContentType     <- alphaNum(2, 2)
    numberOfPackagesContentType01 <- num(8)
    shippingMarksContentType02    <- alphaNum(512)
  } yield Json.obj(
    "sequenceNumber"            -> sequenceNumberContentType,
    "typeOfPackagesConsignment" -> typeOfPackagesContentType,
    "numberOfPackages"          -> numberOfPackagesContentType01,
    "shippingMarks"             -> shippingMarksContentType02
  )

  val PreviousDocumentType08: ArgGen = for {
    sequenceNumberContentType              <- num(5)
    typeContentType02                      <- alphaNum(4)
    referenceNumberContentType04           <- alphaNum(70)
    goodsItemNumberContentType01           <- num(5)
    typeOfPackagesContentType              <- alphaNum(2)
    numberOfPackagesContentType02          <- num(8)
    measurementUnitAndQualifierContentType <- alphaNum(4)
    quantityContentType                    <- decimalNumber(16, 6)
    complementOfInformationContentType02   <- alphaNum(35)

  } yield Json.obj(
    "sequenceNumber"                -> sequenceNumberContentType,
    "type"                          -> typeContentType02,
    "referenceNumberPreviousType08" -> referenceNumberContentType04,
    "goodsItemNumber"               -> goodsItemNumberContentType01,
    "typeOfPackages"                -> typeOfPackagesContentType,
    "numberOfPackages"              -> numberOfPackagesContentType02,
    "measurementUnitAndQualifier"   -> measurementUnitAndQualifierContentType,
    "quantity"                      -> quantityContentType,
    "complementOfInformation"       -> complementOfInformationContentType02
  )

  val ConsigneeType02 = ConsigneeType05

  val AdditionalReferenceType05 = TransportDocumentType04

  val ConsignmentItemType09: ArgGen = for {
    goodsItemNumberContentType02            <- num(5)
    declarationGoodsItemNumberContentType01 <- RegexpGen.from("[1-9][0-9]{0,2}|[1][0-9]{3}")
    declarationTypeContentType02            <- alphaNum(5)
    countryOfDispatchContentType            <- alphaExactly(2)
    countryOfDestinationContentType         <- alphaExactly(2)
    referenceNumberUCRContentType02         <- alphaNum(35)
    consigneeType02                         <- ConsigneeType02
    additionalSupplyChainActorType          <- AdditionalSupplyChainActorType
    commodityType06                         <- CommodityType06
    packagingType03                         <- PackagingType03
    previousDocumentType08                  <- PreviousDocumentType08
    supportingDocumentType05                <- SupportingDocumentType05
    transportDocumentType04                 <- TransportDocumentType04
    additionalReferenceType05               <- AdditionalReferenceType05
    additionalInformationType03             <- AdditionalInformationType03
    transportChargesType                    <- TransportChargesType
  } yield consigneeType02 ++ additionalSupplyChainActorType ++
    commodityType06 ++ packagingType03 ++ previousDocumentType08 ++
    supportingDocumentType05 ++ transportDocumentType04 ++
    additionalReferenceType05 ++ additionalInformationType03 ++
    transportChargesType ++ Json.obj(
      "goodsItemNumber"            -> goodsItemNumberContentType02,
      "declarationGoodsItemNumber" -> declarationGoodsItemNumberContentType01,
      "declarationType"            -> declarationTypeContentType02,
      "countryOfDispatch"          -> countryOfDispatchContentType,
      "countryOfDestination"       -> countryOfDestinationContentType,
      "referenceNumberUCR"         -> referenceNumberUCRContentType02
    )

  val HouseConsignmentType10: ArgGen = for {
    sequenceNumberContentType       <- num(5)
    countryOfDispatchContentType    <- alphaExactly(2)
    grossMassContentType01          <- decimalNumber(16, 6)
    referenceNumberUCRContentType02 <- alphaNum(35)
    consignorType07                 <- ConsignorType07
    consigneeType05                 <- ConsigneeType05
    additionalSupplyChainActorType  <- AdditionalSupplyChainActorType
    departureTransportMeansType05   <- DepartureTransportMeansType05
    previousDocumentType10          <- PreviousDocumentType10
    supportingDocumentType05        <- SupportingDocumentType05
    transportDocumentType04         <- TransportDocumentType04
    additionalReferenceType06       <- AdditionalReferenceType06
    additionalInformationType03     <- AdditionalInformationType03
    transportChargesType            <- TransportChargesType
    consignmentItemType09           <- ConsignmentItemType09

  } yield consignorType07 ++ consigneeType05 ++ additionalSupplyChainActorType ++
    departureTransportMeansType05 ++ previousDocumentType10 ++ supportingDocumentType05 ++
    transportDocumentType04 ++ additionalReferenceType06 ++ additionalInformationType03 ++
    transportChargesType ++ consignmentItemType09 ++ Json.obj(
      "sequenceNumber"     -> sequenceNumberContentType,
      "countryOfDispatch"  -> countryOfDispatchContentType,
      "grossMass"          -> grossMassContentType01,
      "referenceNumberUCR" -> referenceNumberUCRContentType02
    )

  val ConsignmentType20: ArgGen = for {
    countryOfDispatchContentType          <- alphaExactly(2)
    countryOfDestinationContentType       <- alphaExactly(2)
    containerIndicator                    <- indicator
    inlandModeOfTransportContentType      <- num(1)
    modeOfTransportAtTheBorderContentType <- num(1)
    grossMassContentType01                <- decimalNumber(16, 6)
    referenceNumberUCRContentType02       <- alphaNum(35)
    carrierType04                         <- CarrierType04
    consignorType07                       <- ConsignorType07
    consigneeType05                       <- ConsigneeType05
    additionalSupplyChainActorType        <- AdditionalSupplyChainActorType
    transportEquipmentType06              <- TransportEquipmentType06
    locationOfGoodsType05                 <- LocationOfGoodsType05
    departureTransportMeansType03         <- DepartureTransportMeansType03
    countryOfRoutingOfConsignmentType01   <- CountryOfRoutingOfConsignmentType01
    activeBorderTransportMeansType02      <- ActiveBorderTransportMeansType02
    placeOfLoadingType03                  <- PlaceOfLoadingType03
    placeOfUnloadingType01                <- PlaceOfUnloadingType01
    previousDocumentType09                <- PreviousDocumentType09
    supportingDocumentType05              <- SupportingDocumentType05
    transportDocumentType04               <- TransportDocumentType04
    additionalReferenceType06             <- AdditionalReferenceType06
    additionalInformationType03           <- AdditionalInformationType03
    transportChargesType                  <- TransportChargesType
    houseConsignmentType10                <- HouseConsignmentType10

  } yield carrierType04 ++ consignorType07 ++ consigneeType05 ++ additionalSupplyChainActorType ++ transportEquipmentType06 ++ locationOfGoodsType05 ++ departureTransportMeansType03 ++ countryOfRoutingOfConsignmentType01 ++ activeBorderTransportMeansType02 ++ placeOfLoadingType03 ++ placeOfUnloadingType01 ++ previousDocumentType09 ++ supportingDocumentType05 ++ transportDocumentType04 ++ additionalReferenceType06 ++ additionalInformationType03 ++ transportChargesType ++ houseConsignmentType10 ++ Json
    .obj(
      "countryOfDispatch"          -> countryOfDispatchContentType,
      "countryOfDestination"       -> countryOfDestinationContentType,
      "containerIndicator"         -> containerIndicator,
      "inlandModeOfTransport"      -> inlandModeOfTransportContentType,
      "modeOfTransportAtTheBorder" -> modeOfTransportAtTheBorderContentType,
      "grossMass"                  -> grossMassContentType01,
      "referenceNumberUCR"         -> referenceNumberUCRContentType02
    )

  val locationOfGoods01FieldsGen: ArgGen = for {
    typeOfLocation            <- alphaExactly(1)
    qualifierOfIdentification <- alphaExactly(1)
    authorisationNumber       <- alphaNum(35)
    additionalIdentifier      <- alphaNum(4)
    unlocode                  <- alphaNum(17)
    latitude                  <- RegexpGen.from("""[+-]?([0-8]?[0-9]\.[0-9]{5,7}|90.000000?0?)""")
    longitude                 <- RegexpGen.from("""[+-]?((0?[0-9]?|1[0-7])[0-9]\.[0-9]{5,7}|180.000000?0?)""")
  } yield Json.obj(
    "typeOfLocation"            -> typeOfLocation,
    "qualifierOfIdentification" -> qualifierOfIdentification,
    "authorisationNumber"       -> authorisationNumber,
    "additionalIdentifier"      -> additionalIdentifier,
    "UNLocode"                  -> unlocode,
    "latitude"                  -> latitude,
    "longitude"                 -> longitude
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
    messageFields                               <- messageFieldsGen("CC015C")
    transitOperation06Fields                    <- TransitOperationType06
    authorisationType03                         <- authorisationType03
    customsOfficeOfDepartureType03              <- CustomsOfficeOfDepartureType03
    customsOfficeOfDestinationDeclaredType01    <- CustomsOfficeOfDestinationDeclaredType01
    customsOfficeOfTransitDeclaredType03        <- CustomsOfficeOfTransitDeclaredType03
    customsOfficeOfExitForTransitDeclaredType02 <- CustomsOfficeOfExitForTransitDeclaredType02
    holderOfTheTransitProcedureType14           <- HolderOfTheTransitProcedureType14
    representativeType05                        <- RepresentativeType05
    guaranteeType02                             <- GuaranteeType02
    consignment                                 <- ConsignmentType20
  } yield messageFields ++ transitOperation06Fields ++ authorisationType03 ++ customsOfficeOfDepartureType03 ++ customsOfficeOfDestinationDeclaredType01 ++
    customsOfficeOfTransitDeclaredType03 ++ customsOfficeOfExitForTransitDeclaredType02 ++ holderOfTheTransitProcedureType14 ++ representativeType05 ++
    guaranteeType02 ++ consignment

  val cc044cGen: ArgGen = for {
    synIdeMES1    <- Gen.const("UNOC")
    synVerNumMES2 <- Gen.const("3")
  } yield Json.obj(
    "SynIdeMES1"    -> synIdeMES1,
    "SynVerNumMES2" -> synVerNumMES2
  )

  val cc013cGen: ArgGen = for {
    messageFields                         <- messageFieldsGen("CC013C")
    transitOperation                      <- transitOperationType04
    authorisation                         <- authorisationType03
    customsOfficeOfDeparture              <- CustomsOfficeOfDepartureType03
    customsOfficeOfDestinationDeclared    <- CustomsOfficeOfDestinationDeclaredType01
    customsOfficeOfTransitDeclared        <- CustomsOfficeOfTransitDeclaredType03
    customsOfficeOfExitForTransitDeclared <- CustomsOfficeOfExitForTransitDeclaredType02
    holderOfTheTransitProcedure           <- HolderOfTheTransitProcedureType14
    representative                        <- RepresentativeType05
    guarantee                             <- GuaranteeType01
    consignment                           <- ConsignmentType20

  } yield messageFields ++ transitOperation ++ authorisation ++ customsOfficeOfDeparture ++ customsOfficeOfDestinationDeclared ++ customsOfficeOfTransitDeclared ++ customsOfficeOfExitForTransitDeclared ++ holderOfTheTransitProcedure ++ representative ++ guarantee ++ consignment
}
