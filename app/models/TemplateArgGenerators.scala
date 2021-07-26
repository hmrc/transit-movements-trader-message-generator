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
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import org.scalacheck.Arbitrary.arbitrary
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import wolfendale.scalacheck.regexp.RegexpGen
import java.time.LocalDate

object TemplateArgGenerators {
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

  val date8 = RegexpGen.from(
    "[1-2][0-9]{3}[0][1-9][0][1-9]|[1-2][0-9]{3}[0][1-9][1|2][0-9]|[1-2][0-9]{3}[0][1-9][3][0|1]|[1-2][0-9]{3}[1][0-2][0][1-9]|[1-2][0-9]{3}[1][0-2][1|2][0-9]|[1-2][0-9]{3}[1][0-2][3][0|1]"
  )

  val time4 = RegexpGen.from(
    "[0-1]{1}[0-9]{1}[0-5]{1}[0-9]{1}|[2]{1}[0-3]{1}[0-5]{1}[0-9]{1}"
  )

  def leftPad(str: String, len: Int, paddingChar: Char) =
    str.reverse.padTo(len, paddingChar).reverse

  def phase4AddressGen(
    nameField: String,
    streetField: String,
    postCodeField: String,
    cityField: String,
    countryField: String,
    languageCodeField: String,
    tinField: String
  ) = for {
    name         <- alphaNum(35)
    street       <- alphaNum(35)
    postCode     <- alphaNum(9)
    city         <- alphaNum(35)
    country      <- alphaExactly(2)
    languageCode <- alphaExactly(2)
    tin          <- alphaNum(17)
  } yield Json.obj(
    nameField         -> name,
    streetField       -> street,
    postCodeField     -> postCode,
    cityField         -> city,
    countryField      -> country,
    languageCodeField -> languageCode,
    tinField          -> tin
  )

  val phase4CommonFieldsGen: ArgGen = for {
    SynIdeMES1    <- Gen.const("UNOC")
    SynVerNumMES2 <- Gen.const("3")
    MesSenMES3 <- (Gen.oneOf(Seq("ARR", "DEP")), num(4), num(1)).mapN {
      case (prefix, movementNum, correlationId) =>
        s"MDTP-$prefix-${leftPad(movementNum, 23, '0')}-${leftPad(correlationId, 2, '0')}"
    }
    SenIdeCodQuaMES4  <- alphaNum(4)
    MesRecMES6        <- alphaNum(35)
    RecIdeCodQuaMES7  <- alphaNum(4)
    DatOfPreMES9      <- num(8)
    TimOfPreMES10     <- time4
    IntConRefMES11    <- alphaNum(14)
    RecRefMES12       <- alphaNum(14)
    RecRefQuaMES13    <- alphaNumExactly(2)
    AppRefMES14       <- alphaNum(14)
    PriMES15          <- alphaExactly(1)
    AckReqMES16       <- num(1)
    ComAgrIdMES17     <- alphaNum(35)
    TesIndMES18       <- num(1)
    MesIdeMES19       <- alphaNum(14)
    MesTypMES20       <- alphaNum(6)
    ComAccRefMES21    <- alphaNum(35)
    MesSeqNumMES22    <- num(2)
    FirAndLasTraMES23 <- alphaExactly(1)
  } yield Json.obj(
    "SynIdeMES1"        -> SynIdeMES1,
    "SynVerNumMES2"     -> SynVerNumMES2,
    "MesSenMES3"        -> MesSenMES3,
    "SenIdeCodQuaMES4"  -> SenIdeCodQuaMES4,
    "MesRecMES6"        -> MesRecMES6,
    "RecIdeCodQuaMES7"  -> RecIdeCodQuaMES7,
    "DatOfPreMES9"      -> DatOfPreMES9,
    "TimOfPreMES10"     -> TimOfPreMES10,
    "IntConRefMES11"    -> IntConRefMES11,
    "RecRefMES12"       -> RecRefMES12,
    "RecRefQuaMES13"    -> RecRefQuaMES13,
    "AppRefMES14"       -> AppRefMES14,
    "PriMES15"          -> PriMES15,
    "AckReqMES16"       -> AckReqMES16,
    "ComAgrIdMES17"     -> ComAgrIdMES17,
    "TesIndMES18"       -> TesIndMES18,
    "MesIdeMES19"       -> MesIdeMES19,
    "MesTypMES20"       -> MesTypMES20,
    "ComAccRefMES21"    -> ComAccRefMES21,
    "MesSeqNumMES22"    -> MesSeqNumMES22,
    "FirAndLasTraMES23" -> FirAndLasTraMES23
  )

  val tradesTrdFieldsGen: ArgGen = phase4AddressGen(
    "NamTRD7",
    "StrAndNumTRD22",
    "PosCodTRD23",
    "CitTRD24",
    "CouTRD25",
    "NADLNGRD",
    "TINTRD59"
  )

  val enRouEveTevFieldsGen: ArgGen = for {
    PlaTEV10             <- alphaNum(35)
    PlaTEV10LNG          <- alphaExactly(2)
    CouTEV13             <- alphaExactly(2)
    AlrInNCTCTL29        <- num(1)
    IncFlaINC3           <- num(1)
    IncInfINC4           <- alphaNum(350)
    IncInfINC4LNG        <- alphaExactly(2)
    EndDatINC6           <- date8
    EndAutINC7           <- alphaNum(35)
    EndAutINC7LNG        <- alphaExactly(2)
    EndPlaINC10          <- alphaNum(35)
    EndPlaINC10LNG       <- alphaExactly(2)
    EndCouINC12          <- alphaExactly(2)
    NewTraMeaIdeSHP26    <- alphaNum(27)
    NewTraMeaIdeSHP26LNG <- alphaExactly(2)
    NewTraMeaNatSHP54    <- alphaExactly(2)
    EndDatSHP60          <- date8
    EndAutSHP61          <- alphaNum(35)
    EndAutSHP61LNG       <- alphaExactly(2)
    EndPlaSHP63          <- alphaNum(35)
    EndPlaSHP63LNG       <- alphaExactly(2)
    EndCouSHP65          <- alphaExactly(2)
    ConNumNR31           <- alphaExactly(17)
  } yield Json.obj(
    "PlaTEV10"             -> PlaTEV10,
    "PlaTEV10LNG"          -> PlaTEV10LNG,
    "CouTEV13"             -> CouTEV13,
    "AlrInNCTCTL29"        -> AlrInNCTCTL29,
    "IncFlaINC3"           -> IncFlaINC3,
    "IncInfINC4"           -> IncInfINC4,
    "IncInfINC4LNG"        -> IncInfINC4LNG,
    "EndDatINC6"           -> EndDatINC6,
    "EndAutINC7"           -> EndAutINC7,
    "EndAutINC7LNG"        -> EndAutINC7LNG,
    "EndPlaINC10"          -> EndPlaINC10,
    "EndPlaINC10LNG"       -> EndPlaINC10LNG,
    "EndCouINC12"          -> EndCouINC12,
    "NewTraMeaIdeSHP26"    -> NewTraMeaIdeSHP26,
    "NewTraMeaIdeSHP26LNG" -> NewTraMeaIdeSHP26LNG,
    "NewTraMeaNatSHP54"    -> NewTraMeaNatSHP54,
    "EndDatSHP60"          -> EndDatSHP60,
    "EndAutSHP61"          -> EndAutSHP61,
    "EndAutSHP61LNG"       -> EndAutSHP61LNG,
    "EndPlaSHP63"          -> EndPlaSHP63,
    "EndPlaSHP63LNG"       -> EndPlaSHP63LNG,
    "EndCouSHP65"          -> EndCouSHP65,
    "ConNumNR31"           -> ConNumNR31
  )

  val seaInfSf1FieldsGen: ArgGen = for {
    SeaNumSF12    <- num(4)
    SeaIdeSI11    <- alphaNum(20)
    SeaIdeSI11LNG <- alphaExactly(2)
  } yield Json.obj(
    "SeaNumSF12"    -> SeaNumSF12,
    "SeaIdeSI11"    -> SeaIdeSI11,
    "SeaIdeSI11LNG" -> SeaIdeSI11LNG
  )

  val seaInfSliFieldsGen: ArgGen = for {
    SeaNumSLI2    <- num(4)
    SeaIdeSID1    <- alphaNum(20)
    SeaIdeSID1LNG <- alphaExactly(2)
  } yield Json.obj(
    "SeaNumSLI2"    -> SeaNumSLI2,
    "SeaIdeSID1"    -> SeaIdeSID1,
    "SeaIdeSID1LNG" -> SeaIdeSID1LNG
  )

  val unlRemRemFieldsGen: ArgGen = for {
    StaOfTheSeaOKREM19 <- num(1)
    UnlRemREM53        <- alphaNum(350)
    UnlRemREM53LNG     <- alphaExactly(2)
    ConREM65           <- num(1)
    UnlComREM66        <- num(1)
    UnlDatREM67        <- date8
  } yield Json.obj(
    "StaOfTheSeaOKREM19" -> StaOfTheSeaOKREM19,
    "UnlRemREM53"        -> UnlRemREM53,
    "UnlRemREM53LNG"     -> UnlRemREM53LNG,
    "ConREM65"           -> ConREM65,
    "UnlComREM66"        -> UnlComREM66,
    "UnlDatREM67"        -> UnlDatREM67
  )

  val resOfCon534FieldsGen: ArgGen = for {
    DesTOC2         <- alphaNum(140)
    DesTOC2LNG      <- alphaExactly(2)
    ConInd424       <- alphaNumExactly(2)
    PoiToTheAttTOC5 <- alphaNum(35)
    CorValTOC4      <- alphaNum(27)
  } yield Json.obj(
    "DesTOC2"         -> DesTOC2,
    "DesTOC2LNG"      -> DesTOC2LNG,
    "ConInd424"       -> ConInd424,
    "PoiToTheAttTOC5" -> PoiToTheAttTOC5,
    "CorValTOC4"      -> CorValTOC4
  )

  val pacGs2FieldsGen: ArgGen = for {
    MarNumOfPacGS21    <- alphaNum(42)
    MarNumOfPacGS21LNG <- alphaExactly(2)
    KinOfPacGS23       <- alphaNum(3)
    NumOfPacGS24       <- num(5)
    NumOfPieGS25       <- num(5)
  } yield Json.obj(
    "MarNumOfPacGS21"    -> MarNumOfPacGS21,
    "MarNumOfPacGS21LNG" -> MarNumOfPacGS21LNG,
    "KinOfPacGS23"       -> KinOfPacGS23,
    "NumOfPacGS24"       -> NumOfPacGS24,
    "NumOfPieGS25"       -> NumOfPieGS25
  )

  val sgiCodSd2FieldsGen: ArgGen = for {
    SenGooCodSD22 <- num(2)
    SenQuaSD23    <- (num1(8), num(3)).mapN(_ + "." + _)
  } yield Json.obj(
    "SenGooCodSD22" -> SenGooCodSD22,
    "SenQuaSD23"    -> SenQuaSD23
  )

  val proDocDc2FieldsGen: ArgGen = for {
    DocTypDC21      <- alphaNum(4)
    DocRefDC23      <- alphaNum(35)
    DocRefDCLNG     <- alphaExactly(2)
    ComOfInfDC25    <- alphaNum(26)
    ComOfInfDC25LNG <- alphaExactly(2)
  } yield Json.obj(
    "DocTypDC21"      -> DocTypDC21,
    "DocRefDC23"      -> DocRefDC23,
    "DocRefDCLNG"     -> DocRefDCLNG,
    "ComOfInfDC25"    -> ComOfInfDC25,
    "ComOfInfDC25LNG" -> ComOfInfDC25LNG
  )

  val resOfConRocFieldsGen: ArgGen = for {
    DesROC2          <- alphaNum(140)
    DesROC2LNG       <- alphaExactly(2)
    ConIndROC1       <- alphaNum(2)
    PoiToTheAttROC51 <- alphaNum(35)
  } yield Json.obj(
    "DesROC2"          -> DesROC2,
    "DesROC2LNG"       -> DesROC2LNG,
    "ConIndROC1"       -> ConIndROC1,
    "PoiToTheAttROC51" -> PoiToTheAttROC51
  )

  val traPriPc1FieldsGen: ArgGen = for {
    addressFields <- phase4AddressGen(
      "NamPC17",
      "StrAndNumPC122",
      "PosCodPC123",
      "CitPC124",
      "CouPC125",
      "NADLNGPC",
      "TINPC159"
    )
    HITPC126 <- alphaNum(17)
  } yield addressFields ++ Json.obj(
    "HITPC126" -> HITPC126
  )

  val traConCo1FieldsGen: ArgGen = phase4AddressGen(
    "NamCO17",
    "StrAndNumCO122",
    "PosCodCO123",
    "CitCO124",
    "CouCO125",
    "TRACONCO1LNG",
    "TINCO159"
  )

  val traConCe1FieldsGen: ArgGen = phase4AddressGen(
    "NamCE17",
    "StrAndNumCE122",
    "PosCodCE123",
    "CitCE124",
    "CouCE125",
    "NADLNGCE",
    "TINCE159"
  )

  val guaGuaFieldsGen: ArgGen = for {
    GuaTypGUA1           <- alphaNum(1)
    GuaRefNumGRNREF1     <- alphaNum(24)
    OthGuaRefREF4        <- alphaNum(35)
    AccCodREF6           <- alphaNumExactly(4)
    NotValForECVLE1      <- num(1)
    NotValForOthConPLIM2 <- alphaExactly(2)
  } yield Json.obj(
    "GuaTypGUA1"           -> GuaTypGUA1,
    "GuaRefNumGRNREF1"     -> GuaRefNumGRNREF1,
    "OthGuaRefREF4"        -> OthGuaRefREF4,
    "AccCodREF6"           -> AccCodREF6,
    "NotValForECVLE1"      -> NotValForECVLE1,
    "NotValForOthConPLIM2" -> NotValForOthConPLIM2
  )

  val preAdmRefAr2FieldsGen: ArgGen = for {
    PreDocTypAR21   <- alphaNum(6)
    PreDocRefAR26   <- alphaNum(35)
    PreDocRefLNG    <- alphaExactly(2)
    ComOfInfAR29    <- alphaNum(26)
    ComOfInfAR29LNG <- alphaExactly(2)
  } yield Json.obj(
    "PreDocTypAR21"   -> PreDocTypAR21,
    "PreDocRefAR26"   -> PreDocRefAR26,
    "PreDocRefLNG"    -> PreDocRefLNG,
    "ComOfInfAR29"    -> ComOfInfAR29,
    "ComOfInfAR29LNG" -> ComOfInfAR29LNG
  )

  val speMentMt2FieldsGen: ArgGen = for {
    AddInfMT21    <- alphaNum(70)
    AddInfMT21LNG <- alphaExactly(2)
    AddInfCodMT23 <- alphaNum(5)
    ExpFroECMT24  <- num(1)
    ExpFroCouMT25 <- alphaExactly(2)
  } yield Json.obj(
    "AddInfMT21"    -> AddInfMT21,
    "AddInfMT21LNG" -> AddInfMT21LNG,
    "AddInfCodMT23" -> AddInfCodMT23,
    "ExpFroECMT24"  -> ExpFroECMT24,
    "ExpFroCouMT25" -> ExpFroCouMT25
  )

  val traConCo2FieldsGen: ArgGen = phase4AddressGen(
    "NamCO27",
    "StrAndNumCO222",
    "PosCodCO223",
    "CitCO224",
    "CouCO225",
    "NADLNGGTCO",
    "TINCO259"
  )

  val traConCe2FieldsGen: ArgGen = phase4AddressGen(
    "NamCE27",
    "StrAndNumCE222",
    "PosCodCE223",
    "CitCE224",
    "CouCE225",
    "NADLNGGICE",
    "TINCE259"
  )

  val traCorSecGoo021FieldsGen: ArgGen = phase4AddressGen(
    "NamTRACORSECGOO025",
    "StrNumTRACORSECGOO027",
    "PosCodTRACORSECGOO026",
    "CitTRACORSECGOO022",
    "CouCodTRACORSECGOO023",
    "TRACORSECGOO021LNG",
    "TINTRACORSECGOO028"
  )

  val traConSecGoo013FieldsGen: ArgGen = phase4AddressGen(
    "NamTRACONSECGOO017",
    "StrNumTRACONSECGOO019",
    "PosCodTRACONSECGOO018",
    "CityTRACONSECGOO014",
    "CouCodTRACONSECGOO015",
    "TRACONSECGOO013LNG",
    "TINTRACONSECGOO020"
  )

  val carTra100FieldsGen: ArgGen = phase4AddressGen(
    "NamCARTRA121",
    "StrAndNumCARTRA254",
    "PosCodCARTRA121",
    "CitCARTRA789",
    "CouCodCARTRA587",
    "NADCARTRA121",
    "TINCARTRA254"
  )

  val traCorSec037FieldsGen: ArgGen = phase4AddressGen(
    "NamTRACORSEC041",
    "StrNumTRACORSEC043",
    "PosCodTRACORSEC042",
    "CitTRACORSEC038",
    "CouCodTRACORSEC039",
    "TRACORSEC037LNG",
    "TINTRACORSEC044"
  )

  val traConSec029FieldsGen: ArgGen = phase4AddressGen(
    "NameTRACONSEC033",
    "StrNumTRACONSEC035",
    "PosCodTRACONSEC034",
    "CitTRACONSEC030",
    "CouCodTRACONSEC031",
    "TRACONSEC029LNG",
    "TINTRACONSEC036"
  )

  val cc007aGen: ArgGen = for {
    commonFields           <- phase4CommonFieldsGen
    tradesTrdFields        <- tradesTrdFieldsGen
    enRouEveTevFields      <- enRouEveTevFieldsGen
    seaInfSf1Fields        <- seaInfSf1FieldsGen
    DocNumHEA5             <- alphaNum(21)
    CusSubPlaHEA66         <- alphaNum(17)
    ArrNotPlaHEA60         <- alphaNum(35)
    ArrNotPlaHEA60LNG      <- alphaExactly(2)
    ArrAgrLocCodHEA62      <- alphaNum(17)
    ArrAgrLocOfGooHEA63    <- alphaNum(35)
    ArrAgrLocOfGooHEA63LNG <- alphaExactly(2)
    ArrAutLocOfGooHEA65    <- alphaNum(17)
    SimProFlaHEA132        <- num(1)
    ArrNotDatHEA141        <- date8
    DiaLanIndAtDesHEA255   <- alphaExactly(2)
    RefNumRES1             <- alphaNumExactly(8)
  } yield commonFields ++ tradesTrdFields ++ enRouEveTevFields ++ seaInfSf1Fields ++ Json.obj(
    "DocNumHEA5"             -> DocNumHEA5,
    "CusSubPlaHEA66"         -> CusSubPlaHEA66,
    "ArrNotPlaHEA60"         -> ArrNotPlaHEA60,
    "ArrNotPlaHEA60LNG"      -> ArrNotPlaHEA60LNG,
    "ArrAgrLocCodHEA62"      -> ArrAgrLocCodHEA62,
    "ArrAgrLocOfGooHEA63"    -> ArrAgrLocOfGooHEA63,
    "ArrAgrLocOfGooHEA63LNG" -> ArrAgrLocOfGooHEA63LNG,
    "ArrAutLocOfGooHEA65"    -> ArrAutLocOfGooHEA65,
    "SimProFlaHEA132"        -> SimProFlaHEA132,
    "ArrNotDatHEA141"        -> ArrNotDatHEA141,
    "DiaLanIndAtDesHEA255"   -> DiaLanIndAtDesHEA255,
    "RefNumRES1"             -> RefNumRES1
  )

  val cc015bGen: ArgGen = for {
    commonFields             <- phase4CommonFieldsGen
    traPriPc1Fields          <- traPriPc1FieldsGen
    traConCo1Fields          <- traConCo1FieldsGen
    traConCe1Fields          <- traConCe1FieldsGen
    seaInfSliFields          <- seaInfSliFieldsGen
    guaGuaFields             <- guaGuaFieldsGen
    preAdmRefAr2Fields       <- preAdmRefAr2FieldsGen
    proDocDc2Fields          <- proDocDc2FieldsGen
    speMentMt2Fields         <- speMentMt2FieldsGen
    traConCo2Fields          <- traConCo2FieldsGen
    traConCe2Fields          <- traConCe2FieldsGen
    pacGs2Fields             <- pacGs2FieldsGen
    sgiCodSd2Fields          <- sgiCodSd2FieldsGen
    traCorSecGoo021Fields    <- traCorSecGoo021FieldsGen
    traConSecGoo013Fields    <- traConSecGoo013FieldsGen
    carTra100Fields          <- carTra100FieldsGen
    traCorSec037Fields       <- traCorSec037FieldsGen
    traConSec029Fields       <- traConSec029FieldsGen
    RefNumHEA4               <- alphaNum(22)
    TypOfDecHEA24            <- alphaNum(9)
    CouOfDesCodHEA30         <- alphaExactly(2)
    AgrLocOfGooCodHEA38      <- alphaNum(17)
    AgrLocOfGooHEA39         <- alphaNum(35)
    AgrLocOfGooHEA39LNG      <- alphaExactly(2)
    AutLocOfGooCodHEA41      <- alphaNum(17)
    PlaOfLoaCodHEA46         <- alphaNum(17)
    CouOfDisCodHEA55         <- alphaExactly(2)
    CusSubPlaHEA66           <- alphaNum(17)
    InlTraModHEA75           <- num(2)
    TraModAtBorHEA76         <- num(2)
    IdeOfMeaOfTraAtDHEA78    <- alphaNum(27)
    IdeOfMeaOfTraAtDHEA78LNG <- alphaExactly(2)
    NatOfMeaOfTraAtDHEA80    <- alphaExactly(2)
    IdeOfMeaOfTraCroHEA85    <- alphaNum(27)
    IdeOfMeaOfTraCroHEA85LNG <- alphaExactly(2)
    NatOfMeaOfTraCroHEA87    <- alphaExactly(2)
    TypOfMeaOfTraCroHEA88    <- num(2)
    ConIndHEA96              <- num(1)
    DiaLanIndAtDepHEA254     <- alphaExactly(2)
    NCTSAccDocHEA601LNG      <- alphaExactly(2)
    NumOfLoaLisHEA304        <- num(5)
    TotNumOfIteHEA305        <- num(5)
    TotNumOfPacHEA306        <- num(7)
    TotGroMasHEA307          <- (num1(8), num(3)).mapN(_ + "." + _)
    DecDatHEA383             <- num(8)
    DecPlaHEA394             <- alphaNum(35)
    DecPlaHEA394LNG          <- alphaExactly(2)
    SpeCirIndHEA1            <- alphaExactly(1)
    TraChaMetOfPayHEA1       <- alphaExactly(1)
    ComRefNumHEA             <- alphaNum(70)
    SecHEA358                <- num(1)
    ConRefNumHEA             <- alphaNum(35)
    CodPlUnHEA357            <- alphaNum(35)
    CodPlUnHEA357LNG         <- alphaExactly(2)
    TINTRA59                 <- alphaNum(17)
    RefNumEPT1               <- alphaNumExactly(8)
    RefNumRNS1               <- alphaNumExactly(8)
    ArrTimTRACUS085          <- num(12)
    RefNumEST1               <- alphaNumExactly(8)
    ConResCodERS16           <- alphaNumExactly(2)
    DatLimERS69              <- date8
    NamREP5                  <- alphaNum(35)
    RepCapREP18              <- alphaNum(35)
    RepCapREP18LNG           <- alphaExactly(2)
    IteNumGDS7               <- num(5)
    ComCodTarCodGDS10        <- alphaNum(22)
    DecTypGDS15              <- alphaNum(9)
    GooDesGDS23              <- alphaNum(280)
    GooDesGDS23LNG           <- alphaExactly(2)
    GroMasGDS46              <- (num1(8), num(3)).mapN(_ + "." + _)
    NetMasGDS48              <- (num1(8), num(3)).mapN(_ + "." + _)
    CouOfDisGDS58            <- alphaExactly(2)
    CouOfDesGDS59            <- alphaExactly(2)
    MetOfPayGDI12            <- alphaExactly(1)
    ComRefNumGIM1            <- alphaNum(70)
    UNDanGooCodGDI1          <- alphaNumExactly(4)
    ConNumNR21               <- alphaNum(17)
    CouOfRouCodITI1          <- alphaExactly(2)
  } yield commonFields ++ traPriPc1Fields ++ traConCo1Fields ++ traConCe1Fields ++ seaInfSliFields ++ guaGuaFields ++ preAdmRefAr2Fields ++ proDocDc2Fields ++ speMentMt2Fields ++ traConCo2Fields ++ traConCe2Fields ++ pacGs2Fields ++ sgiCodSd2Fields ++ traCorSecGoo021Fields ++ traConSecGoo013Fields ++ carTra100Fields ++ traCorSec037Fields ++ traConSec029Fields ++ Json
    .obj(
      "RefNumHEA4"               -> RefNumHEA4,
      "TypOfDecHEA24"            -> TypOfDecHEA24,
      "CouOfDesCodHEA30"         -> CouOfDesCodHEA30,
      "AgrLocOfGooCodHEA38"      -> AgrLocOfGooCodHEA38,
      "AgrLocOfGooHEA39"         -> AgrLocOfGooHEA39,
      "AgrLocOfGooHEA39LNG"      -> AgrLocOfGooHEA39LNG,
      "AutLocOfGooCodHEA41"      -> AutLocOfGooCodHEA41,
      "PlaOfLoaCodHEA46"         -> PlaOfLoaCodHEA46,
      "CouOfDisCodHEA55"         -> CouOfDisCodHEA55,
      "CusSubPlaHEA66"           -> CusSubPlaHEA66,
      "InlTraModHEA75"           -> InlTraModHEA75,
      "TraModAtBorHEA76"         -> TraModAtBorHEA76,
      "IdeOfMeaOfTraAtDHEA78"    -> IdeOfMeaOfTraAtDHEA78,
      "IdeOfMeaOfTraAtDHEA78LNG" -> IdeOfMeaOfTraAtDHEA78LNG,
      "NatOfMeaOfTraAtDHEA80"    -> NatOfMeaOfTraAtDHEA80,
      "IdeOfMeaOfTraCroHEA85"    -> IdeOfMeaOfTraCroHEA85,
      "IdeOfMeaOfTraCroHEA85LNG" -> IdeOfMeaOfTraCroHEA85LNG,
      "NatOfMeaOfTraCroHEA87"    -> NatOfMeaOfTraCroHEA87,
      "TypOfMeaOfTraCroHEA88"    -> TypOfMeaOfTraCroHEA88,
      "ConIndHEA96"              -> ConIndHEA96,
      "DiaLanIndAtDepHEA254"     -> DiaLanIndAtDepHEA254,
      "NCTSAccDocHEA601LNG"      -> NCTSAccDocHEA601LNG,
      "NumOfLoaLisHEA304"        -> NumOfLoaLisHEA304,
      "TotNumOfIteHEA305"        -> TotNumOfIteHEA305,
      "TotNumOfPacHEA306"        -> TotNumOfPacHEA306,
      "TotGroMasHEA307"          -> TotGroMasHEA307,
      "DecDatHEA383"             -> DecDatHEA383,
      "DecPlaHEA394"             -> DecPlaHEA394,
      "DecPlaHEA394LNG"          -> DecPlaHEA394LNG,
      "SpeCirIndHEA1"            -> SpeCirIndHEA1,
      "TraChaMetOfPayHEA1"       -> TraChaMetOfPayHEA1,
      "ComRefNumHEA"             -> ComRefNumHEA,
      "SecHEA358"                -> SecHEA358,
      "ConRefNumHEA"             -> ConRefNumHEA,
      "CodPlUnHEA357"            -> CodPlUnHEA357,
      "CodPlUnHEA357LNG"         -> CodPlUnHEA357LNG,
      "TINTRA59"                 -> TINTRA59,
      "RefNumEPT1"               -> RefNumEPT1,
      "RefNumRNS1"               -> RefNumRNS1,
      "ArrTimTRACUS085"          -> ArrTimTRACUS085,
      "RefNumEST1"               -> RefNumEST1,
      "ConResCodERS16"           -> ConResCodERS16,
      "DatLimERS69"              -> DatLimERS69,
      "NamREP5"                  -> NamREP5,
      "RepCapREP18"              -> RepCapREP18,
      "RepCapREP18LNG"           -> RepCapREP18LNG,
      "IteNumGDS7"               -> IteNumGDS7,
      "ComCodTarCodGDS10"        -> ComCodTarCodGDS10,
      "DecTypGDS15"              -> DecTypGDS15,
      "GooDesGDS23"              -> GooDesGDS23,
      "GooDesGDS23LNG"           -> GooDesGDS23LNG,
      "GroMasGDS46"              -> GroMasGDS46,
      "NetMasGDS48"              -> NetMasGDS48,
      "CouOfDisGDS58"            -> CouOfDisGDS58,
      "CouOfDesGDS59"            -> CouOfDesGDS59,
      "MetOfPayGDI12"            -> MetOfPayGDI12,
      "ComRefNumGIM1"            -> ComRefNumGIM1,
      "UNDanGooCodGDI1"          -> UNDanGooCodGDI1,
      "ConNumNR21"               -> ConNumNR21,
      "CouOfRouCodITI1"          -> CouOfRouCodITI1
    )

  val cc044aGen: ArgGen = for {
    commonFields             <- phase4CommonFieldsGen
    tradesTrdFields          <- tradesTrdFieldsGen
    unlRemRemFields          <- unlRemRemFieldsGen
    resOfCon534Fields        <- resOfCon534FieldsGen
    seaInfSliFields          <- seaInfSliFieldsGen
    proDocDc2Fields          <- proDocDc2FieldsGen
    resOfConRocFields        <- resOfConRocFieldsGen
    pacGs2Fields             <- pacGs2FieldsGen
    sgiCodSd2Fields          <- sgiCodSd2FieldsGen
    DocNumHEA5               <- alphaNum(21)
    IdeOfMeaOfTraAtDHEA78    <- alphaNum(27)
    IdeOfMeaOfTraAtDHEA78LNG <- alphaExactly(2)
    NatOfMeaOfTraAtDHEA80    <- alphaExactly(2)
    TotNumOfIteHEA305        <- num(5)
    TotNumOfPacHEA306        <- num(7)
    TotGroMasHEA307          <- (num1(8), num(3)).mapN(_ + "." + _)
    RefNumRES1               <- alphaNumExactly(8)
    IteNumGDS7               <- num(5)
    ComCodTarCodGDS10        <- alphaNum(22)
    GooDesGDS23              <- alphaNum(280)
    GooDesGDS23LNG           <- alphaExactly(2)
    GroMasGDS46              <- (num1(8), num(3)).mapN(_ + "." + _)
    NetMasGDS48              <- (num1(8), num(3)).mapN(_ + "." + _)
    ConNumNR21               <- alphaNum(17)
  } yield commonFields ++ tradesTrdFields ++ seaInfSliFields ++ unlRemRemFields ++ resOfCon534Fields ++ pacGs2Fields ++ sgiCodSd2Fields ++ proDocDc2Fields ++ resOfConRocFields ++ Json
    .obj(
      "DocNumHEA5"               -> DocNumHEA5,
      "IdeOfMeaOfTraAtDHEA78"    -> IdeOfMeaOfTraAtDHEA78,
      "IdeOfMeaOfTraAtDHEA78LNG" -> IdeOfMeaOfTraAtDHEA78LNG,
      "NatOfMeaOfTraAtDHEA80"    -> NatOfMeaOfTraAtDHEA80,
      "TotNumOfIteHEA305"        -> TotNumOfIteHEA305,
      "TotNumOfPacHEA306"        -> TotNumOfPacHEA306,
      "TotGroMasHEA307"          -> TotGroMasHEA307,
      "RefNumRES1"               -> RefNumRES1,
      "IteNumGDS7"               -> IteNumGDS7,
      "ComCodTarCodGDS10"        -> ComCodTarCodGDS10,
      "GooDesGDS23"              -> GooDesGDS23,
      "GooDesGDS23LNG"           -> GooDesGDS23LNG,
      "GroMasGDS46"              -> GroMasGDS46,
      "NetMasGDS48"              -> NetMasGDS48,
      "ConNumNR21"               -> ConNumNR21
    )

  val dateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
  val dateFormatter     = DateTimeFormatter.ISO_LOCAL_DATE

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
    reducedDatasetIndicator          <- num(1)
    specificCircumstanceIndicator    <- alphaNum(3)
    communicationLanguageAtDeparture <- alphaExactly(2)
    bindingItinerary                 <- num(1)
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
    quantity                       <- (num1(16), num(6)).mapN(_ + "." + _)
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
    grossMass                          <- (num1(16), num(6)).mapN(_ + "." + _)
    netMass                            <- (num1(16), num(6)).mapN(_ + "." + _)
    supplementaryUnits                 <- (num1(16), num(6)).mapN(_ + "." + _)
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
