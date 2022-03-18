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
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import play.api.libs.json.Json
import wolfendale.scalacheck.regexp.RegexpGen

object Phase4Generators extends TemplateArgGenerators {
  val date8 = RegexpGen.from(
    """[1-2][0-9]{3}[0][1-9][0][1-9]|[1-2][0-9]{3}[0][1-9](1|2)[0-9]|[1-2][0-9]{3}[0][1-9][3](0|1)|[1-2][0-9]{3}[1][0-2][0][1-9]|[1-2][0-9]{3}[1][0-2](1|2)[0-9]|(1-2][0-9]{3}[1][0-2][3][0|1)"""
  )

  val time4 = RegexpGen.from(
    """[0-1]{1}[0-9]{1}[0-5]{1}[0-9]{1}|[2]{1}[0-3]{1}[0-5]{1}[0-9]{1}"""
  )

  def addressGen(
    nameField: String,
    streetField: String,
    postCodeField: String,
    cityField: String,
    countryField: String,
    languageCodeField: String
  ): ArgGen = for {
    name         <- alphaNum(35)
    street       <- alphaNum(35)
    postCode     <- alphaNum(9)
    city         <- alphaNum(35)
    country      <- alphaExactly(2)
    languageCode <- alphaExactly(2)
  } yield Json.obj(
    nameField         -> name,
    streetField       -> street,
    postCodeField     -> postCode,
    cityField         -> city,
    countryField      -> country,
    languageCodeField -> languageCode
  )

  def addressGen(
    nameField: String,
    streetField: String,
    postCodeField: String,
    cityField: String,
    countryField: String,
    languageCodeField: String,
    tinField: String
  ): ArgGen = for {
    tin <- alphaNum(17)
    addressFields <- addressGen(
      nameField,
      streetField,
      postCodeField,
      cityField,
      countryField,
      languageCodeField
    )
  } yield addressFields ++ Json.obj(tinField -> tin)

  def phase4CommonFieldsGen(senderPrefix: String): ArgGen = for {
    SynIdeMES1    <- Gen.const("UNOC")
    SynVerNumMES2 <- Gen.const("3")
    MesSenMES3 <- (num(4), num(1)).mapN { case (movementNum, correlationId) =>
      s"MDTP-$senderPrefix-${leftPad(movementNum, 23, '0')}-${leftPad(correlationId, 2, '0')}"
    }
    SenIdeCodQuaMES4  <- alphaNum(4)
    MesRecMES6        <- alphaNum(35)
    RecIdeCodQuaMES7  <- alphaNum(4)
    DatOfPreMES9      <- date8
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
    MesTypMES20       <- alphaExactly(6)
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

  val tradesTrdFieldsGen: ArgGen = addressGen(
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
    MarNumOfPacGS21    <- alphaNumCSV(21,2)
    MarNumOfPacGS21LNG <- alphaExactly(2)
    KinOfPacGS23       <- alphaNum(3)
    NumOfPacGS24       <- Gen.const(2)
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
    ConIndROC1       <- alphaExactly(2)
    PoiToTheAttROC51 <- alphaNum(35)
  } yield Json.obj(
    "DesROC2"          -> DesROC2,
    "DesROC2LNG"       -> DesROC2LNG,
    "ConIndROC1"       -> ConIndROC1,
    "PoiToTheAttROC51" -> PoiToTheAttROC51
  )

  val traPriPc1FieldsGen: ArgGen = for {
    addressFields <- addressGen(
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

  val traConCo1FieldsGen: ArgGen = addressGen(
    "NamCO17",
    "StrAndNumCO122",
    "PosCodCO123",
    "CitCO124",
    "CouCO125",
    "TRACONCO1LNG",
    "TINCO159"
  )

  val traConCe1FieldsGen: ArgGen = addressGen(
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

  val traConCo2FieldsGen: ArgGen = addressGen(
    "NamCO27",
    "StrAndNumCO222",
    "PosCodCO223",
    "CitCO224",
    "CouCO225",
    "NADLNGGTCO",
    "TINCO259"
  )

  val traConCe2FieldsGen: ArgGen = addressGen(
    "NamCE27",
    "StrAndNumCE222",
    "PosCodCE223",
    "CitCE224",
    "CouCE225",
    "NADLNGGICE",
    "TINCE259"
  )

  val traCorSecGoo021FieldsGen: ArgGen = addressGen(
    "NamTRACORSECGOO025",
    "StrNumTRACORSECGOO027",
    "PosCodTRACORSECGOO026",
    "CitTRACORSECGOO022",
    "CouCodTRACORSECGOO023",
    "TRACORSECGOO021LNG",
    "TINTRACORSECGOO028"
  )

  val traConSecGoo013FieldsGen: ArgGen = addressGen(
    "NamTRACONSECGOO017",
    "StrNumTRACONSECGOO019",
    "PosCodTRACONSECGOO018",
    "CityTRACONSECGOO014",
    "CouCodTRACONSECGOO015",
    "TRACONSECGOO013LNG",
    "TINTRACONSECGOO020"
  )

  val carTra100FieldsGen: ArgGen = addressGen(
    "NamCARTRA121",
    "StrAndNumCARTRA254",
    "PosCodCARTRA121",
    "CitCARTRA789",
    "CouCodCARTRA587",
    "NADCARTRA121",
    "TINCARTRA254"
  )

  val traCorSec037FieldsGen: ArgGen = addressGen(
    "NamTRACORSEC041",
    "StrNumTRACORSEC043",
    "PosCodTRACORSEC042",
    "CitTRACORSEC038",
    "CouCodTRACORSEC039",
    "TRACORSEC037LNG",
    "TINTRACORSEC044"
  )

  val traConSec029FieldsGen: ArgGen = addressGen(
    "NameTRACONSEC033",
    "StrNumTRACONSEC035",
    "PosCodTRACONSEC034",
    "CitTRACONSEC030",
    "CouCodTRACONSEC031",
    "TRACONSEC029LNG",
    "TINTRACONSEC036"
  )

  val cc007aGen: ArgGen = for {
    commonFields           <- phase4CommonFieldsGen("ARR")
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
    commonFields             <- phase4CommonFieldsGen("DEP")
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
    RefNumEPT1               <- alphaNumExactly(6)
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
    commonFields             <- phase4CommonFieldsGen("ARR")
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

  val guaQueFieldsGen: ArgGen = for {
    QueIdeQUE1   <- num(1)
    PerFroQUE2   <- date8
    PerToDatQUE3 <- date8
  } yield Json.obj(
    "QueIdeQUE1"   -> QueIdeQUE1,
    "PerFroQUE2"   -> PerFroQUE2,
    "PerToDatQUE3" -> PerToDatQUE3
  )

  val traPrioTgFieldsGen: ArgGen = addressGen(
    "NamOTG7",
    "StrAndNumOTG22",
    "PosCodOTG23",
    "CitOTG24",
    "CouOTG25",
    "NADLNGOTG",
    "TINOTG59"
  )

  val usaUsaFieldsGen: ArgGen = for {
    DocNumUSA1   <- alphaNum(21)
    CovAmoUSA2   <- (num1(13), num(2)).mapN(_ + "." + _)
    CurUSA3      <- alphaNum(3)
    LocDatUSA4   <- date8
    ArrDatUSA5   <- date8
    DatOfRelUSA6 <- date8
  } yield Json.obj(
    "DocNumUSA1"   -> DocNumUSA1,
    "CovAmoUSA2"   -> CovAmoUSA2,
    "CurUSA3"      -> CurUSA3,
    "LocDatUSA4"   -> LocDatUSA4,
    "ArrDatUSA5"   -> ArrDatUSA5,
    "DatOfRelUSA6" -> DatOfRelUSA6
  )

  val expExpFieldsGen: ArgGen = for {
    ExpEXP1    <- (num1(13), num(2)).mapN(_ + "." + _)
    ExpCouEXP2 <- alphaNum(8)
    BalEXP3    <- (num1(13), num(2)).mapN(_ + "." + _)
    CurEXP4    <- alphaNumExactly(3)
  } yield Json.obj(
    "ExpEXP1"    -> ExpEXP1,
    "ExpCouEXP2" -> ExpCouEXP2,
    "BalEXP3"    -> BalEXP3,
    "CurEXP4"    -> CurEXP4
  )

  val guaOthAddFieldsGen: ArgGen = for {
    AddTypADD11 <- alphaExactly(1)
    guaOthAddressFields <- addressGen(
      "NamADD12",
      "StrAndNumADD13",
      "PosCodADD22",
      "CitADD15",
      "CouADD14",
      "NADLNGGOA"
    )
    ConPerADD16 <- alphaNum(35)
    PhoNumADD17 <- alphaNum(35)
    FaxNumADD18 <- alphaNum(35)
    TelNumADD19 <- alphaNum(35)
    MaiAddADD20 <- alphaNum(70)
    LanCodADD21 <- alphaExactly(2)
  } yield guaOthAddressFields ++ Json.obj(
    "AddTypADD11" -> AddTypADD11,
    "ConPerADD16" -> ConPerADD16,
    "PhoNumADD17" -> PhoNumADD17,
    "FaxNumADD18" -> FaxNumADD18,
    "TelNumADD19" -> TelNumADD19,
    "MaiAddADD20" -> MaiAddADD20,
    "LanCodADD21" -> LanCodADD21
  )

  val guarnrFieldsGen: ArgGen = for {
    guarnrAddressFields <- addressGen(
      "NamRNR735",
      "StrAndNumRNR736",
      "PosCodRNR737",
      "CitRNR738",
      "CouRNR745",
      "NADLNGGGR",
      "TINRNR746"
    )
    guaOthAddFields <- guaOthAddFieldsGen
    ConPerRNR739    <- alphaNum(35)
    PhoNumRNR740    <- alphaNum(35)
    FaxNumRNR741    <- alphaNum(35)
    TelNumRNR742    <- alphaNum(35)
    MaiAddRNR743    <- alphaNum(70)
    LanCodRNR744    <- alphaExactly(2)
  } yield guarnrAddressFields ++ guaOthAddFields ++ Json.obj(
    "ConPerRNR739" -> ConPerRNR739,
    "PhoNumRNR740" -> PhoNumRNR740,
    "FaxNumRNR741" -> FaxNumRNR741,
    "TelNumRNR742" -> TelNumRNR742,
    "MaiAddRNR743" -> MaiAddRNR743,
    "LanCodRNR744" -> LanCodRNR744
  )

  val guaComGuaGds2FieldsGen: ArgGen = for {
    ComCodGDS210    <- alphaNum(22)
    SenGooCodGDS213 <- num(2)
    GooDesGDS211    <- alphaNum(280)
    GooDesGDS211LNG <- alphaExactly(2)
  } yield Json.obj(
    "ComCodGDS210"    -> ComCodGDS210,
    "SenGooCodGDS213" -> SenGooCodGDS213,
    "GooDesGDS211"    -> GooDesGDS211,
    "GooDesGDS211LNG" -> GooDesGDS211LNG
  )

  val comGuaCmpFieldsGen: ArgGen = for {
    guaComGuaGds2Fields  <- guaComGuaGds2FieldsGen
    RefAmoCMP11          <- (num1(13), num(2)).mapN(_ + "." + _)
    PerOfRefAmoCMP12     <- num(3)
    GuaAmoCMP13          <- (num1(13), num(2)).mapN(_ + "." + _)
    CurCMP14             <- alphaNumExactly(3)
    NumOfCerCMP15        <- alphaNum(8)
    ValDatCMP16          <- date8
    InvDatCMP17          <- date8
    InvReaCodCMP18       <- alphaNumExactly(3)
    InvReaCMP19          <- alphaNum(350)
    InvReaCMP19LNG       <- alphaExactly(2)
    LiaLibDatCMP21       <- date8
    LimValCMP22          <- num(1)
    ResUseCMP23          <- num(1)
    NotValForECCMG2      <- num(1)
    NotValForOthConPVLD2 <- alphaExactly(2)
  } yield guaComGuaGds2Fields ++ Json.obj(
    "RefAmoCMP11"          -> RefAmoCMP11,
    "PerOfRefAmoCMP12"     -> PerOfRefAmoCMP12,
    "GuaAmoCMP13"          -> GuaAmoCMP13,
    "CurCMP14"             -> CurCMP14,
    "NumOfCerCMP15"        -> NumOfCerCMP15,
    "ValDatCMP16"          -> ValDatCMP16,
    "InvDatCMP17"          -> InvDatCMP17,
    "InvReaCodCMP18"       -> InvReaCodCMP18,
    "InvReaCMP19"          -> InvReaCMP19,
    "InvReaCMP19LNG"       -> InvReaCMP19LNG,
    "LiaLibDatCMP21"       -> LiaLibDatCMP21,
    "LimValCMP22"          -> LimValCMP22,
    "ResUseCMP23"          -> ResUseCMP23,
    "NotValForECCMG2"      -> NotValForECCMG2,
    "NotValForOthConPVLD2" -> NotValForOthConPVLD2
  )

  val guaGrtGds3FieldsGen: ArgGen = for {
    ComCodGDS311    <- alphaNum(22)
    SenGooGDS314    <- num(2)
    GooDesGDS312    <- alphaNum(280)
    GooDesGDS312LNG <- alphaExactly(2)
  } yield Json.obj(
    "ComCodGDS311"    -> ComCodGDS311,
    "SenGooGDS314"    -> SenGooGDS314,
    "GooDesGDS312"    -> GooDesGDS312,
    "GooDesGDS312LNG" -> GooDesGDS312LNG
  )

  val indGuaByGnrFieldsGen: ArgGen = for {
    guaGrtGds3Fields    <- guaGrtGds3FieldsGen
    GuaAmoGNR11         <- (num1(13), num(2)).mapN(_ + "." + _)
    CurGNR12            <- alphaNumExactly(3)
    RefNumEPT22         <- alphaNumExactly(8)
    RefNumEST22         <- alphaExactly(8)
    NotValForECVLD1     <- num(1)
    NoValForOthConPMIT2 <- alphaExactly(2)
  } yield guaGrtGds3Fields ++ Json.obj(
    "GuaAmoGNR11"         -> GuaAmoGNR11,
    "CurGNR12"            -> CurGNR12,
    "RefNumEPT22"         -> RefNumEPT22,
    "RefNumEST22"         -> RefNumEST22,
    "NotValForECVLD1"     -> NotValForECVLD1,
    "NoValForOthConPMIT2" -> NoValForOthConPMIT2
  )

  val indGuaVouFieldsGen: ArgGen = for {
    DatOfIssVOU745       <- date8
    ExpDatVOU746         <- date8
    HarCopGivToPriVOU747 <- num(1)
    LimValVOU748         <- num(1)
    TIRCarVOU749         <- num(1)
    VouAmoVOU750         <- (num1(13), num(2)).mapN(_ + "." + _)
    CurVOU751            <- alphaNumExactly(3)
  } yield Json.obj(
    "DatOfIssVOU745"       -> DatOfIssVOU745,
    "ExpDatVOU746"         -> ExpDatVOU746,
    "HarCopGivToPriVOU747" -> HarCopGivToPriVOU747,
    "LimValVOU748"         -> LimValVOU748,
    "TIRCarVOU749"         -> TIRCarVOU749,
    "VouAmoVOU750"         -> VouAmoVOU750,
    "CurVOU751"            -> CurVOU751
  )

  val guaRef2FieldsGen: ArgGen = for {
    guaQueFields      <- guaQueFieldsGen
    traPrioTgFields   <- traPrioTgFieldsGen
    usaUsaFields      <- usaUsaFieldsGen
    expExpFields      <- expExpFieldsGen
    guarnrFields      <- guarnrFieldsGen
    comGuaCmpFields   <- comGuaCmpFieldsGen
    indGuaByGnrFields <- indGuaByGnrFieldsGen
    indGuaVouFields   <- indGuaVouFieldsGen
    GuaRefNumGRNREF21 <- alphaNum(24)
    AccDatREF24       <- date8
    GuaTypREF22       <- alphaNumExactly(1)
    GuaMonCodREF23    <- num(1)
  } yield guaQueFields ++ traPrioTgFields ++ usaUsaFields ++ expExpFields ++ guarnrFields ++ comGuaCmpFields ++ indGuaByGnrFields ++ indGuaVouFields ++ Json
    .obj(
      "GuaRefNumGRNREF21" -> GuaRefNumGRNREF21,
      "AccDatREF24"       -> AccDatREF24,
      "GuaTypREF22"       -> GuaTypREF22,
      "GuaMonCodREF23"    -> GuaMonCodREF23
    )

  val cd034aGen: ArgGen = for {
    commonFields  <- phase4CommonFieldsGen("GUA")
    guaRef2Fields <- guaRef2FieldsGen
    TINRC159      <- alphaNum(17)
    CouCodGRT764  <- alphaExactly(2)
    RefNumEPR1    <- alphaNumExactly(8)
    RefNumSIM1    <- alphaNumExactly(8)
    RefNumRCV1    <- alphaNumExactly(8)
    AccCodCOD729 <- alphaExactly(4)
  } yield commonFields ++ guaRef2Fields ++ Json.obj(
    "TINRC159"     -> TINRC159,
    "CouCodGRT764" -> CouCodGRT764,
    "RefNumEPR1"   -> RefNumEPR1,
    "RefNumSIM1"   -> RefNumSIM1,
    "RefNumRCV1"   -> RefNumRCV1,
    "AccCodCOD729" -> AccCodCOD729
  )

  val cd037aGen: ArgGen = for {
    commonFields  <- phase4CommonFieldsGen("GUA")
    guaRef2Fields <- guaRef2FieldsGen
    TINRC159      <- alphaNum(17)
    RefNumRNT1    <- alphaNumExactly(8)
    RefNumEPR1    <- alphaNumExactly(8)
    RefNumSIM1    <- alphaNumExactly(8)
    RefNumRCV1    <- alphaNumExactly(8)
  } yield commonFields ++ guaRef2Fields ++ Json.obj(
    "TINRC159"   -> TINRC159,
    "RefNumRNT1" -> RefNumRNT1,
    "RefNumEPR1" -> RefNumEPR1,
    "RefNumSIM1" -> RefNumSIM1,
    "RefNumRCV1" -> RefNumRCV1
  )
}
