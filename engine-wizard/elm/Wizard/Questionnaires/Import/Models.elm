module Wizard.Questionnaires.Import.Models exposing
    ( Model
    , MaDMP
    , MaDMP_Project
    , MaDMP_Contributor
    , MaDMP_Funding
    , dropzoneId
    , fileInputId
    , initialModel
    , decoder
    , supportedPackageId
    )

import Form exposing (Form)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import ActionResult exposing (ActionResult(..))
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Ports as Ports exposing (FilePortData)
import Wizard.Questionnaires.Common.QuestionnaireCreateForm as QuestionnaireCreateForm exposing (QuestionnaireCreateForm)
import Wizard.Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)

type alias Model =
    { dnd : Int
    , file : Maybe FilePortData
    , madmp : Maybe MaDMP
    , importing : ActionResult String
    , form : Form CustomFormError QuestionnaireCreateForm
    , savingQuestionnaire : ActionResult String
    , questionnaireUuid: Maybe String
    , questionnaire: Maybe QuestionnaireDetail
    }


initialModel : Model
initialModel =
    { dnd = 0
    , file = Nothing
    , madmp = Nothing
    , importing = Unset
    , savingQuestionnaire = Unset
    , form = QuestionnaireCreateForm.init (Just supportedPackageId)
    , questionnaireUuid = Nothing
    , questionnaire = Nothing
    }


supportedPackageId : String
supportedPackageId = "dsw:root:2.4.0"


dropzoneId : String
dropzoneId =
    "madmp-import-dropzone"


fileInputId : String
fileInputId =
    "madmp-import-input"


type alias MaDMP = { dmp: MaDMP_DMP }

type alias MaDMP_DMP =
    { title: String
    , description: String
    , language: String
    , contributors: List MaDMP_Contributor
    , projects: List MaDMP_Project
    }

type alias MaDMP_Project =
    { title: String
    , description: String
    , start: String
    , end: String
    , funding: List MaDMP_Funding
    }

type alias MaDMP_Funding =
    { funder_id: MaDMP_Identifier
    , funding_status: String
    , grant_id: MaDMP_Identifier
    }

type alias MaDMP_Contributor =
    { contributorId: MaDMP_Identifier
    , mbox: String
    , name: String
    , roles: List String
    }

type alias MaDMP_Identifier =
    { identifier: String
    , idType: String
    }


decoder : Decoder MaDMP
decoder =
    D.succeed MaDMP
        |> D.required "dmp" dmpDecoder


dmpDecoder : Decoder MaDMP_DMP
dmpDecoder =
    D.succeed MaDMP_DMP
        |> D.required "title" D.string
        |> D.optional "description" D.string ""
        |> D.required "language" D.string
        |> D.optional "contributor" (D.list contributorDecoder) []
        |> D.optional "project" (D.list projectDecoder) []


contributorDecoder : Decoder MaDMP_Contributor
contributorDecoder =
    D.succeed MaDMP_Contributor
        |> D.required "contributor_id" identifierDecoder
        |> D.optional "mbox" D.string ""
        |> D.required "name" D.string
        |> D.required "role" (D.list D.string)


identifierDecoder : Decoder MaDMP_Identifier
identifierDecoder =
    D.succeed MaDMP_Identifier
        |> D.required "identifier" D.string
        |> D.required "type" D.string


projectDecoder : Decoder MaDMP_Project
projectDecoder =
    D.succeed MaDMP_Project
        |> D.required "title" D.string
        |> D.optional "description" D.string ""
        |> D.required "start" D.string
        |> D.required "end" D.string
        |> D.optional "funding" (D.list fundingDecoder) []


fundingDecoder : Decoder MaDMP_Funding
fundingDecoder =
    D.succeed MaDMP_Funding
        |> D.required "funder_id" identifierDecoder
        |> D.optional "funding_status" D.string ""
        |> D.required "grant_id" identifierDecoder
