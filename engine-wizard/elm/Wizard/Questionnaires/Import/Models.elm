module Wizard.Questionnaires.Import.Models exposing
    ( MaDMP
    , MaDMP_Contributor
    , MaDMP_Dataset
    , MaDMP_Distribution
    , MaDMP_Funding
    , MaDMP_Identifier
    , MaDMP_License
    , MaDMP_Project
    , Model
    , decoder
    , dropzoneId
    , fileInputId
    , initialModel
    , supportedPackageId
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
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
    , questionnaireUuid : Maybe String
    , questionnaire : Maybe QuestionnaireDetail
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
supportedPackageId =
    "dsw:root:2.4.0"


dropzoneId : String
dropzoneId =
    "madmp-import-dropzone"


fileInputId : String
fileInputId =
    "madmp-import-input"


type alias MaDMP =
    { dmp : MaDMP_DMP }


type alias MaDMP_DMP =
    { title : String
    , description : String
    , language : String
    , contributors : List MaDMP_Contributor
    , projects : List MaDMP_Project
    , datasets : List MaDMP_Dataset
    }


type alias MaDMP_Project =
    { title : String
    , description : String
    , start : String
    , end : String
    , funding : List MaDMP_Funding
    }


type alias MaDMP_Funding =
    { funder_id : MaDMP_Identifier
    , funding_status : String
    , grant_id : MaDMP_Identifier
    }


type alias MaDMP_Contributor =
    { contributorId : MaDMP_Identifier
    , mbox : String
    , name : String
    , roles : List String
    }


type alias MaDMP_Dataset =
    { datasetId : MaDMP_Identifier
    , title : String
    , description : String
    , personalData : String
    , sensitiveData : String
    , distributions : List MaDMP_Distribution
    }


type alias MaDMP_Distribution =
    { title : String
    , dataAccess : String
    , licenses : List MaDMP_License
    }


type alias MaDMP_License =
    { licenseRef : String
    , startDate : String
    }


type alias MaDMP_Identifier =
    { identifier : String
    , idType : String
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
        |> D.optional "language" D.string "eng"
        |> D.optional "contributor" (D.list contributorDecoder) []
        |> D.optional "project" (D.list projectDecoder) []
        |> D.optional "dataset" (D.list datasetDecoder) []


contributorDecoder : Decoder MaDMP_Contributor
contributorDecoder =
    D.succeed MaDMP_Contributor
        |> D.required "contributor_id" identifierDecoder
        |> D.optional "mbox" D.string ""
        |> D.optional "name" D.string ""
        |> D.optional "role" (D.list D.string) []


identifierDecoder : Decoder MaDMP_Identifier
identifierDecoder =
    D.succeed MaDMP_Identifier
        |> D.optional "identifier" D.string ""
        |> D.optional "type" D.string "other"


projectDecoder : Decoder MaDMP_Project
projectDecoder =
    D.succeed MaDMP_Project
        |> D.optional "title" D.string ""
        |> D.optional "description" D.string ""
        |> D.optional "start" D.string ""
        |> D.optional "end" D.string ""
        |> D.optional "funding" (D.list fundingDecoder) []


fundingDecoder : Decoder MaDMP_Funding
fundingDecoder =
    D.succeed MaDMP_Funding
        |> D.required "funder_id" identifierDecoder
        |> D.optional "funding_status" D.string ""
        |> D.required "grant_id" identifierDecoder


datasetDecoder : Decoder MaDMP_Dataset
datasetDecoder =
    D.succeed MaDMP_Dataset
        |> D.required "dataset_id" identifierDecoder
        |> D.optional "title" D.string ""
        |> D.optional "description" D.string ""
        |> D.optional "personal_data" D.string "unknown"
        |> D.optional "sensitive_data" D.string "unknown"
        |> D.optional "distribution" (D.list distributionDecoder) []


distributionDecoder : Decoder MaDMP_Distribution
distributionDecoder =
    D.succeed MaDMP_Distribution
        |> D.optional "title" D.string ""
        |> D.optional "data_access" D.string ""
        |> D.optional "license" (D.list licenseDecoder) []


licenseDecoder : Decoder MaDMP_License
licenseDecoder =
    D.succeed MaDMP_License
        |> D.optional "license_ref" D.string ""
        |> D.optional "start_date" D.string ""
