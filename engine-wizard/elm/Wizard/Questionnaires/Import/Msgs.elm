module Wizard.Questionnaires.Import.Msgs exposing (Msg(..))

import Form
import Shared.Error.ApiError exposing (ApiError)
import Wizard.KnowledgeModels.Common.Package exposing (Package)
import Wizard.Ports as Ports exposing (FilePortData)
import Wizard.Questionnaires.Common.Questionnaire exposing (Questionnaire)
import Wizard.Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)


type Msg
    = FormMsg Form.Msg
    | GetPackagesCompleted (Result ApiError (List Package))
    | DragEnter
    | DragOver
    | DragLeave
    | FileSelected
    | FileRead FilePortData
    | Submit
    | Cancel
    | PostQuestionnaireCompleted (Result ApiError Questionnaire)
    | GetQuestionnaireCompleted (Result ApiError QuestionnaireDetail)
    | ImportQuestionnaireCompleted (Result ApiError ())
