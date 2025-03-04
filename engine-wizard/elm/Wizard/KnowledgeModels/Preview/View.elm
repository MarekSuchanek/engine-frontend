module Wizard.KnowledgeModels.Preview.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Shared.Data.Package exposing (Package)
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.ActionResultView as ActionResultView
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.Questionnaire.DefaultQuestionnaireRenderer as DefaultQuestionnaireRenderer
import Wizard.Common.Feature as Features
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.Page as Page
import Wizard.KnowledgeModels.Preview.Models exposing (Model)
import Wizard.KnowledgeModels.Preview.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewProject appState model) model.questionnaireModel


viewProject : AppState -> Model -> Questionnaire.Model -> Html Msg
viewProject appState model questionnaireModel =
    let
        questionnaire =
            Questionnaire.view appState
                { features =
                    { feedbackEnabled = False
                    , todosEnabled = False
                    , commentsEnabled = False
                    , readonly = True
                    , toolbarEnabled = False
                    }
                , renderer = DefaultQuestionnaireRenderer.create appState questionnaireModel.questionnaire.knowledgeModel
                , wrapMsg = QuestionnaireMsg
                , previewQuestionnaireEventMsg = Nothing
                , revertQuestionnaireMsg = Nothing
                }
                { events = [] }
                questionnaireModel
    in
    div [ class "KnowledgeModels__Preview" ]
        [ viewHeader appState model questionnaireModel.questionnaire.package
        , questionnaire
        ]


viewHeader : AppState -> Model -> Package -> Html Msg
viewHeader appState model package =
    let
        actions =
            if Features.projectsCreateCustom appState then
                let
                    cfg =
                        { label = gettext "Create project" appState.locale
                        , result = model.creatingQuestionnaire
                        , msg = CreateProjectMsg
                        , dangerous = False
                        }
                in
                [ ActionResultView.error model.creatingQuestionnaire
                , ActionButton.button appState cfg
                ]

            else
                []
    in
    div [ class "top-header" ]
        [ div [ class "top-header-content" ]
            [ div [ class "top-header-title", dataCy "km-preview_header_title" ] [ text <| package.name ++ ", " ++ Version.toString package.version ]
            , div [ class "top-header-actions" ] actions
            ]
        ]
