module Wizard.Questionnaires.Import.View exposing (view)

import Form exposing (Form)
import ActionResult exposing (ActionResult(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lx, lg)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionResult
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Questionnaires.Common.QuestionnaireAccessibility as QuestionnaireAccessibility
import Wizard.Questionnaires.Import.Models exposing (..)
import Wizard.Questionnaires.Import.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Routes exposing (Route(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Questionnaires.Import.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Questionnaires.Import.View"


view : AppState -> Model -> Html Msg
view appState model =
    let
        content = case model.madmp of
            Just madmp ->
                createContentView appState model madmp

            _ ->
                fileInputView appState model
    in
    div [ detailClass "Questionnaires__Import" ]
        [ Page.header (l_ "header.title" appState) []
        , content
        ]


fileInputView : AppState -> Model -> Html Msg
fileInputView appState model =
    let
        content = dropzoneView appState model
    in
    div []
        [ p [] [text "Select you maDMP JSON file to import according questionnaire."]
        , hr [] []
        , content
        ]


createContentView : AppState -> Model -> MaDMP -> Html Msg
createContentView appState model madmp =
    div []
        [ FormResult.view appState model.savingQuestionnaire
        , formView appState model madmp |> Html.map FormMsg
        , FormActions.view appState
            (Routes.QuestionnairesRoute ImportRoute)
            (ActionResult.ButtonConfig (l_ "header.save" appState) model.savingQuestionnaire (FormMsg Form.Submit) False)
        , madmpContentView appState model
        ]



formView : AppState -> Model -> MaDMP -> Html Form.Msg
formView appState model madmp =
    let
        parentInput = FormGroup.codeView supportedPackageId

        accessibilitySelect =
            if appState.config.questionnaires.questionnaireAccessibility.enabled then
                FormGroup.richRadioGroup appState (QuestionnaireAccessibility.formOptions appState) model.form "accessibility" <| lg "questionnaire.accessibility" appState

            else
                emptyNode

        formHtml =
            div []
                [ FormGroup.input appState model.form "name" <| lg "questionnaire.name" appState
                , parentInput <| lg "knowledgeModel" appState
                , accessibilitySelect
                ]
    in
    formHtml


madmpContentView : AppState -> Model -> Html Msg
madmpContentView appState model =
    case model.madmp of
        Just madmp ->
            div [ ]
                [ h3 [] [ text "maDMP import preview" ]
                    , table [ class "table table-striped" ]
                    [ tr []
                        [ th [] [ text "Title" ]
                        , td [] [ text madmp.dmp.title ]
                        ]
                    , tr []
                        [ th [] [ text "Description" ]
                        , td [] [ text madmp.dmp.description ]
                        ]
                    , tr []
                        [ th [] [ text "Language" ]
                        , td [] [ text madmp.dmp.language ]
                        ]
                    , tr []
                        [ th [] [ text "Contributors" ]
                        , td [] [ ul [] (List.map (\c -> li [] [ text c.name ]) madmp.dmp.contributors) ]
                        ]
                    , tr []
                        [ th [] [ text "Projects" ]
                        , td [] [ ul [] (List.map (\p -> li [] [ text p.title ]) madmp.dmp.projects) ]
                        ]
                    ]
                ]

        Nothing ->
            div [] []


dropzoneView : AppState -> Model -> Html Msg
dropzoneView appState model =
    let
        content =
            case model.file of
                Just file ->
                    fileView appState model file.filename

                Nothing ->
                    dropzone appState model
    in
    div [ class "Questionnaires__Import__FileImport", id dropzoneId ]
        [ FormResult.view appState model.importing
        , content
        ]

{- copy-paste from KM import -}
fileView : AppState -> Model -> String -> Html Msg
fileView appState model fileName =
    let
        cancelDisabled =
            case model.importing of
                Loading ->
                    True

                _ ->
                    False
    in
    div [ class "file-view" ]
        [ div [ class "file" ]
            [ faSet "kmImport.file" appState
            , div [ class "filename" ]
                [ text fileName ]
            ]
        , div [ class "actions" ]
            [ button [ disabled cancelDisabled, onClick Cancel, class "btn btn-secondary" ]
                [ lx_ "fileView.cancel" appState ]
            , ActionResult.button appState <| ActionResult.ButtonConfig (l_ "fileView.upload" appState) model.importing Submit False
            ]
        ]


dropzone : AppState -> Model -> Html Msg
dropzone appState model =
    div (dropzoneAttributes model)
        [ label [ class "btn btn-secondary btn-file" ]
            [ lx_ "dropzone.choose" appState
            , input [ id fileInputId, type_ "file", on "change" (Decode.succeed FileSelected) ] []
            ]
        , p [] [ lx_ "dropzone.drop" appState ]
        ]


dropzoneAttributes : Model -> List (Attribute Msg)
dropzoneAttributes model =
    let
        cssClass =
            case model.dnd of
                0 ->
                    ""

                _ ->
                    "active"
    in
    [ class ("dropzone " ++ cssClass)
    , id dropzoneId
    , onDragEvent "dragenter" DragEnter
    , onDragEvent "dragover" DragOver
    , onDragEvent "dragleave" DragLeave
    ]


onDragEvent : String -> Msg -> Attribute Msg
onDragEvent event msg =
    custom event <|
        Decode.succeed
            { stopPropagation = True
            , preventDefault = True
            , message = msg
            }

