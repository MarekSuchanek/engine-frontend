module Wizard.Projects.Create.CustomCreate.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Form
import Form.Field as Field
import Gettext exposing (gettext)
import Result exposing (Result)
import Shared.Api.KnowledgeModels as KnowledgeModelsApi
import Shared.Api.Packages as PackagesApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.PackageSuggestion exposing (PackageSuggestion)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Utils exposing (withNoCmd)
import String.Extra as String
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Msgs
import Wizard.Projects.Common.QuestionnaireCustomCreateForm as QuestionnaireCreateForm
import Wizard.Projects.Create.CustomCreate.Models exposing (Model)
import Wizard.Projects.Create.CustomCreate.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Model -> Cmd Msg
fetchData appState model =
    case model.selectedPackage of
        Just packageId ->
            KnowledgeModelsApi.fetchPreview (Just packageId) [] [] appState GetKnowledgeModelPreviewCompleted

        Nothing ->
            Cmd.none


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        AddTag tagUuid ->
            handleAddTag model tagUuid

        RemoveTag tagUuid ->
            handleRemoveTag model tagUuid

        GetKnowledgeModelPreviewCompleted result ->
            handleGetKnowledgeModelPreviewCompleted appState model result

        FormMsg formMsg ->
            handleForm wrapMsg formMsg appState model

        PostQuestionnaireCompleted result ->
            handlePostQuestionnaireCompleted appState model result

        PackageTypeHintInputMsg typeHintInputMsg ->
            handlePackageTypeHintInputMsg wrapMsg typeHintInputMsg appState model



-- Handlers


handleAddTag : Model -> String -> ( Model, Cmd Wizard.Msgs.Msg )
handleAddTag model tagUuid =
    withNoCmd <|
        { model | selectedTags = tagUuid :: model.selectedTags }


handleRemoveTag : Model -> String -> ( Model, Cmd Wizard.Msgs.Msg )
handleRemoveTag model tagUuid =
    withNoCmd <|
        { model | selectedTags = List.filter (\t -> t /= tagUuid) model.selectedTags }


handleGetKnowledgeModelPreviewCompleted : AppState -> Model -> Result ApiError KnowledgeModel -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetKnowledgeModelPreviewCompleted appState model result =
    let
        newModel =
            case result of
                Ok knowledgeModel ->
                    { model | knowledgeModelPreview = Success knowledgeModel }

                Err error ->
                    { model | knowledgeModelPreview = ApiError.toActionResult appState (gettext "Unable to get question tags for the Knowledge Model." appState.locale) error }

        cmd =
            getResultCmd Wizard.Msgs.logoutMsg result
    in
    ( newModel, cmd )


handleForm : (Msg -> Wizard.Msgs.Msg) -> Form.Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleForm wrapMsg formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just form ) ->
            let
                body =
                    QuestionnaireCreateForm.encode model.selectedTags form

                cmd =
                    Cmd.map wrapMsg <|
                        QuestionnairesApi.postQuestionnaire body appState PostQuestionnaireCompleted
            in
            ( { model | savingQuestionnaire = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update QuestionnaireCreateForm.validation formMsg model.form }
            in
            case getSelectedPackageId newModel of
                Just packageId ->
                    if needFetchKnowledgeModelPreview model packageId then
                        ( { newModel
                            | lastFetchedPreview = Just packageId
                            , knowledgeModelPreview = Loading
                            , selectedTags = []
                          }
                        , Cmd.map wrapMsg <|
                            KnowledgeModelsApi.fetchPreview (Just packageId) [] [] appState GetKnowledgeModelPreviewCompleted
                        )

                    else
                        ( newModel, Cmd.none )

                Nothing ->
                    ( { newModel | knowledgeModelPreview = Unset, selectedTags = [] }, Cmd.none )


handlePostQuestionnaireCompleted : AppState -> Model -> Result ApiError Questionnaire -> ( Model, Cmd Wizard.Msgs.Msg )
handlePostQuestionnaireCompleted appState model result =
    case result of
        Ok questionnaire ->
            ( model
            , cmdNavigate appState <| Routes.projectsDetailQuestionnaire questionnaire.uuid
            )

        Err error ->
            ( { model | savingQuestionnaire = ApiError.toActionResult appState (gettext "Questionnaire could not be created." appState.locale) error }
            , getResultCmd Wizard.Msgs.logoutMsg result
            )


handlePackageTypeHintInputMsg : (Msg -> Wizard.Msgs.Msg) -> TypeHintInput.Msg PackageSuggestion -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handlePackageTypeHintInputMsg wrapMsg typeHintInputMsg appState model =
    let
        formMsg =
            wrapMsg << FormMsg << Form.Input "packageId" Form.Select << Field.String

        cfg =
            { wrapMsg = wrapMsg << PackageTypeHintInputMsg
            , getTypeHints = PackagesApi.getPackagesSuggestions
            , getError = gettext "Unable to get Knowledge Models." appState.locale
            , setReply = formMsg << .id
            , clearReply = Just <| formMsg ""
            , filterResults = Nothing
            }

        ( packageTypeHintInputModel, cmd ) =
            TypeHintInput.update cfg typeHintInputMsg appState model.packageTypeHintInputModel
    in
    ( { model | packageTypeHintInputModel = packageTypeHintInputModel }, cmd )



-- Helpers


getSelectedPackageId : Model -> Maybe String
getSelectedPackageId model =
    Maybe.andThen String.toMaybe (Form.getFieldAsString "packageId" model.form).value


needFetchKnowledgeModelPreview : Model -> String -> Bool
needFetchKnowledgeModelPreview model packageId =
    model.lastFetchedPreview /= Just packageId
