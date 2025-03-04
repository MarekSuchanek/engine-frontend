module Wizard.Projects.Detail.Components.ShareModal exposing
    ( Model
    , Msg
    , UpdateConfig
    , init
    , openMsg
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Form.Field as Field
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, hr, input, span, strong, text)
import Html.Attributes exposing (class, classList, id, readonly, title, value)
import Html.Events exposing (onClick)
import List.Extra as List
import Random exposing (Seed)
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Api.Users as UsersApi
import Shared.Copy as Copy
import Shared.Data.Member as Member
import Shared.Data.Permission exposing (Permission)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnairePermission as QuestionnairePermission
import Shared.Data.User as User
import Shared.Data.UserSuggestion exposing (UserSuggestion)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Utils exposing (getUuid)
import String.Format as String
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Common.Components.TypeHintInput.TypeHintItem as TypeHintInput
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.UserIcon as UserIcon
import Wizard.Ports as Ports
import Wizard.Projects.Common.QuestionnaireEditForm as QuestionnaireEditForm exposing (QuestionnaireEditForm)
import Wizard.Projects.Common.QuestionnaireEditFormMemberPerms as QuestionnaireEditFormMemberPerms
import Wizard.Projects.Detail.ProjectDetailRoute as ProjectDetailRoute
import Wizard.Projects.Routes as Routes
import Wizard.Routes as Routes
import Wizard.Routing as Routing



-- MODEL


type alias Model =
    { visible : Bool
    , savingSharing : ActionResult String
    , questionnaireEditForm : Form FormError QuestionnaireEditForm
    , questionnaireUuid : Uuid
    , userTypeHintInputModel : TypeHintInput.Model UserSuggestion
    , users : List UserSuggestion
    }


init : AppState -> Model
init appState =
    { visible = False
    , savingSharing = Unset
    , questionnaireEditForm = QuestionnaireEditForm.initEmpty appState
    , questionnaireUuid = Uuid.nil
    , userTypeHintInputModel = TypeHintInput.init "memberId"
    , users = []
    }


setQuestionnaire : AppState -> QuestionnaireDetail -> Model -> Model
setQuestionnaire appState questionnaire model =
    { model
        | questionnaireEditForm = QuestionnaireEditForm.init appState questionnaire
        , questionnaireUuid = questionnaire.uuid
        , users = List.map (.member >> Member.toUserSuggestion) questionnaire.permissions
    }



-- UPDATE


type Msg
    = Open QuestionnaireDetail
    | Close
    | UserTypeHintInputMsg (TypeHintInput.Msg UserSuggestion)
    | AddUser UserSuggestion
    | FormMsg Form.Msg
    | PutQuestionnaireComplete (Result ApiError ())
    | CopyPublicLink String


openMsg : QuestionnaireDetail -> Msg
openMsg =
    Open


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , questionnaireUuid : Uuid
    , permissions : List Permission
    }


update : UpdateConfig msg -> Msg -> AppState -> Model -> ( Seed, Model, Cmd msg )
update cfg msg appState model =
    let
        withSeed ( m, c ) =
            ( appState.seed, m, c )
    in
    case msg of
        Open questionnaire ->
            withSeed ( setQuestionnaire appState questionnaire { model | visible = True }, Cmd.none )

        Close ->
            withSeed ( { model | visible = False }, Cmd.none )

        UserTypeHintInputMsg typeHintInputMsg ->
            withSeed <| handleUserTypeHintInputMsg cfg typeHintInputMsg appState model

        AddUser user ->
            handleAddUser appState model user

        FormMsg formMsg ->
            withSeed <| handleFormMsg cfg formMsg appState model

        PutQuestionnaireComplete result ->
            withSeed <| handlePutQuestionnaireComplete appState model result

        CopyPublicLink publicLink ->
            withSeed ( model, Copy.copyToClipboard publicLink )


handleUserTypeHintInputMsg : UpdateConfig msg -> TypeHintInput.Msg UserSuggestion -> AppState -> Model -> ( Model, Cmd msg )
handleUserTypeHintInputMsg cfg typeHintInputMsg appState model =
    let
        projectUsersUuids =
            QuestionnaireEditForm.getUserUuids model.questionnaireEditForm

        filterResults user =
            not <| List.member (Uuid.toString user.uuid) projectUsersUuids

        typeHintInputCfg =
            { wrapMsg = cfg.wrapMsg << UserTypeHintInputMsg
            , getTypeHints = UsersApi.getUsersSuggestions
            , getError = gettext "Unable to get users." appState.locale
            , setReply = cfg.wrapMsg << AddUser
            , clearReply = Nothing
            , filterResults = Just filterResults
            }

        ( userTypeHintInputModel, cmd ) =
            TypeHintInput.update typeHintInputCfg typeHintInputMsg appState model.userTypeHintInputModel
    in
    ( { model | userTypeHintInputModel = userTypeHintInputModel }, cmd )


handleAddUser : AppState -> Model -> UserSuggestion -> ( Seed, Model, Cmd msg )
handleAddUser appState model user =
    let
        userTypeHintInputModel =
            TypeHintInput.clear model.userTypeHintInputModel

        permissionsLength =
            List.length <| Form.getListIndexes "permissions" model.questionnaireEditForm

        formUpdate =
            Form.update (QuestionnaireEditForm.validation appState)

        createInputMessage field value =
            Form.Input field Form.Text (Field.String value)

        ( newUuid, newSeed ) =
            getUuid appState.seed

        msgs =
            [ Form.Append "permissions"
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".uuid") (Uuid.toString newUuid)
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".questionnaireUuid") (Uuid.toString model.questionnaireUuid)
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".member.uuid") (Uuid.toString user.uuid)
            , createInputMessage ("permissions." ++ String.fromInt permissionsLength ++ ".perms") (QuestionnaireEditFormMemberPerms.toString QuestionnaireEditFormMemberPerms.Viewer)
            ]

        newForm =
            List.foldl formUpdate model.questionnaireEditForm msgs
    in
    ( newSeed
    , { model
        | userTypeHintInputModel = userTypeHintInputModel
        , questionnaireEditForm = newForm
        , users = List.uniqueBy (Uuid.toString << .uuid) (user :: model.users)
      }
    , Cmd.none
    )


handleFormMsg : UpdateConfig msg -> Form.Msg -> AppState -> Model -> ( Model, Cmd msg )
handleFormMsg cfg formMsg appState model =
    case ( formMsg, Form.getOutput model.questionnaireEditForm ) of
        ( Form.Submit, Just form ) ->
            let
                body =
                    QuestionnaireEditForm.encode form

                cmd =
                    Cmd.map cfg.wrapMsg <|
                        QuestionnairesApi.putQuestionnaire cfg.questionnaireUuid body appState PutQuestionnaireComplete
            in
            ( { model | savingSharing = Loading }
            , cmd
            )

        _ ->
            ( { model | questionnaireEditForm = Form.update (QuestionnaireEditForm.validation appState) formMsg model.questionnaireEditForm }
            , Cmd.none
            )


handlePutQuestionnaireComplete : AppState -> Model -> Result ApiError () -> ( Model, Cmd msg )
handlePutQuestionnaireComplete appState model result =
    case result of
        Ok _ ->
            ( { model | visible = False, savingSharing = Unset }, Ports.refresh () )

        Err error ->
            ( { model | savingSharing = ApiError.toActionResult appState (gettext "Questionnaire could not be saved." appState.locale) error }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map UserTypeHintInputMsg <|
        TypeHintInput.subscriptions model.userTypeHintInputModel



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    let
        modalContent =
            [ usersView appState model
            , formView appState model.questionnaireUuid model.questionnaireEditForm
            ]

        modalConfig =
            { modalTitle = gettext "Share Project" appState.locale
            , modalContent = modalContent
            , visible = model.visible
            , actionResult = model.savingSharing
            , actionName = gettext "Save" appState.locale
            , actionMsg = FormMsg Form.Submit
            , cancelMsg = Just Close
            , dangerous = False
            , dataCy = "project-share"
            }
    in
    Modal.confirm appState modalConfig


usersView : AppState -> Model -> Html Msg
usersView appState model =
    let
        cfg =
            { viewItem = TypeHintInput.memberSuggestion
            , wrapMsg = UserTypeHintInputMsg
            , nothingSelectedItem = span [ class "text-muted" ] [ text <| gettext "Add users" appState.locale ]
            , clearEnabled = False
            }

        typeHintInput =
            TypeHintInput.view appState cfg model.userTypeHintInputModel False

        separator =
            if appState.config.questionnaire.questionnaireVisibility.enabled || appState.config.questionnaire.questionnaireSharing.enabled then
                hr [] []

            else
                emptyNode
    in
    div [ class "ShareModal__Users" ]
        [ strong [] [ text (gettext "Users" appState.locale) ]
        , typeHintInput
        , Html.map FormMsg <| FormGroup.viewList appState (userView appState model.users) model.questionnaireEditForm "permissions" ""
        , separator
        ]


userView : AppState -> List UserSuggestion -> Form FormError QuestionnaireEditForm -> Int -> Html Form.Msg
userView appState users form i =
    let
        memberUuid =
            (Form.getFieldAsString ("permissions." ++ String.fromInt i ++ ".member.uuid") form).value

        mbUser =
            List.find (.uuid >> Uuid.toString >> Just >> (==) memberUuid) users
    in
    case mbUser of
        Just user ->
            let
                roleOptions =
                    QuestionnaireEditFormMemberPerms.formOptions appState

                roleSelect =
                    FormExtra.inlineSelect roleOptions form ("permissions." ++ String.fromInt i ++ ".perms")
            in
            div [ class "user-row" ]
                [ div []
                    [ UserIcon.viewSmall user
                    , text <| User.fullName user
                    ]
                , div []
                    [ roleSelect
                    , a
                        [ class "text-danger"
                        , onClick (Form.RemoveItem "permissions" i)
                        , title (gettext "Remove" appState.locale)
                        ]
                        [ faSet "_global.remove" appState ]
                    ]
                ]

        Nothing ->
            emptyNode


formView : AppState -> Uuid -> Form FormError QuestionnaireEditForm -> Html Msg
formView appState questionnaireUuid form =
    let
        visibilityInputs =
            if appState.config.questionnaire.questionnaireVisibility.enabled then
                let
                    visibilitySelect =
                        if (Form.getFieldAsString "sharingPermission" form).value == Just "edit" then
                            strong [] [ text (gettext "edit" appState.locale) ]

                        else
                            FormExtra.inlineSelect (QuestionnairePermission.formOptions appState) form "visibilityPermission"

                    visibilityEnabled =
                        Maybe.withDefault False (Form.getFieldAsBool "visibilityEnabled" form).value

                    visibilityPermissionInput =
                        div
                            [ class "form-group form-group-toggle-extra"
                            , classList [ ( "visible", visibilityEnabled ) ]
                            ]
                            (String.formatHtml
                                (gettext "Other logged-in users can %s the project." appState.locale)
                                [ visibilitySelect ]
                            )

                    visibilityEnabledInput =
                        FormGroup.toggle form "visibilityEnabled" (gettext "Visible by all other logged-in users" appState.locale)
                in
                [ Html.map FormMsg visibilityEnabledInput
                , Html.map FormMsg visibilityPermissionInput
                ]

            else
                []

        sharingInputs =
            if appState.config.questionnaire.questionnaireSharing.enabled then
                let
                    publicLink =
                        appState.clientUrl ++ Routing.toUrl appState (Routes.ProjectsRoute (Routes.DetailRoute questionnaireUuid ProjectDetailRoute.Questionnaire))

                    sharingEnabled =
                        Maybe.withDefault False (Form.getFieldAsBool "sharingEnabled" form).value

                    publicLinkView =
                        div
                            [ class "form-group form-group-toggle-extra"
                            , classList [ ( "visible", sharingEnabled ) ]
                            ]
                            [ div [ class "d-flex" ]
                                [ input [ readonly True, class "form-control", id "public-link", value publicLink ] []
                                , button [ class "btn btn-link text-nowrap", onClick (CopyPublicLink publicLink) ] [ text (gettext "Copy link" appState.locale) ]
                                ]
                            ]

                    sharingSelect =
                        FormExtra.inlineSelect (QuestionnairePermission.formOptions appState) form "sharingPermission"

                    sharingPermissionInput =
                        div
                            [ class "form-group form-group-toggle-extra"
                            , classList [ ( "visible", sharingEnabled ) ]
                            ]
                            (String.formatHtml
                                (gettext "Anyone with the link can %s the project." appState.locale)
                                [ sharingSelect ]
                            )

                    sharingEnabledInput =
                        FormGroup.toggle form "sharingEnabled" (gettext "Public link" appState.locale)
                in
                [ Html.map FormMsg sharingEnabledInput
                , Html.map FormMsg sharingPermissionInput
                , publicLinkView
                ]

            else
                []
    in
    div []
        (visibilityInputs ++ sharingInputs)
