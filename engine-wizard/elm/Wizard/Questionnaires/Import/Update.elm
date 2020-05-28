module Wizard.Questionnaires.Import.Update exposing (update, fetchData)

import ActionResult exposing (ActionResult(..))
import Dict exposing (Dict)
import Form exposing (Form)
import Form.Field as Field
import Json.Decode exposing (decodeString)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.Api.Packages as PackagesApi
import Wizard.Common.Api.Questionnaires as QuestionnairesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.FormEngine.Model exposing (FormValue, IntegrationReplyValue(..), ReplyValue(..))
import Wizard.Questionnaires.Common.Questionnaire exposing (Questionnaire)
import Wizard.Questionnaires.Common.QuestionnaireCreateForm as QuestionnaireCreateForm exposing (QuestionnaireCreateForm)
import Wizard.Questionnaires.Common.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Wizard.Questionnaires.Import.Models exposing (..)
import Wizard.Questionnaires.Import.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Routes exposing (Route(..))
import Wizard.Msgs
import Wizard.Ports exposing (FilePortData, createDropzone, fileSelected)
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Model -> Cmd Msg
fetchData appState model =
    let
        getPackagesCmd =
            PackagesApi.getPackages appState GetPackagesCompleted
    in
    Cmd.batch [ getPackagesCmd ]


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        DragEnter ->
            ( { model | dnd = model.dnd + 1 }, createDropzone dropzoneId )

        DragLeave ->
            ( { model | dnd = model.dnd - 1 }, Cmd.none )

        FileSelected ->
            ( model, fileSelected fileInputId )

        FileRead data ->
            ( { model | file = Just data }, Cmd.none )

        Submit ->
            handleSubmit wrapMsg appState model

        Cancel ->
            ( { model | file = Nothing, importing = Unset, dnd = 0 }, Cmd.none )

        FormMsg formMsg ->
            handleForm wrapMsg formMsg appState model

        PostQuestionnaireCompleted result ->
            handlePostQuestionnaireCompleted wrapMsg appState model result

        GetQuestionnaireCompleted result ->
            handleGetQuestionnaireCompleted wrapMsg appState model result

        ImportQuestionnaireCompleted result ->
            handleImportQuestionnaireCompleted appState model result

        _ ->
            ( model, Cmd.none )


handleSubmit : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleSubmit wrapMsg appState model =
    case model.file of
            Just file ->
                case decodeString decoder file.contents of
                    Ok madmp ->
                        ( updateModelFromMaDMP madmp model, Cmd.none )
                    Err err ->
                        ( { model | importing = Error "Invalid maDMP" }, Cmd.none )

            Nothing ->
                ( model, Cmd.none )


updateModelFromMaDMP : MaDMP -> Model -> Model
updateModelFromMaDMP madmp model =
    let
        formMsg field value =
            Form.Input field Form.Text <| Field.String value

        form =
            model.form
                |> Form.update QuestionnaireCreateForm.validation (formMsg "name" madmp.dmp.title)
    in
    { model | madmp = Just madmp, form = form }


handleForm : (Msg -> Wizard.Msgs.Msg) -> Form.Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleForm wrapMsg formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just form ) ->
            let
                body =
                    QuestionnaireCreateForm.encode [] form

                cmd =
                    Cmd.map wrapMsg <|
                        QuestionnairesApi.postQuestionnaire body appState PostQuestionnaireCompleted
            in
            ( { model | savingQuestionnaire = Loading }, cmd )

        _ ->
            ( { model | form = Form.update QuestionnaireCreateForm.validation formMsg model.form }, Cmd.none)



handleImportQuestionnaireCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handleImportQuestionnaireCompleted appState model result =
    case (result, model.questionnaire) of
        (Ok (), Just questionnaire) ->
            ( model
            , cmdNavigate appState <| Routes.QuestionnairesRoute <| DetailRoute questionnaire.uuid
            )

        (Err error, _) ->
            ( { model | savingQuestionnaire = ApiError.toActionResult (lg "apiError.questionnaires.putError" appState) error }
            , getResultCmd result
            )

        _ ->
            ( model, Cmd.none)


handlePostQuestionnaireCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError Questionnaire -> ( Model, Cmd Wizard.Msgs.Msg )
handlePostQuestionnaireCompleted wrapMsg appState model result =
    case result of
        Ok questionnaire ->
            let
                cmd = Cmd.map wrapMsg <|
                    QuestionnairesApi.getQuestionnaire questionnaire.uuid appState GetQuestionnaireCompleted
            in
            ( { model | savingQuestionnaire = Loading }, cmd )

        Err error ->
            ( { model | savingQuestionnaire = ApiError.toActionResult (lg "apiError.questionnaires.postError" appState) error }
            , getResultCmd result
            )


handleGetQuestionnaireCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError QuestionnaireDetail -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetQuestionnaireCompleted wrapMsg appState model result =
    case (result, model.madmp) of
        (Ok questionnaireDetails, Just madmp) ->
            let
                questionnaire =
                    setRepliesFromMaDMP madmp questionnaireDetails

                body =
                    QuestionnaireDetail.encode questionnaire

                cmd =
                    Cmd.map wrapMsg <|
                        QuestionnairesApi.putQuestionnaire questionnaire.uuid body appState ImportQuestionnaireCompleted
            in
            ( { model | savingQuestionnaire = Loading, questionnaire = Just questionnaire}, cmd )

        (Err error, _) ->
            ( { model | savingQuestionnaire = ApiError.toActionResult (lg "apiError.questionnaires.getError" appState) error }
            , getResultCmd result
            )

        _ ->
            ( model, Cmd.none)


getSelectedPackageId : Model -> Maybe String
getSelectedPackageId model =
    (Form.getFieldAsString "packageId" model.form).value


setRepliesFromMaDMP : MaDMP -> QuestionnaireDetail -> QuestionnaireDetail
setRepliesFromMaDMP madmp questionnaire =
    { questionnaire | replies = makeRepliesList madmp }


makeRepliesList : MaDMP -> List FormValue
makeRepliesList madmp =
    List.concat
        [ [ FormValue "1e85da40-bbfc-4180-903e-6c569ed2da38.c3dabaaf-c946-4a0d-889c-ede966f97667" (ItemListReply (List.length madmp.dmp.projects))
          , FormValue "1e85da40-bbfc-4180-903e-6c569ed2da38.73d686bd-7939-412e-8631-502ee6d9ea7b" (ItemListReply (List.length madmp.dmp.contributors))
          ]
        , List.concat <| List.indexedMap (\i p -> makeRepliesForProject ("1e85da40-bbfc-4180-903e-6c569ed2da38.c3dabaaf-c946-4a0d-889c-ede966f97667." ++ String.fromInt i) p) madmp.dmp.projects
        , List.concat <| List.indexedMap (\i p -> makeRepliesForContributor ("1e85da40-bbfc-4180-903e-6c569ed2da38.73d686bd-7939-412e-8631-502ee6d9ea7b." ++ String.fromInt i) p) madmp.dmp.contributors
        ]


makeRepliesForProject : String -> MaDMP_Project -> List FormValue
makeRepliesForProject prefix project =
    [ FormValue (prefix ++ ".f0ef08fd-d733-465c-bc66-5de0b826c41b") (StringReply project.title)
    , FormValue (prefix ++ ".22583d74-3c98-4e0a-b363-26d767c88212") (StringReply project.description)
    , FormValue (prefix ++ ".de84b9b5-bcd0-4954-8370-72ea83916b8c") (StringReply project.start)
    , FormValue (prefix ++ ".cabc6f07-6015-454e-b97a-c34db4ec0c60") (StringReply project.end)
    , FormValue (prefix ++ ".36a87eac-402d-43fb-a0df-ac5963bdf87d") (ItemListReply (List.length project.funding))
    ]
    ++
    (List.concat <| List.indexedMap (\i p -> makeRepliesForFunding (prefix ++ ".36a87eac-402d-43fb-a0df-ac5963bdf87d." ++ String.fromInt i) p) project.funding)


makeRepliesForContributor : String -> MaDMP_Contributor -> List FormValue
makeRepliesForContributor prefix contributor =
    [ FormValue (prefix ++ ".6155ad47-3d1e-4488-9f2a-742de1e56580") (StringReply contributor.name)
    , FormValue (prefix ++ ".3a2ffc13-6a0e-4976-bb34-14ab6d938348") (StringReply contributor.mbox)
    , FormValue (prefix ++ ".6295a55d-48d7-4f3c-961a-45b38eeea41f") (StringReply (getContributorOrcid contributor))
    ]
    ++
    case contributor.roles of
        x :: _ ->
            [ FormValue (prefix ++ ".829dcda6-db8a-40ac-819a-92b9b52490f5") (AnswerReply (getContributorRole x)) ]

        _ ->
            []


makeRepliesForFunding : String -> MaDMP_Funding -> List FormValue
makeRepliesForFunding prefix funding =
    [ FormValue (prefix ++ ".0b12fb8c-ee0f-40c0-9c53-b6826b786a0c") (IntegrationReply (PlainValue funding.funder_id.identifier))
    , FormValue (prefix ++ ".1ccbd0bb-4263-4240-9dc5-936ef09eef53") (StringReply funding.grant_id.identifier)
    ]
    ++
    case Dict.get funding.funding_status fundingStatuses of
        Just uuid ->
            [ FormValue (prefix ++ ".1ccbd0bb-4263-4240-9dc5-936ef09eef53") (AnswerReply uuid) ]

        _ ->
            []



getContributorOrcid : MaDMP_Contributor -> String
getContributorOrcid contributor =
    case contributor.contributorId.idType of
        "orcid" ->
            contributor.contributorId.identifier
        _ ->
            ""


getContributorRole : String -> String
getContributorRole role =
    Maybe.withDefault defaultRole (Dict.get role roles)


fundingStatuses : Dict String String
fundingStatuses =
  Dict.fromList
    [ ("planned", "59ed0193-8211-4ee8-8d36-0640d99ce870")
    , ("applied", "85fad342-a89d-414b-bc83-286a7417bb78")
    , ("granted", "dcbeab22-d188-4fa0-b50b-5c9d1a2fbefe")
    , ("rejected", "8c0c9f28-4672-46ba-a939-48c2c892d790")
    ]


roles : Dict String String
roles =
  Dict.fromList
    [ ("contact person", "f7468e79-c621-4ac9-95e0-263ebdf23c73")
    , ("data collector", "fe411838-170e-45d7-9d91-14f95ad347e6")
    , ("data curator", "0ee99167-c1a2-4fe9-a799-ef07a31ccf35")
    , ("data manager", "6eef8c10-150a-496c-b02a-61c90e62e95b")
    , ("distributor", "ae91b0b1-a591-49d1-a85c-13071550c043")
    , ("editor", "b7ee1e51-2628-486e-a341-2437c546897d")
    , ("producer", "ac02d12c-db42-4d47-a557-5fbd1d7ae524")
    , ("project leader", "c1caf2a7-fd84-4856-b3c4-35e09b5e1aca")
    , ("project manager", "99f5e968-cea8-49bf-a406-f134e664e657")
    , ("project member", "53dade1c-828c-4887-829a-65b4540291d5")
    , ("researcher", "369db75c-cf52-459a-836c-e3bcac3590bb")
    , ("rights holder", "5d758307-4e3a-439b-8c88-4dbdbfcd2154")
    , ("sponsor", "82ecfb2d-e321-47e4-93d8-e36a9a7031e9")
    , ("supervisor", "c6883c1f-6dd8-4c5b-aa7d-b93e4e2d9378")
    , ("work package leader", "d24fd64b-6a66-4d22-80f0-2d1aae4a554f")
    , ("other", "dead02bb-d5b2-4036-9e99-3318f191b3d0")
    ]


defaultRole : String
defaultRole = "dead02bb-d5b2-4036-9e99-3318f191b3d0"
