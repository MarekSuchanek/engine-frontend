module Wizard.Questionnaires.Import.Update exposing (fetchData, update)

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
import Wizard.Msgs
import Wizard.Ports exposing (FilePortData, createDropzone, fileSelected)
import Wizard.Questionnaires.Common.Questionnaire exposing (Questionnaire)
import Wizard.Questionnaires.Common.QuestionnaireCreateForm as QuestionnaireCreateForm exposing (QuestionnaireCreateForm)
import Wizard.Questionnaires.Common.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Wizard.Questionnaires.Import.Models exposing (..)
import Wizard.Questionnaires.Import.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Routes exposing (Route(..))
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
            ( { model | form = Form.update QuestionnaireCreateForm.validation formMsg model.form }, Cmd.none )


handleImportQuestionnaireCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handleImportQuestionnaireCompleted appState model result =
    case ( result, model.questionnaire ) of
        ( Ok (), Just questionnaire ) ->
            ( model
            , cmdNavigate appState <| Routes.QuestionnairesRoute <| DetailRoute questionnaire.uuid
            )

        ( Err error, _ ) ->
            ( { model | savingQuestionnaire = ApiError.toActionResult (lg "apiError.questionnaires.putError" appState) error }
            , getResultCmd result
            )

        _ ->
            ( model, Cmd.none )


handlePostQuestionnaireCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError Questionnaire -> ( Model, Cmd Wizard.Msgs.Msg )
handlePostQuestionnaireCompleted wrapMsg appState model result =
    case result of
        Ok questionnaire ->
            let
                cmd =
                    Cmd.map wrapMsg <|
                        QuestionnairesApi.getQuestionnaire questionnaire.uuid appState GetQuestionnaireCompleted
            in
            ( { model | savingQuestionnaire = Loading }, cmd )

        Err error ->
            ( { model | savingQuestionnaire = ApiError.toActionResult (lg "apiError.questionnaires.postError" appState) error }
            , getResultCmd result
            )


handleGetQuestionnaireCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError QuestionnaireDetail -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetQuestionnaireCompleted wrapMsg appState model result =
    case ( result, model.madmp ) of
        ( Ok questionnaireDetails, Just madmp ) ->
            let
                questionnaire =
                    setRepliesFromMaDMP madmp questionnaireDetails

                body =
                    QuestionnaireDetail.encode questionnaire

                cmd =
                    Cmd.map wrapMsg <|
                        QuestionnairesApi.putQuestionnaire questionnaire.uuid body appState ImportQuestionnaireCompleted
            in
            ( { model | savingQuestionnaire = Loading, questionnaire = Just questionnaire }, cmd )

        ( Err error, _ ) ->
            ( { model | savingQuestionnaire = ApiError.toActionResult (lg "apiError.questionnaires.getError" appState) error }
            , getResultCmd result
            )

        _ ->
            ( model, Cmd.none )


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
          , FormValue "d5b27482-b598-4b8c-b534-417d4ad27394.4e0c1edf-660c-4ebf-81f5-9fa959dead30" (ItemListReply (List.length madmp.dmp.datasets))
          ]
        , List.concat <| List.indexedMap (\i p -> makeRepliesForProject ("1e85da40-bbfc-4180-903e-6c569ed2da38.c3dabaaf-c946-4a0d-889c-ede966f97667." ++ String.fromInt i) p) madmp.dmp.projects
        , List.concat <| List.indexedMap (\i p -> makeRepliesForContributor ("1e85da40-bbfc-4180-903e-6c569ed2da38.73d686bd-7939-412e-8631-502ee6d9ea7b." ++ String.fromInt i) p) madmp.dmp.contributors
        , List.concat <| List.indexedMap (\i p -> makeRepliesForDataset ("d5b27482-b598-4b8c-b534-417d4ad27394.4e0c1edf-660c-4ebf-81f5-9fa959dead30." ++ String.fromInt i) p) madmp.dmp.datasets
        ]


makeRepliesForDataset : String -> MaDMP_Dataset -> List FormValue
makeRepliesForDataset prefix dataset =
    [ FormValue (prefix ++ ".b0949d09-d179-4491-9fb4-14b0deb9f862") (StringReply dataset.title)
    , FormValue (prefix ++ ".205a886d-83d7-4359-ae63-7103e05357c3") (StringReply dataset.description)
    , FormValue (prefix ++ ".cf727a0a-78c4-45a7-aa9b-cf7650ae873a") (ItemListReply 1)
    , FormValue (prefix ++ ".cf727a0a-78c4-45a7-aa9b-cf7650ae873a.0.9e13b2d3-5f00-4e19-8a52-5c33c5b1cb07") (StringReply dataset.datasetId.identifier)
    ]
        ++ (case String.toLower dataset.datasetId.idType of
                "handle" ->
                    [ FormValue (prefix ++ ".cf727a0a-78c4-45a7-aa9b-cf7650ae873a.0.5c22cf59-89e3-43a1-af10-1af43a97bcb2") (AnswerReply "b93a037a-006a-486f-87e0-6bef5c28879b") ]

                "doi" ->
                    [ FormValue (prefix ++ ".cf727a0a-78c4-45a7-aa9b-cf7650ae873a.0.5c22cf59-89e3-43a1-af10-1af43a97bcb2") (AnswerReply "48062bc9-0ffb-4509-bec6-e90641a30569") ]

                "ark" ->
                    [ FormValue (prefix ++ ".cf727a0a-78c4-45a7-aa9b-cf7650ae873a.0.5c22cf59-89e3-43a1-af10-1af43a97bcb2") (AnswerReply "c353f027-823b-4242-9149-37dca26cf4bc") ]

                "url" ->
                    [ FormValue (prefix ++ ".cf727a0a-78c4-45a7-aa9b-cf7650ae873a.0.5c22cf59-89e3-43a1-af10-1af43a97bcb2") (AnswerReply "7a1d3b28-5f85-48b8-b052-2448c276d9fc") ]

                _ ->
                    [ FormValue (prefix ++ ".cf727a0a-78c4-45a7-aa9b-cf7650ae873a.0.5c22cf59-89e3-43a1-af10-1af43a97bcb2") (AnswerReply "97236701-7b62-40f8-99a0-3b18d3fe3658") ]
           )
        ++ (case String.toLower dataset.personalData of
                "no" ->
                    [ FormValue (prefix ++ ".a1d76760-053c-4706-80a2-cfb6c6a061f3") (AnswerReply "4b2a08c7-4942-41fc-8114-d3868c882624") ]

                "yes" ->
                    [ FormValue (prefix ++ ".a1d76760-053c-4706-80a2-cfb6c6a061f3") (AnswerReply "0cdc4817-7c54-4ec1-b2f4-5c007a85c7b8") ]

                _ ->
                    []
           )
        ++ (case String.toLower dataset.sensitiveData of
                "no" ->
                    [ FormValue (prefix ++ ".cc95b399-7d8d-4232-bccf-686f78c91bff") (AnswerReply "60de66a3-d303-4784-8931-bc58f8a3e747") ]

                "yes" ->
                    [ FormValue (prefix ++ ".cc95b399-7d8d-4232-bccf-686f78c91bff") (AnswerReply "2686575d-cd74-4e2c-8524-eaca6f510425") ]

                _ ->
                    []
           )
        ++ (case dataset.distributions of
                [] ->
                    [ FormValue (prefix ++ ".a063da1c-aaea-4e18-85ec-f560d833f292") (AnswerReply "771279ea-3ffe-4d22-aeb4-8d656a77c25c") ]

                _ ->
                    [ FormValue (prefix ++ ".a063da1c-aaea-4e18-85ec-f560d833f292") (AnswerReply "8d1b07a7-f177-41f5-9532-05536223a8d6")
                    , FormValue (prefix ++ ".a063da1c-aaea-4e18-85ec-f560d833f292.8d1b07a7-f177-41f5-9532-05536223a8d6.81d3095e-a530-40a4-878e-ced42fabc4cd") (ItemListReply (List.length dataset.distributions))
                    ]
                        ++ (List.concat <| List.indexedMap (\i p -> makeRepliesForDistribution (prefix ++ ".a063da1c-aaea-4e18-85ec-f560d833f292.8d1b07a7-f177-41f5-9532-05536223a8d6.81d3095e-a530-40a4-878e-ced42fabc4cd." ++ String.fromInt i) p) dataset.distributions)
           )


makeRepliesForDistribution : String -> MaDMP_Distribution -> List FormValue
makeRepliesForDistribution prefix distribution =
    [ FormValue (prefix ++ ".3d89e23d-ff5c-45da-97a8-169ad8c39be6") (ItemListReply (List.length distribution.licenses)) ]
        ++ (case String.toLower distribution.dataAccess of
                "open" ->
                    [ FormValue (prefix ++ ".82fc0a41-8be0-407c-b2f8-95bf5b366187") (AnswerReply "1fd3e838-f92a-4086-8308-de17f6fa9d73") ]

                "shared" ->
                    [ FormValue (prefix ++ ".82fc0a41-8be0-407c-b2f8-95bf5b366187") (AnswerReply "985366e7-7504-4f67-a8ee-90c340ff977a") ]

                "closed" ->
                    [ FormValue (prefix ++ ".82fc0a41-8be0-407c-b2f8-95bf5b366187") (AnswerReply "a8adc972-a2b6-4f5b-837b-20f83a685ed6") ]

                _ ->
                    []
           )
        ++ (List.concat <| List.indexedMap (\i p -> makeRepliesForLicense (prefix ++ ".3d89e23d-ff5c-45da-97a8-169ad8c39be6." ++ String.fromInt i) p) distribution.licenses)


makeRepliesForLicense : String -> MaDMP_License -> List FormValue
makeRepliesForLicense prefix license =
    [ FormValue (prefix ++ ".28d494ef-26c0-4632-956e-5cafcc498a32") (StringReply license.startDate) ]
        ++ (case ( String.contains "publicdomain" (String.toLower license.licenseRef), String.contains "cc-by" (String.toLower license.licenseRef) ) of
                ( True, _ ) ->
                    [ FormValue (prefix ++ ".ca0f9465-3116-4824-8651-b592151c5368") (AnswerReply "d27a6e0f-55ea-4b25-bfb9-dcb4d6346fe0") ]

                ( _, True ) ->
                    [ FormValue (prefix ++ ".ca0f9465-3116-4824-8651-b592151c5368") (AnswerReply "9186e183-e328-41f9-b012-149d0bbad9ea") ]

                ( _, _ ) ->
                    [ FormValue (prefix ++ ".ca0f9465-3116-4824-8651-b592151c5368") (AnswerReply "734d5f4e-91c0-4019-8164-8c70c2e0c8f2")
                    , FormValue (prefix ++ ".ca0f9465-3116-4824-8651-b592151c5368.734d5f4e-91c0-4019-8164-8c70c2e0c8f2.375792f1-d7c3-4c8d-bf9e-f15ffa38e2fb") (StringReply license.licenseRef)
                    ]
           )


makeRepliesForProject : String -> MaDMP_Project -> List FormValue
makeRepliesForProject prefix project =
    [ FormValue (prefix ++ ".f0ef08fd-d733-465c-bc66-5de0b826c41b") (StringReply project.title)
    , FormValue (prefix ++ ".22583d74-3c98-4e0a-b363-26d767c88212") (StringReply project.description)
    , FormValue (prefix ++ ".de84b9b5-bcd0-4954-8370-72ea83916b8c") (StringReply project.start)
    , FormValue (prefix ++ ".cabc6f07-6015-454e-b97a-c34db4ec0c60") (StringReply project.end)
    , FormValue (prefix ++ ".36a87eac-402d-43fb-a0df-ac5963bdf87d") (ItemListReply (List.length project.funding))
    ]
        ++ (List.concat <| List.indexedMap (\i p -> makeRepliesForFunding (prefix ++ ".36a87eac-402d-43fb-a0df-ac5963bdf87d." ++ String.fromInt i) p) project.funding)


makeRepliesForContributor : String -> MaDMP_Contributor -> List FormValue
makeRepliesForContributor prefix contributor =
    [ FormValue (prefix ++ ".6155ad47-3d1e-4488-9f2a-742de1e56580") (StringReply contributor.name)
    , FormValue (prefix ++ ".3a2ffc13-6a0e-4976-bb34-14ab6d938348") (StringReply contributor.mbox)
    , FormValue (prefix ++ ".6295a55d-48d7-4f3c-961a-45b38eeea41f") (StringReply (getContributorOrcid contributor))
    ]
        ++ (case contributor.roles of
                x :: _ ->
                    [ FormValue (prefix ++ ".829dcda6-db8a-40ac-819a-92b9b52490f5") (AnswerReply (getContributorRole x)) ]

                _ ->
                    []
           )


makeRepliesForFunding : String -> MaDMP_Funding -> List FormValue
makeRepliesForFunding prefix funding =
    [ FormValue (prefix ++ ".0b12fb8c-ee0f-40c0-9c53-b6826b786a0c") (IntegrationReply (PlainValue funding.funder_id.identifier))
    , FormValue (prefix ++ ".1ccbd0bb-4263-4240-9dc5-936ef09eef53") (StringReply funding.grant_id.identifier)
    ]
        ++ (case Dict.get (String.toLower funding.funding_status) fundingStatuses of
                Just uuid ->
                    [ FormValue (prefix ++ ".1ccbd0bb-4263-4240-9dc5-936ef09eef53") (AnswerReply uuid) ]

                _ ->
                    []
           )


getContributorOrcid : MaDMP_Contributor -> String
getContributorOrcid contributor =
    case String.toLower contributor.contributorId.idType of
        "orcid" ->
            contributor.contributorId.identifier

        _ ->
            ""


getContributorRole : String -> String
getContributorRole role =
    Maybe.withDefault defaultRole (Dict.get (String.toLower role) roles)


fundingStatuses : Dict String String
fundingStatuses =
    Dict.fromList
        [ ( "planned", "59ed0193-8211-4ee8-8d36-0640d99ce870" )
        , ( "applied", "85fad342-a89d-414b-bc83-286a7417bb78" )
        , ( "granted", "dcbeab22-d188-4fa0-b50b-5c9d1a2fbefe" )
        , ( "rejected", "8c0c9f28-4672-46ba-a939-48c2c892d790" )
        ]


roles : Dict String String
roles =
    Dict.fromList
        [ ( "contact person", "f7468e79-c621-4ac9-95e0-263ebdf23c73" )
        , ( "data collector", "fe411838-170e-45d7-9d91-14f95ad347e6" )
        , ( "data curator", "0ee99167-c1a2-4fe9-a799-ef07a31ccf35" )
        , ( "data manager", "6eef8c10-150a-496c-b02a-61c90e62e95b" )
        , ( "distributor", "ae91b0b1-a591-49d1-a85c-13071550c043" )
        , ( "editor", "b7ee1e51-2628-486e-a341-2437c546897d" )
        , ( "producer", "ac02d12c-db42-4d47-a557-5fbd1d7ae524" )
        , ( "project leader", "c1caf2a7-fd84-4856-b3c4-35e09b5e1aca" )
        , ( "project manager", "99f5e968-cea8-49bf-a406-f134e664e657" )
        , ( "project member", "53dade1c-828c-4887-829a-65b4540291d5" )
        , ( "researcher", "369db75c-cf52-459a-836c-e3bcac3590bb" )
        , ( "rights holder", "5d758307-4e3a-439b-8c88-4dbdbfcd2154" )
        , ( "sponsor", "82ecfb2d-e321-47e4-93d8-e36a9a7031e9" )
        , ( "supervisor", "c6883c1f-6dd8-4c5b-aa7d-b93e4e2d9378" )
        , ( "work package leader", "d24fd64b-6a66-4d22-80f0-2d1aae4a554f" )
        , ( "other", "dead02bb-d5b2-4036-9e99-3318f191b3d0" )
        ]


defaultRole : String
defaultRole =
    "dead02bb-d5b2-4036-9e99-3318f191b3d0"
