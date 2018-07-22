module Common.Questionnaire.View exposing (..)

import Common.Html exposing (emptyNode)
import Common.Questionnaire.Models exposing (Feedback, FeedbackForm, FormExtraData, Model)
import Common.Questionnaire.Msgs exposing (CustomFormMessage(FeedbackMsg), Msg(..))
import Common.Types exposing (ActionResult(Success, Unset))
import Common.View exposing (fullPageActionResultView, modalView)
import Common.View.Forms exposing (inputGroup, textAreaGroup)
import FormEngine.View exposing (FormViewConfig, viewForm)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import KMEditor.Common.Models.Entities exposing (Chapter)


viewQuestionnaire : Model -> Html Msg
viewQuestionnaire model =
    div [ class "Questionnaire row" ]
        [ div [ class "col-sm-12 col-md-3 col-lg-3 col-xl-3" ]
            [ chapterList model ]
        , div [ class "col-sm-11 col-md-8 col-lg-8 col-xl-7" ]
            [ chapterHeader model.activeChapter
            , viewChapterForm model
            ]
        , feedbackModal model
        ]


chapterList : Model -> Html Msg
chapterList model =
    div [ class "nav nav-pills flex-column" ]
        (List.map (chapterListChapter model.activeChapter) model.questionnaire.knowledgeModel.chapters)


chapterListChapter : Maybe Chapter -> Chapter -> Html Msg
chapterListChapter activeChapter chapter =
    a
        [ classList [ ( "nav-link", True ), ( "active", activeChapter == Just chapter ) ]
        , onClick <| SetActiveChapter chapter
        ]
        [ text chapter.title ]


chapterHeader : Maybe Chapter -> Html Msg
chapterHeader maybeChapter =
    case maybeChapter of
        Just chapter ->
            div []
                [ h3 [] [ text chapter.title ]
                , p [ class "chapter-description" ] [ text chapter.text ]
                ]

        _ ->
            emptyNode


viewExtraData : FormExtraData -> Html msg
viewExtraData data =
    emptyNode


formConfig : FormViewConfig CustomFormMessage FormExtraData
formConfig =
    { customActions = [ ( "fa-exclamation-circle", FeedbackMsg ) ]
    , viewExtraData = Just viewExtraData
    }


viewChapterForm : Model -> Html Msg
viewChapterForm model =
    case model.activeChapterForm of
        Just form ->
            viewForm formConfig form |> Html.map FormMsg

        _ ->
            emptyNode


feedbackModal : Model -> Html Msg
feedbackModal model =
    let
        visible =
            case model.feedback of
                Unset ->
                    False

                _ ->
                    True

        modalContent =
            case model.sendingFeedback of
                Success _ ->
                    case model.feedbackResult of
                        Just feedback ->
                            [ p []
                                [ text "You can follow the GitHub "
                                , a [ href feedback.issueUrl, target "_blank" ]
                                    [ text <| "issue " ++ toString feedback.issueId ]
                                , text "."
                                ]
                            ]

                        Nothing ->
                            [ emptyNode ]

                _ ->
                    feedbackModalContent model

        ( actionName, actionMsg ) =
            case model.sendingFeedback of
                Success _ ->
                    ( "Done", CloseFeedback )

                _ ->
                    ( "Send", SendFeedbackForm )

        modalConfig =
            { modalTitle = "Feedback"
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.sendingFeedback
            , actionName = actionName
            , actionMsg = actionMsg
            , cancelMsg = CloseFeedback
            }
    in
    modalView modalConfig


feedbackModalContent : Model -> List (Html Msg)
feedbackModalContent model =
    let
        feedbackList =
            case model.feedback of
                Success feedbacks ->
                    if List.length feedbacks > 0 then
                        div []
                            [ div []
                                [ text "There are already some issues reported with this question" ]
                            , ul [] (List.map feedbackIssue feedbacks)
                            ]
                    else
                        emptyNode

                _ ->
                    emptyNode
    in
    [ div [ class "alert alert-info" ]
        [ text "If you found something wrong with the question, you can send us your feedback how to improve it." ]
    , feedbackList
    , inputGroup model.feedbackForm "title" "Title" |> Html.map FeedbackFormMsg
    , textAreaGroup model.feedbackForm "content" "Description" |> Html.map FeedbackFormMsg
    ]


feedbackIssue : Feedback -> Html Msg
feedbackIssue feedback =
    li []
        [ a [ href feedback.issueUrl, target "_blank" ]
            [ text feedback.title ]
        ]
