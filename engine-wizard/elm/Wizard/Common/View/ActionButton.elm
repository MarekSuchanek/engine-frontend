module Wizard.Common.View.ActionButton exposing
    ( ButtonConfig
    , ButtonCustomConfig
    , ButtonWithAttrsConfig
    , SubmitConfig
    , button
    , buttonCustom
    , buttonWithAttrs
    , loader
    , submit
    )

import ActionResult exposing (ActionResult(..))
import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (class, disabled, type_)
import Html.Events exposing (onClick)
import Shared.Html exposing (faSet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)


type alias ButtonConfig a msg =
    { label : String
    , result : ActionResult a
    , msg : msg
    , dangerous : Bool
    }


button : AppState -> ButtonConfig a msg -> Html msg
button appState cfg =
    actionButtonView appState
        [ onClick cfg.msg, class <| "btn btn-with-loader " ++ buttonClass cfg.dangerous ]
        [ text cfg.label ]
        cfg.result


type alias ButtonWithAttrsConfig a msg =
    { label : String
    , result : ActionResult a
    , msg : msg
    , dangerous : Bool
    , attrs : List (Attribute msg)
    }


buttonWithAttrs : AppState -> ButtonWithAttrsConfig a msg -> Html msg
buttonWithAttrs appState cfg =
    actionButtonView appState
        ([ onClick cfg.msg, class <| "btn btn-with-loader " ++ buttonClass cfg.dangerous ] ++ cfg.attrs)
        [ text cfg.label ]
        cfg.result


type alias ButtonCustomConfig a msg =
    { content : List (Html msg)
    , result : ActionResult a
    , msg : msg
    , btnClass : String
    }


buttonCustom : AppState -> ButtonCustomConfig a msg -> Html msg
buttonCustom appState cfg =
    actionButtonView appState
        [ onClick cfg.msg, class <| "btn btn-with-loader " ++ cfg.btnClass ]
        cfg.content
        cfg.result


type alias SubmitConfig a =
    { label : String
    , result : ActionResult a
    }


submit : AppState -> SubmitConfig a -> Html msg
submit appState { label, result } =
    actionButtonView appState
        [ type_ "submit"
        , class "btn btn-primary btn-with-loader"
        , dataCy "form_submit"
        ]
        [ text label ]
        result


actionButtonView : AppState -> List (Attribute msg) -> List (Html msg) -> ActionResult a -> Html msg
actionButtonView appState attributes content result =
    let
        buttonContent =
            case result of
                Loading ->
                    [ loader appState ]

                _ ->
                    content

        buttonAttributes =
            disabled (result == Loading) :: attributes
    in
    Html.button buttonAttributes buttonContent


loader : AppState -> Html msg
loader appState =
    faSet "_global.spinner" appState


buttonClass : Bool -> String
buttonClass dangerous =
    if dangerous then
        "btn-danger"

    else
        "btn-primary"
