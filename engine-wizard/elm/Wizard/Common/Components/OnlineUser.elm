module Wizard.Common.Components.OnlineUser exposing (Model, Msg, init, update, view)

import Bootstrap.Popover as Popover
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src)
import List.Extra as List
import Shared.Data.OnlineUserInfo as OnlineUserInfo exposing (LoggedData, OnlineUserInfo)
import Shared.Data.User as User
import Shared.Html exposing (fa, faKeyClass)
import Shared.Locale exposing (lg)
import Wizard.Common.AppState exposing (AppState)


type alias Model =
    { userInfo : OnlineUserInfo
    , popoverState : Popover.State
    }


type Msg
    = PopoverMsg Popover.State


init : OnlineUserInfo -> Model
init userInfo =
    { userInfo = userInfo
    , popoverState = Popover.initialState
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        PopoverMsg state ->
            { model | popoverState = state }


view : AppState -> Model -> Html Msg
view appState model =
    let
        ( username, colorClass, content ) =
            case model.userInfo of
                OnlineUserInfo.Logged data ->
                    viewLogged data

                OnlineUserInfo.Anonymous { avatarNumber, colorNumber } ->
                    viewAnonymous appState avatarNumber colorNumber
    in
    Popover.config
        (div
            (class "OnlineUser" :: class colorClass :: Popover.onHover model.popoverState PopoverMsg)
            [ content ]
        )
        |> Popover.bottom
        |> Popover.content [] [ text username ]
        |> Popover.view model.popoverState


viewLogged : LoggedData -> ( String, String, Html msg )
viewLogged userData =
    ( User.fullName userData
    , "color-" ++ String.fromInt userData.colorNumber
    , div [ class "Logged" ] [ img [ src (User.imageUrlOrGravatar userData) ] [] ]
    )


viewAnonymous : AppState -> Int -> Int -> ( String, String, Html msg )
viewAnonymous appState avatarNumber colorNumber =
    let
        avatars =
            getAvatars appState

        ( avatarIcon, avatarName ) =
            List.getAt (modBy (List.length avatars) avatarNumber) avatars
                |> Maybe.withDefault ( faKeyClass "avatar.0" appState, lg "avatar.0" appState )
    in
    ( avatarName
    , "color-" ++ String.fromInt colorNumber
    , div [ class "Anonymous" ] [ fa ("fa-lg " ++ avatarIcon) ]
    )


getAvatars : AppState -> List ( String, String )
getAvatars appState =
    [ ( faKeyClass "avatar.0" appState, lg "avatar.0" appState )
    , ( faKeyClass "avatar.1" appState, lg "avatar.1" appState )
    , ( faKeyClass "avatar.2" appState, lg "avatar.2" appState )
    , ( faKeyClass "avatar.3" appState, lg "avatar.3" appState )
    , ( faKeyClass "avatar.4" appState, lg "avatar.4" appState )
    , ( faKeyClass "avatar.5" appState, lg "avatar.5" appState )
    , ( faKeyClass "avatar.6" appState, lg "avatar.6" appState )
    , ( faKeyClass "avatar.7" appState, lg "avatar.7" appState )
    , ( faKeyClass "avatar.8" appState, lg "avatar.8" appState )
    , ( faKeyClass "avatar.9" appState, lg "avatar.9" appState )
    , ( faKeyClass "avatar.10" appState, lg "avatar.10" appState )
    , ( faKeyClass "avatar.11" appState, lg "avatar.11" appState )
    , ( faKeyClass "avatar.12" appState, lg "avatar.12" appState )
    , ( faKeyClass "avatar.13" appState, lg "avatar.13" appState )
    , ( faKeyClass "avatar.14" appState, lg "avatar.14" appState )
    , ( faKeyClass "avatar.15" appState, lg "avatar.15" appState )
    , ( faKeyClass "avatar.16" appState, lg "avatar.16" appState )
    , ( faKeyClass "avatar.17" appState, lg "avatar.17" appState )
    , ( faKeyClass "avatar.18" appState, lg "avatar.18" appState )
    , ( faKeyClass "avatar.19" appState, lg "avatar.19" appState )
    ]
