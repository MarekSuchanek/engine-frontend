module Wizard.Documents.Update exposing (..)

import Wizard.Common.AppState exposing (AppState)
import Wizard.Documents.Create.Update
import Wizard.Documents.Index.Update
import Wizard.Documents.Models exposing (Model)
import Wizard.Documents.Msgs exposing (Msg(..))
import Wizard.Documents.Routes exposing (Route(..))
import Wizard.Msgs


fetchData : Route -> AppState -> Model -> Cmd Msg
fetchData route appState model =
    case route of
        CreateRoute questionnaireUuid ->
            Cmd.map CreateMsg <|
                Wizard.Documents.Create.Update.fetchData appState questionnaireUuid

        IndexRoute _ _ ->
            Cmd.map IndexMsg <|
                Wizard.Documents.Index.Update.fetchData appState model.indexModel


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        CreateMsg cMsg ->
            let
                ( createModel, cmd ) =
                    Wizard.Documents.Create.Update.update (wrapMsg << CreateMsg) cMsg appState model.createModel
            in
            ( { model | createModel = createModel }, cmd )

        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.Documents.Index.Update.update (wrapMsg << IndexMsg) iMsg appState model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )
