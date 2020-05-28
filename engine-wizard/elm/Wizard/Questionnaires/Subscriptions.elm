module Wizard.Questionnaires.Subscriptions exposing (..)

import Wizard.Questionnaires.Import.Subscriptions
import Wizard.Questionnaires.Index.Subscriptions
import Wizard.Questionnaires.Models exposing (Model)
import Wizard.Questionnaires.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        ImportRoute ->
            Sub.map ImportMsg <| Wizard.Questionnaires.Import.Subscriptions.subscriptions model.importModel

        IndexRoute ->
            Sub.map IndexMsg <| Wizard.Questionnaires.Index.Subscriptions.subscriptions model.indexModel

        _ ->
            Sub.none
