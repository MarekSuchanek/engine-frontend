module Wizard.Questionnaires.Import.Subscriptions exposing (subscriptions)

import Wizard.Questionnaires.Import.Models exposing (Model)
import Wizard.Questionnaires.Import.Msgs exposing (Msg(..))
import Wizard.Ports as Ports exposing (fileContentRead)


subscriptions : Model -> Sub Msg
subscriptions _ =
    fileContentRead FileRead
