module Wizard.Subscriptions exposing (subscriptions)

import Wizard.Common.Menu.Subscriptions
import Wizard.KMEditor.Subscriptions
import Wizard.KnowledgeModels.Subscriptions
import Wizard.Models exposing (Model)
import Wizard.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Subscriptions
import Wizard.Routes as Routes
import Wizard.Users.Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        currentViewSubscriptions =
            case model.appState.route of
                Routes.KMEditorRoute route ->
                    Wizard.KMEditor.Subscriptions.subscriptions KMEditorMsg route model.kmEditorModel

                Routes.KnowledgeModelsRoute route ->
                    Sub.map KnowledgeModelsMsg <| Wizard.KnowledgeModels.Subscriptions.subscriptions route model.kmPackagesModel

                Routes.QuestionnairesRoute route ->
                    Sub.map QuestionnairesMsg <| Wizard.Questionnaires.Subscriptions.subscriptions route model.questionnairesModel

                Routes.UsersRoute route ->
                    Sub.map UsersMsg <| Wizard.Users.Subscriptions.subscriptions route model.users

                _ ->
                    Sub.none

        menuSubscriptions =
            Wizard.Common.Menu.Subscriptions.subscriptions model.menuModel
    in
    Sub.batch [ currentViewSubscriptions, menuSubscriptions ]
