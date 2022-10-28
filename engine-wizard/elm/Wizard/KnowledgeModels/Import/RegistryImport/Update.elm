module Wizard.KnowledgeModels.Import.RegistryImport.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Shared.Api.Packages as PackagesApi
import Shared.Setters exposing (setPulling)
import Wizard.Common.Api exposing (applyResult)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KnowledgeModels.Import.RegistryImport.Models exposing (Model)
import Wizard.KnowledgeModels.Import.RegistryImport.Msgs exposing (Msg(..))
import Wizard.Msgs


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ChangePackageId packageId ->
            ( { model | packageId = packageId }, Cmd.none )

        Submit ->
            if String.length model.packageId > 0 then
                ( { model | pulling = Loading }
                , PackagesApi.pullPackage model.packageId appState (wrapMsg << PullPackageCompleted)
                )

            else
                ( model, Cmd.none )

        PullPackageCompleted result ->
            applyResult appState
                { setResult = setPulling
                , defaultError = gettext "Unable to import the package." appState.locale
                , model = model
                , result = result
                }
