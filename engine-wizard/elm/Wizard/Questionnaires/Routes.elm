module Wizard.Questionnaires.Routes exposing (Route(..))


type Route
    = CreateRoute (Maybe String)
    | ImportRoute
    | CreateMigrationRoute String
    | DetailRoute String
    | EditRoute String
    | IndexRoute
    | MigrationRoute String
