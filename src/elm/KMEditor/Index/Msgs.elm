module KMEditor.Index.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Form
import KMEditor.Common.Models exposing (KnowledgeModel)
import KnowledgeModels.Common.Models exposing (PackageDetail)


type Msg
    = GetKnowledgeModelsCompleted (Result ApiError (List KnowledgeModel))
    | ShowHideDeleteKnowledgeModal (Maybe KnowledgeModel)
    | DeleteKnowledgeModel
    | DeleteKnowledgeModelCompleted (Result ApiError ())
    | PostMigrationCompleted (Result ApiError ())
    | ShowHideUpgradeModal (Maybe KnowledgeModel)
    | GetPackagesCompleted (Result ApiError (List PackageDetail))
    | UpgradeFormMsg Form.Msg
    | DeleteMigration String
    | DeleteMigrationCompleted (Result ApiError ())
