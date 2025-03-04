module Shared.Data.Package exposing
    ( Package
    , decoder
    , dummy
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Shared.Data.OrganizationInfo as OrganizationInfo exposing (OrganizationInfo)
import Shared.Data.Package.PackageState as PackageState exposing (PackageState)
import Time
import Version exposing (Version)


type alias Package =
    { id : String
    , name : String
    , organizationId : String
    , kmId : String
    , version : Version
    , description : String
    , organization : Maybe OrganizationInfo
    , remoteLatestVersion : Maybe String
    , state : PackageState
    , createdAt : Time.Posix
    }


decoder : Decoder Package
decoder =
    D.succeed Package
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "organizationId" D.string
        |> D.required "kmId" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "organization" (D.maybe OrganizationInfo.decoder)
        |> D.required "remoteLatestVersion" (D.maybe D.string)
        |> D.required "state" PackageState.decoder
        |> D.required "createdAt" D.datetime


dummy : Package
dummy =
    { id = ""
    , name = ""
    , organizationId = ""
    , kmId = ""
    , version = Version.create 0 0 0
    , description = ""
    , organization = Nothing
    , remoteLatestVersion = Nothing
    , state = PackageState.unknown
    , createdAt = Time.millisToPosix 0
    }
