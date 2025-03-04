module Shared.Api.Packages exposing
    ( deletePackage
    , deletePackageVersion
    , exportPackageUrl
    , getOutdatedPackages
    , getPackage
    , getPackages
    , getPackagesSuggestions
    , getPackagesSuggestionsWithOptions
    , importFromOwl
    , importPackage
    , pullPackage
    )

import File exposing (File)
import Http
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, authorizedUrl, jwtDelete, jwtGet, jwtOrHttpGet, jwtPostEmpty, jwtPostFile, jwtPostFileWithData)
import Shared.Data.Package as Package exposing (Package)
import Shared.Data.PackageDetail as PackageDetail exposing (PackageDetail)
import Shared.Data.PackageSuggestion as PackageSuggestion exposing (PackageSuggestion)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)


getPackages : PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination Package) msg -> Cmd msg
getPackages qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/packages" ++ queryString
    in
    jwtGet url (Pagination.decoder "packages" Package.decoder)


getOutdatedPackages : AbstractAppState a -> ToMsg (Pagination Package) msg -> Cmd msg
getOutdatedPackages =
    let
        queryString =
            PaginationQueryString.empty
                |> PaginationQueryString.withSize (Just 5)
                |> PaginationQueryString.toApiUrlWith [ ( "state", "OutdatedPackageState" ) ]

        url =
            "/packages" ++ queryString
    in
    jwtGet url (Pagination.decoder "packages" Package.decoder)


getPackagesSuggestions : PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination PackageSuggestion) msg -> Cmd msg
getPackagesSuggestions qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/packages/suggestions" ++ queryString
    in
    jwtGet url (Pagination.decoder "packages" PackageSuggestion.decoder)


getPackagesSuggestionsWithOptions : PaginationQueryString -> List String -> List String -> AbstractAppState a -> ToMsg (Pagination PackageSuggestion) msg -> Cmd msg
getPackagesSuggestionsWithOptions qs select exclude =
    let
        queryString =
            PaginationQueryString.toApiUrlWith
                [ ( "select", String.join "," select )
                , ( "exclude", String.join "," exclude )
                ]
                qs

        url =
            "/packages/suggestions" ++ queryString
    in
    jwtGet url (Pagination.decoder "packages" PackageSuggestion.decoder)


getPackage : String -> AbstractAppState a -> ToMsg PackageDetail msg -> Cmd msg
getPackage packageId =
    jwtOrHttpGet ("/packages/" ++ packageId) PackageDetail.decoder


deletePackage : String -> String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deletePackage organizationId kmId =
    jwtDelete ("/packages/?organizationId=" ++ organizationId ++ "&kmId=" ++ kmId)


deletePackageVersion : String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deletePackageVersion packageId =
    jwtDelete ("/packages/" ++ packageId)


pullPackage : String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
pullPackage packageId =
    jwtPostEmpty ("/packages/" ++ packageId ++ "/pull")


importPackage : File -> AbstractAppState a -> ToMsg () msg -> Cmd msg
importPackage =
    jwtPostFile "/packages/bundle"


importFromOwl : List ( String, String ) -> File -> AbstractAppState a -> ToMsg () msg -> Cmd msg
importFromOwl params =
    let
        httpParams =
            List.map (\( k, v ) -> Http.stringPart k v) params
    in
    jwtPostFileWithData "/packages/bundle" httpParams


exportPackageUrl : String -> AbstractAppState a -> String
exportPackageUrl packageId =
    authorizedUrl ("/packages/" ++ packageId ++ "/bundle")
