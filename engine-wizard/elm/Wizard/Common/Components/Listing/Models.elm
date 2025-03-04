module Wizard.Common.Components.Listing.Models exposing
    ( Item
    , Model
    , initialModel
    , initialModelWithFilters
    , initialModelWithFiltersAndStates
    , setPagination
    , toRouteAfterDelete
    , updateItems
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import Debouncer.Extra as Debouncer exposing (Debouncer)
import Dict exposing (Dict)
import Maybe.Extra as Maybe
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Wizard.Common.Components.Listing.Msgs exposing (Msg)


type alias Model a =
    { pagination : ActionResult (Pagination a)
    , paginationQueryString : PaginationQueryString
    , items : List (Item a)
    , qInput : String
    , qDebouncer : Debouncer (Msg a)
    , sortDropdownState : Dropdown.State
    , filters : PaginationQueryFilters
    , filterDropdownStates : Dict String Dropdown.State
    }


type alias Item a =
    { dropdownState : Dropdown.State
    , item : a
    }


initialModel : PaginationQueryString -> Model a
initialModel paginationQueryString =
    { pagination = Loading
    , paginationQueryString = paginationQueryString
    , items = []
    , qInput = Maybe.withDefault "" paginationQueryString.q
    , qDebouncer = Debouncer.toDebouncer <| Debouncer.debounce 500
    , sortDropdownState = Dropdown.initialState
    , filters = PaginationQueryFilters.empty
    , filterDropdownStates = Dict.empty
    }


initialModelWithFilters : PaginationQueryString -> PaginationQueryFilters -> Model a
initialModelWithFilters paginationQueryString filters =
    { pagination = Loading
    , paginationQueryString = paginationQueryString
    , items = []
    , qInput = Maybe.withDefault "" paginationQueryString.q
    , qDebouncer = Debouncer.toDebouncer <| Debouncer.debounce 500
    , sortDropdownState = Dropdown.initialState
    , filters = filters
    , filterDropdownStates = Dict.empty
    }


initialModelWithFiltersAndStates : PaginationQueryString -> PaginationQueryFilters -> Maybe (Model a) -> Model a
initialModelWithFiltersAndStates paginationQueryString filters oldModel =
    { pagination = Loading
    , paginationQueryString = paginationQueryString
    , items = []
    , qInput = Maybe.withDefault "" paginationQueryString.q
    , qDebouncer = Debouncer.toDebouncer <| Debouncer.debounce 500
    , sortDropdownState = Dropdown.initialState
    , filters = filters
    , filterDropdownStates = Maybe.unwrap Dict.empty .filterDropdownStates oldModel
    }


updateItems : (a -> a) -> Model a -> Model a
updateItems updateItem model =
    { model | items = List.map (\item -> { item | item = updateItem item.item }) model.items }


setPagination : Pagination a -> Model a -> Model a
setPagination pagination model =
    let
        wrap item =
            { dropdownState = Dropdown.initialState
            , item = item
            }
    in
    { model
        | pagination = Success pagination
        , items = List.map wrap pagination.items
    }


toRouteAfterDelete : (PaginationQueryFilters -> PaginationQueryString -> b) -> Model a -> b
toRouteAfterDelete toRoute model =
    let
        paginationQueryString =
            case model.pagination of
                Success pagination ->
                    let
                        isLastPage =
                            pagination.page.number == (pagination.page.totalPages - 1)

                        isLastElement =
                            modBy pagination.page.size pagination.page.totalElements == 1
                    in
                    if isLastPage && isLastElement then
                        PaginationQueryString.setPage model.paginationQueryString pagination.page.number

                    else
                        model.paginationQueryString

                _ ->
                    model.paginationQueryString
    in
    toRoute model.filters paginationQueryString
