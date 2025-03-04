module Wizard.Common.Components.Listing.View exposing
    ( CustomFilterConfig
    , Filter(..)
    , ListingActionConfig
    , ListingActionType(..)
    , ListingDropdownItem
    , SimpleFilterConfig
    , SimpleMultiFilterConfig
    , UpdatedTimeConfig
    , ViewConfig
    , dropdownAction
    , dropdownSeparator
    , view
    )

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Dict
import Gettext exposing (gettext)
import Html exposing (Html, a, div, input, li, nav, span, text, ul)
import Html.Attributes exposing (class, classList, href, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra as List
import Maybe.Extra as Maybe
import Shared.Common.TimeUtils as TimeUtils
import Shared.Components.Badge as Badge
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.Pagination.Page exposing (Page)
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString, SortDirection(..))
import Shared.Html exposing (emptyNode, fa, faSet)
import Shared.Undraw as Undraw
import String.Format as String
import Time
import Time.Distance exposing (inWordsWithConfig)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Models exposing (Item, Model)
import Wizard.Common.Components.Listing.Msgs exposing (Msg(..))
import Wizard.Common.Components.ListingDropdown as ListingDropdown
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy, tooltip)
import Wizard.Common.TimeDistance exposing (locale)
import Wizard.Common.View.ItemIcon as ItemIcon
import Wizard.Common.View.Page as Page
import Wizard.Routes as Routes exposing (Route)
import Wizard.Routing as Routing


type alias ViewConfig a msg =
    { title : a -> Html msg
    , description : a -> Html msg
    , itemAdditionalData : a -> Maybe (List (Html msg))
    , dropdownItems : a -> List (ListingDropdownItem msg)
    , textTitle : a -> String
    , emptyText : String
    , updated : Maybe (UpdatedTimeConfig a)
    , iconView : Maybe (a -> Html msg)
    , searchPlaceholderText : Maybe String
    , sortOptions : List ( String, String )
    , filters : List (Filter msg)
    , wrapMsg : Msg a -> msg
    , toRoute : PaginationQueryFilters -> PaginationQueryString -> Route
    , toolbarExtra : Maybe (Html msg)
    }


type alias UpdatedTimeConfig a =
    { getTime : a -> Time.Posix
    , currentTime : Time.Posix
    }


type Filter msg
    = SimpleFilter String SimpleFilterConfig
    | SimpleMultiFilter String SimpleMultiFilterConfig
    | CustomFilter String (CustomFilterConfig msg)


type alias SimpleFilterConfig =
    { name : String
    , options : List ( String, String )
    }


type alias SimpleMultiFilterConfig =
    { name : String
    , options : List ( String, String )
    , maxVisibleValues : Int
    }


type alias CustomFilterConfig msg =
    { label : List (Html msg)
    , items : List (Dropdown.DropdownItem msg)
    }


type ListingDropdownItem msg
    = ListingDropdownAction (ListingActionConfig msg)
    | ListingDropdownSeparator


type alias ListingActionConfig msg =
    { extraClass : Maybe String
    , icon : Html msg
    , label : String
    , msg : ListingActionType msg
    , dataCy : String
    }


type ListingActionType msg
    = ListingActionMsg msg
    | ListingActionLink Routes.Route


dropdownAction : ListingActionConfig msg -> ListingDropdownItem msg
dropdownAction =
    ListingDropdownAction


dropdownSeparator : ListingDropdownItem msg
dropdownSeparator =
    ListingDropdownSeparator


view : AppState -> ViewConfig a msg -> Model a -> Html msg
view appState config model =
    div [ class "Listing" ]
        [ viewToolbar appState config model
        , Page.actionResultView appState (viewList appState config model) model.pagination
        ]


viewToolbar : AppState -> ViewConfig a msg -> Model a -> Html msg
viewToolbar appState cfg model =
    div [ class "listing-toolbar mb-2" ]
        [ div [ class "filter-sort" ]
            ([ viewToolbarSearch appState cfg model
             , viewToolbarSort appState cfg model
             ]
                ++ viewToolbarFilters appState cfg model
            )
        , Maybe.unwrap emptyNode (div [ class "ms-4" ] << List.singleton) cfg.toolbarExtra
        ]


viewToolbarSearch : AppState -> ViewConfig a msg -> Model a -> Html msg
viewToolbarSearch appState cfg model =
    let
        placeholderText =
            Maybe.withDefault (gettext "Filter by name..." appState.locale) cfg.searchPlaceholderText
    in
    input
        [ type_ "text"
        , placeholder placeholderText
        , onInput (cfg.wrapMsg << QueryInput)
        , value model.qInput
        , class "form-control d-inline w-auto align-top me-3"
        , id "filter"
        ]
        []


viewToolbarSort : AppState -> ViewConfig a msg -> Model a -> Html msg
viewToolbarSort appState cfg model =
    let
        paginationQueryString =
            model.paginationQueryString

        currentSort =
            List.find (\( k, _ ) -> paginationQueryString.sortBy == Just k) cfg.sortOptions
                |> Maybe.unwrap "" Tuple.second

        sortOption ( name, visibleName ) =
            let
                route =
                    cfg.toRoute model.filters { paginationQueryString | sortBy = Just name, page = Just 1 }
            in
            Dropdown.anchorItem [ href <| Routing.toUrl appState route ]
                [ text visibleName ]

        ( sortDirectionButtonUrl, sortDirectionButtonIcon ) =
            if paginationQueryString.sortDirection == SortASC then
                ( cfg.toRoute model.filters { paginationQueryString | sortDirection = SortDESC, page = Just 1 }
                , faSet "_global.sortAsc" appState
                )

            else
                ( cfg.toRoute model.filters { paginationQueryString | sortDirection = SortASC, page = Just 1 }
                , faSet "_global.sortDesc" appState
                )
    in
    div [ class "btn-group" ]
        [ Dropdown.dropdown model.sortDropdownState
            { options = []
            , toggleMsg = cfg.wrapMsg << SortDropdownMsg
            , toggleButton =
                Dropdown.toggle [ Button.outlineSecondary ] [ text currentSort ]
            , items =
                Dropdown.header [ text (gettext "Order by" appState.locale) ] :: List.map sortOption cfg.sortOptions
            }
        , linkTo appState
            sortDirectionButtonUrl
            [ class "btn btn-outline-secondary", dataCy "listing_toolbar_sort-direction" ]
            [ sortDirectionButtonIcon ]
        ]


viewToolbarFilters : AppState -> ViewConfig a msg -> Model a -> List (Html msg)
viewToolbarFilters appState cfg model =
    List.map (viewToolbarFilter appState cfg model) cfg.filters


viewToolbarFilter : AppState -> ViewConfig a msg -> Model a -> Filter msg -> Html msg
viewToolbarFilter appState cfg model filter =
    case filter of
        SimpleFilter filterId filterCfg ->
            viewToolbarSimpleFilter appState cfg model filterId filterCfg

        SimpleMultiFilter filterId filterCfg ->
            viewToolbarSimpleMultiFilter appState cfg model filterId filterCfg

        CustomFilter filterId filterCfg ->
            viewToolbarCustomFilter appState cfg model filterId filterCfg


viewToolbarSimpleFilter : AppState -> ViewConfig a msg -> Model a -> String -> SimpleFilterConfig -> Html msg
viewToolbarSimpleFilter appState cfg model filterId filterCfg =
    let
        item ( value, visibleName ) =
            let
                route =
                    cfg.toRoute
                        (PaginationQueryFilters.insertValue filterId value model.filters)
                        (PaginationQueryString.resetPage model.paginationQueryString)

                icon =
                    if Maybe.unwrap False ((==) value << Tuple.first) maybeFilterValue then
                        faSet "listing.filter.single.selected" appState

                    else
                        faSet "listing.filter.single.notSelected" appState
            in
            Dropdown.anchorItem [ href <| Routing.toUrl appState route, class "dropdown-item-icon" ]
                [ icon, text visibleName ]

        maybeFilterValue =
            PaginationQueryFilters.getValue filterId model.filters
                |> Maybe.andThen (\value -> List.find (Tuple.first >> (==) value) filterCfg.options)

        filterLabel =
            maybeFilterValue
                |> Maybe.map Tuple.second
                |> Maybe.withDefault filterCfg.name

        label =
            [ span [ class "filter-text-label" ] [ text filterLabel ] ]

        items =
            List.map item filterCfg.options
    in
    viewFilter appState cfg model filterId label items


viewToolbarSimpleMultiFilter : AppState -> ViewConfig a msg -> Model a -> String -> SimpleMultiFilterConfig -> Html msg
viewToolbarSimpleMultiFilter appState cfg model filterId filterCfg =
    let
        item ( value, visibleName ) =
            let
                ( icon, newFilterValue ) =
                    if List.member value filterValues then
                        ( faSet "listing.filter.multi.selected" appState
                        , removeValue value
                        )

                    else
                        ( faSet "listing.filter.multi.notSelected" appState
                        , addValue value
                        )

                route =
                    cfg.toRoute
                        (PaginationQueryFilters.insertValue filterId newFilterValue model.filters)
                        (PaginationQueryString.resetPage model.paginationQueryString)
            in
            Dropdown.anchorItem [ href <| Routing.toUrl appState route, class "dropdown-item-icon" ]
                [ icon, text visibleName ]

        addValue value =
            String.join "," <|
                filterValues
                    ++ [ value ]

        removeValue value =
            String.join "," <|
                List.filter ((/=) value) filterValues

        filterValues =
            PaginationQueryFilters.getValue filterId model.filters
                |> Maybe.unwrap [] (String.split ",")

        filterValuesCount =
            List.length filterValues

        filterValueToLabel value =
            List.find ((==) value << Tuple.first) filterCfg.options
                |> Maybe.unwrap value Tuple.second

        filterLabel =
            if filterValuesCount == 0 then
                filterCfg.name

            else
                List.take filterCfg.maxVisibleValues filterValues
                    |> List.map filterValueToLabel
                    |> String.join ", "

        filterBadge =
            if filterValuesCount > filterCfg.maxVisibleValues then
                Badge.dark [ class "rounded-pill" ] [ text ("+" ++ String.fromInt (filterValuesCount - filterCfg.maxVisibleValues)) ]

            else
                emptyNode

        label =
            [ span [ class "filter-text-label" ] [ text filterLabel ], filterBadge ]

        items =
            List.map item filterCfg.options
    in
    viewFilter appState cfg model filterId label items


viewToolbarCustomFilter : AppState -> ViewConfig a msg -> Model a -> String -> CustomFilterConfig msg -> Html msg
viewToolbarCustomFilter appState cfg model filterId filterCfg =
    viewFilter appState cfg model filterId filterCfg.label filterCfg.items


viewFilter : AppState -> ViewConfig a msg -> Model a -> String -> List (Html msg) -> List (Dropdown.DropdownItem msg) -> Html msg
viewFilter appState cfg model filterId label items =
    let
        state =
            Maybe.withDefault Dropdown.initialState (Dict.get filterId model.filterDropdownStates)

        filterActive =
            PaginationQueryFilters.isFilterActive filterId model.filters

        buttonClass =
            if filterActive then
                Button.secondary

            else
                Button.outlineSecondary

        clearSelection =
            if filterActive then
                let
                    clearAllRoute =
                        Routing.toUrl appState <|
                            cfg.toRoute
                                (PaginationQueryFilters.removeFilter filterId model.filters)
                                (PaginationQueryString.resetPage model.paginationQueryString)

                    clearAllItem =
                        Dropdown.anchorItem [ href clearAllRoute ]
                            [ text (gettext "Clear selection" appState.locale) ]
                in
                [ Dropdown.divider
                , clearAllItem
                ]

            else
                []
    in
    Dropdown.dropdown state
        { options = [ Dropdown.attrs [ class "btn-group-filter", id ("filter-" ++ filterId) ] ]
        , toggleMsg = cfg.wrapMsg << FilterDropdownMsg filterId
        , toggleButton =
            Dropdown.toggle [ buttonClass ] label
        , items = items ++ clearSelection
        }


viewList : AppState -> ViewConfig a msg -> Model a -> Pagination a -> Html msg
viewList appState cfg model pagination =
    if List.length pagination.items > 0 then
        div []
            [ div [ class "list-group list-group-flush", dataCy "listing_list" ]
                (List.indexedMap (viewItem appState cfg) model.items)
            , viewPagination appState cfg model pagination.page
            ]

    else
        viewEmpty appState cfg model


viewPagination : AppState -> ViewConfig a msg -> Model a -> Page -> Html msg
viewPagination appState cfg model page =
    let
        paginationQueryString =
            model.paginationQueryString

        currentPage =
            Maybe.withDefault 1 model.paginationQueryString.page

        viewPageLink pageNumber attributes content =
            li (class "page-item" :: attributes)
                [ linkTo appState
                    (cfg.toRoute model.filters { paginationQueryString | page = Just pageNumber })
                    [ class "page-link" ]
                    content
                ]

        viewNavLink number =
            viewPageLink number
                [ classList [ ( "active", number == currentPage ) ]
                , dataCy "listing_page-link"
                ]
                [ text (String.fromInt number) ]

        dots =
            li [ class "page-item disabled" ] [ a [ class "page-link" ] [ text "..." ] ]

        ( left, leftDots ) =
            if currentPage - 4 > 1 then
                ( currentPage - 4, dots )

            else
                ( 1, emptyNode )

        ( right, rightDots ) =
            if currentPage + 4 < page.totalPages then
                ( currentPage + 4, dots )

            else
                ( page.totalPages, emptyNode )
    in
    if page.totalPages > 1 then
        let
            lastLink =
                if currentPage < page.totalPages then
                    viewPageLink page.totalPages
                        [ class "icon-right" ]
                        [ text (gettext "Last" appState.locale)
                        , fa "fas fa-angle-double-right"
                        ]

                else
                    emptyNode

            nextLink =
                viewPageLink (currentPage + 1)
                    [ class "icon-right"
                    , classList [ ( "disabled", currentPage == page.totalPages ) ]
                    , dataCy "listing_page-link_next"
                    ]
                    [ text (gettext "Next" appState.locale)
                    , fa "fas fa-angle-right"
                    ]

            pageLinks =
                List.map viewNavLink (List.range left right)

            prevLink =
                viewPageLink (currentPage - 1)
                    [ class "icon-left"
                    , classList [ ( "disabled", currentPage == 1 ) ]
                    , dataCy "listing_page-link_prev"
                    ]
                    [ fa "fas fa-angle-left"
                    , text (gettext "Prev" appState.locale)
                    ]

            firstLink =
                if currentPage > 1 then
                    viewPageLink 1
                        [ class "icon-left" ]
                        [ fa "fas fa-angle-double-left"
                        , text (gettext "First" appState.locale)
                        ]

                else
                    emptyNode

            links =
                [ firstLink, prevLink, leftDots ] ++ pageLinks ++ [ rightDots, nextLink, lastLink ]
        in
        nav [] [ ul [ class "pagination" ] links ]

    else
        emptyNode


viewEmpty : AppState -> ViewConfig a msg -> Model a -> Html msg
viewEmpty appState config model =
    let
        filtersActive =
            not (String.isEmpty model.qInput && List.isEmpty (Dict.toList model.filters.values))

        emptyText =
            if filtersActive then
                gettext "There are no results matching your search and filters." appState.locale

            else
                config.emptyText
    in
    Page.illustratedMessage
        { image = Undraw.noData
        , heading = gettext "No data" appState.locale
        , lines = [ emptyText ]
        , cy = "listing-empty"
        }


viewItem : AppState -> ViewConfig a msg -> Int -> Item a -> Html msg
viewItem appState config index item =
    let
        actions =
            config.dropdownItems item.item

        dropdown =
            if List.length actions > 0 then
                ListingDropdown.dropdown appState
                    { dropdownState = item.dropdownState
                    , toggleMsg = config.wrapMsg << ItemDropdownMsg index
                    , items = List.map (viewAction appState) actions
                    }

            else
                emptyNode

        icon =
            config.iconView
                |> Maybe.andMap (Just item.item)
                |> Maybe.withDefault (ItemIcon.view { text = config.textTitle item.item, image = Nothing })

        additionalData =
            Maybe.unwrap emptyNode (div [ class "additional-data" ]) (config.itemAdditionalData item.item)
    in
    div [ class "list-group-item", dataCy "listing_item" ]
        [ icon
        , div [ class "content" ]
            [ div [ class "title-row" ]
                [ span [ class "title" ] [ config.title item.item ]
                ]
            , div [ class "extra" ]
                [ div [ class "description" ]
                    [ config.description item.item ]
                ]
            ]
        , div [ class "updated" ]
            [ viewUpdated appState config item.item ]
        , div [ class "actions" ]
            [ dropdown ]
        , additionalData
        ]


viewUpdated : AppState -> ViewConfig a msg -> a -> Html msg
viewUpdated appState config item =
    case config.updated of
        Just updated ->
            let
                time =
                    updated.getTime item

                readableTime =
                    TimeUtils.toReadableDateTime appState.timeZone time
            in
            span (tooltip readableTime)
                [ text <| String.format (gettext "Updated %s" appState.locale) [ inWordsWithConfig { withAffix = True } (locale appState) time updated.currentTime ] ]

        Nothing ->
            emptyNode


viewAction : AppState -> ListingDropdownItem msg -> Dropdown.DropdownItem msg
viewAction appState dropdownItem =
    case dropdownItem of
        ListingDropdownAction action ->
            let
                attrs =
                    case action.msg of
                        ListingActionLink route ->
                            [ href <| Routing.toUrl appState route ]

                        ListingActionMsg msg ->
                            [ onClick msg ]
            in
            Dropdown.anchorItem
                ([ class <| Maybe.withDefault "" action.extraClass
                 , dataCy ("listing-item_action_" ++ action.dataCy)
                 ]
                    ++ attrs
                )
                [ action.icon, text action.label ]

        ListingDropdownSeparator ->
            Dropdown.divider
