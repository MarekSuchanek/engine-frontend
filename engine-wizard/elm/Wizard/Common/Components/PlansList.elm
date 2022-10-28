module Wizard.Common.Components.PlansList exposing
    ( ViewConfig
    , view
    )

import Gettext exposing (gettext)
import Html exposing (Html, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Maybe.Extra as Maybe
import Shared.Common.TimeUtils as TimeUtils
import Shared.Components.Badge as Badge
import Shared.Data.Plan exposing (Plan)
import Shared.Html exposing (emptyNode)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.Flash as Flash


type alias ViewConfig msg =
    { actions : Maybe (Plan -> List (Html msg)) }


view : AppState -> ViewConfig msg -> List Plan -> Html msg
view appState cfg plans =
    let
        viewTrialBadge plan =
            if plan.test then
                Badge.secondary [ class "ms-2" ] [ text (gettext "Trial" appState.locale) ]

            else
                emptyNode

        viewActiveBadge plan =
            let
                active =
                    case ( plan.since, plan.until ) of
                        ( Just since, Just until ) ->
                            TimeUtils.isBetween since until appState.currentTime

                        ( Just since, Nothing ) ->
                            TimeUtils.isAfter since appState.currentTime

                        ( Nothing, Just until ) ->
                            TimeUtils.isBefore until appState.currentTime

                        ( Nothing, Nothing ) ->
                            True
            in
            if active then
                Badge.success [ class "ms-2" ] [ text (gettext "Active" appState.locale) ]

            else
                emptyNode

        viewPlanTime mbTime =
            case mbTime of
                Just time ->
                    TimeUtils.toReadableDate appState.timeZone time

                Nothing ->
                    "-"

        viewPlan plan =
            let
                planActions =
                    case cfg.actions of
                        Just actions ->
                            td [ class "text-center" ] (actions plan)

                        Nothing ->
                            emptyNode
            in
            tr []
                [ td [ dataCy "plans-list_name" ] [ text plan.name, viewTrialBadge plan, viewActiveBadge plan ]
                , td [ dataCy "plans-list_users" ] [ text (Maybe.unwrap "-" String.fromInt plan.users) ]
                , td [ dataCy "plans-list_from" ] [ text (viewPlanTime plan.since) ]
                , td [ dataCy "plans-list_to" ] [ text (viewPlanTime plan.until) ]
                , planActions
                ]
    in
    if List.isEmpty plans then
        Flash.info appState (gettext "There are no plans for this instance." appState.locale)

    else
        let
            headerActions =
                if Maybe.isJust cfg.actions then
                    th [] []

                else
                    emptyNode
        in
        table [ class "table table-striped table-hover" ]
            [ thead []
                [ tr []
                    [ th [] [ text (gettext "Plan" appState.locale) ]
                    , th [] [ text (gettext "Users" appState.locale) ]
                    , th [] [ text (gettext "From" appState.locale) ]
                    , th [] [ text (gettext "To" appState.locale) ]
                    , headerActions
                    ]
                ]
            , tbody [] (List.map viewPlan plans)
            ]
