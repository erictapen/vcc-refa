module FilterBar.View exposing (view)

import Css
import Dict exposing (Dict)
import FilterBar.Model exposing (Filters, SelectElement, emptyFilters, emptySelect)
import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class, href, id, style)
import Html.Styled
import List exposing (map)
import Model exposing (ArtwalkMode(..), buildUrl)
import Msg exposing (Msg(..))
import Select
import Select.Styles as SS
import String exposing (fromInt)
import Types


{-| Get all the registered filtertypes as menuitems, for a given FilterType (e.g. Head)
-}
menuItemsForFilterType : Types.FilterType -> List (Select.MenuItem Int)
menuItemsForFilterType filterType =
    map (\( typeId, ( _, label ) ) -> Select.basicMenuItem { item = typeId, label = label }) <|
        Dict.toList <|
            Dict.filter
                (\_ ( ft, _ ) -> ft == filterType)
                Types.filterTypeRegistry


{-| An individual filter widget, somewhat like a drop-down menu.
-}
filterWidget : Dict String SelectElement -> Types.FilterType -> Maybe Int -> List (Html Msg)
filterWidget selects filterType typeId =
    [ span
        [ style "font-weight" "bold"
        , style "color" <| Types.toColor filterType
        , class "filter-description"
        , class <| Types.toIdentifier filterType
        ]
        [ div [ class "filter-icon" ] [ Types.toIcon filterType ]
        , text <| Types.toString filterType
        ]
    , div
        [ class "filter-select"
        , class <| Types.toIdentifier filterType
        ]
        [ case ( typeId, Maybe.andThen (\t -> Dict.get t Types.filterTypeRegistry) typeId ) of
            ( Just t, Nothing ) ->
                text <| "Type " ++ fromInt t ++ " is not registered"

            -- TODO: we never check, wether the typeId set via url actually
            -- belongs to the right category. Maybe not important enough to
            -- check?
            ( _, registeredFilterType ) ->
                let
                    select =
                        Maybe.withDefault (emptySelect filterType) <| Dict.get (Types.toIdentifier filterType) selects
                in
                Html.Styled.toUnstyled <|
                    Html.Styled.map (SelectMsg filterType) <|
                        Select.view
                            (Select.single
                                (case ( typeId, registeredFilterType ) of
                                    ( Just t, Just ( _, humanReadableLabel ) ) ->
                                        Just <| Select.basicMenuItem { item = t, label = humanReadableLabel }

                                    _ ->
                                        Nothing
                                )
                                |> Select.state select.selectState
                                |> Select.menuItems (menuItemsForFilterType filterType)
                                |> Select.placeholder "Add Filter +"
                                |> Select.clearable True
                                |> Select.setStyles
                                    (SS.default
                                        |> SS.setMenuStyles
                                            (SS.getMenuConfig SS.default
                                                |> SS.setMenuBorderRadius 14
                                            )
                                        |> SS.setControlStyles
                                            (SS.getControlConfig SS.default
                                                |> SS.setControlBackgroundColor (Css.hex "#09161B")
                                                |> SS.setControlBackgroundColorHover (Css.hex "#09161B")
                                                |> SS.setControlColor (Css.hex "#E2BBDB")
                                                |> SS.setControlSelectedColor (Css.hex "#E2BBDB")
                                                |> SS.setControlBorderRadius 14
                                            )
                                    )
                            )
        ]
    ]


{-| The filter bar displayed ontop of the site, that lets users select their
filters for each of the four filter categories.
-}
view : ArtwalkMode -> Dict String SelectElement -> Filters -> Html Msg
view mode selects filters =
    div
        [ id "filterbar"
        ]
        ([ div [ id "filter-sharpsign" ] [ text "#" ]

         -- TODO display actual history count
         , div [ id "filter-historycount" ] [ text "1" ]
         ]
            ++ filterWidget selects Types.Head filters.head
            ++ filterWidget selects Types.UpperBody filters.upperBody
            ++ filterWidget selects Types.LowerBody filters.lowerBody
            ++ filterWidget selects Types.Accessories filters.accessories
            ++ [ case mode of
                    Artwalk _ ->
                        a
                            [ id "go-to-network-view"
                            , class "mode-button"

                            -- TODO use correct paintingId here
                            , href <| buildUrl (Relational { paintingId = 102 }) filters
                            ]
                            [ text "Go to Network View" ]

                    Relational _ ->
                        a
                            [ id "go-to-artwalk-view"
                            , class "mode-button"
                            , href <| buildUrl (Artwalk { position = 0 }) filters
                            ]
                            [ text "Go to Artwalk View" ]
               , a
                    [ id "clear-all"
                    , href <| buildUrl mode emptyFilters
                    ]
                    [ text "Clear all" ]
               ]
        )
