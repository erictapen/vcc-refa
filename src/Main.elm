module Main exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Constants exposing (baseUrlPath, refaBaseUrl)
import Css
import Dict exposing (Dict)
import Html exposing (Html, a, details, div, h1, h2, img, li, p, span, summary, text, ul)
import Html.Attributes exposing (class, href, id, src, style)
import Html.Styled
import Http
import List exposing (map)
import OmekaS as O exposing (..)
import Platform.Cmd
import Platform.Sub
import Select
import Select.Styles as SS
import Set
import String exposing (fromInt)
import Types
import Url
import Url.Builder as UB
import Url.Parser as UP exposing ((</>), (<?>))
import Url.Parser.Query as UQ
import Utils exposing (isNothing, removeNothings)


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlChange
        , onUrlChange = UrlChange << Browser.Internal
        }


type alias SelectElement =
    { selectState : Select.State
    , selectedItem : Maybe Int
    }


type alias Model =
    { mode : ArtwalkMode
    , filters : Filters
    , navigationKey : Browser.Navigation.Key
    , typesCache : Dict Int Type
    , hmoCache : Dict Int (Result String HMO)
    , selects : Dict String SelectElement
    }


type ArtwalkMode
    = Artwalk
        { position : Int
        }
    | Relational
        { paintingId : Int
        }


type alias Filters =
    { head : Maybe Int
    , upperBody : Maybe Int
    , lowerBody : Maybe Int
    , accessories : Maybe Int
    }


emptyFilters =
    { head = Nothing
    , upperBody = Nothing
    , lowerBody = Nothing
    , accessories = Nothing
    }


{-| A utility function to quickly get the correct filter from Filters.
-}
getFilter : Filters -> Types.FilterType -> Maybe Int
getFilter filters filterType =
    case filterType of
        Types.Head ->
            filters.head

        Types.UpperBody ->
            filters.upperBody

        Types.LowerBody ->
            filters.lowerBody

        Types.Accessories ->
            filters.accessories


queryParser : UQ.Parser Filters
queryParser =
    UQ.map4 Filters
        (UQ.int "head")
        (UQ.int "upperBody")
        (UQ.int "lowerBody")
        (UQ.int "accessories")


urlParser : UP.Parser (( ArtwalkMode, Filters ) -> a) a
urlParser =
    UP.s "refa"
        </> UP.oneOf
                [ UP.map (\i f -> ( Relational { paintingId = i }, f )) (UP.int <?> queryParser)
                , UP.map (\f -> ( Artwalk { position = 0 }, f )) (UP.top <?> queryParser)
                ]


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd.Cmd Msg )
init _ url key =
    let
        initialModel =
            { navigationKey = key
            , filters = emptyFilters
            , mode = Artwalk { position = 0 }
            , typesCache = Dict.empty
            , hmoCache = Dict.empty
            , selects =
                Dict.fromList <|
                    map (\ft -> ( Types.toIdentifier ft, emptySelect ft ))
                        Types.allFilterTypes
            }

        model =
            case UP.parse urlParser url of
                Just ( mode, filters ) ->
                    { initialModel | mode = mode, filters = filters }

                Nothing ->
                    initialModel
    in
    ( model
    , loadResources model.mode model.filters model.typesCache model.hmoCache
    )


emptySelect filterType =
    { selectState = Select.initState (Select.selectIdentifier <| Types.toIdentifier filterType)
    , selectedItem = Nothing
    }


type Msg
    = UrlChange UrlRequest
    | GotHMO Int (Result Http.Error HMO)
    | GotType Int (Result Http.Error Type)
    | SelectMsg Types.FilterType (Select.Msg Int)


subscriptions model =
    Sub.none


{-| Build an URL from those components of the model, that are reflected in the URL.
These components is everything besides the caches, as we want the whole
application state to be reflected in the URL.
-}
buildUrl : ArtwalkMode -> Filters -> String
buildUrl mode filters =
    baseUrlPath
        ++ (UB.absolute
                (case mode of
                    Relational r ->
                        [ fromInt r.paintingId ]

                    _ ->
                        []
                )
            <|
                removeNothings
                    [ Maybe.map (UB.int "head") filters.head
                    , Maybe.map (UB.int "upperBody") filters.upperBody
                    , Maybe.map (UB.int "lowerBody") filters.lowerBody
                    , Maybe.map (UB.int "accessories") filters.accessories
                    ]
           )


{-| Variation of buildUrl for building an url with filters and the painting id
to be displayed in relational mode.
-}
buildUrlRelationalFromId : Filters -> Int -> String
buildUrlRelationalFromId filters id =
    buildUrl (Relational { paintingId = id }) filters


update : Msg -> Model -> ( Model, Platform.Cmd.Cmd Msg )
update msg model =
    case msg of
        GotHMO paintingId hmoResult ->
            case hmoResult of
                Err (Http.BadBody str) ->
                    ( { model | hmoCache = Dict.insert paintingId (Err str) model.hmoCache }, Cmd.none )

                Err _ ->
                    ( { model
                        | hmoCache =
                            Dict.insert paintingId (Err "something went wrong") model.hmoCache
                      }
                    , Cmd.none
                    )

                Ok (HMO hmoRecord) ->
                    ( { model | hmoCache = Dict.insert paintingId (Ok (HMO hmoRecord)) model.hmoCache }
                      -- TODO check wether we have it in cache before making the request
                    , Cmd.batch <|
                        map (\id -> fetchTypeById GotType id) hmoRecord.p67refersTo
                    )

        GotType typeId typeResult ->
            case typeResult of
                Err _ ->
                    ( model, Cmd.none )

                Ok t ->
                    ( { model | typesCache = Dict.insert typeId t model.typesCache }
                    , Cmd.none
                    )

        UrlChange urlRequest ->
            case urlRequest of
                External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

                Internal url ->
                    case UP.parse urlParser url of
                        Just ( newMode, newFilters ) ->
                            if ( newMode, newFilters ) == ( model.mode, model.filters ) then
                                -- Otherwise we enter an infinite loop of pushUrl's
                                ( model, Cmd.none )

                            else
                                let
                                    newModel =
                                        { model | mode = newMode, filters = newFilters }
                                in
                                ( newModel
                                , Cmd.batch
                                    [ -- An internal request doesn't automatically pushUrl, so we have
                                      -- to do it by hand.
                                      Browser.Navigation.pushUrl model.navigationKey <|
                                        buildUrl newModel.mode newModel.filters
                                    , loadResources newMode model.filters model.typesCache model.hmoCache
                                    ]
                                )

                        Nothing ->
                            ( model, Cmd.none )

        SelectMsg filterType selectMsg ->
            let
                selectModel =
                    Maybe.withDefault (emptySelect filterType) <|
                        Dict.get (Types.toIdentifier filterType) model.selects

                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg selectModel.selectState

                oldFilters =
                    model.filters

                setFilter f =
                    case filterType of
                        Types.Head ->
                            { oldFilters | head = f }

                        Types.UpperBody ->
                            { oldFilters | upperBody = f }

                        Types.LowerBody ->
                            { oldFilters | lowerBody = f }

                        Types.Accessories ->
                            { oldFilters | accessories = f }

                newFilters =
                    case maybeAction of
                        Just (Select.Select filterId) ->
                            setFilter <| Just filterId

                        -- Is this even possible?
                        Just Select.Clear ->
                            setFilter Nothing

                        _ ->
                            model.filters

                newModel =
                    { model
                        | selects =
                            Dict.update (Types.toIdentifier filterType)
                                (Maybe.andThen
                                    (\oldSelect -> Just { oldSelect | selectState = updatedSelectState })
                                )
                                model.selects
                        , filters = newFilters
                    }
            in
            ( newModel
            , Cmd.batch
                [ Cmd.map (SelectMsg filterType) selectCmds
                , Browser.Navigation.pushUrl model.navigationKey <|
                    buildUrl newModel.mode newModel.filters
                , loadResources newModel.mode newModel.filters newModel.typesCache newModel.hmoCache
                ]
            )


{-| Whenever there is an update to a view, we use this one function to issue
the necessary GET requests so all the required data is in the cache. This
also checks wether data is already in the caches before loading.
-}
loadResources : ArtwalkMode -> Filters -> Dict Int Type -> Dict Int (Result String HMO) -> Cmd Msg
loadResources mode filters typesCache hmoCache =
    Cmd.batch <|
        -- In any case we check, wether all the filters are in the cache.
        map
            (Maybe.withDefault Cmd.none
                << Maybe.map
                    (\t ->
                        if Dict.member t typesCache then
                            Cmd.none

                        else
                            fetchTypeById GotType t
                    )
            )
            [ filters.head
            , filters.upperBody
            , filters.lowerBody
            , filters.accessories
            ]
            -- Stuff that we only need to fetch if we are in the correct mode for it.
            ++ (case mode of
                    Relational r ->
                        if Dict.member r.paintingId hmoCache then
                            [ Cmd.none ]

                        else
                            [ fetchHmoById GotHMO r.paintingId ]

                    Artwalk _ ->
                        [ Cmd.none ]
               )


paintingItem : (Int -> String) -> ( Int, String ) -> Html Msg
paintingItem paintingUrl ( id, name ) =
    li [] [ a [ href <| paintingUrl id ] [ text name ] ]


{-| This list of tags is currently only shown for developing purposes.
Eventually we are going to show only one tag.
-}
tagListItem : (Int -> String) -> Dict Int Type -> Int -> Html Msg
tagListItem paintingUrl typesCache typesId =
    li [] <|
        case Dict.get typesId typesCache of
            Nothing ->
                [ text "Loading..." ]

            Just (Type t) ->
                [ details []
                    [ let
                        refaUrl =
                            refaBaseUrl ++ fromInt typesId
                      in
                      summary []
                        (Maybe.withDefault
                            [ text <| fromInt typesId ++ ": "
                            , a [ href <| refaUrl ] [ text t.label ]
                            ]
                         <|
                            Maybe.map
                                (\( fType, label ) ->
                                    [ span [ style "font-weight" "bold" ]
                                        [ text <| Types.toString fType ++ ": " ]
                                    , a [ href <| refaUrl ] [ text label ]
                                    ]
                                )
                                (Dict.get typesId Types.filterTypeRegistry)
                        )
                    , span [] [ ul [] <| map (paintingItem paintingUrl) t.reverseP67 ]
                    ]
                ]


{-| The artwalk view.
-}
artwalkView filters typesCache hmoCache =
    div []
        [ h1 [] [ text "Artwalk view" ]
        , let
            setFilters : List Int
            setFilters =
                removeNothings <|
                    map (getFilter filters) Types.allFilterTypes

            typeCacheResults : List (Maybe Type)
            typeCacheResults =
                map (\t -> Dict.get t typesCache) setFilters

            typeCacheMiss : Bool
            typeCacheMiss =
                not <| List.foldr (&&) True <| map (not << isNothing) typeCacheResults
          in
          if typeCacheMiss then
            -- We don't have every of the four possible filters in the type cache yet.
            text "Loading"

          else
            case
                Maybe.map Set.toList <|
                    List.foldr
                        (\(Type tr) maybeSet ->
                            case maybeSet of
                                Nothing ->
                                    Just <| Set.fromList tr.reverseP67

                                Just set ->
                                    Just <| Set.intersect set <| Set.fromList tr.reverseP67
                        )
                        Nothing
                    <|
                        removeNothings typeCacheResults
            of
                Nothing ->
                    text "Artwalk for no filters at all is not implemented yet. Please make a choice."

                -- The intersection of all reverseP67's doesn't yield any results.
                Just [] ->
                    text "Zero results. Seems like your filters were to rigid. Try removing some!"

                Just paintings ->
                    ul [] <| map (paintingItem (buildUrlRelationalFromId filters)) paintings
        ]


{-| One of the four widgets that display pictures belonging to the selected category
-}
relationalTile :
    (Int -> String)
    -> Maybe Type
    -> Maybe Int
    -> Dict Int (Result String HMO)
    -> Types.FilterType
    -> Html Msg
relationalTile paintingUrl maybeType maybeTypeId hmoCache filterType =
    div []
        [ h2 [] [ text <| Types.toString filterType ]
        , case ( maybeTypeId, maybeType ) of
            ( Nothing, _ ) ->
                text <| "Select a filter for " ++ Types.toString filterType ++ "."

            ( _, Nothing ) ->
                text "Loading.."

            ( Just typeId, Just (Type tr) ) ->
                ul [] <| map (paintingItem paintingUrl) tr.reverseP67
        ]


{-| The relational view
-}
relationalView typesCache hmoCache paintingId filters =
    div []
        [ a
            [ style "float" "right"
            , href <| buildUrl (Artwalk { position = 0 }) filters
            ]
            [ text "Back to Artwalk" ]
        , h1 [] [ text "Relational view" ]
        , div []
            [ p []
                [ a [ href <| refaBaseUrl ++ fromInt paintingId ]
                    [ text "Link to the ReFa web interface" ]
                ]
            , case Dict.get paintingId hmoCache of
                Nothing ->
                    text "Loading..."

                Just (Err err) ->
                    text err

                Just (Ok (HMO hmoData)) ->
                    case hmoData.thumbnailUrl of
                        Nothing ->
                            text "Kein Thumbnail!"

                        Just thumbnailUrl ->
                            div []
                                [ img [ src thumbnailUrl ] []
                                , div [] <|
                                    map
                                        (\ft ->
                                            relationalTile
                                                (buildUrlRelationalFromId filters)
                                                (Maybe.andThen
                                                    (\t -> Dict.get t typesCache)
                                                    (getFilter filters ft)
                                                )
                                                (getFilter filters ft)
                                                hmoCache
                                                ft
                                        )
                                        Types.allFilterTypes
                                , div []
                                    [ h2 []
                                        [ text "Debug view"
                                        ]
                                    , text "This view is for debugging purposes only and will eventually be removed"
                                    , ul [] <|
                                        map
                                            (tagListItem
                                                (buildUrlRelationalFromId filters)
                                                typesCache
                                            )
                                            hmoData.p67refersTo
                                    ]
                                ]
            ]
        ]


{-| Get all the registered filtertypes as menuitems, for a given FilterType (e.g. Head)
-}
menuItemsForFilterType : Types.FilterType -> List (Select.MenuItem Int)
menuItemsForFilterType filterType =
    map (\( typeId, ( _, label ) ) -> Select.basicMenuItem { item = typeId, label = label }) <|
        Dict.toList <|
            Dict.filter
                (\k ( ft, _ ) -> ft == filterType)
                Types.filterTypeRegistry


{-| An individual filter widget, somewhat like a drop-down menu.
-}
filterWidget : Dict String SelectElement -> Types.FilterType -> Maybe Int -> List (Html Msg)
filterWidget selects filterType typeId =
    let
        select =
            Maybe.withDefault (emptySelect filterType) <| Dict.get (Types.toIdentifier filterType) selects
    in
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
                                                -- |> SS.setMenuBackgroundColor (Css.hex "#09161B")
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
filterBar : ArtwalkMode -> Dict String SelectElement -> Filters -> Html Msg
filterBar mode selects filters =
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
                            , href <| buildUrl (Relational { paintingId = 0 }) filters
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


view model =
    { title = "Visualising Cultural Collections – Restaging Fashion"
    , body =
        [ div [ id "header" ]
            [ h1 [] [ text "The Artwalk of History" ]
            , div [ id "headerlinks" ]
                -- TODO add link
                [ div [ class "headerlink", class "refabold", class "gelb" ] [ text "The Collection" ]

                -- TODO add link
                , div [ class "headerlink", class "primary-grey" ] [ text "About" ]

                -- TODO add link
                , div [ class "headerlink", class "primary-grey" ] [ text "Contact" ]
                ]
            ]
        , filterBar model.mode model.selects model.filters
        ]
            ++ (case model.mode of
                    Artwalk _ ->
                        [ artwalkView
                            model.filters
                            model.typesCache
                            model.hmoCache
                        ]

                    Relational r ->
                        [ relationalView
                            model.typesCache
                            model.hmoCache
                            r.paintingId
                            model.filters
                        ]
               )
    }
