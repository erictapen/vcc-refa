module Main exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Constants exposing (refaBaseUrl)
import Dict exposing (Dict)
import FilterBar.Model
import FilterBar.View
import Html exposing (Html, a, details, div, h1, h2, img, li, p, span, summary, text, ul)
import Html.Attributes exposing (class, href, id, src, style)
import Http
import List exposing (map)
import Model exposing (ArtwalkMode(..), Model, buildUrl, urlParser)
import Msg exposing (Msg(..))
import OmekaS exposing (HMO(..), Type(..), fetchHmoById, fetchTypeById)
import Platform.Cmd
import Platform.Sub
import Select
import Set
import String exposing (fromInt)
import Types
import Url
import Url.Parser as UP
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


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd.Cmd Msg )
init _ url key =
    let
        initialModel =
            { navigationKey = key
            , filters = FilterBar.Model.emptyFilters
            , mode = Artwalk { position = 0 }
            , typesCache = Dict.empty
            , hmoCache = Dict.empty
            , selects =
                Dict.fromList <|
                    map (\ft -> ( Types.toIdentifier ft, FilterBar.Model.emptySelect ft ))
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


subscriptions model =
    Sub.none


{-| Variation of buildUrl for building an url with filters and the painting id
to be displayed in relational mode.
-}
buildUrlRelationalFromId : FilterBar.Model.Filters -> Int -> String
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
                    Maybe.withDefault (FilterBar.Model.emptySelect filterType) <|
                        Dict.get (Types.toIdentifier filterType) model.selects

                ( maybeAction, updatedSelectState, selectCmds ) =
                    Select.update selectMsg selectModel.selectState

                oldFilters : FilterBar.Model.Filters
                oldFilters =
                    model.filters

                setFilter : Maybe Int -> FilterBar.Model.Filters
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

                newFilters : FilterBar.Model.Filters
                newFilters =
                    case maybeAction of
                        Just (Select.Select filterId) ->
                            setFilter <| Just filterId

                        -- Is this even possible?
                        Just Select.Clear ->
                            setFilter Nothing

                        _ ->
                            model.filters

                newModel : Model
                newModel =
                    { model
                        | selects =
                            Dict.update (Types.toIdentifier filterType)
                                (Maybe.map
                                    (\oldSelect -> { oldSelect | selectState = updatedSelectState })
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
loadResources : ArtwalkMode -> FilterBar.Model.Filters -> Dict Int Type -> Dict Int (Result String HMO) -> Cmd Msg
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
                        refaUrl : String
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
                    map (FilterBar.Model.getFilter filters) Types.allFilterTypes

            typeCacheResults : List (Maybe Type)
            typeCacheResults =
                map (\t -> Dict.get t typesCache) setFilters

            typeCacheMiss : Bool
            typeCacheMiss =
                not <| List.all identity <| map (not << isNothing) typeCacheResults
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

            ( Just _, Just (Type tr) ) ->
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
                                                    (FilterBar.Model.getFilter filters ft)
                                                )
                                                (FilterBar.Model.getFilter filters ft)
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
        , FilterBar.View.viewFilterBar model.mode model.selects model.filters
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
