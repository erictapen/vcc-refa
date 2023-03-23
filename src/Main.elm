module Main exposing (main)

import Artwalk.Model
import Artwalk.View
import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Dict exposing (Dict)
import FilterBar.Model
import FilterBar.View
import Html exposing (div, h1, text)
import Html.Attributes exposing (class, id)
import Http
import List exposing (map)
import Model exposing (ArtwalkMode(..), Model, buildUrl, urlParser)
import Msg exposing (Msg(..))
import OmekaS exposing (HMO(..), Type, fetchHmoById, fetchTypeById)
import Platform.Cmd
import Platform.Sub
import Relational.View
import Select
import Types
import Url
import Url.Parser as UP
import Utils exposing (httpErrorToString)


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
                    , Cmd.none
                    )

        GotType typeId typeResult ->
            let
                newTypesCache =
                    Dict.insert typeId
                        (Result.mapError httpErrorToString typeResult)
                        model.typesCache
            in
            ( { model
                | typesCache = newTypesCache
              }
            , loadResources model.mode model.filters newTypesCache model.hmoCache
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

                ( filtersChanged, newFilters ) =
                    case maybeAction of
                        Just (Select.Select filterId) ->
                            ( True, setFilter <| Just filterId )

                        -- Is this even possible?
                        Just Select.Clear ->
                            ( True, setFilter Nothing )

                        _ ->
                            ( False, model.filters )

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

                -- Only do this when an element got actually selected (check maybeAction)
                , if filtersChanged then
                    Cmd.batch
                        [ Browser.Navigation.pushUrl model.navigationKey <|
                            buildUrl newModel.mode newModel.filters
                        , loadResources newModel.mode newModel.filters newModel.typesCache newModel.hmoCache
                        ]

                  else
                    Cmd.none
                ]
            )


{-| Whenever there is an update to a view, we use this one function to issue
the necessary GET requests so all the required data is in the cache. This
also checks wether data is already in the caches before loading.
-}
loadResources : ArtwalkMode -> FilterBar.Model.Filters -> Dict Int (Result String Type) -> Dict Int (Result String HMO) -> Cmd Msg
loadResources mode filters typesCache hmoCache =
    Cmd.batch <|
        map
            -- In any case we check, wether all the filters are in the cache.
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
                        case Dict.get r.paintingId hmoCache of
                            Just (Ok (HMO hmo)) ->
                                [ Cmd.batch <|
                                    map (fetchTypeById GotType) <|
                                        List.filter (\i -> not <| Dict.member i typesCache) <|
                                            hmo.p67refersTo
                                ]

                            Just (Err _) ->
                                [ Cmd.none ]

                            Nothing ->
                                [ fetchHmoById GotHMO r.paintingId ]

                    Artwalk _ ->
                        map (fetchHmoById GotHMO) <|
                            List.filter (\id -> not <| Dict.member id hmoCache) <|
                                map Tuple.first <|
                                    Maybe.withDefault [] <|
                                        Artwalk.Model.artwalkPaintings typesCache filters
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
        , FilterBar.View.view model.mode model.selects model.filters
        ]
            ++ (case model.mode of
                    Artwalk _ ->
                        [ Artwalk.View.view
                            model.filters
                            model.typesCache
                            model.hmoCache
                        ]

                    Relational r ->
                        [ Relational.View.view
                            model.typesCache
                            model.hmoCache
                            r.paintingId
                            model.filters
                        ]
               )
    }
