module Main exposing (main)

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
