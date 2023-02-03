module Main exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Html, a, details, div, h1, img, li, p, span, summary, text, ul)
import Html.Attributes exposing (href, src, style)
import Http
import List exposing (map)
import OmekaS as O exposing (..)
import Platform.Cmd
import Platform.Sub
import Select
import String exposing (fromInt)
import Types
import Url
import Url.Builder as UB
import Url.Parser as UP exposing ((</>), (<?>))
import Url.Parser.Query as UQ


refaBaseUrl =
    "https://uclab.fh-potsdam.de/refa/admin/item/"


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
    , items : List (Select.MenuItem Int)
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


queryParser : UQ.Parser Filters
queryParser =
    UQ.map4 Filters
        (UQ.int "head")
        (UQ.int "upperBody")
        (UQ.int "lowerBody")
        (UQ.int "accessories")


urlParser : UP.Parser (( ArtwalkMode, Filters ) -> a) a
urlParser =
    let
        refaUrl =
            UP.oneOf
                [ UP.map (\i f -> ( Relational { paintingId = i }, f )) (UP.int <?> queryParser)
                , UP.map (\f -> ( Artwalk { position = 0 }, f )) (UP.top <?> queryParser)
                ]
    in
    -- We allow "refa/" in front of the actual url, for easier hosting atm
    UP.oneOf
        [ refaUrl
        , UP.s "refa" </> refaUrl
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
                        [ Types.Head
                        , Types.UpperBody
                        , Types.LowerBody
                        , Types.Accessories
                        ]
            }

        model =
            case UP.parse urlParser url of
                Just ( mode, filters ) ->
                    { initialModel | mode = mode, filters = filters }

                Nothing ->
                    initialModel
    in
    ( model
    , loadResources model.mode model.typesCache model.hmoCache
    )


emptySelect filterType =
    { selectState = Select.initState (Select.selectIdentifier <| Types.toIdentifier filterType)
    , items = []
    , selectedItem = Nothing
    }


type Msg
    = UrlChange UrlRequest
    | GotHMO Int (Result Http.Error HMO)
    | GotType Int (Result Http.Error Type)
    | SelectMsg Types.FilterType (Select.Msg (Maybe Int))


subscriptions model =
    Sub.none


{-| Build an URL from those components of the model, that are reflected in the URL.
These components is everything besides the caches, as we want the whole
application state to be reflected in the URL.
-}
buildUrl : ArtwalkMode -> Filters -> String
buildUrl mode filters =
    UB.absolute
        (case mode of
            Relational r ->
                [ fromInt r.paintingId ]

            _ ->
                []
        )
    <|
        List.filterMap identity
            [ Maybe.map (UB.int "head") filters.head
            , Maybe.map (UB.int "upperBody") filters.upperBody
            , Maybe.map (UB.int "lowerBody") filters.lowerBody
            , Maybe.map (UB.int "accessories") filters.accessories
            ]


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
                                    , loadResources newMode model.typesCache model.hmoCache
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
                            setFilter filterId

                        -- Is this even possible?
                        Just Select.Clear ->
                            setFilter Nothing

                        _ ->
                            model.filters
            in
            ( { model
                | selects =
                    Dict.update (Types.toIdentifier filterType)
                        (Maybe.andThen (\oldSelect -> Just { oldSelect | selectState = updatedSelectState }))
                        model.selects
                , filters = newFilters
              }
            , Cmd.map (SelectMsg filterType) selectCmds
            )


{-| Whenever there is an update to a view, we use this one function to issue
the necessary GET requests so all the required data is in the cache. This
also checks wether data is already in the caches before loading.
-}
loadResources : ArtwalkMode -> Dict Int Type -> Dict Int (Result String HMO) -> Cmd Msg
loadResources newMode typesCache hmoCache =
    case newMode of
        Relational r ->
            if Dict.member r.paintingId hmoCache then
                Cmd.none

            else
                fetchHmoById GotHMO r.paintingId

        _ ->
            Cmd.none


pictureItem : ( Int, String ) -> Html Msg
pictureItem ( id, name ) =
    li [] [ a [ href <| fromInt id ] [ text name ] ]


{-| This list of tags is currently only shown for developing purposes.
Eventually we are going to show only one tag.
-}
tagListItem : Dict Int Type -> Int -> Html Msg
tagListItem typesCache typesId =
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
                                (Dict.get typesId Types.filterTypes)
                        )
                    , span [] [ ul [] <| map pictureItem t.reverseP67 ]
                    ]
                ]


{-| The artwalk view.
-}
artwalkView =
    div []
        [ h1 [] [ text "Artwalk view" ]
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
                                , ul [] <| map (tagListItem typesCache) hmoData.p67refersTo
                                ]
            ]
        ]


{-| An individual filter widget, somewhat like a drop-down menu.
-}
filterWidget : Dict String SelectElement -> Types.FilterType -> Maybe Int -> Html Msg
filterWidget selects filterType typeId =
    let
        select =
            Maybe.withDefault (emptySelect filterType) <| Dict.get (Types.toIdentifier filterType) selects
    in
    div [ style "width" "20%" ]
        [ span [ style "font-weight" "bold" ] [ text <| Types.toString filterType ++ ": " ]
        , text <|
            case ( typeId, Maybe.andThen (\t -> Dict.get t Types.filterTypes) typeId ) of
                ( Nothing, _ ) ->
                    "None selected"

                ( Just t, Nothing ) ->
                    "Type " ++ fromInt t ++ " is not registered"

                ( Just t, Just ( registeredFilterType, humanReadableLabel ) ) ->
                    if registeredFilterType == filterType then
                        humanReadableLabel

                    else
                        "Invalid type id for category "
                            ++ fromInt t
                            ++ ", is "
                            ++ Types.toString registeredFilterType
                            ++ "."
        ]


{-| The filter bar displayed ontop of the site, that lets users select their
filters for each of the four filter categories.
-}
filterBar : Dict String SelectElement -> Filters -> Html Msg
filterBar selects filters =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        ]
        [ filterWidget selects Types.Head filters.head
        , filterWidget selects Types.UpperBody filters.upperBody
        , filterWidget selects Types.LowerBody filters.lowerBody
        , filterWidget selects Types.Accessories filters.accessories
        ]


view model =
    { title = "Visualising Cultural Collections – Restaging Fashion"
    , body =
        [ filterBar model.selects model.filters
        , case model.mode of
            Artwalk _ ->
                artwalkView

            Relational r ->
                relationalView model.typesCache model.hmoCache r.paintingId model.filters
        ]
    }
