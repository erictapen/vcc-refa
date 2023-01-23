module Main exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Html, a, details, div, img, li, p, span, summary, text, ul)
import Html.Attributes exposing (href, src, style)
import Http
import List exposing (map)
import OmekaS as O exposing (..)
import Platform.Cmd
import Platform.Sub
import String exposing (fromInt)
import Url
import Url.Parser as UP exposing ((</>))


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


type alias Model =
    { mode : ArtwalkMode
    , navigationKey : Browser.Navigation.Key
    , typesCache : Dict Int Type
    , hmoCache : Dict Int (Result String HMO)
    }


type ArtwalkMode
    = Artwalk
        { paintings : List Int
        }
    | Relational
        { paintingId : Int
        }


paintingIdParser : UP.Parser (ArtwalkMode -> a) a
paintingIdParser =
    let
        refaUrl =
            UP.oneOf
                [ UP.map (\i -> Relational { paintingId = i }) UP.int
                , UP.map (Artwalk { paintings = [ 127 ] }) UP.top
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
            , mode = Artwalk { paintings = [ 127 ] }
            , typesCache = Dict.empty
            , hmoCache = Dict.empty
            }

        model =
            case UP.parse paintingIdParser url of
                Just mode ->
                    { initialModel | mode = mode }

                Nothing ->
                    initialModel
    in
    ( model
    , case model.mode of
        Relational r ->
            Cmd.batch
                [ fetchHmoById GotHMO r.paintingId
                , Browser.Navigation.pushUrl key (fromInt r.paintingId)
                ]

        _ ->
            Cmd.none
    )


type Msg
    = UrlChange UrlRequest
    | GotHMO Int (Result Http.Error HMO)
    | GotType Int (Result Http.Error Type)


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
                Internal url ->
                    case ( UP.parse paintingIdParser url, model.mode ) of
                        ( Just (Relational rUrl), Relational rModel ) ->
                            if rUrl.paintingId == rModel.paintingId then
                                ( model, Cmd.none )

                            else
                                ( { model | mode = Relational rUrl }
                                , Cmd.batch
                                    [ Browser.Navigation.pushUrl model.navigationKey (Url.toString url)

                                    -- TODO check wether we have it in cache before making the request
                                    , fetchHmoById GotHMO rUrl.paintingId
                                    ]
                                )

                        ( Just (Artwalk aUrl), Artwalk aModel ) ->
                            ( { model | mode = Artwalk aUrl }
                            , Cmd.batch
                                [ -- TODO check wether we have it in cache before making the request
                                  Cmd.batch <|
                                    map (\id -> fetchHmoById GotHMO id) aUrl.paintings
                                ]
                            )

                        _ ->
                            ( model, Cmd.none )

                External url ->
                    ( model
                    , Browser.Navigation.load url
                    )


pictureItem : ( Int, String ) -> Html Msg
pictureItem ( id, name ) =
    li [] [ a [ href <| fromInt id ] [ text name ] ]


tagListItem : Dict Int Type -> Int -> Html Msg
tagListItem typesCache typesId =
    li [] <|
        case Dict.get typesId typesCache of
            Nothing ->
                [ text "Loading..." ]

            Just (Type t) ->
                [ details []
                    [ summary []
                        [ a [ href <| refaBaseUrl ++ fromInt typesId ] [ text t.label ]
                        ]
                    , span [] [ ul [] <| map pictureItem t.reverseP67 ]
                    ]
                ]


artwalkView =
    div []
        [ div
            [ style "font-weight" "bold"
            ]
            [ text "Artwalk view" ]
        ]


relationalView typesCache hmoCache paintingId =
    div []
        [ div
            [ style "font-weight" "bold"
            ]
            [ text "Relational view" ]
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


view model =
    { title = "Visualising Cultural Collections – Restaging Fashion"
    , body =
        [ case model.mode of
            Artwalk _ ->
                artwalkView

            Relational r ->
                relationalView model.typesCache model.hmoCache r.paintingId
        ]
    }
