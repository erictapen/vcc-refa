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
    { key : Browser.Navigation.Key
    , paintingId : Int
    , painting : Result String O.HMO
    , typesCache : Dict Int Type
    }


paintingIdParser : UP.Parser (Int -> a) a
paintingIdParser =
    UP.oneOf
        [ UP.int
        , UP.s "refa" </> UP.int
        ]


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd.Cmd Msg )
init _ url key =
    let
        initialModel =
            { key = key
            , paintingId = 127
            , painting = Err "Das Bild wird noch geladen."
            , typesCache = Dict.empty
            }

        model =
            case UP.parse paintingIdParser url of
                Just id ->
                    { initialModel | paintingId = id }

                Nothing ->
                    initialModel
    in
    ( model
    , Cmd.batch
        [ fetchHmoById GotHMO model.paintingId
        , Browser.Navigation.pushUrl key (fromInt model.paintingId)
        ]
    )


type Msg
    = UrlChange UrlRequest
    | GotHMO (Result Http.Error HMO)
    | GotType Int (Result Http.Error Type)


subscriptions model =
    Sub.none


update msg model =
    case msg of
        GotHMO hmoResult ->
            case hmoResult of
                Err (Http.BadBody str) ->
                    ( { model | painting = Err str }, Cmd.none )

                Err _ ->
                    ( { model | painting = Err "something went wrong" }, Cmd.none )

                Ok (HMO hmoRecord) ->
                    ( { model | painting = Ok (HMO hmoRecord) }
                    , Cmd.batch <|
                        map (\id -> fetchTypeById (GotType id) id) hmoRecord.p67refersTo
                    )

        GotType typeId typeResult ->
            case typeResult of
                Err (Http.BadBody str) ->
                    ( { model | painting = Err str }, Cmd.none )

                Err _ ->
                    ( { model | painting = Err "something went wrong" }, Cmd.none )

                Ok t ->
                    ( { model | typesCache = Dict.insert typeId t model.typesCache }
                    , Cmd.none
                    )

        UrlChange urlRequest ->
            case urlRequest of
                Internal url ->
                    case UP.parse paintingIdParser url of
                        Nothing ->
                            ( model, Cmd.none )

                        Just id ->
                            if id == model.paintingId then
                                ( model, Cmd.none )

                            else
                                ( { model | paintingId = id }
                                , Cmd.batch
                                    [ Browser.Navigation.pushUrl model.key (Url.toString url)
                                    , fetchHmoById GotHMO id
                                    ]
                                )

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


view model =
    { title = "Visualising Cultural Collections – Restaging Fashion"
    , body =
        [ div []
            [ div
                [ style "font-weight" "bold"
                ]
                [ text "Hello world 💃" ]
            , div []
                [ case model.painting of
                    Err err ->
                        text err

                    Ok (HMO hmoData) ->
                        case hmoData.thumbnailUrl of
                            Nothing ->
                                text "Kein Thumbnail!"

                            Just thumbnailUrl ->
                                div []
                                    [ img [ src thumbnailUrl ] []
                                    , p []
                                        [ a [ href <| refaBaseUrl ++ fromInt model.paintingId ]
                                            [ text "Link to the ReFa web interface" ]
                                        ]
                                    , ul [] <| map (tagListItem model.typesCache) hmoData.p67refersTo
                                    ]
                ]
            ]
        ]
    }
