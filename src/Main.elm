module Main exposing (..)

import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (div, img, text)
import Html.Attributes exposing (src, style)
import Http
import List exposing (map)
import OmekaS as O exposing (..)
import Platform.Cmd
import Platform.Sub
import Url


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


type alias Model =
    { painting : Result String O.HMO
    , typesCache : Dict Int Type
    }


initialModel : Model
initialModel =
    { painting = Err "Das Bild wird noch geladen."
    , typesCache = Dict.empty
    }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd.Cmd Msg )
init _ url key =
    ( initialModel, fetchHmoById GotHMO 127 )


type Msg
    = OnUrlChange Url.Url
    | OnUrlRequest Browser.UrlRequest
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

        _ ->
            ( model, Cmd.none )


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
                                img [ src thumbnailUrl ] []
                ]
            ]
        ]
    }
