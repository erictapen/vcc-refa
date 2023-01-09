module Main exposing (..)

import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (div, img, text)
import Html.Attributes exposing (src, style)
import Http
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


subscriptions model =
    Sub.none


update msg model =
    case msg of
        GotHMO itemResult ->
            case itemResult of
                Err (Http.BadBody str) ->
                    ( { model | painting = Err str }, Cmd.none )

                Err _ ->
                    ( { model | painting = Err "something went wrong" }, Cmd.none )

                Ok item ->
                    ( { model | painting = Ok item }, Cmd.none )

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

                    Ok (HMO itemData) ->
                        case itemData.thumbnailUrl of
                            Nothing ->
                                text "Kein Thumbnail!"

                            Just thumbnailUrl ->
                                img [ src thumbnailUrl ] []
                ]
            ]
        ]
    }
