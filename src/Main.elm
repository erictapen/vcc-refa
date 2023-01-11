module Main exposing (..)

import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Html, div, img, li, text, ul)
import Html.Attributes exposing (src, style)
import Http
import List exposing (map)
import OmekaS as O exposing (..)
import Platform.Cmd
import Platform.Sub
import String exposing (fromInt)
import Url
import Url.Parser as UP exposing ((</>))


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
    { paintingId : Int
    , painting : Result String O.HMO
    , typesCache : Dict Int Type
    }


initialModel : Model
initialModel =
    { paintingId = 127
    , painting = Err "Das Bild wird noch geladen."
    , typesCache = Dict.empty
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
        , Browser.Navigation.pushUrl key (String.fromInt model.paintingId)
        ]
    )


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


tagListItem : Dict Int Type -> Int -> Html Msg
tagListItem typesCache typesId =
    li []
        [ case Dict.get typesId typesCache of
            Nothing ->
                text "Loading..."

            Just (Type t) ->
                text <| fromInt typesId ++ ": " ++ t.label
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
                                    , ul [] <| map (tagListItem model.typesCache) hmoData.p67refersTo
                                    ]
                ]
            ]
        ]
    }
