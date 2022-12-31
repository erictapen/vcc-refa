module Main exposing (..)

import Browser
import Browser.Navigation
import Platform.Cmd
import Platform.Sub
import Html exposing (div)
import Url

main = Browser.application {
    init = init,
    view = view,
    update = update,
    subscriptions = subscriptions,
    onUrlRequest = OnUrlRequest,
    onUrlChange = OnUrlChange
  }

init : Int -> Url.Url -> Browser.Navigation.Key -> ( Int, Cmd.Cmd Msg )
init _ url key = ( 0, Cmd.none )

type Msg = OnUrlChange Url.Url | OnUrlRequest Browser.UrlRequest

subscriptions model = Sub.none

update msg model = (model, Cmd.none)

view model = {
    title = "Visualising Cultural Collections – Restaging Fashion",
    body = [ div [] [] ]
  }
