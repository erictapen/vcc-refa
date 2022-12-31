module OmekaS exposing (Item, fetchItem)

import Http
import Json.Decode as JD exposing (at, field, int, maybe, string)
import String exposing (fromInt)


baseUrl =
    "https://uclab.fh-potsdam.de/refa/api"


type alias Item =
    { id : Int
    , thumbnailUrl : Maybe String
    }


fetchItem msgConstructor id =
    Http.get
        { url = baseUrl ++ "/items/" ++ fromInt id
        , expect = Http.expectJson msgConstructor itemDecoder
        }


itemDecoder : JD.Decoder Item
itemDecoder =
    JD.map2 Item
        (field "o:id" int)
        (maybe (at [ "thumbnail_display_urls", "medium" ] string))
