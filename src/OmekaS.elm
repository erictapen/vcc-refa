module OmekaS exposing (OItem(..), fetchItemById, oItemDecoder)

import Http
import Json.Decode as JD exposing (andThen, at, field, int, list, maybe, string, succeed)
import List exposing (member)
import String exposing (fromInt)


baseUrl =
    "https://uclab.fh-potsdam.de/refa/api"


type OItem
    = E24Hmo
        { id : Int
        , thumbnailUrl : Maybe String
        , p67refersTo : List Int
        }
    | E55Type { label : String }
    | UnknownOItem


fetchItemById msgConstructor id =
    Http.get
        { url = baseUrl ++ "/items/" ++ fromInt id
        , expect = Http.expectJson msgConstructor oItemDecoder
        }


oItemDecoder : JD.Decoder OItem
oItemDecoder =
    let
        chooseOItemVariant types =
            if member "ecrm:E22_Human-Made_Object" types then
                e24HmoDecoder

            else if member "ecrm:E55_Type" types then
                e55TypeDecoder

            else
                succeed UnknownOItem
    in
    field "@type" (list string)
        |> andThen chooseOItemVariant


e24Hmo id thumbnailUrl p67refersTo =
    { id = id
    , thumbnailUrl = thumbnailUrl
    , p67refersTo = p67refersTo
    }


e24HmoDecoder : JD.Decoder OItem
e24HmoDecoder =
    JD.map E24Hmo <|
        JD.map3 e24Hmo
            (field "o:id" int)
            (maybe (at [ "thumbnail_display_urls", "medium" ] string))
            (JD.map (List.filterMap identity) (field "ecrm:P67_refers_to" (list oResourceDecoder)))


e55TypeDecoder : JD.Decoder OItem
e55TypeDecoder =
    JD.map (\l -> E55Type { label = l }) (field "o:title" string)


oResourceDecoder : JD.Decoder (Maybe Int)
oResourceDecoder =
    maybe (field "value_resource_id" int)
