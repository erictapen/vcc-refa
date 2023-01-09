module OmekaS exposing
    ( HMO(..)
    , Type(..)
    , e24HmoDecoder
    , fetchHmoById
    , fetchTypeById
    )

import Http
import Json.Decode as JD exposing (andThen, at, fail, field, int, list, maybe, string, succeed)
import List exposing (member)
import String exposing (fromInt)


baseUrl =
    "https://uclab.fh-potsdam.de/refa/api"


{-| E24 Human made object
-}
type HMO
    = HMO
        { id : Int
        , thumbnailUrl : Maybe String
        , p67refersTo : List Int
        }


{-| E55 Type
-}
type Type
    = Type
        { label : String
        }


fetchHmoById =
    fetchOItemById e24HmoDecoder


fetchTypeById =
    fetchOItemById e55TypeDecoder


fetchOItemById decoder msgConstructor id =
    Http.get
        { url = baseUrl ++ "/items/" ++ fromInt id
        , expect = Http.expectJson msgConstructor decoder
        }


e24Hmo id thumbnailUrl p67refersTo =
    { id = id
    , thumbnailUrl = thumbnailUrl
    , p67refersTo = p67refersTo
    }


{-| The OmekaS API returns a bunch of stuff under the item/ endpoint. We have
to check the "@types" field to make sure we took the correct assumption
here.
-}
checkForCorrectType : String -> JD.Decoder a -> JD.Decoder a
checkForCorrectType checkStr decoder =
    field "@type" (list string)
        |> andThen
            (\types ->
                if member checkStr types then
                    decoder

                else
                    fail <| "The object should be of type " ++ checkStr ++ ", but isn't."
            )


e24HmoDecoder : JD.Decoder HMO
e24HmoDecoder =
    checkForCorrectType "ecrm:E22_Human-Made_Object" <|
        JD.map HMO <|
            JD.map3 e24Hmo
                (field "o:id" int)
                (maybe (at [ "thumbnail_display_urls", "medium" ] string))
                (JD.map (List.filterMap identity) (field "ecrm:P67_refers_to" (list oResourceDecoder)))


e55TypeDecoder : JD.Decoder Type
e55TypeDecoder =
    checkForCorrectType "ecrm:E55_Type" <|
        JD.map (\l -> Type { label = l }) (field "o:title" string)


oResourceDecoder : JD.Decoder (Maybe Int)
oResourceDecoder =
    maybe <| field "value_resource_id" int
