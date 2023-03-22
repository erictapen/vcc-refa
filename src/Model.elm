module Model exposing (ArtwalkMode(..), Model, buildUrl, queryParser, urlParser)

import Browser.Navigation
import Constants exposing (baseUrlPath, refaBaseUrl)
import Dict exposing (Dict)
import FilterBar.Model exposing (Filters, SelectElement)
import OmekaS
import Select
import String exposing (fromInt)
import Url.Builder as UB
import Url.Parser as UP exposing ((</>), (<?>))
import Url.Parser.Query as UQ
import Utils exposing (isNothing, removeNothings)


type alias Model =
    { mode : ArtwalkMode
    , filters : Filters
    , navigationKey : Browser.Navigation.Key
    , typesCache : Dict Int OmekaS.Type
    , hmoCache : Dict Int (Result String OmekaS.HMO)
    , selects : Dict String SelectElement
    }


type ArtwalkMode
    = Artwalk
        { position : Int
        }
    | Relational
        { paintingId : Int
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
    UP.s "refa"
        </> UP.oneOf
                [ UP.map (\i f -> ( Relational { paintingId = i }, f )) (UP.int <?> queryParser)
                , UP.map (\f -> ( Artwalk { position = 0 }, f )) (UP.top <?> queryParser)
                ]


{-| Build an URL from those components of the model, that are reflected in the URL.
These components is everything besides the caches, as we want the whole
application state to be reflected in the URL.
-}
buildUrl : ArtwalkMode -> Filters -> String
buildUrl mode filters =
    baseUrlPath
        ++ (UB.absolute
                (case mode of
                    Relational r ->
                        [ fromInt r.paintingId ]

                    _ ->
                        []
                )
            <|
                removeNothings
                    [ Maybe.map (UB.int "head") filters.head
                    , Maybe.map (UB.int "upperBody") filters.upperBody
                    , Maybe.map (UB.int "lowerBody") filters.lowerBody
                    , Maybe.map (UB.int "accessories") filters.accessories
                    ]
           )
