module MainTest exposing (..)

import Expect
import List exposing (map)
import Main exposing (ArtwalkMode(..), Filters, buildUrl, urlParser)
import Test exposing (..)
import Url
import Url.Parser


baseUrl =
    "https://erictapen.name/refa"


urlModelPairs : List ( String, String, ( ArtwalkMode, Filters ) )
urlModelPairs =
    [ ( "simple artwalk"
      , baseUrl ++ "/"
      , ( Artwalk { position = 0 }
        , { head = Nothing
          , upperBody = Nothing
          , lowerBody = Nothing
          , accessories = Nothing
          }
        )
      )
    , ( "simple relational"
      , baseUrl ++ "/127"
      , ( Relational { paintingId = 127 }
        , { head = Nothing
          , upperBody = Nothing
          , lowerBody = Nothing
          , accessories = Nothing
          }
        )
      )
    ]


suite : Test
suite =
    describe "test that buildUrl and urlParser are idempotent" <|
        map
            (\( name, urlString, ( mode, filters ) ) ->
                test name <|
                    \_ ->
                        Expect.equal
                            ( Maybe.andThen (Url.Parser.parse urlParser) <|
                                Url.fromString urlString
                            , baseUrl ++ buildUrl mode filters
                            )
                            ( Just ( mode, filters )
                            , urlString
                            )
            )
            urlModelPairs
