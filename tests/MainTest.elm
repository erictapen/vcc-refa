module MainTest exposing (suite)

import Expect
import FilterBar.Model exposing (Filters, emptyFilters)
import List exposing (map)
import Model exposing (ArtwalkMode(..), buildUrl, urlParser)
import Test exposing (Test, describe, test)
import Url
import Url.Parser


baseUrl : String
baseUrl =
    "https://erictapen.name"


urlModelPairs : List ( String, String, ( ArtwalkMode, Filters ) )
urlModelPairs =
    [ ( "simple artwalk"
      , baseUrl ++ "/refa/"
      , ( Artwalk { position = 0 }
        , emptyFilters
        )
      )
    , ( "simple relational"
      , baseUrl ++ "/refa/127"
      , ( Relational { paintingId = 127 }
        , emptyFilters
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
