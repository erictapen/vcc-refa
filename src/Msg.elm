module Msg exposing (Msg(..))

import Browser exposing (UrlRequest)
import Http
import OmekaS
import Select
import Types


type Msg
    = UrlChange UrlRequest
    | GotHMO Int (Result Http.Error OmekaS.HMO)
    | GotType Int (Result Http.Error OmekaS.Type)
    | SelectMsg Types.FilterType (Select.Msg Int)
