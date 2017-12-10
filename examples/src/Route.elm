module Route exposing (Route(..), fromLocation)

import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), (<?>), Parser, oneOf, parseHash, s, string, stringParam)


type Route
    = Home
    | SimpleDayPicker
    | SimpleDayPickerInput


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Url.map Home (s "")
        , Url.map SimpleDayPicker (s "simple-day-picker")
        , Url.map SimpleDayPickerInput (s "simple-day-picker-input")
        ]


fromLocation : Location -> Maybe Route
fromLocation location =
    parseHash routeParser location
