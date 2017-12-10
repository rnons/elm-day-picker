module DayPickerInput exposing (..)

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import DayPicker


type Props msg
    = Props { dayPickerProps : DayPicker.Props msg }


dayPickerInput : Props msg -> Html msg
dayPickerInput (Props { dayPickerProps }) =
    div []
        [ input [] []
        , DayPicker.dayPicker dayPickerProps
        ]
