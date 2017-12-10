module DayPickerInput exposing (..)

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import DayPicker


type alias Props =
    { dayPickerProps : DayPicker.Props }


dayPickerInput : Props -> (DayPicker.Props -> msg) -> Html msg
dayPickerInput { dayPickerProps } onChange =
    div []
        [ input [] []
        , DayPicker.dayPicker dayPickerProps onChange
        ]
