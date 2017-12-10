module DayPickerInput exposing (..)

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import DayPicker


type alias Props msg =
    { dayPickerProps : DayPicker.PropsData msg }


dayPickerInput : Props msg -> Html msg
dayPickerInput { dayPickerProps } =
    div []
        [ input [] []
        , DayPicker.dayPicker dayPickerProps
        ]
