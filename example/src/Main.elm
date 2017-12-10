module Main exposing (..)

import Html exposing (Html, div, text)
import Date exposing (Date)
import Task
import DayPicker
import DayPickerInput


type Msg
    = DayChange (DayPicker.PropsData Msg)
    | Today Date


type alias Model =
    { selectedDay : Maybe Date
    , today : Maybe Date
    , dayPickerProps : Maybe (DayPicker.PropsData Msg)
    , dayPickerInputProps : Maybe (DayPickerInput.Props Msg)
    }


init : ( Model, Cmd Msg )
init =
    ( { selectedDay = Nothing
      , today = Nothing
      , dayPickerProps = Nothing
      , dayPickerInputProps = Nothing
      }
    , Task.perform Today Date.now
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DayChange props ->
            ( { model | dayPickerProps = Just props }, Cmd.none )

        Today date ->
            let
                dayPickerProps =
                    DayPicker.mkDefaultProps date DayChange

                (DayPicker.PropsData props) =
                    dayPickerProps

                dayPickerInputProps =
                    { dayPickerProps = DayPicker.PropsData { props | monthCount = 2 } }
            in
                ( { model
                    | today = Just date
                    , dayPickerProps = Just dayPickerProps
                    , dayPickerInputProps = Just dayPickerInputProps
                  }
                , Cmd.none
                )


view : Model -> Html Msg
view { today, dayPickerProps, dayPickerInputProps } =
    let
        dayPicker =
            case dayPickerProps of
                Nothing ->
                    text ""

                Just props ->
                    DayPicker.dayPicker props

        dayPickerInput =
            case dayPickerInputProps of
                Nothing ->
                    text ""

                Just props ->
                    DayPickerInput.dayPickerInput props
    in
        div
            []
            [ dayPicker
            , dayPickerInput
            ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
