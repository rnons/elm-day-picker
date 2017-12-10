module Examples.SimpleDayPickerInput exposing (..)

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import DayPicker
import DayPickerInput


type Msg
    = DayChange (DayPicker.PropsData Msg)


type alias Model =
    { props : DayPickerInput.Props Msg
    }


init : Date -> ( Model, Cmd Msg )
init today =
    let
        defaultProps =
            DayPicker.mkDefaultProps today DayChange

        dayPickerProps =
            { defaultProps | monthCount = 2 }
    in
        ( { props =
                { dayPickerProps = dayPickerProps
                }
          }
        , Cmd.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DayChange propsData ->
            let
                (DayPicker.PropsData dayPickerProps) =
                    propsData
            in
                ( { model | props = { dayPickerProps = dayPickerProps } }, Cmd.none )


view : Model -> Html Msg
view model =
    DayPickerInput.dayPickerInput model.props
