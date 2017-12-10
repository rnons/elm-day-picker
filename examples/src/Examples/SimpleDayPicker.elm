module Examples.SimpleDayPicker exposing (..)

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import DayPicker


type Msg
    = DayChange (DayPicker.PropsData Msg) (Maybe Date)


type alias Model =
    { props : DayPicker.PropsData Msg
    }


init : Date -> ( Model, Cmd Msg )
init today =
    let
        props =
            DayPicker.mkDefaultPropsData today DayChange
    in
        ( { props = props }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DayChange props mSelected ->
            ( { props = props }, Cmd.none )


view : Model -> Html Msg
view model =
    DayPicker.dayPicker model.props
