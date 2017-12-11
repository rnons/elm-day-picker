module Examples.SimpleDayPicker exposing (..)

import Date exposing (Date)
import Set
import Html exposing (..)
import Html.Attributes exposing (..)
import DayPicker


type Msg
    = DayChange (DayPicker.PropsData Msg) (Maybe Date)


type alias Model =
    { propsData : DayPicker.PropsData Msg
    }


init : Date -> ( Model, Cmd Msg )
init today =
    let
        propsData =
            DayPicker.mkDefaultPropsData today DayChange
    in
        ( { propsData = propsData }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DayChange propsData mSelected ->
            case mSelected of
                Just selected ->
                    let
                        (DayPicker.PropsData props) =
                            propsData

                        newProps =
                            { props | selectedDays = Set.singleton selected }
                    in
                        ( { propsData = DayPicker.PropsData newProps }, Cmd.none )

                Nothing ->
                    ( { propsData = propsData }, Cmd.none )


view : Model -> Html Msg
view model =
    DayPicker.dayPicker model.propsData
