module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Date exposing (Date)
import Task
import Tuple
import Navigation
import Route
import DayPicker
import DayPickerInput
import Examples.SimpleDayPicker as ExSimpleDayPicker
import Examples.SimpleDayPickerInput as ExSimpleDayPickerInput


type Example
    = Empty
    | SimpleDayPicker ExSimpleDayPicker.Model
    | SimpleDayPickerInput ExSimpleDayPickerInput.Model
    | Pending Route.Route


type Msg
    = UrlChange Navigation.Location
    | DayChange (DayPicker.PropsData Msg)
    | Today Date
    | SimpleDayPickerMsg ExSimpleDayPicker.Msg
    | SimpleDayPickerInputMsg ExSimpleDayPickerInput.Msg


type alias Model =
    { selectedDay : Maybe Date
    , today : Maybe Date
    , dayPickerProps : Maybe (DayPicker.PropsData Msg)
    , dayPickerInputProps : Maybe (DayPickerInput.Props Msg)
    , example : Example
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        ( model, cmd ) =
            setRoute location
                { selectedDay = Nothing
                , today = Nothing
                , dayPickerProps = Nothing
                , dayPickerInputProps = Nothing
                , example = Empty
                }
    in
        ( model, Cmd.batch [ Task.perform Today Date.now, cmd ] )


handlePendingRoute : Route.Route -> Model -> Date -> ( Model, Cmd Msg )
handlePendingRoute route model today =
    case route of
        Route.SimpleDayPicker ->
            let
                ( subModel, subCmd ) =
                    ExSimpleDayPicker.init today
            in
                ( { model | example = SimpleDayPicker subModel }
                , Cmd.map SimpleDayPickerMsg subCmd
                )

        Route.SimpleDayPickerInput ->
            let
                ( subModel, subCmd ) =
                    ExSimpleDayPickerInput.init today
            in
                ( { model | example = SimpleDayPickerInput subModel }
                , Cmd.map SimpleDayPickerInputMsg subCmd
                )

        _ ->
            ( model, Cmd.none )


setRoute : Navigation.Location -> Model -> ( Model, Cmd Msg )
setRoute location model =
    case Route.fromLocation location of
        Just Route.Home ->
            ( { model | example = Empty }, Cmd.none )

        Just Route.SimpleDayPicker ->
            let
                ( example, cmd ) =
                    case model.today of
                        Nothing ->
                            ( Pending Route.SimpleDayPicker, Cmd.none )

                        Just today ->
                            Tuple.mapFirst SimpleDayPicker <|
                                ExSimpleDayPicker.init today
            in
                ( { model | example = example }, Cmd.none )

        Just Route.SimpleDayPickerInput ->
            let
                ( example, cmd ) =
                    case model.today of
                        Nothing ->
                            ( Pending Route.SimpleDayPickerInput, Cmd.none )

                        Just today ->
                            Tuple.mapFirst SimpleDayPickerInput <|
                                ExSimpleDayPickerInput.init today
            in
                ( { model | example = example }, Cmd.none )

        Nothing ->
            ( { model | example = Empty }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateExample exModel exMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
                ( { model | example = exModel newModel }
                , Cmd.map exMsg newCmd
                )
    in
        case ( msg, model.example ) of
            ( UrlChange location, _ ) ->
                setRoute location model

            ( DayChange props, _ ) ->
                ( { model | dayPickerProps = Just props }, Cmd.none )

            ( Today today, _ ) ->
                let
                    newModel =
                        { model | today = Just today }
                in
                    case model.example of
                        Pending route ->
                            handlePendingRoute route newModel today

                        _ ->
                            ( newModel, Cmd.none )

            ( SimpleDayPickerMsg subMsg, SimpleDayPicker subModel ) ->
                updateExample SimpleDayPicker
                    SimpleDayPickerMsg
                    ExSimpleDayPicker.update
                    subMsg
                    subModel

            ( SimpleDayPickerMsg _, _ ) ->
                ( model, Cmd.none )

            ( SimpleDayPickerInputMsg subMsg, SimpleDayPickerInput subModel ) ->
                updateExample SimpleDayPickerInput
                    SimpleDayPickerInputMsg
                    ExSimpleDayPickerInput.update
                    subMsg
                    subModel

            ( SimpleDayPickerInputMsg _, _ ) ->
                ( model, Cmd.none )


viewSidebar : Html Msg
viewSidebar =
    div []
        [ ul []
            [ li []
                [ a [ href "#simple-day-picker" ]
                    [ text "simple DayPicker" ]
                ]
            , li []
                [ a [ href "#simple-day-picker-input" ]
                    [ text "simple DayPickerInput" ]
                ]
            ]
        ]


viewMain : Model -> Html Msg
viewMain model =
    case model.example of
        Empty ->
            text "hello examples"

        Pending _ ->
            text "loading"

        SimpleDayPicker submodel ->
            Html.map SimpleDayPickerMsg <| ExSimpleDayPicker.view submodel

        SimpleDayPickerInput submodel ->
            Html.map SimpleDayPickerInputMsg <| ExSimpleDayPickerInput.view submodel


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "sidebar" ] [ viewSidebar ]
        , div [ class "main" ] [ viewMain model ]
        ]


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
