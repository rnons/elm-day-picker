module DayPicker exposing (..)

import Date exposing (Date, Month, Day(..))
import Set exposing (Set)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Date.Extra.Core as DateExtra
import Date.Extra.Create exposing (dateFromFields)
import Date.Extra.Config.Config_ja_jp exposing (config)
import List.Extra exposing (elemIndex)


type alias OnChange msg =
    PropsData msg -> Maybe Date -> msg


type alias Props msg =
    { today : Date
    , selectedDays : Set Date
    , monthCount : Int
    , firstDayOfMonth : Date
    , onChange : OnChange msg
    }


type PropsData msg
    = PropsData (Props msg)


applyN : Int -> (a -> a) -> a -> a
applyN n fn val =
    List.foldl (\_ acc -> fn acc) val (List.range 1 n)


weekDays : List Day
weekDays =
    [ Sun, Mon, Tue, Wed, Thu, Fri, Sat ]


mkDefaultProps : Date -> OnChange msg -> Props msg
mkDefaultProps today onChange =
    { today = today
    , selectedDays = Set.empty
    , monthCount = 1
    , firstDayOfMonth = DateExtra.toFirstOfMonth today
    , onChange = onChange
    }


mkDefaultPropsData : Date -> OnChange msg -> PropsData msg
mkDefaultPropsData today onChange =
    PropsData <| mkDefaultProps today onChange


toPrevMonth : Props msg -> Props msg
toPrevMonth props =
    let
        getter =
            DateExtra.toFirstOfMonth
                << DateExtra.lastOfPrevMonthDate
    in
        { props | firstDayOfMonth = getter props.firstDayOfMonth }


toNextMonth : Props msg -> Props msg
toNextMonth props =
    let
        getter =
            DateExtra.firstOfNextMonthDate
    in
        { props | firstDayOfMonth = getter props.firstDayOfMonth }


selectDay : Props msg -> Date -> Int -> msg
selectDay props firstDayOfMonth dayOfMonth =
    props.onChange (PropsData props) (Just firstDayOfMonth)


viewHeader : Props msg -> Date -> Html msg
viewHeader props firstDay =
    let
        year =
            toString <| Date.year firstDay

        month =
            toString <| DateExtra.monthToInt <| Date.month firstDay
    in
        div [ class "DayPicker-header" ]
            [ button
                [ onClick <|
                    props.onChange
                        (PropsData <| toPrevMonth props)
                        Nothing
                ]
                [ text "<" ]
            , text <| year ++ "年" ++ month ++ "月"
            , button
                [ onClick <|
                    props.onChange
                        (PropsData <| toNextMonth props)
                        Nothing
                ]
                [ text ">" ]
            ]


viewTableHeader : Html msg
viewTableHeader =
    let
        viewDay day =
            td [] [ text <| config.i18n.dayShort day ]
    in
        thead []
            [ tr [] (List.map viewDay weekDays) ]


viewDay : Props msg -> Date -> Maybe Int -> Html msg
viewDay props firstDay mDayOfMonth =
    case mDayOfMonth of
        Just dayOfMonth ->
            td [ onClick <| selectDay props firstDay dayOfMonth ]
                [ text <| toString dayOfMonth ]

        Nothing ->
            td [] [ text "" ]


viewDayRow : Props msg -> Date -> Int -> Int -> Int -> Int -> Html msg
viewDayRow props firstDay firstDayColIndex daysInMonth rowIndex colIndex =
    let
        colIndex =
            if rowIndex == 0 then
                firstDayColIndex
            else
                0

        startDayOfMonth =
            if rowIndex == 0 then
                1
            else
                rowIndex * 7 - firstDayColIndex + 1

        endDayOfMonth =
            if startDayOfMonth + 6 < daysInMonth then
                startDayOfMonth + 6 - colIndex
            else
                daysInMonth

        days =
            List.range startDayOfMonth endDayOfMonth

        dayCount =
            List.length days

        paddingRight =
            if colIndex + dayCount == 7 then
                0
            else
                7 - colIndex - dayCount

        row =
            List.repeat colIndex (viewDay props firstDay Nothing)
                ++ List.map (viewDay props firstDay << Just) days
                ++ List.repeat paddingRight (viewDay props firstDay Nothing)
    in
        tr [] row


viewDayRows : Props msg -> Date -> List (Html msg)
viewDayRows props firstDay =
    let
        year =
            Date.year firstDay

        month =
            Date.month firstDay

        daysInMonth =
            DateExtra.daysInMonth year month

        mWeekDayIndex =
            elemIndex (Date.dayOfWeek firstDay) weekDays

        ( firstDayColIndex, rowCount ) =
            case mWeekDayIndex of
                Nothing ->
                    ( 0, 0 )

                Just index ->
                    ( index, (index + daysInMonth) // 7 )
    in
        List.indexedMap (viewDayRow props firstDay firstDayColIndex daysInMonth)
            (List.range 0 rowCount)


viewTableBody : Props msg -> Date -> Html msg
viewTableBody props firstDay =
    tbody [] <| viewDayRows props firstDay


viewMonth : Props msg -> Int -> Html msg
viewMonth props index =
    let
        firstDay =
            applyN (index - 1)
                DateExtra.firstOfNextMonthDate
                props.firstDayOfMonth
    in
        div [ class "DayPicker-month" ]
            [ viewHeader props firstDay
            , table [ class "DayPicker-table" ]
                [ viewTableHeader
                , viewTableBody props firstDay
                ]
            ]


dayPicker : PropsData msg -> Html msg
dayPicker (PropsData props) =
    List.range
        1
        props.monthCount
        |> List.map (viewMonth props)
        |> div [ class "DayPicker" ]
