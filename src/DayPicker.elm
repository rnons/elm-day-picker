module DayPicker exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Date exposing (Date, Month, Day(..))
import Date.Extra.Core as DateExtra
import Date.Extra.Create exposing (dateFromFields)
import Date.Extra.Config.Config_ja_jp exposing (config)
import List.Extra exposing (elemIndex)


type alias OnChange msg =
    Props msg -> msg


type Props msg
    = Props
        { today : Date
        , selectedDays : List Date
        , monthCount : Int
        , firstDayOfMonth : Date
        , onChange : OnChange msg
        }


applyN : Int -> (a -> a) -> a -> a
applyN n fn val =
    List.foldl (\_ acc -> fn acc) val (List.range 1 n)


weekDays : List Day
weekDays =
    [ Sun, Mon, Tue, Wed, Thu, Fri, Sat ]


mkDefaultProps : Date -> OnChange msg -> Props msg
mkDefaultProps today onChange =
    Props
        { today = today
        , selectedDays = []
        , monthCount = 1
        , firstDayOfMonth = DateExtra.toFirstOfMonth today
        , onChange = onChange
        }


toPrevMonth : Props msg -> Props msg
toPrevMonth (Props props) =
    let
        getter =
            DateExtra.toFirstOfMonth
                << DateExtra.lastOfPrevMonthDate
    in
        Props { props | firstDayOfMonth = getter props.firstDayOfMonth }


toNextMonth : Props msg -> Props msg
toNextMonth (Props props) =
    let
        getter =
            DateExtra.firstOfNextMonthDate
    in
        Props { props | firstDayOfMonth = getter props.firstDayOfMonth }


viewHeader : Props msg -> Date -> Html msg
viewHeader ((Props props) as propsData) firstDay =
    let
        year =
            toString <| Date.year firstDay

        month =
            toString <| DateExtra.monthToInt <| Date.month firstDay
    in
        div [ class "DayPicker-header" ]
            [ button [ onClick <| props.onChange <| toPrevMonth propsData ] [ text "<" ]
            , text <| year ++ "年" ++ month ++ "月"
            , button [ onClick <| props.onChange <| toNextMonth propsData ] [ text ">" ]
            ]


viewTableHeader : Html msg
viewTableHeader =
    let
        viewDay day =
            td [] [ text <| config.i18n.dayShort day ]
    in
        thead []
            [ tr [] (List.map viewDay weekDays) ]


viewDay : Maybe Int -> Html msg
viewDay mDayOfMonth =
    let
        content =
            case mDayOfMonth of
                Just mDayOfMonth ->
                    text <| toString mDayOfMonth

                Nothing ->
                    text ""
    in
        td [] [ content ]


viewDayRow : Int -> Int -> Int -> Int -> Html msg
viewDayRow firstDayColIndex daysInMonth rowIndex colIndex =
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
            List.repeat colIndex (viewDay Nothing)
                ++ List.map (viewDay << Just) days
                ++ List.repeat paddingRight (viewDay Nothing)
    in
        tr [] row


viewDayRows : Props msg -> Date -> List (Html msg)
viewDayRows (Props props) firstDay =
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
        List.indexedMap (viewDayRow firstDayColIndex daysInMonth)
            (List.range 0 rowCount)


viewTableBody : Props msg -> Date -> Html msg
viewTableBody props firstDay =
    tbody [] <| viewDayRows props firstDay


viewMonth : Props msg -> Int -> Html msg
viewMonth ((Props props) as propsData) index =
    let
        firstDay =
            applyN (index - 1)
                DateExtra.firstOfNextMonthDate
                props.firstDayOfMonth
    in
        div [ class "DayPicker-month" ]
            [ viewHeader propsData firstDay
            , table [ class "DayPicker-table" ]
                [ viewTableHeader
                , viewTableBody propsData firstDay
                ]
            ]


dayPicker : Props msg -> Html msg
dayPicker ((Props props) as propsData) =
    List.range
        1
        props.monthCount
        |> List.map (viewMonth propsData)
        |> div [ class "DayPicker" ]
