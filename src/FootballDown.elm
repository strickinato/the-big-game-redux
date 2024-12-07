module FootballDown exposing (..)


type FootballDown
    = FirstDown
    | SecondDown
    | ThirdDown
    | FourthDown


first : FootballDown
first =
    FirstDown


next : FootballDown -> Maybe FootballDown
next footballDown =
    case footballDown of
        FirstDown ->
            Just SecondDown

        SecondDown ->
            Just ThirdDown

        ThirdDown ->
            Just FourthDown

        FourthDown ->
            Nothing


toString : FootballDown -> String
toString footballDown =
    case footballDown of
        FirstDown ->
            "First Down"

        SecondDown ->
            "Second Down"

        ThirdDown ->
            "Third Down"

        FourthDown ->
            "Fourth Down"
