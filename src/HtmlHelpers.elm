module HtmlHelpers exposing (..)

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)


flexRow : List (Html.Attribute msg) -> List (Html msg) -> Html msg
flexRow attrs children =
    Html.div
        (css [ displayFlex ] :: attrs)
        children


flexColumn : List (Html.Attribute msg) -> List (Html msg) -> Html msg
flexColumn attrs children =
    Html.div
        (css [ displayFlex, flexDirection column ] :: attrs)
        children


gap : Int -> Css.Style
gap pixels =
    property "gap" (String.fromInt pixels ++ "px")


layout : List (Html.Attribute msg) -> List (Html msg) -> Html msg
layout attrs children =
    Html.div
        (css
            [ maxWidth (px 960)
            , margin2 zero auto
            ]
            :: attrs
        )
        children
