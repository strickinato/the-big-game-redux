module HtmlHelpers exposing (..)

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs exposing (css)


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


modal : { height : Int, width : Int, transparent : Bool } -> List (Html msg) -> Html msg
modal opts children =
    flexRow
        [ css
            [ justifyContent center
            , height (px <| toFloat opts.height)
            , width (px <| toFloat opts.width)
            ]
        ]
        [ flexColumn
            [ css
                [ zIndex (int 1)
                , padding2 zero (px 32)
                , justifyContent center
                , property "gap" "24px"
                ]
            ]
            children
        , Html.node "rough-modal"
            [ Attrs.attribute "height" (String.fromInt opts.height)
            , Attrs.attribute "width" (String.fromInt opts.width)
            , Attrs.attribute "transparent"
                (if opts.transparent then
                    "true"

                 else
                    "false"
                )
            , css [ position absolute ]
            ]
            []
        ]


layout : List (Html.Attribute msg) -> List (Html msg) -> Html msg
layout attrs children =
    Html.div
        (css
            [ maxWidth (px 960)
            , margin2 zero auto
            , paddingTop (px 120)
            , displayFlex
            , justifyContent center
            ]
            :: attrs
        )
        children
