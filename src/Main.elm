module Main exposing (..)

import Browser
import Browser.Events
import Coord exposing (Coord)
import Css exposing (..)
import FootballDown exposing (FootballDown)
import Html as PlainHtml
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs exposing (css)
import Html.Styled.Events as Events
import HtmlHelpers as Html
import Json.Decode as Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Key
import Random


type Model
    = Playing PlayingModel
    | Ready ReadyModel


type alias PlayingModel =
    { badGuys : List Coord
    , protagonist : Coord
    , footballDown : FootballDown
    , startingYard : Int
    , setStartingYard : Int
    }


type alias ReadyModel =
    { footballDown : FootballDown
    , currentYard : Int
    , setStartingYard : Int
    }


type Msg
    = NoOp
    | HandleKeyboardEvent KeyboardEvent
    | StartDown


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view >> Html.toUnstyled
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleKeyboardEvent keyboardEvent ->
            case keyboardEvent.keyCode of
                Key.Down ->
                    handleProtagonistMove Coord.moveDown model

                Key.J ->
                    handleProtagonistMove Coord.moveDown model

                Key.Up ->
                    handleProtagonistMove Coord.moveUp model

                Key.K ->
                    handleProtagonistMove Coord.moveUp model

                Key.Left ->
                    handleProtagonistMove Coord.moveLeft model

                Key.H ->
                    handleProtagonistMove Coord.moveLeft model

                Key.Right ->
                    handleProtagonistMove Coord.moveRight model

                Key.L ->
                    handleProtagonistMove Coord.moveRight model

                _ ->
                    ( model, Cmd.none )

        StartDown ->
            startDown model

        NoOp ->
            ( model, Cmd.none )


startDown : Model -> ( Model, Cmd Msg )
startDown model =
    case model of
        Ready readyModel ->
            let
                protagonist =
                    { x = 3, y = readyModel.currentYard }

                ( badGuys, _ ) =
                    Random.step
                        (Random.list 200 (badGuyGenerator protagonist))
                        (Random.initialSeed 0)
            in
            ( Playing
                { badGuys = badGuys
                , protagonist = protagonist
                , footballDown = readyModel.footballDown
                , startingYard = readyModel.currentYard
                , setStartingYard = readyModel.setStartingYard
                }
            , Cmd.none
            )

        Playing _ ->
            ( model, Cmd.none )


handleProtagonistMove : (Coord -> Coord) -> Model -> ( Model, Cmd Msg )
handleProtagonistMove moveFn model =
    let
        constraint =
            { xMin = 0
            , xMax = 6
            , yMin = 0
            , yMax = 99
            }
    in
    case model of
        Playing playingModel ->
            ( Playing
                { playingModel
                    | protagonist =
                        playingModel.protagonist
                            |> moveFn
                            |> Coord.constrain constraint
                }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


badGuyGenerator : Coord -> Random.Generator Coord
badGuyGenerator protagonistYardLine =
    -- TODO goal line stands
    Random.map2 Coord.make (Random.int 0 6) (Random.int (protagonistYardLine.y + 2) 99)


init : ( Model, Cmd Msg )
init =
    ( Ready
        { footballDown = FootballDown.first
        , currentYard = 20
        , setStartingYard = 20
        }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    (case model of
        Playing playingModel ->
            [ viewPlayingModel playingModel ]

        Ready readyModel ->
            [ viewReadyModel readyModel ]
    )
        |> (\v -> viewDebug model :: v)
        |> Html.layout []


grid : ( List Int, List Int )
grid =
    ( List.range 0 6, List.range 0 100 |> List.reverse )


viewReadyModel : ReadyModel -> Html Msg
viewReadyModel readyModel =
    Html.flexRow
        [ css [ justifyContent center ] ]
        [ Html.flexColumn [ css [ Html.gap 24 ] ]
            [ Html.text <| FootballDown.toString readyModel.footballDown
            , Html.button [ Events.onClick StartDown ] [ Html.text "hut hike" ]
            ]
        ]


viewPlayingModel : PlayingModel -> Html Msg
viewPlayingModel { protagonist, badGuys } =
    let
        rowsToShow =
            List.range
                (protagonist.y - 1)
                (protagonist.y + 4)
                |> List.reverse

        viewRow y =
            Html.flexRow []
                ((List.map (\x -> viewCell (Coord.make x y)) <| Tuple.first grid)
                    ++ [ Html.div
                            [ css
                                [ displayFlex
                                , alignItems center
                                , transform (translateY (px 12))
                                , height (px 24)
                                , width (px 24)
                                ]
                            ]
                            [ if modBy 10 y == 0 then
                                Html.text (String.fromInt y)

                              else
                                Html.text ""
                            ]
                       ]
                )

        viewCell coord =
            let
                isProtagonist =
                    coord == protagonist

                isBadGuy =
                    List.member coord badGuys

                art =
                    if isProtagonist then
                        "P"

                    else if isBadGuy then
                        "X"

                    else
                        ""
            in
            Html.div
                [ css
                    [ displayFlex
                    , alignItems center
                    , justifyContent center
                    , width (px 24)
                    , height (px 24)
                    , backgroundColor (rgb 100 220 100)
                    , border3 (px 1) solid (rgba 100 100 100 0.2)
                    , if modBy 10 coord.y == 0 then
                        borderBottom3 (px 2) solid (rgb 255 255 255)

                      else
                        batch []
                    ]
                ]
                [ Html.text art ]
    in
    Html.flexRow [ css [ justifyContent center, alignItems center ] ]
        [ Html.flexColumn
            []
            (List.map viewRow rowsToShow)
        ]


viewDebug : Model -> Html msg
viewDebug model =
    Html.text "Hey pepo - makin prog"


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown <|
        Decode.map HandleKeyboardEvent decodeKeyboardEvent
