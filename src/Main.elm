module Main exposing (..)

import BadGuys exposing (BadGuys)
import Bounds exposing (Bounds)
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
import List.Extra as List
import Maybe.Extra as Maybe
import Random
import Time


type Model
    = Playing PlayingModel
    | Ready ReadyModel


type alias PlayingModel =
    { badGuys : BadGuys
    , protagonist : Coord
    , tackled : Maybe { coord : Coord, previousCoord : Coord }
    , footballDown : FootballDown
    , startingYard : Int
    , setStartingYard : Int
    , nextSeed : Random.Seed
    , lastBadGuyMove : Maybe Time.Posix
    }


type alias ReadyModel =
    { nextSeed : Random.Seed
    }


type Msg
    = NoOp
    | HandleKeyboardEvent KeyboardEvent
    | StartDown
    | Tick Time.Posix


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
            handleStartDown model

        Tick posix ->
            handleTick posix model

        NoOp ->
            ( model, Cmd.none )


handleTick : Time.Posix -> Model -> ( Model, Cmd Msg )
handleTick time model =
    case model of
        Playing playingModel ->
            let
                notTackled =
                    Maybe.isNothing playingModel.tackled

                isTime =
                    playingModel.lastBadGuyMove
                        |> Maybe.map (\lastMove -> Time.posixToMillis time - Time.posixToMillis lastMove > 200)
                        |> Maybe.withDefault False
            in
            case ( playingModel.tackled, playingModel.lastBadGuyMove ) of
                ( Just _, _ ) ->
                    -- If tackled, do nothing
                    ( model, Cmd.none )

                ( Nothing, Nothing ) ->
                    -- If time not set, it's the first tick so set it
                    ( Playing { playingModel | lastBadGuyMove = Just time }, Cmd.none )

                ( Nothing, Just lastMove ) ->
                    -- If time not set, it's the first tick so set it
                    if
                        (Time.posixToMillis time - Time.posixToMillis lastMove)
                            > 500
                    then
                        let
                            ( newBadGuyPositions, nextSeed ) =
                                playingModel.badGuys
                                    |> BadGuys.step playingModel.nextSeed bounds

                            currentlyInSpot =
                                BadGuys.currentBadGuy playingModel.protagonist newBadGuyPositions
                        in
                        case currentlyInSpot of
                            Just badGuy ->
                                ( Playing
                                    { playingModel
                                        | tackled = Just badGuy
                                        , badGuys = newBadGuyPositions
                                        , nextSeed = nextSeed
                                    }
                                , Cmd.none
                                )

                            Nothing ->
                                ( Playing
                                    { playingModel
                                        | badGuys = newBadGuyPositions
                                        , lastBadGuyMove = Just time
                                        , nextSeed = nextSeed
                                    }
                                , Cmd.none
                                )

                    else
                        ( model, Cmd.none )

        -- if isTime && notTackled then
        -- else if notTackled then
        --     ( model, Cmd.none )
        _ ->
            ( model, Cmd.none )


handleStartDown : Model -> ( Model, Cmd Msg )
handleStartDown model =
    case model of
        Ready readyModel ->
            let
                startingYard =
                    20

                protagonist =
                    { x = 3, y = startingYard }

                ( badGuys, nextSeed ) =
                    BadGuys.generate
                        protagonist
                        bounds
                        readyModel.nextSeed
            in
            ( Playing
                { badGuys = badGuys
                , protagonist = protagonist
                , tackled = Nothing
                , footballDown = FootballDown.first
                , startingYard = startingYard
                , setStartingYard = startingYard
                , nextSeed = nextSeed
                , lastBadGuyMove = Nothing
                }
            , Cmd.none
            )

        Playing playingModel ->
            case FootballDown.next playingModel.footballDown of
                Just nextDown ->
                    let
                        startingYard =
                            playingModel.protagonist.y

                        protagonist =
                            { x = 3, y = startingYard }

                        ( badGuys, nextSeed ) =
                            BadGuys.generate
                                protagonist
                                bounds
                                playingModel.nextSeed

                        gotFirstDown =
                            startingYard > playingModel.setStartingYard + 10
                    in
                    ( Playing
                        { badGuys = badGuys
                        , protagonist = protagonist
                        , tackled = Nothing
                        , footballDown =
                            if gotFirstDown then
                                FootballDown.first

                            else
                                nextDown
                        , startingYard = startingYard
                        , setStartingYard =
                            if gotFirstDown then
                                startingYard

                            else
                                playingModel.setStartingYard
                        , nextSeed = nextSeed
                        , lastBadGuyMove = Nothing
                        }
                    , Cmd.none
                    )

                Nothing ->
                    -- Shouldn't happen
                    ( model, Cmd.none )


handleProtagonistMove : (Coord -> Coord) -> Model -> ( Model, Cmd Msg )
handleProtagonistMove moveFn model =
    case model of
        Playing playingModel ->
            case playingModel.tackled of
                Nothing ->
                    let
                        spotToMoveTo =
                            playingModel.protagonist
                                |> moveFn
                                |> Coord.constrain bounds

                        currentlyInSpot =
                            BadGuys.currentBadGuy playingModel.protagonist playingModel.badGuys
                    in
                    case currentlyInSpot of
                        Just badGuy ->
                            ( Playing { playingModel | tackled = Just badGuy }
                            , Cmd.none
                            )

                        Nothing ->
                            ( Playing { playingModel | protagonist = spotToMoveTo }
                            , Cmd.none
                            )

                Just _ ->
                    -- Can't move if you're tackled!!
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( Ready
        { nextSeed = Random.initialSeed 0
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


bounds : Bounds
bounds =
    { xMin = 0
    , xMax = 6
    , yMin = 0
    , yMax = 99
    }


viewReadyModel : ReadyModel -> Html Msg
viewReadyModel readyModel =
    Html.flexRow
        [ css [ justifyContent center ] ]
        [ Html.flexColumn [ css [ Html.gap 24 ] ]
            [ Html.button [ Events.onClick StartDown ] [ Html.text "hut hike" ]
            ]
        ]


viewPlayingModel : PlayingModel -> Html Msg
viewPlayingModel { protagonist, badGuys, tackled, setStartingYard, startingYard, footballDown } =
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
                            [ if modBy 5 y == 0 then
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
                    BadGuys.member coord badGuys

                art =
                    case tackled of
                        Nothing ->
                            if isProtagonist then
                                "P"

                            else if isBadGuy then
                                "H"

                            else
                                ""

                        Just tackler ->
                            if tackler.previousCoord == coord then
                                "-"

                            else if tackler.coord == coord then
                                "X"

                            else if isBadGuy then
                                "H"

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
                    , if coord.y == setStartingYard + 10 then
                        borderBottom3 (px 2) solid (rgb 220 220 100)

                      else
                        batch []
                    ]
                ]
                [ Html.text art ]
    in
    Html.flexColumn [ css [ Html.gap 24, alignItems center ] ]
        [ Html.flexRow [ css [ justifyContent center, alignItems center ] ]
            [ Html.flexColumn
                []
                (List.map viewRow rowsToShow)
            ]
        , case tackled of
            Just _ ->
                Html.flexColumn []
                    [ Html.div [] [ Html.text <| "You gained " ++ String.fromInt (protagonist.y - startingYard) ++ " yards." ]
                    , case FootballDown.next footballDown of
                        -- TODO need to handle first down better
                        Just next ->
                            Html.div [] [ Html.text <| "It's now " ++ FootballDown.toString next ]

                        Nothing ->
                            Html.text ""
                    , Html.button [ Events.onClick StartDown ] [ Html.text "hut hike" ]
                    ]

            Nothing ->
                Html.text ""
        ]


viewDebug : Model -> Html msg
viewDebug model =
    Html.text "Hey pepo - makin prog"


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Playing _ ->
            Sub.batch
                [ Browser.Events.onKeyDown <|
                    Decode.map HandleKeyboardEvent decodeKeyboardEvent
                , Browser.Events.onAnimationFrame Tick
                ]

        _ ->
            Sub.none
