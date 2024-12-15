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


constants =
    { badGuyMoveTime = 750
    , tackleCheckTime = 200
    , gridSize = 48
    , startingYard = 20
    , temporaryRandomSeed = Random.initialSeed 30
    }


type Model
    = Playing PlayingModel
    | BetweenDowns BetweenDownsModel
    | Ready ReadyModel


type alias PlayingModel =
    { badGuys : BadGuys
    , protagonist : Coord
    , tackled : Maybe { coord : Coord, previousCoord : Coord }
    , footballDown : FootballDown
    , startingYard : Int
    , setStartingYard : Int
    , nextSeed : Random.Seed
    , sinceLastBadGuyMove : Float
    , sinceLastTackleCheck : Float
    , tickValue : Float
    , timeRemaining : Float
    , touchdowns : Int
    }


type alias BetweenDownsModel =
    { badGuys : BadGuys
    , tackled : Maybe { coord : Coord, previousCoord : Coord }
    , footballDown : FootballDown
    , startingYard : Int
    , setStartingYard : Int
    , nextSeed : Random.Seed
    , protagonist : Coord
    , timeRemaining : Float
    , tickValue : Float
    , touchdowns : Int
    }


type alias ReadyModel =
    { nextSeed : Random.Seed
    }


type Msg
    = NoOp
    | HandleKeyboardEvent KeyboardEvent
    | StartDown
    | Tick Float


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

                Key.Spacebar ->
                    handleStartDown model

                _ ->
                    ( model, Cmd.none )

        StartDown ->
            handleStartDown model

        Tick delta ->
            handleTick delta model

        NoOp ->
            ( model, Cmd.none )


handleTick : Float -> Model -> ( Model, Cmd Msg )
handleTick delta model =
    model
        |> progressTime delta
        |> moveBadGuysIfItsTime
        |> maybeTackleProtagonist
        |> (\m -> ( m, Cmd.none ))


maybeTackleProtagonist : Model -> Model
maybeTackleProtagonist model =
    case model of
        Playing playingModel ->
            let
                notTackled =
                    Maybe.isNothing playingModel.tackled

                shouldPerformTackleCheck =
                    playingModel.sinceLastTackleCheck > constants.tackleCheckTime

                ( probability, newNextSeed ) =
                    Random.step (Random.float 0 1) playingModel.nextSeed

                maybeTackler =
                    [ ( Coord.moveUp, 0.5 ), ( Coord.moveLeft, 0.4 ), ( Coord.moveRight, 0.4 ), ( Coord.moveDown, 0.2 ) ]
                        |> List.map (Tuple.mapFirst (\fn -> fn playingModel.protagonist))
                        |> List.filter (\( coord, tackleChance ) -> BadGuys.member coord playingModel.badGuys && (tackleChance > probability))
                        |> List.head

                gotFirstDown =
                    playingModel.protagonist.y >= playingModel.setStartingYard + 10
            in
            if notTackled && shouldPerformTackleCheck then
                case maybeTackler of
                    Just tackler ->
                        if gotFirstDown then
                            BetweenDowns
                                { badGuys = playingModel.badGuys
                                , tackled = Just { coord = playingModel.protagonist, previousCoord = Tuple.first tackler }
                                , footballDown = FootballDown.first
                                , startingYard = playingModel.startingYard
                                , setStartingYard = playingModel.protagonist.y
                                , nextSeed = newNextSeed
                                , protagonist = playingModel.protagonist
                                , timeRemaining = playingModel.timeRemaining
                                , touchdowns = playingModel.touchdowns
                                , tickValue = 0
                                }

                        else
                            case FootballDown.next playingModel.footballDown of
                                Just nextDown ->
                                    BetweenDowns
                                        { badGuys = playingModel.badGuys
                                        , tackled = Just { coord = playingModel.protagonist, previousCoord = Tuple.first tackler }
                                        , footballDown = nextDown
                                        , startingYard = playingModel.startingYard
                                        , setStartingYard = playingModel.setStartingYard
                                        , nextSeed = newNextSeed
                                        , protagonist = playingModel.protagonist
                                        , timeRemaining = playingModel.timeRemaining
                                        , touchdowns = playingModel.touchdowns
                                        , tickValue = 0
                                        }

                                Nothing ->
                                    Ready { nextSeed = newNextSeed }

                    Nothing ->
                        Playing
                            { playingModel
                                | sinceLastTackleCheck = 0
                                , nextSeed = newNextSeed
                            }

            else
                model

        _ ->
            model


moveBadGuysIfItsTime : Model -> Model
moveBadGuysIfItsTime model =
    case model of
        Playing playingModel ->
            let
                notTackled =
                    Maybe.isNothing playingModel.tackled

                shouldHandleBadGuyMove =
                    playingModel.sinceLastBadGuyMove > constants.badGuyMoveTime
            in
            if notTackled && shouldHandleBadGuyMove then
                let
                    ( newBadGuyPositions, nextSeed ) =
                        playingModel.badGuys
                            |> BadGuys.step playingModel.nextSeed bounds playingModel.protagonist
                in
                Playing
                    { playingModel
                        | badGuys = newBadGuyPositions
                        , sinceLastBadGuyMove = 0
                        , nextSeed = nextSeed
                    }

            else
                model

        _ ->
            model


progressTime : Float -> Model -> Model
progressTime delta model =
    case model of
        Playing playingModel ->
            Playing
                { playingModel
                    | sinceLastBadGuyMove = playingModel.sinceLastBadGuyMove + delta
                    , sinceLastTackleCheck = playingModel.sinceLastTackleCheck + delta
                    , tickValue = playingModel.tickValue + delta
                    , timeRemaining = playingModel.timeRemaining - delta
                }

        _ ->
            model


handleStartDown : Model -> ( Model, Cmd Msg )
handleStartDown model =
    case model of
        Ready readyModel ->
            let
                protagonist =
                    { x = 3, y = constants.startingYard }

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
                , startingYard = constants.startingYard
                , setStartingYard = constants.startingYard
                , nextSeed = nextSeed
                , sinceLastBadGuyMove = 0
                , sinceLastTackleCheck = 0
                , tickValue = 0
                , timeRemaining = 120000
                , touchdowns = 0
                }
            , Cmd.none
            )

        BetweenDowns { protagonist, nextSeed, footballDown, setStartingYard, timeRemaining, touchdowns } ->
            let
                startingYard =
                    protagonist.y

                newProtagonist =
                    { x = 3, y = startingYard }

                ( badGuys, newNextSeed ) =
                    BadGuys.generate
                        newProtagonist
                        bounds
                        nextSeed
            in
            ( Playing
                { badGuys = badGuys
                , protagonist = protagonist
                , tackled = Nothing
                , footballDown = footballDown
                , startingYard = startingYard
                , setStartingYard = setStartingYard
                , nextSeed = newNextSeed
                , sinceLastBadGuyMove = 0
                , sinceLastTackleCheck = 0
                , tickValue = 0
                , timeRemaining = timeRemaining
                , touchdowns = touchdowns
                }
            , Cmd.none
            )

        Playing playingModel ->
            ( Playing playingModel, Cmd.none )


handleProtagonistMove : (Coord -> Coord) -> Model -> ( Model, Cmd Msg )
handleProtagonistMove moveFn model =
    case model of
        Playing playingModel ->
            case playingModel.tackled of
                Nothing ->
                    -- Try to move the protagonist
                    -- but he can't move out of bounds
                    -- OR into a bad guy
                    let
                        spotToMoveTo =
                            playingModel.protagonist
                                |> moveFn
                                |> Coord.constrain bounds

                        currentlyInSpot =
                            BadGuys.currentBadGuy spotToMoveTo playingModel.badGuys

                        scoredTouchdown =
                            spotToMoveTo.y
                                == 100
                    in
                    case currentlyInSpot of
                        Just _ ->
                            ( Playing playingModel
                            , Cmd.none
                            )

                        Nothing ->
                            if scoredTouchdown then
                                ( Ready { nextSeed = playingModel.nextSeed }
                                , Cmd.none
                                )

                            else
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
        -- TODO get seed from javascript
        { nextSeed = constants.temporaryRandomSeed
        }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    (case model of
        Playing playingModel ->
            [ viewPlayingModel playingModel ]

        BetweenDowns betweenDownsModel ->
            [ viewBetweenDowns betweenDownsModel ]

        Ready readyModel ->
            [ viewReadyModel readyModel ]
    )
        |> Html.layout []


grid : ( List Int, List Int )
grid =
    ( List.range 0 6, List.range 0 100 |> List.reverse )


bounds : Bounds
bounds =
    { xMin = 0
    , xMax = 6
    , yMin = 0
    , yMax = 100
    }


viewReadyModel : ReadyModel -> Html Msg
viewReadyModel readyModel =
    Html.flexRow
        [ css [ justifyContent center ] ]
        [ Html.flexColumn []
            [ Html.h2 [] [ Html.text "Rabbit vs Duck Goons MMXXIV" ]
            , Html.p [] [ Html.text "This is an unfinished homage to The Big Game: Bugs vs Daffy" ]
            , Html.p [] [ Html.text "The goal is to score 3 touchdowns in 2 minutes" ]
            , Html.p [] [ Html.text "Space Bar to start" ]
            , Html.p [] [ Html.text "Arrow Keys to run" ]
            , Html.button [ Events.onClick StartDown ] [ Html.text "hut hike" ]
            ]
        ]


viewBetweenDowns : BetweenDownsModel -> Html Msg
viewBetweenDowns ({ tackled, footballDown, protagonist, startingYard } as betweenDownsModel) =
    Html.flexColumn [ css [ Html.gap 24, alignItems center ] ]
        [ viewField betweenDownsModel
        , viewScoreboard betweenDownsModel
        , case tackled of
            Just _ ->
                Html.flexColumn []
                    [ Html.div [] [ Html.text <| "You gained " ++ String.fromInt (protagonist.y - startingYard) ++ " yards." ]
                    , Html.div [] [ Html.text <| "It's now " ++ FootballDown.toString footballDown ]
                    , Html.button [ Events.onClick StartDown ] [ Html.text "hut hike" ]
                    ]

            Nothing ->
                Html.text ""
        ]


viewPlayingModel : PlayingModel -> Html Msg
viewPlayingModel playingModel =
    Html.flexColumn [ css [ Html.gap 24, alignItems center ] ]
        [ viewField playingModel
        , viewScoreboard playingModel
        ]


viewScoreboard : { a | timeRemaining : Float, touchdowns : Int, footballDown : FootballDown } -> Html msg
viewScoreboard { timeRemaining, touchdowns, footballDown } =
    Html.flexColumn []
        [ Html.div [] [ Html.text <| formatTime timeRemaining ]
        , Html.div [] [ Html.text <| FootballDown.toString footballDown ]
        , Html.div [] [ Html.text <| "Score: " ++ String.fromInt touchdowns ]
        ]


formatTime : Float -> String
formatTime timeRemaining =
    let
        minutesLeft =
            floor (timeRemaining / 60000)

        secondsRemaining =
            modBy 60000 (floor timeRemaining)
    in
    String.concat
        [ "0"
        , minutesLeft |> String.fromInt
        , ":"
        , secondsRemaining |> String.fromInt |> String.slice 0 2
        ]


viewField :
    { a
        | badGuys : BadGuys
        , protagonist : Coord
        , tackled : Maybe { coord : Coord, previousCoord : Coord }
        , setStartingYard : Int
        , tickValue : Float
    }
    -> Html msg
viewField { badGuys, protagonist, tackled, setStartingYard, tickValue } =
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
                                , height (px constants.gridSize)
                                , width (px constants.gridSize)
                                , fontSize (px constants.gridSize)
                                , backgroundColor (rgb 80 210 90)
                                , color (rgb 255 255 255)
                                , borderLeft3 (px 8) solid (rgb 255 255 255)
                                ]
                            ]
                            [ if modBy 5 y == 0 then
                                Html.span
                                    [ css [ transform (translateY (px (constants.gridSize / 2))) ]
                                    ]
                                    [ Html.text (String.fromInt y) ]

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
                                Html.div []
                                    [ Html.img
                                        [ Attrs.height (floor <| constants.gridSize * 1.2)
                                        , Attrs.width (floor <| constants.gridSize * 1.2)
                                        , if modBy 2 (floor (tickValue / 200)) == 0 then
                                            Attrs.src "src/assets/rabbit-1.svg"

                                          else
                                            Attrs.src "src/assets/rabbit-2.svg"
                                        ]
                                        []
                                    ]

                            else if isBadGuy then
                                Html.div []
                                    [ Html.img
                                        [ Attrs.height (floor <| constants.gridSize * 1.3)
                                        , Attrs.width (floor <| constants.gridSize * 1.3)
                                        , if modBy 2 (floor (tickValue / 200)) == 0 then
                                            Attrs.src "src/assets/bad-guy-1.svg"

                                          else
                                            Attrs.src "src/assets/bad-guy-2.svg"
                                        ]
                                        []
                                    ]

                            else
                                Html.text ""

                        Just tackler ->
                            if tackler.previousCoord == coord then
                                Html.text ""

                            else if tackler.coord == coord then
                                let
                                    rotation =
                                        if tackler.previousCoord.x - tackler.coord.x == 1 then
                                            Attrs.style "transform" "rotate(90deg)"

                                        else if tackler.previousCoord.x - tackler.coord.x == -1 then
                                            Attrs.style "transform" "rotate(-90deg)"

                                        else if tackler.previousCoord.y - tackler.coord.y == -1 then
                                            Attrs.style "transform" "rotate(180deg)"

                                        else
                                            Attrs.style "transform" "rotate(0)"
                                in
                                Html.div []
                                    [ Html.img
                                        [ Attrs.height (floor <| constants.gridSize * 1.3)
                                        , Attrs.width (floor <| constants.gridSize * 1.3)
                                        , Attrs.src "src/assets/tackled.svg"
                                        , rotation
                                        ]
                                        []
                                    ]

                            else if isBadGuy then
                                Html.div []
                                    [ Html.img
                                        [ Attrs.height (floor <| constants.gridSize * 1.3)
                                        , Attrs.width (floor <| constants.gridSize * 1.3)
                                        , if modBy 2 (floor (tickValue / 200)) == 0 then
                                            Attrs.src "src/assets/bad-guy-1.svg"

                                          else
                                            Attrs.src "src/assets/bad-guy-2.svg"
                                        ]
                                        []
                                    ]

                            else
                                Html.text ""
            in
            Html.div
                [ css
                    [ displayFlex
                    , alignItems center
                    , justifyContent center
                    , width (px constants.gridSize)
                    , height (px constants.gridSize)
                    , backgroundColor (rgb 100 220 100)
                    , border3 (px 1) solid (rgba 100 100 100 0.2)
                    , if modBy 5 coord.y == 0 then
                        borderBottom3 (px 10) solid (rgb 255 255 255)

                      else
                        batch []
                    , if coord.y == setStartingYard + 10 then
                        borderBottom3 (px 10) solid (rgb 220 220 100)

                      else
                        batch []
                    ]
                ]
                [ art ]
    in
    Html.flexRow [ css [ justifyContent center, alignItems center ] ]
        [ Html.flexColumn
            []
            (List.map viewRow rowsToShow)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        keyHandlerSub =
            Browser.Events.onKeyDown <| Decode.map HandleKeyboardEvent decodeKeyboardEvent

        animationSub =
            case model of
                Playing _ ->
                    Browser.Events.onAnimationFrameDelta Tick

                _ ->
                    Sub.none
    in
    Sub.batch
        [ keyHandlerSub
        , animationSub
        ]
