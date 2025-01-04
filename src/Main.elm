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
import String.Extra as String


constants =
    { badGuyMoveTime = 750
    , gridSize = 72
    , startingYard = 30
    , temporaryRandomSeed = Random.initialSeed 30
    , throwLengthConstant = 1000
    }


difficultyMap numberOfTouchdowns =
    case numberOfTouchdowns of
        0 ->
            { tackleCheckFrequency = 250
            , tackleChances = [ ( Coord.moveUp, 0.4 ), ( Coord.moveLeft, 0.3 ), ( Coord.moveRight, 0.3 ), ( Coord.moveDown, 0.1 ) ]
            , numBadGuys = 120
            }

        1 ->
            { tackleCheckFrequency = 200
            , tackleChances = [ ( Coord.moveUp, 0.5 ), ( Coord.moveLeft, 0.4 ), ( Coord.moveRight, 0.4 ), ( Coord.moveDown, 0.3 ) ]
            , numBadGuys = 140
            }

        _ ->
            { tackleCheckFrequency = 175
            , tackleChances = [ ( Coord.moveUp, 0.6 ), ( Coord.moveLeft, 0.5 ), ( Coord.moveRight, 0.5 ), ( Coord.moveDown, 0.3 ) ]
            , numBadGuys = 150
            }


type Model
    = Playing PlayingModel
    | CountingDown CountingDownModel
    | BetweenDowns BetweenDownsModel
    | Ready ReadyModel


type alias PlayingModel =
    { badGuys : BadGuys
    , protagonist : Coord
    , playType : PlayType
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


type alias CountingDownModel =
    { badGuys : BadGuys
    , footballDown : FootballDown
    , startingYard : Int
    , setStartingYard : Int
    , nextSeed : Random.Seed
    , protagonist : Coord
    , timeRemaining : Float
    , tickValue : Float
    , touchdowns : Int
    , playType : PlayType
    }


type alias BetweenDownsModel =
    { badGuys : BadGuys
    , footballDown : FootballDown
    , startingYard : Int
    , setStartingYard : Int
    , nextSeed : Random.Seed
    , protagonist : Coord
    , timeRemaining : Float
    , tickValue : Float
    , touchdowns : Int
    , howPlayEnded : HowPlayEnded
    }


type PlayType
    = RunPlay
    | PassPlay { ballTarget : Coord, ballHangTime : Float, caught : Bool }


type HowPlayEnded
    = Touchdown
    | Tackled { coord : Coord, previousCoord : Coord }
    | Incomplete
    | FirstPlay


type alias ReadyModel =
    { nextSeed : Random.Seed
    , howGameEnded : Maybe HowGameEnded
    }


type HowGameEnded
    = Won { timeRemaining : Float }
    | LossBySetOfDowns { score : Int }
    | LossByTimeLimit { score : Int }


type Msg
    = NoOp
    | HandleKeyboardEvent KeyboardEvent
    | StartDown { run : Bool }
    | NewGame
    | Tick Float


type alias Flags =
    { seed : Int }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
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

                Key.P ->
                    handleStartDown False model

                Key.R ->
                    handleStartDown True model

                Key.Spacebar ->
                    case model of
                        Ready _ ->
                            handleNewGame model

                        BetweenDowns _ ->
                            handleStartDown True model

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StartDown { run } ->
            handleStartDown run model

        NewGame ->
            handleNewGame model

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
        |> maybeCatchBall
        |> (\m -> ( m, Cmd.none ))


maybeCatchBall : Model -> Model
maybeCatchBall model =
    case model of
        Playing ({ playType, tickValue, protagonist } as playingModel) ->
            case playType of
                PassPlay ({ ballHangTime, ballTarget, caught } as pass) ->
                    if not caught && tickValue >= ballHangTime then
                        if ballTarget == protagonist then
                            Playing { playingModel | playType = PassPlay { pass | caught = True } }

                        else
                            case FootballDown.next playingModel.footballDown of
                                Just nextDown ->
                                    BetweenDowns
                                        { badGuys = playingModel.badGuys
                                        , footballDown = nextDown
                                        , startingYard = playingModel.startingYard
                                        , setStartingYard = playingModel.setStartingYard
                                        , nextSeed = playingModel.nextSeed
                                        , protagonist = playingModel.protagonist
                                        , timeRemaining = playingModel.timeRemaining
                                        , touchdowns = playingModel.touchdowns
                                        , tickValue = 0
                                        , howPlayEnded = Incomplete
                                        }

                                Nothing ->
                                    Ready
                                        { nextSeed = playingModel.nextSeed
                                        , howGameEnded = Just (LossBySetOfDowns { score = playingModel.touchdowns })
                                        }

                    else
                        model

                RunPlay ->
                    model

        _ ->
            model


maybeTackleProtagonist : Model -> Model
maybeTackleProtagonist model =
    case model of
        Playing playingModel ->
            let
                { tackleChances, tackleCheckFrequency } =
                    difficultyMap playingModel.touchdowns

                playerTacklable =
                    case playingModel.playType of
                        RunPlay ->
                            True

                        PassPlay { caught } ->
                            caught

                shouldPerformTackleCheck =
                    playingModel.sinceLastTackleCheck > tackleCheckFrequency && playerTacklable

                ( probability, newNextSeed ) =
                    Random.step (Random.float 0 1) playingModel.nextSeed

                maybeTackler =
                    tackleChances
                        |> List.map (Tuple.mapFirst (\fn -> fn playingModel.protagonist))
                        |> List.filter (\( coord, tackleChance ) -> BadGuys.member coord playingModel.badGuys && (tackleChance > probability))
                        |> List.head

                gotFirstDown =
                    playingModel.protagonist.y >= playingModel.setStartingYard + 10
            in
            if shouldPerformTackleCheck then
                case maybeTackler of
                    Just tackler ->
                        if gotFirstDown then
                            BetweenDowns
                                { badGuys = playingModel.badGuys
                                , footballDown = FootballDown.first
                                , startingYard = playingModel.startingYard
                                , setStartingYard = playingModel.protagonist.y
                                , nextSeed = newNextSeed
                                , protagonist = playingModel.protagonist
                                , timeRemaining = playingModel.timeRemaining
                                , touchdowns = playingModel.touchdowns
                                , tickValue = 0
                                , howPlayEnded = Tackled { coord = playingModel.protagonist, previousCoord = Tuple.first tackler }
                                }

                        else
                            case FootballDown.next playingModel.footballDown of
                                Just nextDown ->
                                    BetweenDowns
                                        { badGuys = playingModel.badGuys
                                        , footballDown = nextDown
                                        , startingYard = playingModel.startingYard
                                        , setStartingYard = playingModel.setStartingYard
                                        , nextSeed = newNextSeed
                                        , protagonist = playingModel.protagonist
                                        , timeRemaining = playingModel.timeRemaining
                                        , touchdowns = playingModel.touchdowns
                                        , tickValue = 0
                                        , howPlayEnded = Tackled { coord = playingModel.protagonist, previousCoord = Tuple.first tackler }
                                        }

                                Nothing ->
                                    Ready { nextSeed = newNextSeed, howGameEnded = Just (LossBySetOfDowns { score = playingModel.touchdowns }) }

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
                shouldHandleBadGuyMove =
                    playingModel.sinceLastBadGuyMove > constants.badGuyMoveTime
            in
            if shouldHandleBadGuyMove then
                let
                    ( newBadGuyPositions, nextSeed ) =
                        playingModel.badGuys
                            |> BadGuys.step
                                { seed = playingModel.nextSeed
                                , bounds = bounds
                                , protagonist = playingModel.protagonist
                                , strategy =
                                    case playingModel.playType of
                                        RunPlay ->
                                            BadGuys.TowardsProtagonist

                                        PassPlay { caught } ->
                                            if caught then
                                                BadGuys.TowardsProtagonist

                                            else
                                                BadGuys.Random
                                }
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
            let
                newTimeRemaining =
                    playingModel.timeRemaining - delta
            in
            if newTimeRemaining < 0 then
                Ready
                    { nextSeed = playingModel.nextSeed
                    , howGameEnded = Just (LossByTimeLimit { score = playingModel.touchdowns })
                    }

            else
                Playing
                    { playingModel
                        | sinceLastBadGuyMove = playingModel.sinceLastBadGuyMove + delta
                        , sinceLastTackleCheck = playingModel.sinceLastTackleCheck + delta
                        , tickValue = playingModel.tickValue + delta
                        , timeRemaining = newTimeRemaining
                    }

        CountingDown countingDownModel ->
            let
                newTickValue =
                    countingDownModel.tickValue + delta
            in
            if newTickValue > 2000 then
                Playing
                    { badGuys = countingDownModel.badGuys
                    , protagonist = countingDownModel.protagonist
                    , playType = countingDownModel.playType
                    , footballDown = countingDownModel.footballDown
                    , startingYard = countingDownModel.startingYard
                    , setStartingYard = countingDownModel.setStartingYard
                    , nextSeed = countingDownModel.nextSeed
                    , sinceLastBadGuyMove = 0
                    , sinceLastTackleCheck = 0
                    , tickValue = 0
                    , timeRemaining = countingDownModel.timeRemaining
                    , touchdowns = countingDownModel.touchdowns
                    }

            else
                CountingDown { countingDownModel | tickValue = newTickValue }

        BetweenDowns betweenDownsModel ->
            BetweenDowns
                { betweenDownsModel | tickValue = betweenDownsModel.tickValue + delta }

        _ ->
            model


handleNewGame : Model -> ( Model, Cmd Msg )
handleNewGame model =
    case model of
        Ready readyModel ->
            let
                protagonist =
                    { x = 3, y = constants.startingYard }

                ( badGuys, nextSeed ) =
                    BadGuys.generate
                        protagonist
                        (difficultyMap 0).numBadGuys
                        bounds
                        readyModel.nextSeed
            in
            ( BetweenDowns
                { badGuys = badGuys
                , protagonist = protagonist
                , footballDown = FootballDown.first
                , startingYard = constants.startingYard
                , setStartingYard = constants.startingYard
                , nextSeed = nextSeed
                , tickValue = 0
                , timeRemaining = 120000
                , touchdowns = 0
                , howPlayEnded = FirstPlay
                }
            , Cmd.none
            )

        BetweenDowns betweenDownsModel ->
            ( BetweenDowns betweenDownsModel, Cmd.none )

        CountingDown countingDownModel ->
            ( CountingDown countingDownModel, Cmd.none )

        Playing playingModel ->
            ( Playing playingModel, Cmd.none )


handleStartDown : Bool -> Model -> ( Model, Cmd Msg )
handleStartDown isRun model =
    case model of
        BetweenDowns { howPlayEnded, protagonist, nextSeed, footballDown, setStartingYard, timeRemaining, touchdowns, startingYard } ->
            let
                newStartingYard =
                    case howPlayEnded of
                        Touchdown ->
                            constants.startingYard

                        FirstPlay ->
                            constants.startingYard

                        Incomplete ->
                            startingYard

                        Tackled _ ->
                            protagonist.y

                newProtagonist =
                    case howPlayEnded of
                        Touchdown ->
                            { x = 3, y = constants.startingYard }

                        FirstPlay ->
                            { x = 3, y = constants.startingYard }

                        Incomplete ->
                            { x = 3, y = startingYard }

                        Tackled _ ->
                            { x = 3, y = protagonist.y }

                newSetStartingYard =
                    case howPlayEnded of
                        Touchdown ->
                            constants.startingYard

                        FirstPlay ->
                            constants.startingYard

                        Incomplete ->
                            setStartingYard

                        Tackled _ ->
                            setStartingYard

                ( badGuys, steppedNextSeed ) =
                    BadGuys.generate
                        newProtagonist
                        (difficultyMap touchdowns).numBadGuys
                        bounds
                        nextSeed

                ( playType, newNextSeed ) =
                    if isRun then
                        ( RunPlay, steppedNextSeed )

                    else
                        let
                            ( ballTarget, steppedSeed ) =
                                generateThrow protagonist steppedNextSeed

                            ( ballHangTime, _ ) =
                                Random.step (Random.float 4000 6000) steppedNextSeed
                        in
                        ( PassPlay
                            { ballTarget = ballTarget
                            , ballHangTime = ballHangTime
                            , caught = False
                            }
                        , steppedSeed
                        )
            in
            ( CountingDown
                { badGuys = badGuys
                , protagonist = newProtagonist
                , footballDown = footballDown
                , startingYard = newStartingYard
                , setStartingYard = newSetStartingYard
                , nextSeed = newNextSeed
                , tickValue = 0
                , timeRemaining = timeRemaining
                , touchdowns = touchdowns
                , playType = playType
                }
            , Cmd.none
            )

        Ready readyModel ->
            ( Ready readyModel, Cmd.none )

        CountingDown countingDownModel ->
            ( CountingDown countingDownModel, Cmd.none )

        Playing playingModel ->
            ( Playing playingModel, Cmd.none )


generateThrow : Coord -> Random.Seed -> ( Coord, Random.Seed )
generateThrow protagonist seed =
    let
        randomX =
            Random.int bounds.xMin bounds.xMax

        randomY =
            Random.int (protagonist.y + 8) (protagonist.y + 14)
                |> Random.map (min bounds.yMax)
    in
    Random.step
        (Random.map2 Coord.make randomX randomY)
        seed


handleProtagonistMove : (Coord -> Coord) -> Model -> ( Model, Cmd Msg )
handleProtagonistMove moveFn model =
    case model of
        Playing playingModel ->
            let
                spotToMoveTo =
                    playingModel.protagonist
                        |> moveFn
                        |> Coord.constrain bounds

                currentlyInSpot =
                    BadGuys.currentBadGuy spotToMoveTo playingModel.badGuys

                scoredTouchdown =
                    spotToMoveTo.y == 101
            in
            case currentlyInSpot of
                Just _ ->
                    ( Playing playingModel
                    , Cmd.none
                    )

                Nothing ->
                    if scoredTouchdown && playingModel.touchdowns == 2 then
                        ( Ready
                            { nextSeed = playingModel.nextSeed
                            , howGameEnded = Just (Won { timeRemaining = playingModel.timeRemaining })
                            }
                        , Cmd.none
                        )

                    else if scoredTouchdown then
                        ( BetweenDowns
                            { badGuys = playingModel.badGuys
                            , footballDown = FootballDown.first
                            , startingYard = constants.startingYard
                            , setStartingYard = constants.startingYard
                            , nextSeed = playingModel.nextSeed
                            , protagonist = spotToMoveTo
                            , timeRemaining = playingModel.timeRemaining
                            , touchdowns = playingModel.touchdowns + 1
                            , tickValue = 0
                            , howPlayEnded = Touchdown
                            }
                        , Cmd.none
                        )

                    else
                        ( Playing { playingModel | protagonist = spotToMoveTo }
                        , Cmd.none
                        )

        _ ->
            ( model, Cmd.none )


init : Flags -> ( Model, Cmd Msg )
init { seed } =
    ( Ready
        { nextSeed = Random.initialSeed seed
        , howGameEnded = Nothing
        }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    (case model of
        Playing playingModel ->
            [ viewPlayingModel playingModel ]

        CountingDown countingDownModel ->
            [ viewCountingDown countingDownModel ]

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
    , yMin = 1
    , yMax = 102
    }


viewCountingDown : CountingDownModel -> Html Msg
viewCountingDown ({ protagonist, tickValue } as countingDownModel) =
    let
        ( infoWidth, infoHeight ) =
            ( constants.gridSize * 4, constants.gridSize * 2 )

        infoText =
            if tickValue < (2000 * 1 / 3) then
                "Ready..."

            else if tickValue < (2000 * 2 / 3) then
                "Hut..."

            else
                "Hike!"
    in
    Html.div [ css [ position relative ] ]
        [ Html.flexColumn [ css [ Html.gap 24, alignItems center ] ]
            [ viewField
                { badGuys = countingDownModel.badGuys
                , protagonist = protagonist
                , tackled = Nothing
                , maybePassData = Nothing
                , setStartingYard = countingDownModel.setStartingYard
                , tickValue = countingDownModel.tickValue
                }
            , viewScoreboard countingDownModel
            ]
        , Html.div
            [ css
                [ position absolute
                , top (px <| constants.gridSize * 2)
                , left (px <| (constants.gridSize * 3.5) - (infoWidth / 2))
                ]
            ]
            [ Html.modal
                { height = infoHeight
                , width = infoWidth
                , transparent = True
                }
                [ Html.h1 [] [ Html.text infoText ] ]
            ]
        ]


viewReadyModel : ReadyModel -> Html Msg
viewReadyModel { howGameEnded } =
    let
        container =
            Html.modal
                { height = constants.gridSize * 6
                , width = constants.gridSize * 7
                , transparent = False
                }
    in
    case howGameEnded of
        Nothing ->
            container
                [ Html.h2 [] [ Html.text "Rabbit vs Duck Goons MMXXIV" ]
                , Html.p [] [ Html.text "It was supposed to be a fair football game, but it looks like it's going to be you against a whoooole lotta goons." ]
                , Html.p [] [ Html.text "Run or Pass 10 yards to get a First Down" ]
                , Html.p [] [ Html.text "Score 3 Touchdowns to Win!" ]
                , Html.p [ css [ alignSelf center, fontFamily monospace ] ] [ Html.text "Arrow Keys to run" ]
                , Html.button [ Events.onClick NewGame ] [ Html.text "hut hike" ]
                ]

        Just (Won { timeRemaining }) ->
            container
                [ Html.h2 [] [ Html.text "🎉🎉🎉🎉🎉🎉" ]
                , Html.p [] [ Html.text "You won!!" ]
                , Html.p [] [ Html.text <| "And you did it with " ++ formatTime timeRemaining ++ " remaining!" ]
                , Html.button [ Events.onClick NewGame ] [ Html.text "play again?" ]
                ]

        Just (LossBySetOfDowns { score }) ->
            let
                text =
                    if score == 2 then
                        "You we're so close to 3 touchdowns..."

                    else if score == 1 then
                        "At least you scored once..."

                    else
                        "Next time, maybe you'll score..."
            in
            container
                [ Html.h2 [] [ Html.text "OUCH!! The goons had your number!" ]
                , Html.h2 [] [ Html.text text ]
                , Html.button [ Events.onClick NewGame ] [ Html.text "try again?" ]
                ]

        Just (LossByTimeLimit { score }) ->
            let
                text =
                    if score == 2 then
                        "You we're so close, if only you had a little more time..."

                    else if score == 1 then
                        "Looks like you gotta run faster..."

                    else
                        "We're you even playing there... what happened?"
            in
            container
                [ Html.h2 [] [ Html.text "TOO SLOW!!" ]
                , Html.p [] [ Html.text text ]
                , Html.button [ Events.onClick NewGame ] [ Html.text "try again?" ]
                ]


viewBetweenDowns : BetweenDownsModel -> Html Msg
viewBetweenDowns ({ howPlayEnded, footballDown, protagonist, startingYard, touchdowns, setStartingYard } as betweenDownsModel) =
    let
        playButtons =
            Html.flexRow [ css [ justifyContent spaceAround ] ]
                [ Html.button [ Events.onClick (StartDown { run = True }) ] [ Html.text "RUN" ]
                , Html.button [ Events.onClick (StartDown { run = False }) ] [ Html.text "PASS" ]
                ]

        info =
            let
                ( infoWidth, infoHeight ) =
                    ( constants.gridSize * 4, constants.gridSize * 2 )

                yardsGained =
                    protagonist.y - startingYard

                yardsRemaining =
                    setStartingYard + 10 - protagonist.y
            in
            Html.div
                [ css
                    [ position absolute
                    , top (px <| constants.gridSize * 2)
                    , left (px <| (constants.gridSize * 3.5) - (infoWidth / 2))
                    ]
                ]
                [ Html.modal { height = infoHeight, width = infoWidth, transparent = True }
                    [ case howPlayEnded of
                        Touchdown ->
                            Html.flexColumn []
                                [ Html.h2 [] [ Html.text <| "🎉Touchdown🎉" ]
                                , Html.p [] [ Html.text <| "You now have " ++ String.fromInt touchdowns ++ " points." ]
                                , playButtons
                                ]

                        Tackled tackled ->
                            Html.flexColumn []
                                [ Html.h2 []
                                    [ Html.text <|
                                        case footballDown of
                                            FootballDown.FirstDown ->
                                                "First Down! 🎉"

                                            FootballDown.SecondDown ->
                                                "Second Down"

                                            FootballDown.ThirdDown ->
                                                "Third Down"

                                            FootballDown.FourthDown ->
                                                "Fouth Down 😬"
                                    ]
                                , Html.p []
                                    [ Html.text <|
                                        if yardsGained == 0 then
                                            "No gain."

                                        else if yardsGained > 0 then
                                            "You gained " ++ String.pluralize "yard" "yards" yardsGained ++ "."

                                        else
                                            "You lost " ++ String.pluralize "yard" "yards" yardsGained ++ "."
                                    ]
                                , Html.p []
                                    [ if footballDown /= FootballDown.FirstDown then
                                        Html.text (String.pluralize "yard" "yards" yardsRemaining ++ " to go.")

                                      else
                                        Html.text ""
                                    ]
                                , playButtons
                                ]

                        Incomplete ->
                            Html.flexColumn [ css [ Html.gap 16 ] ]
                                [ Html.h2 [] [ Html.text <| "Incomplete" ]
                                , Html.p [] [ Html.text "No gain." ]
                                , Html.p [] [ Html.text (String.pluralize "yard" "yards" yardsRemaining ++ " to go.") ]
                                , playButtons
                                ]

                        FirstPlay ->
                            Html.flexColumn [ css [ Html.gap 16 ] ]
                                [ Html.div [] [ Html.text <| "Get ready to run the ball!" ]
                                , playButtons
                                ]
                    ]
                ]
    in
    Html.div [ css [ position relative ] ]
        [ Html.flexColumn [ css [ Html.gap 24, alignItems center ] ]
            [ viewField
                { badGuys = betweenDownsModel.badGuys
                , protagonist = protagonist
                , tackled =
                    case howPlayEnded of
                        Tackled tackled_ ->
                            Just tackled_

                        Touchdown ->
                            Nothing

                        Incomplete ->
                            Nothing

                        FirstPlay ->
                            Nothing
                , maybePassData = Nothing
                , setStartingYard = betweenDownsModel.setStartingYard
                , tickValue = betweenDownsModel.tickValue
                }
            , viewScoreboard betweenDownsModel
            ]
        , info
        ]


viewPlayingModel : PlayingModel -> Html Msg
viewPlayingModel playingModel =
    let
        maybePassData =
            case playingModel.playType of
                PassPlay pass ->
                    Just
                        { startingYard = playingModel.startingYard
                        , ballTarget = pass.ballTarget
                        , caught = pass.caught
                        , ballHangTime = pass.ballHangTime
                        }

                RunPlay ->
                    Nothing
    in
    Html.flexColumn [ css [ Html.gap 24, alignItems center ] ]
        [ viewField
            { badGuys = playingModel.badGuys
            , protagonist = playingModel.protagonist
            , tackled = Nothing
            , setStartingYard = playingModel.setStartingYard
            , tickValue = playingModel.tickValue
            , maybePassData = maybePassData
            }
        , viewScoreboard playingModel
        ]


viewScoreboard : { a | timeRemaining : Float, touchdowns : Int, footballDown : FootballDown, setStartingYard : Int, startingYard : Int } -> Html msg
viewScoreboard { timeRemaining, touchdowns, footballDown, setStartingYard, startingYard } =
    Html.flexColumn
        [ css
            [ width (px 400)
            , padding (px 16)
            , backgroundColor (rgb 0 0 0)
            , border3 (px 4) solid (rgb 180 60 60)
            , color (rgb 0 255 0)
            , fontFamily monospace
            , fontSize (px 22)
            ]
        ]
        [ Html.pre [] [ Html.text <| "Time:  " ++ formatTime timeRemaining ]
        , Html.pre [] [ Html.text <| "Score: " ++ String.fromInt touchdowns ]
        , Html.pre [] [ Html.text <| "Down:  " ++ FootballDown.toString footballDown ]
        , Html.pre [] [ Html.text <| "Yards: " ++ String.fromInt (setStartingYard + 10 - startingYard) ]
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
        , maybePassData : Maybe { ballTarget : Coord, ballHangTime : Float, caught : Bool, startingYard : Int }
    }
    -> Html msg
viewField { badGuys, protagonist, tackled, setStartingYard, tickValue, maybePassData } =
    let
        triangle direction =
            Html.node "field-triangle"
                [ css
                    [ position absolute
                    , top (px 12)
                    , if direction == "left" then
                        left (px -24)

                      else
                        right (px -20)
                    ]
                , Attrs.attribute "direction" direction
                ]
                []

        rowsToShow =
            List.range
                (protagonist.y - 1)
                (protagonist.y + 4)
                |> List.reverse

        viewRow y =
            Html.flexRow []
                ((List.map (\x -> viewCell (Coord.make x y)) <| Tuple.first grid)
                    ++ [ if modBy 5 y == 0 then
                            Html.node "field-line"
                                [ Attrs.attribute "size" (String.fromInt constants.gridSize)
                                , Attrs.attribute "color" "white"
                                , css
                                    [ position absolute
                                    , width (px <| constants.gridSize * 7)
                                    , zIndex (int -1)
                                    ]
                                ]
                                []

                         else
                            Html.text ""
                       , -- This protagonist /= 6 nonsense
                         -- is kinda a dumb hack so the field numbers aren't don't
                         -- get rendered off the field
                         if modBy 10 y == 0 && (modBy 10 protagonist.y /= 6) && y /= 100 then
                            Html.div
                                [ css
                                    [ position absolute
                                    , transforms
                                        [ translateX (px (constants.gridSize * 6))
                                        , rotate (deg -90)
                                        , translateX (px 42)
                                        ]
                                    , displayFlex
                                    , property "gap" "12px"
                                    , zIndex (int -1)
                                    ]
                                ]
                                [ if y < 50 then
                                    triangle "left"

                                  else
                                    Html.text ""
                                , Html.node "field-number"
                                    [ Attrs.attribute "size" (String.fromInt constants.gridSize)
                                    , Attrs.attribute "number" (String.fromInt <| 5 - (abs <| (y // 10) - 5))
                                    ]
                                    []
                                , Html.node "field-number"
                                    [ Attrs.attribute "size" (String.fromInt constants.gridSize)
                                    , Attrs.attribute "number" "0"
                                    ]
                                    []
                                , if y > 50 then
                                    triangle "right"

                                  else
                                    Html.text ""
                                ]

                         else
                            Html.text ""
                       , if y == setStartingYard + 9 then
                            -- NOTE - this is 9 instead of 10 because it's more satisfying to CROSS the line
                            --        so we render the yellow line off-by-one closer
                            Html.node "field-line"
                                [ Attrs.attribute "size" (String.fromInt constants.gridSize)
                                , Attrs.attribute "color" "yellow"
                                , css
                                    [ position absolute
                                    , width (px <| constants.gridSize * 7)
                                    , zIndex (int -1)
                                    ]
                                ]
                                []

                         else
                            Html.text ""
                       ]
                )

        viewCell coord =
            let
                isProtagonist =
                    coord == protagonist

                isBadGuy =
                    BadGuys.member coord badGuys

                viewTarget =
                    case maybePassData of
                        Just { ballTarget, caught } ->
                            if not caught && ballTarget == coord then
                                Html.div [ css [ position absolute ] ] [ Html.text "X" ]

                            else
                                Html.text ""

                        Nothing ->
                            Html.text ""

                art =
                    case tackled of
                        Nothing ->
                            if isProtagonist then
                                Html.div []
                                    [ Html.img
                                        [ Attrs.height (floor <| constants.gridSize * 1.2)
                                        , Attrs.width (floor <| constants.gridSize * 1.2)
                                        , if modBy 2 (floor (tickValue / 200)) == 0 then
                                            Attrs.src "./assets/rabbit-with-ball-1.svg"

                                          else
                                            Attrs.src "./assets/rabbit-with-ball-2.svg"
                                        ]
                                        []
                                    ]

                            else if isBadGuy then
                                Html.div []
                                    [ Html.img
                                        [ Attrs.height (floor <| constants.gridSize * 1.3)
                                        , Attrs.width (floor <| constants.gridSize * 1.3)
                                        , if modBy 2 (floor (tickValue / 200)) == 0 then
                                            Attrs.src "./assets/bad-guy-1.svg"

                                          else
                                            Attrs.src "./assets/bad-guy-2.svg"
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
                                        , Attrs.src "./assets/tackled.svg"
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
                                            Attrs.src "./assets/bad-guy-1.svg"

                                          else
                                            Attrs.src "./assets/bad-guy-2.svg"
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
                    , position relative
                    , if coord.y > 102 then
                        backgroundColor (rgba 255 0 0 1)

                      else if coord.y > 100 then
                        backgroundColor (rgba 255 0 0 0.2)

                      else
                        batch []
                    ]
                ]
                [ art, viewTarget ]

        viewBall =
            case maybePassData of
                Just { startingYard, caught, ballTarget, ballHangTime } ->
                    let
                        normalizedTime =
                            tickValue / ballHangTime

                        xPosition =
                            normalizedTime
                                * (toFloat <| ballTarget.x - 3)
                                |> (+) 3
                                |> (+) 0.5
                                |> (*) constants.gridSize

                        yPosition =
                            normalizedTime
                                * (toFloat <| ballTarget.y - startingYard)
                                |> (+) (toFloat startingYard + 1)
                                |> (\ballYard -> ballYard - toFloat protagonist.y)
                                |> (+) 0.5
                                |> (*) constants.gridSize
                                |> (*) -1

                        scaleValue =
                            (1.4 * sin (pi * normalizedTime))
                                |> (+) 1
                    in
                    if not caught then
                        Html.div
                            [ css
                                [ position absolute
                                , transforms
                                    [ translate2 (px xPosition) (px yPosition)
                                    , scale scaleValue
                                    ]
                                ]
                            ]
                            [ Html.text "O" ]

                    else
                        Html.text ""

                Nothing ->
                    Html.text ""
    in
    Html.div []
        [ Html.div
            [ css
                [ position absolute
                , height (px <| constants.gridSize * 6)
                , width (px <| constants.gridSize * 7)
                , zIndex (int -1)
                ]
            ]
            [ Html.node "field-tile"
                [ Attrs.attribute "size" (String.fromInt constants.gridSize) ]
                []
            ]
        , Html.flexRow [ css [ justifyContent center, alignItems center, zIndex (int 1) ] ]
            [ Html.flexColumn
                []
                (List.map viewRow rowsToShow)
            ]
        , viewBall
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

                BetweenDowns _ ->
                    Browser.Events.onAnimationFrameDelta Tick

                CountingDown _ ->
                    Browser.Events.onAnimationFrameDelta Tick

                _ ->
                    Sub.none
    in
    Sub.batch
        [ keyHandlerSub
        , animationSub
        ]
