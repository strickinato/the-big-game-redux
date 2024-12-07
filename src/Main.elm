module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Random


type Model
    = Playing PlayingModel


type alias PlayingModel =
    { badGuys : List Coord
    , protagonist : Coord
    }


type alias Coord =
    ( Int, Int )


type Msg
    = NoOp


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = \msg model -> ( model, Cmd.none )
        , view = view
        , subscriptions = \_ -> Sub.none
        }


badGuyGenerator : Coord -> Random.Generator ( Int, Int )
badGuyGenerator protagonistYardLine =
    -- TODO goal line stands
    Random.pair (Random.int 0 6) (Random.int (Tuple.second protagonistYardLine + 2) 99)


init : ( Model, Cmd Msg )
init =
    let
        protagonist =
            ( 3, 95 )

        ( badGuys, _ ) =
            Random.step
                (Random.list (floor (100 * (1 - (Tuple.second protagonist / 100)))) (badGuyGenerator protagonist))
                (Random.initialSeed 0)
    in
    ( Playing
        { badGuys = badGuys |> Debug.log "bad guys"
        , protagonist = protagonist
        }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    case model of
        Playing playingModel ->
            viewPlayingModel playingModel


grid : ( List Int, List Int )
grid =
    ( List.range 0 7, List.range 0 100 |> List.reverse )


viewPlayingModel : PlayingModel -> Html Msg
viewPlayingModel { protagonist, badGuys } =
    let
        viewRow y =
            Html.div [ style "display" "flex" ]
                (List.map (\x -> viewCell ( x, y )) <| Tuple.first grid)

        viewCell ( x, y ) =
            let
                isProtagonist =
                    ( x, y ) == protagonist

                isBadGuy =
                    List.member ( x, y ) badGuys

                art =
                    if isProtagonist then
                        "P"

                    else if isBadGuy then
                        "X"

                    else
                        "_"
            in
            div []
                [ text art ]
    in
    Html.div
        [ style "display" "flex"
        , style "flex-direction" "column"
        ]
        (List.map viewRow (Tuple.second grid))
