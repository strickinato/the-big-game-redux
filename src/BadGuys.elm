module BadGuys exposing (BadGuys, Strategy(..), currentBadGuy, generate, member, step)

import Bounds exposing (Bounds)
import Coord exposing (Coord)
import Dict exposing (Dict)
import List.Extra as List
import Random
import Random.Extra as Random


type BadGuys
    = BadGuys (Dict ( Int, Int ) { previousSpot : ( Int, Int ) })


generate : Coord -> Int -> Bounds -> Random.Seed -> ( BadGuys, Random.Seed )
generate protagonistCoord numBadGuys bounds seed =
    Random.step
        (Random.list numBadGuys (generator protagonistCoord)
            |> Random.map (List.map (\c -> ( c, { previousSpot = c } )))
            |> Random.map Dict.fromList
            |> Random.map BadGuys
        )
        seed


generator : Coord -> Random.Generator ( Int, Int )
generator protagonistCoord =
    -- TODO goal line stands
    let
        aboveProtagonist =
            Random.int 0 6
                |> Random.andThen
                    (\x ->
                        if List.member x [ 2, 3, 4 ] then
                            Random.pair
                                (Random.constant x)
                                (Random.int (protagonistCoord.y + 2) 102)

                        else
                            Random.pair
                                (Random.constant x)
                                (Random.int (protagonistCoord.y + 1) 102)
                    )

        belowProtagonist =
            Random.map2 Tuple.pair
                (Random.int 0 6)
                (Random.int 0 (protagonistCoord.y - 2))
    in
    Random.andThen2
        (\above below ->
            Random.weighted
                ( 1 - toFloat protagonistCoord.y / 100, above )
                [ ( toFloat protagonistCoord.y / 100, below ) ]
        )
        aboveProtagonist
        belowProtagonist


type Strategy
    = Random
    | TowardsProtagonist


step :
    { seed : Random.Seed
    , bounds : Bounds
    , protagonist : Coord
    , strategy : Strategy
    }
    -> BadGuys
    -> ( BadGuys, Random.Seed )
step { seed, bounds, protagonist, strategy } (BadGuys badGuys) =
    let
        { newBadGuys, newSeed } =
            Dict.foldl
                (\( x, y ) _ acc ->
                    let
                        validMoves =
                            validMovesForBadGuy
                                { bounds = bounds
                                , protagonist = protagonist
                                , badGuy = Coord.make x y
                                , allBadGuys = BadGuys acc.newBadGuys
                                , strategy = strategy
                                }

                        ( randomSpot, steppedSeed ) =
                            Random.step (Random.int 0 (List.length validMoves - 1)) acc.newSeed

                        newCoord =
                            validMoves
                                |> List.getAt randomSpot
                                |> Maybe.map Coord.toComparable
                                |> Maybe.withDefault ( x, y )
                    in
                    { newSeed = steppedSeed
                    , newBadGuys = Dict.insert newCoord { previousSpot = ( x, y ) } acc.newBadGuys
                    }
                )
                { newSeed = seed, newBadGuys = Dict.empty }
                badGuys
    in
    ( newBadGuys |> BadGuys
    , newSeed
    )


validMovesForBadGuy : { bounds : Bounds, protagonist : Coord, badGuy : Coord, allBadGuys : BadGuys, strategy : Strategy } -> List Coord
validMovesForBadGuy { bounds, protagonist, badGuy, allBadGuys, strategy } =
    let
        movedCloser coord =
            case strategy of
                Random ->
                    True

                TowardsProtagonist ->
                    Coord.distanceBetween protagonist coord < Coord.distanceBetween protagonist badGuy
    in
    [ Coord.moveUp, Coord.moveDown, Coord.moveLeft, Coord.moveRight ]
        |> List.map (\fn -> fn badGuy |> Coord.constrain bounds)
        |> List.filter (\c -> not (member c allBadGuys) && c /= protagonist && movedCloser c)


member : Coord -> BadGuys -> Bool
member coord (BadGuys badGuys) =
    Dict.member ( coord.x, coord.y ) badGuys


currentBadGuy : Coord -> BadGuys -> Maybe { coord : Coord, previousCoord : Coord }
currentBadGuy coord (BadGuys badGuys) =
    Dict.get (Coord.toComparable coord) badGuys
        |> Maybe.map
            (\{ previousSpot } ->
                { coord = coord
                , previousCoord = Coord.fromComparable previousSpot
                }
            )
