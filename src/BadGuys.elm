module BadGuys exposing (BadGuys, currentBadGuy, generate, member, step)

import Bounds exposing (Bounds)
import Coord exposing (Coord)
import Dict exposing (Dict)
import List.Extra as List
import Random
import Random.Extra as Random


type BadGuys
    = BadGuys (Dict ( Int, Int ) { previousSpot : ( Int, Int ) })


numBadGuys =
    200


generate : Coord -> Bounds -> Random.Seed -> ( BadGuys, Random.Seed )
generate protagonistCoord bounds seed =
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
            Random.map2 Tuple.pair
                (Random.int 0 6)
                (Random.int (protagonistCoord.y + 2) 99)

        belowProtagonist =
            Random.map2 Tuple.pair
                (Random.int 0 6)
                (Random.int 0 (protagonistCoord.y - 2))
    in
    Random.andThen2
        (\above below ->
            Random.weighted
                ( Debug.log "above" <| 1 - toFloat protagonistCoord.y / 100, above )
                [ ( Debug.log "below" <| (toFloat protagonistCoord.y / 100), below ) ]
        )
        aboveProtagonist
        belowProtagonist


step : Random.Seed -> Bounds -> Coord -> BadGuys -> ( BadGuys, Random.Seed )
step seed bounds protagonist (BadGuys badGuys) =
    let
        { newBadGuys, newSeed } =
            Dict.foldl
                (\( x, y ) _ acc ->
                    let
                        validMoves =
                            validMovesForBadGuy
                                bounds
                                protagonist
                                { x = x, y = y }
                                (BadGuys acc.newBadGuys)

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


validMovesForBadGuy : Bounds -> Coord -> Coord -> BadGuys -> List Coord
validMovesForBadGuy bounds protagonist testCoord badGuys =
    [ Coord.moveUp, Coord.moveDown, Coord.moveLeft, Coord.moveRight ]
        |> List.map (\fn -> fn testCoord |> Coord.constrain bounds)
        |> List.filter (\c -> not (member c badGuys) && c /= protagonist)


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
