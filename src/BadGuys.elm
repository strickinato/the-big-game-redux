module BadGuys exposing (BadGuys, currentBadGuy, generate, member, step)

import Bounds exposing (Bounds)
import Coord exposing (Coord)
import List.Extra as List
import Random
import Set exposing (Set)


type BadGuys
    = BadGuys (Set ( Int, Int ))


numBadGuys =
    200


generate : Coord -> Bounds -> Random.Seed -> ( BadGuys, Random.Seed )
generate protagonistCoord bounds seed =
    Random.step
        (Random.list numBadGuys (generator protagonistCoord)
            |> Random.map Set.fromList
            |> Random.map BadGuys
        )
        seed


generator : Coord -> Random.Generator ( Int, Int )
generator protagonistCoord =
    -- TODO goal line stands
    Random.map2 Tuple.pair (Random.int 0 6) (Random.int (protagonistCoord.y + 2) 99)


step : Random.Seed -> Bounds -> BadGuys -> ( BadGuys, Random.Seed )
step seed bounds (BadGuys badGuys) =
    let
        { newBadGuys, newSeed } =
            Set.foldl
                (\( x, y ) acc ->
                    let
                        validMoves =
                            validMovesForBadGuy
                                bounds
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
                    , newBadGuys = Set.insert newCoord acc.newBadGuys
                    }
                )
                { newSeed = seed, newBadGuys = Set.empty }
                badGuys
    in
    ( newBadGuys |> BadGuys
    , newSeed
    )


validMovesForBadGuy : Bounds -> Coord -> BadGuys -> List Coord
validMovesForBadGuy bounds coord badGuys =
    [ Coord.moveUp, Coord.moveDown, Coord.moveLeft, Coord.moveRight ]
        |> List.map (\fn -> fn coord |> Coord.constrain bounds)
        |> List.filter (\c -> not (member c badGuys))


member : Coord -> BadGuys -> Bool
member coord (BadGuys badGuys) =
    Set.member ( coord.x, coord.y ) badGuys


currentBadGuy : Coord -> BadGuys -> Maybe Coord
currentBadGuy coord badGuys =
    if member coord badGuys then
        Just coord

    else
        Nothing
