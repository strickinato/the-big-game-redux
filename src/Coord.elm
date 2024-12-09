module Coord exposing (..)

import Bounds exposing (Bounds)


type alias Coord =
    { x : Int
    , y : Int
    }


make : Int -> Int -> Coord
make x y =
    { x = x, y = y }


moveDown : Coord -> Coord
moveDown coord =
    { coord | y = coord.y - 1 }


moveUp : Coord -> Coord
moveUp coord =
    { coord | y = coord.y + 1 }


moveLeft : Coord -> Coord
moveLeft coord =
    { coord | x = coord.x - 1 }


moveRight : Coord -> Coord
moveRight coord =
    { coord | x = coord.x + 1 }


constrain : Bounds -> Coord -> Coord
constrain { xMin, yMin, xMax, yMax } { x, y } =
    { x = clamp xMin xMax x
    , y = clamp yMin yMax y
    }


toComparable : Coord -> ( Int, Int )
toComparable { x, y } =
    ( x, y )
