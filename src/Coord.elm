module Coord exposing (..)


type alias Coord =
    { x : Int, y : Int }


type alias Constraint =
    { xMin : Int
    , yMin : Int
    , xMax : Int
    , yMax : Int
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


constrain : Constraint -> Coord -> Coord
constrain { xMin, yMin, xMax, yMax } { x, y } =
    { x = clamp xMin xMax x
    , y = clamp yMin yMax y
    }
