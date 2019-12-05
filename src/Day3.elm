module Day3 exposing (..)

import Dict exposing (..)
import Set exposing (..)


type Dir
    = Up
    | Down
    | Left
    | Right
    | Invalid


type alias Path =
    { dir : Dir, amount : Int }


type alias Point =
    ( Int, Int )


type alias Wire =
    List Point


part1 : String -> String -> Maybe Int
part1 one two =
    wireIntersections (wire one) (wire two)
        |> Set.map manHattanDistance
        |> Set.toList
        |> List.minimum


manHattanDistance : Point -> Int
manHattanDistance ( x, y ) =
    abs x + abs y


wire : String -> Set Point
wire input =
    wireHelper (readDirs input) ( 0, 0 )
        |> Set.fromList


wireHelper : List Path -> Point -> Wire
wireHelper dirs startingPoint =
    case dirs of
        [] ->
            []

        first :: rest ->
            let
                one =
                    travel first startingPoint
            in
            one ++ wireHelper rest (unsafeEnd one)


wireIntersections : Set Point -> Set Point -> Set Point
wireIntersections =
    Set.intersect


unsafeEnd : List Point -> Point
unsafeEnd l =
    Maybe.withDefault ( 0, 0 ) (List.head <| List.reverse l)


travel : Path -> Point -> List Point
travel path startingPoint =
    let
        { dir, amount } =
            path

        moved =
            movePoint dir startingPoint
    in
    case amount of
        0 ->
            []

        num ->
            [ moved ] ++ travel (dec path) moved


movePoint : Dir -> Point -> Point
movePoint dir ( x, y ) =
    case dir of
        Up ->
            ( x, y + 1 )

        Down ->
            ( x, y - 1 )

        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )

        Invalid ->
            ( x, y )


dec : Path -> Path
dec { dir, amount } =
    { dir = dir, amount = amount - 1 }


readDirs : String -> List Path
readDirs input =
    String.split "," input
        |> List.map readDir


readDir : String -> Path
readDir input =
    case String.uncons input of
        Just ( 'R', num ) ->
            { dir = Right, amount = unsafeToInt num }

        Just ( 'L', num ) ->
            { dir = Left, amount = unsafeToInt num }

        Just ( 'U', num ) ->
            { dir = Up, amount = unsafeToInt num }

        Just ( 'D', num ) ->
            { dir = Down, amount = unsafeToInt num }

        Just ( _, _ ) ->
            { dir = Invalid, amount = 0 }

        Nothing ->
            { dir = Invalid, amount = 0 }


unsafeToInt : String -> Int
unsafeToInt s =
    case String.toInt s of
        Just value ->
            value

        Nothing ->
            0
