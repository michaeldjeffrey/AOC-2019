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


type alias SimplePoint =
    ( Int, Int )


type alias Point =
    ( ( Int, Int ), Int )


type alias Wire =
    List Point


part1 : String -> String -> Maybe Int
part1 one two =
    wireIntersections (wire one) (wire two)
        |> Set.map manHattanDistance
        |> Set.toList
        |> List.minimum


part2 : String -> String -> Maybe Int
part2 one two =
    let
        a =
            wire one

        b =
            wire two

        is =
            wireIntersections a b

        aDict =
            wireToDict a

        bDict =
            wireToDict b

        toDistances : SimplePoint -> ( Int, Int )
        toDistances point =
            ( unsafeGetFromDict point aDict, unsafeGetFromDict point bDict )

        distances =
            List.map toDistances (Set.toList is)
    in
    distances
        |> List.map manHattanDistance
        |> List.minimum


unsafeGetFromDict : SimplePoint -> Dict SimplePoint Int -> Int
unsafeGetFromDict k d =
    Dict.get k d |> Maybe.withDefault 0


manHattanDistance : SimplePoint -> Int
manHattanDistance ( x, y ) =
    abs x + abs y


wire : String -> Set Point
wire input =
    wireHelper (readDirs input) ( ( 0, 0 ), 0 )
        |> Set.fromList


wireToDict : Set Point -> Dict SimplePoint Int
wireToDict points =
    points
        |> Set.toList
        |> Dict.fromList


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


toSimplePoint : Point -> SimplePoint
toSimplePoint ( ( x, y ), _ ) =
    ( x, y )


wireIntersections : Set Point -> Set Point -> Set SimplePoint
wireIntersections one two =
    Set.intersect (Set.map toSimplePoint one) (Set.map toSimplePoint two)


unsafeEnd : List Point -> Point
unsafeEnd l =
    Maybe.withDefault ( ( 0, 0 ), 0 ) (List.head <| List.reverse l)


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
movePoint dir ( ( x, y ), distance ) =
    case dir of
        Up ->
            ( ( x, y + 1 ), distance + 1 )

        Down ->
            ( ( x, y - 1 ), distance + 1 )

        Left ->
            ( ( x - 1, y ), distance + 1 )

        Right ->
            ( ( x + 1, y ), distance + 1 )

        Invalid ->
            ( ( x, y ), distance + 1 )


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
