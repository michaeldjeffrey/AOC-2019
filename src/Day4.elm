module Day4 exposing (..)


part1 : () -> Int
part1 _ =
    List.range 264360 746325
        |> List.map listOfInt
        |> List.filter containsDouble
        |> List.filter alwaysIncreases
        |> List.length


part2 : () -> Int
part2 _ =
    List.range 264360 746325
        |> List.map listOfInt
        |> List.filter containsDouble
        |> List.filter alwaysIncreases
        |> List.filter largerMatchingGroup
        |> List.length


listOfInt : Int -> List Int
listOfInt =
    String.fromInt
        >> String.toList
        >> List.filterMap (String.fromChar >> String.toInt)


ilargerMatchingGroups : Int -> Bool
ilargerMatchingGroups =
    listOfInt >> largerMatchingGroup


largerMatchingGroup : List Int -> Bool
largerMatchingGroup input =
    input
        |> group
        |> List.map List.length
        |> List.member 2


icontainsDouble : Int -> Bool
icontainsDouble =
    listOfInt >> containsDouble


containsDouble : List Int -> Bool
containsDouble input =
    containsDoubleHelpers 0 input


containsDoubleHelpers : Int -> List Int -> Bool
containsDoubleHelpers prev curr =
    case curr of
        [] ->
            False

        first :: rest ->
            if prev == first then
                True

            else
                containsDoubleHelpers first rest


ialwaysIncreases : Int -> Bool
ialwaysIncreases =
    listOfInt >> alwaysIncreases


alwaysIncreases : List Int -> Bool
alwaysIncreases input =
    input == List.sort input


validPart1Password : Int -> Bool
validPart1Password input =
    let
        loi =
            listOfInt input
    in
    containsDouble loi && alwaysIncreases loi


validPart2Password : Int -> Bool
validPart2Password input =
    let
        loi =
            listOfInt input
    in
    containsDouble loi && alwaysIncreases loi && largerMatchingGroup loi


group : List a -> List (List a)
group =
    groupWhile (==)


groupWhile : (a -> a -> Bool) -> List a -> List (List a)
groupWhile eq xs =
    case xs of
        [] ->
            []

        first :: rest ->
            let
                ( ys, zs ) =
                    span (eq first) rest
            in
            (first :: ys) :: groupWhile eq zs


span : (a -> Bool) -> List a -> ( List a, List a )
span p xs =
    ( takeWhile p xs, dropWhile p xs )


takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if predicate x then
                x :: takeWhile predicate xs

            else
                []


dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if predicate x then
                dropWhile predicate xs

            else
                list
