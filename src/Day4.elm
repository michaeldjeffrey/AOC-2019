module Day4 exposing (..)


intToList : Int -> List Char
intToList =
    String.fromInt >> String.toList


listOfInt : Int -> List Int
listOfInt =
    intToList >> List.filterMap (String.fromChar >> String.toInt)


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


validPassword : Int -> Bool
validPassword input =
    let
        loi =
            listOfInt input
    in
    containsDouble loi && alwaysIncreases loi


unsafeInt : String -> Int
unsafeInt input =
    Maybe.withDefault 0 (String.toInt input)
