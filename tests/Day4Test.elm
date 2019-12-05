module Day4Test exposing (..)

import Day4 exposing (group, ialwaysIncreases, icontainsDouble, ilargerMatchingGroups, listOfInt, part1, part2, validPart1Password, validPart2Password)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Day 4"
        [ describe "contains double"
            [ test "111111" <| \_ -> Expect.true "111111" (icontainsDouble 111111)
            , test "223450" <| \_ -> Expect.true "223450" (icontainsDouble 223450)
            , test "123789" <| \_ -> Expect.false "123789" (icontainsDouble 123789)
            , test "122345" <| \_ -> Expect.true "122345" (icontainsDouble 122345)
            ]
        , describe "always increase"
            [ test "111111" <| \_ -> Expect.true "111111" (ialwaysIncreases 111111)
            , test "223450" <| \_ -> Expect.false "223450" (ialwaysIncreases 223450)
            , test "123789" <| \_ -> Expect.true "123789" (ialwaysIncreases 123789)
            , test "111123" <| \_ -> Expect.true "111123" (ialwaysIncreases 111123)
            , test "135679" <| \_ -> Expect.true "135679" (ialwaysIncreases 135679)
            ]
        , describe "larger matching groups"
            [ test "111111" <| \_ -> Expect.false "111111" (ilargerMatchingGroups 111111)
            , test "223450" <| \_ -> Expect.true "223450" (ilargerMatchingGroups 223450)
            , test "123789" <| \_ -> Expect.false "123789" (ilargerMatchingGroups 123789)
            , test "111123" <| \_ -> Expect.false "111123" (ilargerMatchingGroups 111123)
            , test "135679" <| \_ -> Expect.false "135679" (ilargerMatchingGroups 135679)
            , test "112233" <| \_ -> Expect.true "112233" (ilargerMatchingGroups 112233)
            , test "123444" <| \_ -> Expect.false "123444" (ilargerMatchingGroups 123444)
            , test "111122" <| \_ -> Expect.true "111122" (ilargerMatchingGroups 111122)
            ]
        , describe "both"
            [ test "111111" <| \_ -> Expect.true "111111" (validPart1Password 111111)
            , test "223450" <| \_ -> Expect.false "223450" (validPart1Password 223450)
            , test "123789" <| \_ -> Expect.false "123789" (validPart1Password 123789)
            ]
        , describe "three"
            [ test "112233" <| \_ -> Expect.true "112233" (validPart2Password 112233)
            , test "123444" <| \_ -> Expect.false "123444" (validPart2Password 123444)
            , test "111122" <| \_ -> Expect.true "111122" (validPart2Password 111122)
            ]
        , describe "actual input"
            [ test "part1 : 264360 - 746325" <|
                \_ ->
                    part1 ()
                        |> Expect.equal 945
            , test "part2 : 264360 - 746325" <|
                \_ ->
                    part2 ()
                        |> Expect.equal 617
            ]
        ]
