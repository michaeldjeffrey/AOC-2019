module Day4Test exposing (..)

import Day4 exposing (ialwaysIncreases, icontainsDouble, validPassword)
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
        , describe "both"
            [ test "111111" <| \_ -> Expect.true "111111" (validPassword 111111)
            , test "223450" <| \_ -> Expect.false "223450" (validPassword 223450)
            , test "123789" <| \_ -> Expect.false "123789" (validPassword 123789)
            ]
        , describe "actual input"
            [ test "264360 - 746325 /= 481966" <|
                \_ ->
                    Expect.notEqual (List.length <| List.map validPassword (List.range 264361 746324)) 481966
            , test "264360 - 746325 /= 481964" <|
                \_ ->
                    Expect.notEqual (List.length <| List.map validPassword (List.range 264361 746324)) 481964
            ]
        ]
