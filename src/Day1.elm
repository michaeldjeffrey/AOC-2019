module Day1 exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { inputs : List Int
    , single : Int
    , answer : Int
    }


init : Model
init =
    Model [] 12 0


update : Msg -> Model -> Model
update msg model =
    case msg of
        Multiple nums ->
            { model | inputs = multipleCleaned nums }

        AddInput ->
            { model | inputs = model.single :: model.inputs, single = 0 }

        Single num ->
            { model | single = Maybe.withDefault 0 (String.toInt num) }

        Calculate ->
            { model | answer = fuel model.single }


multipleCleaned : String -> List Int
multipleCleaned nums =
    String.split "\n" nums
        |> List.map String.toInt
        |> List.map (Maybe.withDefault 0)


type Msg
    = Single String
    | Multiple String
    | Calculate
    | AddInput


view : Model -> Html Msg
view model =
    Html.form [ onSubmit AddInput ]
        [ text "Inputs"
        , input
            [ type_ "text", placeholder "Number", value (String.fromInt model.single), onInput Single ]
            []
        , textarea [ placeholder "inputs", onInput Multiple ] []
        , hr [] []
        , text ("Answer:" ++ String.fromInt model.answer)
        , button [ onClick Calculate ] [ text "Calculate" ]
        , hr [] []
        , table [] (List.map inputView model.inputs)
        , small [] [ text ("Part 1 Total: " ++ String.fromInt (totalFuel model.inputs)) ]
        , hr [] []
        , small [] [ text ("Part 2 Total: " ++ String.fromInt (part2 model.inputs)) ]
        ]


inputView : Int -> Html Msg
inputView i =
    tr []
        [ td [] [ text (String.fromInt i) ]
        , td [] [ text " -> " ]
        , td [] [ text (String.fromInt (fuel i)) ]
        , td [] [ text " -> " ]
        , td [] [ text (String.fromInt (extraFuel i)) ]
        ]


totalFuel : List Int -> Int
totalFuel inputs =
    List.map fuel inputs
        |> List.sum


fuel : Int -> Int
fuel mass =
    floor (toFloat mass / 3) - 2


extraFuel : Int -> Int
extraFuel mass =
    let
        one =
            fuel mass
    in
    if one <= 0 then
        0

    else
        one + extraFuel one


part2 : List Int -> Int
part2 inputs =
    List.map extraFuel inputs |> List.sum
