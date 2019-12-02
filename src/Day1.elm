module Day1 exposing (..)

import Browser
import Css exposing (..)
import Html
import Html.Styled as Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, placeholder, src, type_, value)
import Html.Styled.Events exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view >> toUnstyled }


type alias Model =
    { inputs : List Int
    , single : Int
    , answer : Int
    }


init : Model
init =
    Model
        [ 54172
        , 58469
        , 92948
        , 143402
        , 57563
        , 54532
        , 68042
        , 89847
        , 70872
        ]
        12
        0


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


container : Style
container =
    Css.batch
        [ border3 (px 5) solid (rgb 120 120 120)
        , width (px 500)
        , margin auto
        , padding (rem 1)
        , displayFlex
        ]


view : Model -> Styled.Html Msg
view model =
    let
        col =
            styled div [ flex (int 1) ]
    in
    Styled.form [ onSubmit AddInput, css [ container, flexDirection column ] ]
        [ h1 [] [ text "Day 1" ]
        , div [ css [ container ] ]
            [ col []
                [ text "Inputs"
                , br [] []
                , input
                    [ type_ "text", placeholder "Single", value (String.fromInt model.single), onInput Single ]
                    []
                , br [] []
                , textarea [ placeholder "Multiple", onInput Multiple ] []
                , hr [] []
                , Styled.small [] [ text ("Part 1 Total: " ++ prettyNumber (totalFuel model.inputs)) ]
                , hr [] []
                , Styled.small [] [ text ("Part 2 Total: " ++ prettyNumber (part2 model.inputs)) ]
                ]
            , col []
                [ Styled.table [ css [] ]
                    [ thead []
                        (let
                            hcell =
                                styled th [ padding (px 5) ]
                         in
                         [ hcell [] [ text "Initial" ]
                         , hcell [] [ text "part 1" ]
                         , hcell [] [ text "part 2" ]
                         ]
                        )
                    , tbody []
                        (List.map inputView model.inputs)
                    ]
                ]
            ]
        ]


inputView : Int -> Html Msg
inputView i =
    let
        cell =
            styled td [ borderTop3 (px 1) solid (rgb 0 0 0), padding (px 5) ]
    in
    tr []
        [ cell [] [ text (prettyNumber i) ]
        , cell [] [ text (prettyNumber (fuel i)) ]
        , cell [] [ text (prettyNumber (extraFuel i)) ]
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


prettyNumber : Int -> String
prettyNumber num =
    String.join "," (splitThousands (String.fromInt num))


splitThousands : String -> List String
splitThousands integers =
    let
        reversedSplitThousands : String -> List String
        reversedSplitThousands value =
            if String.length value > 3 then
                value
                    |> String.dropRight 3
                    |> reversedSplitThousands
                    |> (::) (String.right 3 value)

            else
                [ value ]
    in
    integers
        |> reversedSplitThousands
        |> List.reverse
