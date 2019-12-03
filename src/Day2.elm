module Day2 exposing (..)

import Array exposing (..)
import Browser
import Html
import Html.Styled as Styled exposing (..)
import Html.Styled.Attributes exposing (css, placeholder, type_, value)
import Html.Styled.Events exposing (..)


main =
    Browser.sandbox
        { init = init
        , view = view >> toUnstyled
        , update = update
        }


type Msg
    = Input String
    | RunCommand Command
    | Step
    | StepToFinish
    | ReplacePart1
    | ReplacePart2
    | Reset
    | ReplaceInitialWith Int Int
    | ReplaceInitialVerbNoun
    | UpdateVerb String
    | UpdateNoun String
    | WithNewNoun
    | WithNewVerb


getCommand : Model -> Command
getCommand model =
    Maybe.withDefault DefaultProvided (Array.get model.cursor model.commands)


stepCursor : Model -> Model
stepCursor model =
    { model | cursor = model.cursor + 1 }


fillWithPresets : Int -> Int -> Array Int -> Array Int
fillWithPresets noun verb memory =
    memory
        |> Array.set 1 noun
        |> Array.set 2 verb


part1Preset =
    fillWithPresets 12 2


update : Msg -> Model -> Model
update msg model =
    case msg of
        WithNewVerb ->
            update StepToFinish (update ReplaceInitialVerbNoun (update Reset { model | verb = model.verb + 1 }))

        WithNewNoun ->
            update StepToFinish (update ReplaceInitialVerbNoun (update Reset { model | noun = model.noun + 1 }))

        UpdateVerb verb ->
            { model | verb = unsafeToInt verb }

        UpdateNoun noun ->
            { model | noun = unsafeToInt noun }

        ReplaceInitialWith verb noun ->
            { model | cleaned = fillWithPresets noun verb model.cleaned }

        ReplaceInitialVerbNoun ->
            { model | cleaned = fillWithPresets model.noun model.verb model.cleaned }

        Reset ->
            update (Input model.input) { model | cursor = 0 }

        ReplacePart1 ->
            let
                newCleaned =
                    part1Preset model.cleaned
            in
            { model | cleaned = newCleaned }

        ReplacePart2 ->
            { model | cleaned = fillWithPresets model.verb model.noun model.cleaned }

        Step ->
            update (RunCommand (getCommand model)) (stepCursor model)

        StepToFinish ->
            let
                next =
                    getCommand model

                run =
                    update (RunCommand next) (stepCursor model)
            in
            case next of
                Finished ->
                    { model | success = getOrZero 0 model.cleaned == model.part2Answer }

                _ ->
                    update StepToFinish run

        Input input ->
            reCommand { model | input = input, cleaned = fromList <| read input }

        RunCommand command ->
            case command of
                Add one two dest ->
                    reCommand <| updateCleaned model (+) one two dest

                Mult one two dest ->
                    reCommand <| updateCleaned model (*) one two dest

                DefaultProvided ->
                    { model | error = "Default was provided Maybe we halted" }

                _ ->
                    { model | error = "Something bad happened" }


getOrZero : Int -> Array Int -> Int
getOrZero position collection =
    Maybe.withDefault 0 (Array.get position collection)


updateCleaned : Model -> (Int -> Int -> Int) -> Int -> Int -> Int -> Model
updateCleaned model op x y dest =
    let
        one =
            getOrZero x model.cleaned

        two =
            getOrZero y model.cleaned

        newCleaned =
            Array.set dest (op one two) model.cleaned
    in
    { model | cleaned = newCleaned }


reCommand : Model -> Model
reCommand model =
    { model | commands = fromList <| makeCommands <| toList model.cleaned }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "New Html Program" ]
        , h2 [] [ text model.error ]
        , h2 []
            [ text
                (if model.success then
                    "Success " ++ String.fromInt (100 * model.noun + model.verb)

                 else
                    "Not yet"
                )
            ]
        , h2 [] [ text ("Distance" ++ String.fromInt (getOrZero 0 model.cleaned - model.part2Answer)) ]
        , h3 []
            [ itext model.cursor
            , text ("(" ++ String.fromInt (getOrZero (model.cursor * 4) model.cleaned) ++ ")")
            , button [ onClick Step ] [ text "Step" ]
            , button [ onClick StepToFinish ] [ text "Finish" ]
            , button [ onClick ReplacePart2 ] [ text "Replace Part 2 values" ]
            , button [ onClick ReplacePart1 ] [ text "Replace part1 values" ]
            , button [ onClick Reset ] [ text "Reset" ]
            ]
        , div []
            [ button [ onClick WithNewVerb ] [ text "Go with New Verb" ]
            , button [ onClick WithNewNoun ] [ text "Go with New Noun" ]
            ]
        , div []
            [ input [ type_ "number", placeholder "verb", value (String.fromInt model.verb), onInput UpdateVerb ] []
            , input [ type_ "number", placeholder "noun", value (String.fromInt model.noun), onInput UpdateNoun ] []
            ]
        , textarea
            [ value model.input
            , placeholder "input"
            , onInput Input
            ]
            []
        , div [] [ text model.input ]
        , hr [] []
        , div [] [ text (String.join "--" (Array.toList <| Array.map String.fromInt model.cleaned)) ]
        , hr [] []
        , table []
            [ thead []
                [ th [] [ text "Command" ]
                , th [] [ text "One" ]
                , th [] [ text "Two" ]
                , th [] [ text "Destination" ]
                ]
            , tbody [] (Array.toList <| Array.map viewCommand model.commands)
            ]
        ]


viewCommand : Command -> Html Msg
viewCommand command =
    case command of
        Finished ->
            row "Finished" 0 0 0

        InvalidCommand num ->
            row "Error" num 99 99

        Empty ->
            row "Empty" 50 50 50

        thing ->
            runnableRow thing


itext : Int -> Html Msg
itext int =
    text (String.fromInt int)


runnableRow : Command -> Html Msg
runnableRow command =
    case command of
        Add one two three ->
            tr []
                [ td [] [ text "Add" ]
                , td [] [ itext one ]
                , td [] [ itext two ]
                , td [] [ itext three ]
                , td [] [ button [ onClick (RunCommand command) ] [ text "Add" ] ]
                ]

        Mult one two three ->
            tr []
                [ td [] [ text "Mult" ]
                , td [] [ itext one ]
                , td [] [ itext two ]
                , td [] [ itext three ]
                , td [] [ button [ onClick (RunCommand command) ] [ text "Mult" ] ]
                ]

        _ ->
            tr [] [ td [] [ text "Invalid" ] ]


row : String -> Int -> Int -> Int -> Html Msg
row text_ one two dest =
    tr []
        [ td [] [ text text_ ]
        , td [] [ text (String.fromInt one) ]
        , td [] [ text (String.fromInt two) ]
        , td [] [ text (String.fromInt dest) ]
        ]


type Command
    = Add Int Int Int
    | Mult Int Int Int
    | Finished
    | InvalidCommand Int
    | Empty
    | DefaultProvided


type alias Model =
    { input : String
    , cleaned : Array Int
    , commands : Array Command
    , cursor : Int
    , error : String
    , verb : Int
    , noun : Int
    , part2Answer : Int
    , success : Bool
    }


temp =
    "1,9,10,3,2,3,11,0,99,30,40,50"


init : Model
init =
    { input = temp
    , cleaned = fromList <| read temp
    , commands = fromList <| makeCommands <| read temp
    , cursor = 0
    , error = ""
    , verb = 0
    , noun = 0
    , part2Answer = 19690720
    , success = False
    }


unsafeToInt : String -> Int
unsafeToInt s =
    Maybe.withDefault 0 (String.toInt s)


read : String -> List Int
read input =
    String.split "," input
        |> List.map String.toInt
        |> List.map (Maybe.withDefault 0)


makeCommands : List Int -> List Command
makeCommands list =
    let
        four =
            List.take 4 list

        rest =
            List.drop 4 list
    in
    if List.length rest > 3 then
        makeCommand four :: makeCommands rest

    else
        [ makeCommand four ]


makeCommand : List Int -> Command
makeCommand list =
    case list of
        [] ->
            Empty

        [ 1, one, two, three ] ->
            Add one two three

        [ 2, one, two, three ] ->
            Mult one two three

        [ 99, _, _, _ ] ->
            Finished

        num :: xs ->
            InvalidCommand num
