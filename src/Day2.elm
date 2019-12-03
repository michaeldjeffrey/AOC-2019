module Day2 exposing (..)

import Array exposing (..)
import Browser
import Html
import Html.Styled as Styled exposing (..)
import Html.Styled.Attributes exposing (css, placeholder, value)
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
    | ReplaceInitial


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
        ReplaceInitial ->
            let
                newCleaned =
                    part1Preset model.cleaned
            in
            { model | cleaned = newCleaned }

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
                    model

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
        , h3 []
            [ itext model.cursor
            , text ("(" ++ String.fromInt (getOrZero (model.cursor * 4) model.cleaned) ++ ")")
            , button [ onClick Step ] [ text "Step" ]
            , button [ onClick StepToFinish ] [ text "Finish" ]
            , button [ onClick ReplaceInitial ] [ text "Replace initial values" ]
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
    }


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
