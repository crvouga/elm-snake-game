module Main exposing (main)

import Browser
import Browser.Events as BrowserEvents
import Html exposing (Html, table, td, text, tr)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import List exposing (length)
import List.Nonempty as List1
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


currentLevel : Int
currentLevel =
    0


tickPerSecond : Float
tickPerSecond =
    100


cellContent : Point -> Snake -> CellContent
cellContent point snake =
    if List1.any (\p -> p == point) snake.body then
        SnakeCell

    else
        EmptyCell


createRow : Int -> Int -> Snake -> List Cell
createRow rowIndex nCols snake =
    List.repeat nCols (Cell ( 0, rowIndex ) EmptyCell)
        |> List.indexedMap
            (\colIndex cell ->
                { cell
                    | position = ( colIndex, rowIndex )
                    , content = cellContent ( colIndex, rowIndex ) snake
                }
            )


boardRows : Int
boardRows =
    10


boardColumns : Int
boardColumns =
    15


createBoard : Snake -> Board
createBoard snake =
    List.repeat boardRows []
        |> List.indexedMap (\rowIndex _ -> createRow rowIndex boardColumns snake)


type alias Point =
    ( Int, Int )



-- (x, y) or (column, row)
-- (x, y)


type CellContent
    = EmptyCell
    | SnakeCell


type alias Cell =
    { position : Point
    , content : CellContent
    }


type alias Board =
    List (List Cell)


type SnakeDirection
    = Up
    | Down
    | Left
    | Right


type alias Snake =
    { body : List1.Nonempty Point
    , collision : Maybe Point
    , direction : SnakeDirection
    }


type alias Model =
    { ticks : Int -- 100 ticks per second
    , moveInterval : Int -- how many ticks to move a single step
    , steps : Int -- steps made so far
    , board : Board
    , snake : Snake
    }


initialSnake : Snake
initialSnake =
    Snake (List1.cons ( 0, 0 ) (List1.singleton ( 1, 0 ))) Nothing Right


init : () -> ( Model, Cmd Msg )
init _ =
    ( { ticks = 0
      , moveInterval = 100 - currentLevel
      , steps = 0
      , board = createBoard initialSnake
      , snake = initialSnake
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick
    | ChangeDirection SnakeDirection
    | Noop


incrementSteps : Int -> Int -> Int -> Int
incrementSteps steps moveInterval ticks =
    if modBy moveInterval ticks == 0 then
        steps + 1

    else
        steps


moveSnake : Snake -> Snake
moveSnake snake =
    let
        head : Point
        head =
            List1.head snake.body

        newHead : Point
        newHead =
            case snake.direction of
                Up ->
                    Tuple.mapSecond (\y -> y - 1) head

                Down ->
                    Tuple.mapSecond (\y -> y + 1) head

                Left ->
                    Tuple.mapFirst (\x -> x - 1) head

                Right ->
                    Tuple.mapFirst (\x -> x + 1) head

        tail : List Point
        tail =
            List1.tail snake.body

        newTail : List Point
        newTail =
            head :: tail
    in
    { snake
        | body =
            snake.body
                |> List1.replaceHead newHead
                |> List1.replaceTail (List.take (length tail) newTail)
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        ChangeDirection direction ->
            let
                snake =
                    model.snake

                newSnake =
                    { snake | direction = direction }
            in
            ( { model | snake = newSnake }, Cmd.none )

        Tick ->
            let
                ticks =
                    model.ticks + 1

                steps =
                    incrementSteps model.steps model.moveInterval ticks

                snake =
                    if steps > model.steps then
                        moveSnake model.snake

                    else
                        model.snake

                board =
                    createBoard snake
            in
            ( { model
                | ticks = ticks
                , steps = steps
                , snake = snake
                , board = board
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


tickInterval : Float
tickInterval =
    1000 / tickPerSecond


toDirection : String -> Msg
toDirection string =
    case string of
        "ArrowLeft" ->
            ChangeDirection Left

        "ArrowRight" ->
            ChangeDirection Right

        "ArrowUp" ->
            ChangeDirection Up

        "ArrowDown" ->
            ChangeDirection Down

        _ ->
            Noop


keyToSnakeDirectionDecoder : Decode.Decoder Msg
keyToSnakeDirectionDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every tickInterval (\_ -> Tick)
        , BrowserEvents.onKeyDown keyToSnakeDirectionDecoder
        ]



-- VIEW


createCellView : Cell -> Html Msg
createCellView cell =
    let
        cellText =
            (String.fromInt <| Tuple.first cell.position)
                ++ ", "
                ++ (String.fromInt <| Tuple.second cell.position)
    in
    case cell.content of
        EmptyCell ->
            td [ class "cell cell-empty" ] [ text cellText ]

        SnakeCell ->
            td [ class "cell cell-snake" ] [ text cellText ]


createRowView : List Cell -> Html Msg
createRowView cells =
    tr [] (List.map createCellView cells)


view : Model -> Html Msg
view model =
    table [] (List.map createRowView model.board)
