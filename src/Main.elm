module Main exposing (main)

import Browser
import Browser.Events as BrowserEvents
import Constants
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

cellContent : Point -> Snake -> Point -> CellContent
cellContent point snake foodPoint =
    if List1.any (\p -> p == point) snake.body then
        SnakeCell

    else 
    if point == foodPoint then FoodCell
    else 
        EmptyCell


createRow : Int -> Int -> Snake -> Point -> List Cell
createRow rowIndex nColumns snake foodPoint =
    List.repeat nColumns (Cell ( Col 0, Row rowIndex ) EmptyCell)
        |> List.indexedMap
            (\columnIndex cell ->
                { cell
                    | position = ( Col columnIndex, Row rowIndex )
                    , content = cellContent ( Col columnIndex, Row rowIndex ) snake foodPoint
                }
            )


createBoard : Snake -> Point -> Board
createBoard snake foodPoint =
    List.repeat Constants.boardRows []
        |> List.indexedMap (\rowIndex _ -> createRow rowIndex Constants.boardColumns snake foodPoint)


col : Column -> Int
col (Col n) = n
row : Row -> Int
row (Row n) = n
type Column = Col Int
type Row = Row Int
type alias Point = ( Column, Row )


type CellContent
    = EmptyCell
    | SnakeCell
    | FoodCell


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
    , direction : SnakeDirection
    }

type alias Model =
    { currentLevel : Int
    , board : Board
    , snake : Snake
    , food : Point
    }


initialSnake : Snake
initialSnake =
    Snake (List1.cons ( Col 0, Row 0 ) (List1.singleton ( Col 1, Row 0 ))) Right

initialFoodPoint : Point
initialFoodPoint = ( Col <| Constants.boardColumns // 2, Row <| Constants.boardRows // 2 )

init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentLevel = 0
      , board = createBoard initialSnake initialFoodPoint
      , snake = initialSnake
      , food = initialFoodPoint
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick
    | ChangeDirection SnakeDirection
    | Noop


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
                    Tuple.mapSecond (\(Row y) -> Row (y - 1)) head

                Down ->
                    Tuple.mapSecond (\(Row y) -> Row (y + 1)) head

                Left ->
                    Tuple.mapFirst (\(Col x) -> Col (x - 1)) head

                Right ->
                    Tuple.mapFirst (\(Col x) -> Col (x + 1)) head

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
                snake =
                    moveSnake model.snake

                board =
                    createBoard snake model.food
            in
            ( { model
                | snake = snake
                , board = board
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


toChangeDirectionDirection : String -> Msg
toChangeDirectionDirection string =
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
    Decode.map toChangeDirectionDirection (Decode.field "key" Decode.string)


tickInterval : Int -> Float
tickInterval currentLevel = toFloat <| 1000 - currentLevel * 10

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (tickInterval model.currentLevel) (\_ -> Tick)
        , BrowserEvents.onKeyDown keyToSnakeDirectionDecoder
        ]

-- VIEW


createCellView : Cell -> Html Msg
createCellView cell =
    let
        cellText =
            (String.fromInt <| col <| Tuple.first cell.position)
                ++ ", "
                ++ (String.fromInt <| row <| Tuple.second cell.position)
    in
    case cell.content of
        EmptyCell ->
            td [ class "cell cell-empty" ] [ text cellText ]

        SnakeCell ->
            td [ class "cell cell-snake" ] [ text cellText ]
        
        FoodCell ->
            td [ class "cell cell-food" ] [ text cellText ]


createRowView : List Cell -> Html Msg
createRowView cells =
    tr [] (List.map createCellView cells)


view : Model -> Html Msg
view model =
    table [] (List.map createRowView model.board)
