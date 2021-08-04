module Main exposing (main)

import Browser
import Browser.Events as BrowserEvents
import Html exposing (Html, table, td, text, tr, div, h2, h3, span)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode
import Constants
import List
import List.Nonempty as List1
import Time
import Random



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

type alias SnakeBody = List1.Nonempty Point
type alias Snake =
    { body : SnakeBody
    , direction : SnakeDirection
    }

snakeHead : Snake -> Point
snakeHead snake = List1.head snake.body

snakeTail : Snake -> List Point
snakeTail snake = List1.tail snake.body

type alias Model =
    { currentLevel : Int
    , board : Board
    , snake : Snake
    , food : Point
    , collision : Maybe Point
    }


initialSnake : Snake
initialSnake =
    Snake (List1.cons ( Col 0, Row 0 ) (List1.singleton ( Col 1, Row 0 ))) Right

initialFood : Point
initialFood = ( Col <| Constants.boardColumns // 2, Row <| Constants.boardRows // 2 )

init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentLevel = 0
      , board = createBoard initialSnake initialFood
      , snake = initialSnake
      , food = initialFood
      , collision = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick
    | ChangeDirection SnakeDirection
    | SpawnFood Point
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
                |> List1.replaceTail newTail
                --|> List1.replaceTail (List.take (List.length tail) newTail)
    }

findSnakeCollision : SnakeBody -> Maybe Point
findSnakeCollision body = 
    let
        head = List1.head body
        tail = List1.tail body
        matches 
            = List.map (\point -> point == head) tail
            |> List.filter ((==) True) 
    in
        Maybe.map (\_ -> head) (List.head matches)

findBorderCollision : Point -> Maybe Point
findBorderCollision ((Col x, Row y) as head) = 
    if x < 0 || y < 0 || x > Constants.boardColumns || y > Constants.boardRows then Just head else Nothing

foodGenerator : Random.Generator Point
foodGenerator =
    Random.pair 
        (Random.int 0 ( Constants.boardColumns - 1 )) 
        (Random.int 0 ( Constants.boardRows - 1 ))
    |> Random.map (\(x,y)-> (Col x, Row y))


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

        SpawnFood point ->
            ( {model | food = point}, Cmd.none )
        Tick ->
            let
                movingSnake : Snake
                movingSnake =
                    moveSnake model.snake

                isFoodEaten = snakeHead movingSnake == model.food
                level = if isFoodEaten then model.currentLevel + 1 else model.currentLevel

                snake : Snake
                snake = 
                    if isFoodEaten 
                    then movingSnake 
                    else { movingSnake | body = movingSnake.body |> List1.replaceTail ((List.take (List.length (snakeTail model.snake)) (snakeTail movingSnake)) ) }

                board =
                    createBoard snake model.food
                
                snakeCollision = findSnakeCollision snake.body
                borderCollision = findBorderCollision <| List1.head snake.body
                collision = List.filterMap identity [snakeCollision, borderCollision] |> List.head


            in
            ( { model
                | snake = snake
                , board = board
                , currentLevel = level
                , collision = collision
              }
            , if isFoodEaten then Random.generate SpawnFood foodGenerator else Cmd.none
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
tickInterval currentLevel = toFloat <| max 100 (1000 - currentLevel * 50)

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.collision of
       Nothing -> Sub.batch [ Time.every (tickInterval model.currentLevel) (\_ -> Tick) , BrowserEvents.onKeyDown keyToSnakeDirectionDecoder]
       Just _ -> Sub.none

-- VIEW


createCellView : Cell -> Html Msg
createCellView cell =
        case cell.content of
                EmptyCell ->
                    div [ class "cell cell-empty" ] [  ]

                SnakeCell ->
                    div [ class "cell cell-snake" ] [  ]
                
                FoodCell ->
                    div [ class "cell cell-food" ] [ text "⌗" ]


createRowView : List Cell -> Html Msg
createRowView cells =
    div [class "grid-row"] (List.map createCellView cells)


hudSpeed : Int -> Html Msg
hudSpeed currentLevel =
    div [] [text <| "Speed: " ++ String.padLeft 4 '0' (String.fromFloat (1100.0 - (tickInterval currentLevel)))]

hudPoints : Int -> Html Msg
hudPoints currentLevel 
    = div [] [ text <| "Score: " ++ String.padLeft 4 '0' (String.fromInt (currentLevel*10))]

hud : Model -> Html Msg
hud model =
    div [class "hud"] 
                 [ hudPoints model.currentLevel
                 , hudSpeed model.currentLevel
                 ]

gameOver : Maybe Point -> Html Msg
gameOver collision = 
            (case collision of
                Nothing -> div [style "display" "none"] []
                Just _ -> div [class "game-over"] 
                              [ text "Game Over"]
                              
            )
    
        

view : Model -> Html Msg
view model =
    div [class "center-game"] [
        div [class "game-container"] 
            [ hud model
            , div [] (List.map createRowView model.board)
            ]
        , gameOver model.collision
    ]
