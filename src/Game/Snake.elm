module Game.Snake exposing (Snake, SnakeBody, SnakeDirection(..), snakeHead, snakeTail, moveSnake)

import Game.Common exposing (Point, Column(..), Row(..))
import List.Nonempty as List1


type SnakeDirection
    = Up
    | Down
    | Left
    | Right


type alias SnakeBody =
    List1.Nonempty Point


type alias Snake =
    { body : SnakeBody
    , direction : SnakeDirection
    }


snakeHead : Snake -> Point
snakeHead snake =
    List1.head snake.body


snakeTail : Snake -> List Point
snakeTail snake =
    List1.tail snake.body

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
    }