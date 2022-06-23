module Main exposing (main)

import Browser
import Browser.Events as BrowserEvents
import Debug
import Game.Board as Board exposing (Board, Cell, CellContent(..))
import Game.Common exposing (Column(..), Point, Row(..))
import Game.Constants as Constants
import Game.Snake as Snake exposing (Snake, SnakeBody, SnakeDirection(..))
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode
import List
import List.Nonempty as List1
import Random
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
initialFood =
    ( Col <| Constants.boardColumns // 2, Row <| Constants.boardRows // 2 )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentLevel = 0
      , board = Board.createBoard initialSnake initialFood
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


findSnakeCollision : SnakeBody -> Maybe Point
findSnakeCollision body =
    let
        head =
            List1.head body

        tail =
            List1.tail body

        matches =
            List.map (\point -> point == head) tail
                |> List.filter ((==) True)
    in
    Maybe.map (\_ -> head) (List.head matches)


findBorderCollision : Point -> Maybe Point
findBorderCollision (( Col x, Row y ) as head) =
    if x < 0 || y < 0 || x > Constants.boardColumns || y > Constants.boardRows then
        Just head

    else
        Nothing


foodGenerator : Random.Generator Point
foodGenerator =
    Random.pair
        (Random.int 0 (Constants.boardColumns - 1))
        (Random.int 0 (Constants.boardRows - 1))
        |> Random.map (\( x, y ) -> ( Col x, Row y ))


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
            ( { model | food = point }, Cmd.none )

        Tick ->
            let
                movingSnake : Snake
                movingSnake =
                    Snake.moveSnake model.snake

                isFoodEaten =
                    Snake.snakeHead movingSnake == model.food

                level =
                    if isFoodEaten then
                        model.currentLevel + 1

                    else
                        model.currentLevel

                snake : Snake
                snake =
                    if isFoodEaten then
                        movingSnake

                    else
                        { movingSnake | body = movingSnake.body |> List1.replaceTail (List.take (List.length (Snake.snakeTail model.snake)) (Snake.snakeTail movingSnake)) }

                board =
                    Board.createBoard snake model.food

                snakeCollision =
                    findSnakeCollision snake.body

                borderCollision =
                    findBorderCollision <| List1.head snake.body

                collision =
                    List.filterMap identity [ snakeCollision, borderCollision ] |> List.head
            in
            ( { model
                | snake = snake
                , board = board
                , currentLevel = level
                , collision = collision
              }
            , if isFoodEaten then
                Random.generate SpawnFood foodGenerator

              else
                Cmd.none
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
tickInterval currentLevel =
    toFloat <| max 100 (1000 - currentLevel * 50)


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        _ =
            Debug.log "model" {}
    in
    case model.collision of
        Nothing ->
            Sub.batch [ Time.every (tickInterval model.currentLevel) (\_ -> Tick), BrowserEvents.onKeyDown keyToSnakeDirectionDecoder ]

        Just _ ->
            Sub.none



-- VIEW


createCellView : Cell -> Html Msg
createCellView cell =
    case cell.content of
        EmptyCell ->
            div [ class "cell cell-empty" ] []

        SnakeCell ->
            div [ class "cell cell-snake" ] []

        FoodCell ->
            div [ class "cell cell-food" ] [ text "⌗" ]


createRowView : List Cell -> Html Msg
createRowView cells =
    div [ class "grid-row" ] (List.map createCellView cells)


hudSpeed : Int -> Html Msg
hudSpeed currentLevel =
    div [] [ text <| "Speed: " ++ String.padLeft 4 '0' (String.fromFloat (1100.0 - tickInterval currentLevel)) ]


hudPoints : Int -> Html Msg
hudPoints currentLevel =
    div [] [ text <| "Score: " ++ String.padLeft 4 '0' (String.fromInt (currentLevel * 10)) ]


hud : Model -> Html Msg
hud model =
    div [ class "hud" ]
        [ hudPoints model.currentLevel
        , hudSpeed model.currentLevel
        ]


gameOver : Maybe Point -> Html Msg
gameOver collision =
    case collision of
        Nothing ->
            div [ style "display" "none" ] []

        Just _ ->
            div [ class "game-over" ]
                [ text "Game Over" ]


view : Model -> Html Msg
view model =
    div [ class "center-game" ]
        [ div [ class "game-container" ]
            [ hud model
            , div [] (List.map createRowView model.board)
            ]
        , gameOver model.collision
        ]
