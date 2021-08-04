module Game.Board exposing (Board, Cell, CellContent(..), createBoard)

import Game.Common exposing (Column(..), Point, Row(..))
import Game.Constants as Constants
import Game.Snake exposing (Snake)
import List.Nonempty as List1


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


cellContent : Point -> Snake -> Point -> CellContent
cellContent point snake foodPoint =
    if List1.any (\p -> p == point) snake.body then
        SnakeCell

    else if point == foodPoint then
        FoodCell

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
