module Constants exposing (boardColumns, boardRows, tickInterval)


tickPerSecond : Float
tickPerSecond =
    100


tickInterval : Float
tickInterval =
    1000 / tickPerSecond


boardRows : Int
boardRows =
    10


boardColumns : Int
boardColumns =
    15
