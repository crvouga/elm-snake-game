module Constants exposing (tickPerSecond, boardColumns, boardRows, tickInterval)


tickPerSecond : Int
tickPerSecond =
    10


tickInterval : Float
tickInterval =
    1000 / toFloat tickPerSecond


boardRows : Int
boardRows =
    10


boardColumns : Int
boardColumns =
    15
