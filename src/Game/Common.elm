module Game.Common exposing (Column(..), Point, Row(..))


type Column
    = Col Int


type Row
    = Row Int


type alias Point =
    ( Column, Row )
