module Common exposing (..)

{-| This module contains code shared in Settings, Main and Game.

It's a good place to store the really basic types and functions.

Importantly, if you have a type/function that's used in both Settings.elm and Game.elm,
I strongly suggest putting it here (to avoid circular dependencies).

You can delete any of the functions currently defined here - I just thought
they'd be useful for a lot of different types of games. 

-}

--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------


{-| Basic type representation for a two player game.
-}
type Player
    = Player1
    | Player2


{-| The game either ends up with a winner or as a draw.
-}
type Outcome
    = Winner Player
    | Draw


{-| A game is either in progress of complete.
-}
type Status
    = Playing
    | Complete Outcome

{-| A coordinate on the board. -}
type alias Coord = 
    { x : Float
    , y : Float }

{-| The size of a board -}
type alias BoardSize = Float

{-| An integer coordinate on a heatmap (top left corner). -}
type alias Square = 
    { x : Int 
    , y : Int }

type alias HeatmapSize = Int


--------------------------------------------------------------------------------
-- CONVENIENCE FUNCTIONS
--------------------------------------------------------------------------------


{-| A convenience function for the opposite player.
-}
opponent : Player -> Player
opponent player =
    case player of
        Player1 ->
            Player2

        Player2 ->
            Player1

{-| Convert a square to an index given a heatmap size. -}
squareToIndex : HeatmapSize -> Square -> Int
squareToIndex heatmapSize coord =
    coord.x + coord.y * heatmapSize

{-| Convert an index to a square given a heatmap size. -}
indexToCoord : HeatmapSize -> Int -> Square
indexToCoord heatmapSize index =
    { x = index |> modBy heatmapSize
    , y = index // heatmapSize
    }

{-| Find the euclidean distance between two coords -}
distance : Coord -> Coord -> Float
distance a b = 
    let
        dx = a.x - b.x
        dy = a.y - b.y
    in
        sqrt (dx * dx + dy * dy)

strictWithin : BoardSize -> Coord -> Bool 
strictWithin boardSize coord = 
    coord.x > 0 && coord.x < boardSize && coord.y > 0 && coord.y < boardSize
