import Html exposing (table, tr, td, text, div, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List exposing (take, map, drop, concat, isEmpty, foldr, append, all, any, repeat, indexedMap, head)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String


main =
  StartApp.start { model = initial, view = view, update = update }


-- MODEL

type Symbol = X | O | Nothing

type alias Model = {
  currentPlayer: Symbol,
  board: List Symbol
}

initial = Model X (repeat 9 Nothing)

-- UPDATE

type Action = Play Int | PlayAgain

update action model =
  case action of
    Play position ->
      Model (nextPlayer model.currentPlayer) (putSymbol model.board model.currentPlayer position)
    PlayAgain ->
      initial

putSymbol board symbol position =
  if head (drop (position - 1) board) == Just Nothing then
    ((take (position - 1) board) ++ (symbol :: (drop (position) board)))
  else
    board


nextPlayer x =
  case x of
    X -> O
    O -> X
    _ -> Nothing

-- VIEW

view address model =
  div []
    [ drawBoard address model.board
    , text (statusText model)
    , button [onClick address PlayAgain] [text "Play again"]
    ]
  
statusText model =
  if won model.board X then
    "X won"
  else if won model.board O then
    "O won"
  else if isBoardFull model.board then
    "Draw"
  else
    (symbolToString model.currentPlayer) ++ "'s turn"


drawBoard address board =
  table [class (boardClass board)]
    (map 
      (\row -> tr [] 
        (map (\x -> drawCell address x) row)
      )
      (rows (indexedMap (,) board))
    )

drawCell address (index, symbol) =
  if (symbol == Nothing) then
    td [] [button [onClick address (Play (index + 1)) ] [text (symbolToString symbol)]]
  else
    td [] [button [] [text (symbolToString symbol)]]
  
symbolToString x =
  case x of
    X -> "X"
    O -> "O"
    Nothing -> "."
    

boardClass board =
  if isBoardFinished board then
    "finished"
  else
    ""

-- HELPERS


rows board = 
  [take 3 board, take 3 (drop 3 board), drop 6 board]
  
columns board =
  transpose (rows board)

diagonals board =
  case board of
    [d1, _, d2, _, d3, _, d4, _, d5] ->
      [[d1, d3, d5], [d2, d3, d4]]
    _ -> []

-- get a list of all lines needed to determine the winner (rows, columns, diagonals)
allLines board =
  (rows board) ++ (columns board) ++ (diagonals board)

-- determine whether a line is filled by player x
winningLine line x =
  all (\position -> position == x) line

-- determine whether board was won by player x
won board x =
  any (\line -> winningLine line x) (allLines board)

isBoardFull board =
  all (\position -> position /= Nothing) board

isBoardFinished board =
  won board X || won board O || isBoardFull board

-- transpose a list of lists
transpose list =
  case list of
    []::_ ->
      []
    list ->
      let
        heads = map (take 1) list |> concat
        tails = map (drop 1) list 
      in
        heads::(transpose tails)