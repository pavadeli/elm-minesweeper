import Game exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Array
import Json.Decode exposing (bool, (:=))


input = Signal.mailbox (Game.Try (0,0))
-- game = init 100 100 0.15 0
game = init 20 20 0.15 0
main = Signal.map view (Signal.foldp update game input.signal)

view game =
    div []
    [ viewHeader game
    , viewField game.field
    ]

viewHeader game =
    div []
    [ h1 [] [text "Minesweeper"]
    , viewStatus game.status
    , p [] <| (mineCount game.mineCount) ++ [text ", "] ++ (flagCount game.flagCount)
    ]

viewStatus status =
    case status of
        Playing -> p [] [text "Click to open a cell, shift-click to toggle a flag."]
        GameOver -> h2 [] [text "Game over!"]
        GameWon -> h2 [] [text "You won!"]

mineCount mines =
    [text "Number of mines: ", boldNumber mines]

flagCount flags =
    case flags of
        0 -> [text "no flags placed"]
        _ -> [text "flags: ", boldNumber flags]

boldNumber number =
    span [style [("font-weight", "bold")]] [text <| toString number]

viewField field =
    field
        |> Array.indexedMap (lazy2 viewRow)
        |> Array.toList
        |> table [ style tableStyle ]

viewRow y row =
    row
        |> Array.indexedMap (\x cell -> lazy2 viewCell (x, y) cell)
        |> Array.toList
        |> tr []

viewCell loc cell =
    let handleClick shiftKeyPressed =
            if shiftKeyPressed then
                Signal.message input.address (Game.ToggleFlag loc)
            else
                Signal.message input.address (Game.Try loc)
    in
        td [ on "click" ("shiftKey" := bool) handleClick, style <| cellStyle cell ]
        [ text <| case cell.status of
            Closed -> ""
            -- Closed -> if cell.mine then "*" else ""
            Open -> if cell.closeMines == 0 then "" else toString cell.closeMines
            Exploded -> "E"
            Flagged -> "F"
        ]


-- STYLES

tableStyle =
    [ ("border", "solid black 3px")
    ]

cellStyle cell =
    let
        background =
            case cell.status of
                Closed -> [ ("background", "silver") ]
                Exploded -> [ ("background", "red") ]
                Flagged -> [ ("background", "green") ]
                _ -> []
        text =
            case cell.status of
                Open -> [ ("color", textcolor cell) ]
                _ -> []
    in
        baseCellStyle ++ background ++ text

baseCellStyle =
    [ ("border", "solid black 1px")
    , ("width", "20px")
    , ("height", "20px")
    , ("text-align", "center")
    , ("vertical-align", "middle")
    , ("font-weight", "bold")
    ]

textcolor cell =
    case cell.closeMines of
        1 -> "blue"
        2 -> "green"
        3 -> "red"
        _ -> "black"
