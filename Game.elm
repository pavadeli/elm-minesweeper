module Game (Game, GameStatus(..), Location, CellStatus(..), Action(..), init, update) where

import Array exposing (Array)
import Maybe exposing (andThen)
import Random
import Set exposing (Set)

-- MODEL

{-| Game represents the root state of a game of Minesweeper, it contains the
field, some statistics and the status (won, game-over or still playing).
-}
type alias Game =
    { status : GameStatus
    , totalCount : Int
    , mineCount : Int
    , flagCount : Int
    , openCount : Int
    , field : Field
    }

type GameStatus
    = Playing
    | GameOver
    | GameWon

{-| A two-dimensional array of Cells.
-}
type alias Field =
    Array (Array Cell)

{-| A Cell contains all information about a single location in the field.
-}
type alias Cell =
    { mine : Bool
    , closeMines : Int
    , status : CellStatus
    }

type CellStatus
    = Closed
    | Open
    | Flagged
    | Exploded

{-| Represents the location of mines in the field as a two dimensional array.
Is used as a intermediate representation before creating the Field.
-}
type alias MineDistribution =
    Array (Array Bool)

{-| Coordinates in the Field.
-}
type alias Location =
    (Int, Int)

type alias Width = Int
type alias Height = Int
type alias Difficulty = Float
type alias Level = Int

{-| Creates a new game with given width, height and difficulty. The difficulty
should be given between 0 and 1 and represents the ratio of mines in the field.
A difficulty of 0.2 means that 1 in every 5 Cells contains a mine.

The Random.Seed is currently initialised with the level number, which makes our
game predictable and very boring, but easy to test at the moment. :-) We could
use the current time as seed, or allow the user to choose a random "level".
-}
init : Width -> Height -> Difficulty -> Level -> Game
init width height difficulty level =
    let seed = Random.initialSeed level
        mines = createMineDistribution width height difficulty seed
        createRow y = Array.indexedMap (createCell mines y)
        field = Array.indexedMap createRow mines
        mineCount = mines
            |> Array.foldl (Array.filter identity >> Array.length >> (+)) 0
    in
        Game Playing (width * height) mineCount 0 0 field


{-| Vectors to all neighboring cells.
-}
neighborVectors : List Location
neighborVectors =
    cartesianProduct [-1..1] [-1..1]
        |> List.filter ((/=) (0, 0))


{-| The coordinates of all neighbors.
-}
neighborLocs : Location -> List Location
neighborLocs =
    flip List.map neighborVectors << addTuple


{-| Creates a cell given a mine distribution and a position. To be used
with Array.indexedMap.
-}
createCell : MineDistribution -> Int -> Int -> Bool -> Cell
createCell mineDist y x mine =
    let closeMines = neighborLocs (x, y)
        |> List.filterMap (getCell mineDist)
        |> count identity
    in
        Cell mine closeMines Closed


{-| Get an element of a 2D Array.
-}
getCell : Array (Array a) -> Location -> Maybe a
getCell field (x, y) =
    Array.get y field `andThen` Array.get x


{-| Creates a pseudo-random mine distribution given the Random.Seed.
-}
createMineDistribution : Width -> Height -> Difficulty -> Random.Seed -> MineDistribution
createMineDistribution width height difficulty =
    let randomArray length generator =
            Random.list length generator |> Random.map Array.fromList
        boolGenerator = Random.map ((>) difficulty) (Random.float 0 1)
        rowGenerator = randomArray width boolGenerator
        fieldGenerator = randomArray height rowGenerator
    in
        Random.generate fieldGenerator >> fst


-- UPDATE

{-| Currently two actions are supported on a Game:
1. Try to open a Location. If it is Closed it can either open succesfully or
   explode. If it is open and has nearby closed Cells it will open those Cells
   if the number of Flags imply that it is safe to do so.
2. ToggleFlag on a Location. Will only toggle between Closed and Flagged,
   otherwise nothing happens.
-}
type Action
    = Try Location
    | ToggleFlag Location


{-| Update the game given the Action. Only Games with status Playing will be
processed.
-}
update : Action -> Game -> Game
update action game =
    case game.status of
        Playing ->
            let updatedGame =
                    case action of
                        Try loc -> ifValidLoc tryCell loc game
                        ToggleFlag loc -> ifValidLoc toggleFlag loc game
                gameWon = updatedGame.status == Playing &&
                    updatedGame.openCount + updatedGame.mineCount == updatedGame.totalCount
            in
                if gameWon then
                    { updatedGame | status = GameWon }
                else
                    updatedGame
        _ -> game


{-| Lookup the Cell belonging to the Location and run the updater iff the Cell
was found.
-}
ifValidLoc : (Location -> Cell -> Game -> Game) -> Location -> Game -> Game
ifValidLoc updater loc game =
    case getCell game.field loc of
        Just cell -> updater loc cell game
        Nothing -> game


toggleFlag : Location -> Cell -> Game -> Game
toggleFlag loc cell game =
    case cell.status of
        Closed ->
            { game
                | field = updateCellInField (setStatus Flagged) loc game.field
                , flagCount = game.flagCount + 1
            }
        Flagged ->
            { game
                | field = updateCellInField (setStatus Closed) loc game.field
                , flagCount = game.flagCount - 1
            }
        _ -> game


tryCell : Location -> Cell -> Game -> Game
tryCell loc cell game =
    case cell.status of
        -- Closed cells need to be opened.
        Closed -> openCell loc cell game
        -- Clicking on an open cell with neighboring closed cells will open
        -- the neighbors when possible.
        Open -> openNeighbors loc cell game
        _ -> game


openCell : Location -> Cell -> Game -> Game
openCell loc cell game =
    if cell.mine then
        -- BOOM!
        { game
            | field = updateCellInField (setStatus Exploded) loc game.field
            , status = GameOver
        }
    else if cell.closeMines > 0 then
        -- This is simple, just open this cell and show the number of mines that
        -- are close.
        { game
            | field = updateCellInField (setStatus Open) loc game.field
            , openCount = game.openCount + 1
        }
    else
        -- This is harder, we are in "open field" and need to open all cells we can
        -- find in this "open field".
        openAll game <| findCellsToOpen game loc


openAll : Game -> Set Location -> Game
openAll game locs =
    { game
        | field = updateCellsInField (setStatus Open) locs game.field
        , openCount = game.openCount + (Set.size locs)
    }


openNeighbors : Location -> Cell -> Game -> Game
openNeighbors loc cell =
    if cell.closeMines == 0 then identity else openNeighbors' loc cell


openNeighbors' : Location -> Cell -> Game -> Game
openNeighbors' loc cell game =
    let neighbors = neighborLocs loc
            |> lookupCells game.field
        closedCells = List.filter (snd >> .status >> (==) Closed) neighbors
        flags = count (snd >> .status >> (==) Flagged) neighbors
        explodingCells = neighbors
            |> List.filter (\(_, cell) -> cell.mine && cell.status /= Flagged)
            |> List.map fst
            |> Set.fromList
    in
        if List.length closedCells == 0 || flags /= cell.closeMines then
            -- Protection against premature clicks... if the number of flags don't
            -- match the number of mines -> do nothing.
            game
        else if Set.size explodingCells > 0 then
            { game
                | field = updateCellsInField (setStatus Exploded) explodingCells game.field
                , status = GameOver
            }
        else
            closedCells
                |> List.map fst
                |> List.concatMap (findCellsToOpen game >> Set.toList)
                |> Set.fromList
                |> openAll game


{-| Kicks-off the recursive search for the complete field starting with the
given Location.
-}
findCellsToOpen : Game -> Location -> Set Location
findCellsToOpen game loc =
    findCellsToOpen' game.field (Set.singleton loc) Set.empty


{-
I'm not so sure about this function. It works well, also with fields of size
100x100, but it is really a while loop disguised as recursion. I got the
feeling there is a more functional approach to this that doesn't impact
performance. Anyone any idea?
-}
findCellsToOpen' : Field -> Set Location -> Set Location -> Set Location
findCellsToOpen' field locsToTest locsToOpen =
    let newLocsToOpen = locsToTest
            |> Set.toList
            |> lookupCells field
            |> List.filter (snd >> .status >> (==) Closed)

        -- Using flip Set.union instead of Set.union because it turns out the
        -- performance is much better when adding a small set to a larger one
        -- than the other way around.
        updatedLocsToOpen = List.map fst newLocsToOpen
            |> Set.fromList
            |> flip Set.union locsToOpen

        updatedLocsToTest = newLocsToOpen
            |> List.filter (snd >> .closeMines >> (==) 0)
            |> List.concatMap (fst >> neighborLocs)
            |> Set.fromList
            |> flip Set.diff updatedLocsToOpen
    in
        if Set.size updatedLocsToTest == 0 then
            updatedLocsToOpen
        else
            findCellsToOpen' field updatedLocsToTest updatedLocsToOpen


updateCellInField : (Cell -> Cell) -> Location -> Field -> Field
updateCellInField cellUpdater =
    uncurry (arrayUpdate cellUpdater >> arrayUpdate)


updateCellsInField : (Cell -> Cell) -> Set Location -> Field -> Field
updateCellsInField cellUpdater locs =
    Array.indexedMap (\y ->
        Array.indexedMap (\x ->
            if Set.member (x, y) locs then cellUpdater else identity
        )
    )


-- UTILS

setStatus status record =
    { record | status = status }

addTuple (a, b) (a', b') =
    (a + a', b + b')

cartesianProduct a b =
    a |> List.map (,) |> List.concatMap (flip List.map b)

{-| Counts the number of locations in the 2D array that satisfy the given Bool
function.
-}
count : (a -> Bool) -> List a -> Int
count f =
    List.foldl (f >> (\b -> if b then 1 else 0) >> (+)) 0


lookupCells : Array (Array a) -> List Location -> List (Location, a)
lookupCells field =
    List.filterMap (\loc -> getCell field loc |> Maybe.map ((,) loc))

arrayUpdate : (a -> a) -> Int -> Array a -> Array a
arrayUpdate updater i array =
    case Array.get i array of
        Nothing -> array
        Just v -> Array.set i (updater v) array
