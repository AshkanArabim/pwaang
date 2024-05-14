module Main (main) where

import Lib

-- PWANG :: Professor Ward's Abolutely Amazing New Game,  Spring 2024

import System.Random

-----------  Data Type Definitions ------------


data Bean = Pinto | White       --  like enumerate

next :: Bean -> Bean
next Pinto = White
next White = Pinto

instance Show Bean where
    show Pinto = "P"
    show White = "W"

instance Eq Bean where 
    x == y = show x == show y

data Result = NoBean | OutOfBounds | Valid | Win

instance Show Result where
    show NoBean = "you have no bean there"
    show OutOfBounds = "you moved out of bounds"
    show Valid = "you made a valid move"
    show Win = "you win!"

instance Eq Result where
    x == y = show x == show y

----------------  Game Logic: Inventory of the Possible Moves ----------------

-- the dice roles range from 1 to 8, naming the directions counterclockwise from 1 for East
nameOfMove :: Int -> String
nameOfMove 1 = "east"
nameOfMove 2 = "northeast"
nameOfMove 3 = "north"
nameOfMove 4 = "northwest"
nameOfMove 5 = "west"
nameOfMove 6 = "southwest"
nameOfMove 7 = "south"
nameOfMove 8 = "southeast"

-- xDelta :: Integer -> Integer
-- yDelta :: Integer -> Integer

----------  Boards for Testing  ------------------

-- for I/O purposes cells are numbered like this
---  6 7 8 
---  3 4 5
---  0 1 2

emptyBoard = []
simpleBoard = [(1,Pinto), (7,White)]   
classicInitialBoard = [(0, Pinto), (1, Pinto), (2, Pinto), (6, White), (7, White), (8, White)]
meleeBoard = [(0, Pinto), (1, Pinto), (3, White), (5, White), (7, White), (8, Pinto)]
winnableBoard1 = [(1, White), (4, Pinto)]
winnableBoard2 = [(0, Pinto), (1, White)]
winnableBoard3 = [(0, Pinto), (4, White)]


-----------  ShowBoard and its Helper Functions -----------

contains :: [(Integer, Bean)] -> Integer -> Bean ->  Bool
contains [] cell bean = False
contains (x:xs) cell bean = (cell == (fst x)) && (bean == (snd x)) || contains xs cell bean

-- TODO: what the hell is wrong with this? vv
-- at :: [(Integer, Bean)] -> Integer -> Bean ->  String
at board cell
    | contains board cell Pinto     = "p"
    | contains board cell White   = "w"
    | otherwise                               = "-"

-- TODO: why am I not using this?
showBoard :: [(Integer, Bean)] -> IO ()
showBoard b =
    putStr("   " ++ (at b 6) ++ (at b 7) ++ (at b 8) ++ "\n   " ++ (at b 3) ++ (at b 4) ++ (at b 5) ++ "\n   " ++ (at b 0) ++ (at b 1) ++ (at b 2) ++ "\n")

--  elegant version with fold, map and lambda
-- TODO: why am I not using this?
rowString ::  [(Integer, Bean)] ->  [Integer]-> String
rowString board cells =  (foldl (++)   " " (map (\pos -> at board pos)  cells)) ++ "\n"

showBoard2  :: [(Integer, Bean)] -> IO ()
showBoard2 board =  putStr(foldr (++) "" (map (\row ->  rowString board [row*3..row*3+2])  [0..2]))


--------- Other Bits of Code ---------

-- return something in range 1 to 8 that depends on the move number but looks randomish
-- TODO: does Haskell have true randoms?
randomLike moveNumber = 1 + mod (floor (moveNumber * 2.5))  8

-- -- helper function, enables, e.g.,  search 3 [ (1,2), (2,4), (3,9)]  to get 9 
-- -- ques: what the hell is this for???
-- search ::  Integer -> [(Integer, Integer)] -> Integer
-- search key [] = 0
-- search key ((k,v) : xs)  = if key == k
--     then v
--     else search key xs 


----------------- main function stub & game logic ------------------------------

-- part a: -------------------------------------------------------------
getDeltaX :: Integer -> Integer
getDeltaX 1 = 1
getDeltaX 2 = 1
getDeltaX 3 = 0
getDeltaX 4 = -1
getDeltaX 5 = -1
getDeltaX 6 = -1
getDeltaX 7 = 0
getDeltaX 8 = 1

getDeltaY :: Integer -> Integer
getDeltaY 1 = 0
getDeltaY 2 = 1
getDeltaY 3 = 1
getDeltaY 4 = 1
getDeltaY 5 = 0
getDeltaY 6 = -1
getDeltaY 7 = -1
getDeltaY 8 = -1

getDeltas :: Integer -> (Integer, Integer)
getDeltas n = (getDeltaX n, getDeltaY n)

-- part b: -------------------------------------------------------------
-- ALREADY EXISTS! See the "contains" function

-- part c: -------------------------------------------------------------
-- if the opponent doesn't have any beans, current player has won
isWin :: [(Integer, Bean)] -> Bean -> Bool
isWin board bean = foldr (&&) True [not (contains board n (next bean)) | n <- [0..8]]

-- part d: -------------------------------------------------------------
-- axes are: horizontal (starting from left) and vertical (starting from bottom) respectively
cellToCoords :: Integer -> (Integer, Integer)
cellToCoords 0 = (0, 0)
cellToCoords 1 = (1, 0)
cellToCoords 2 = (2, 0)
cellToCoords 3 = (0, 1)
cellToCoords 4 = (1, 1)
cellToCoords 5 = (2, 1)
cellToCoords 6 = (0, 2)
cellToCoords 7 = (1, 2)
cellToCoords 8 = (2, 2)

-- inverse of the previous function
coordsToCell :: (Integer, Integer) -> Integer
coordsToCell (0, 0) = 0
coordsToCell (1, 0) = 1
coordsToCell (2, 0) = 2
coordsToCell (0, 1) = 3
coordsToCell (1, 1) = 4
coordsToCell (2, 1) = 5
coordsToCell (0, 2) = 6
coordsToCell (1, 2) = 7
coordsToCell (2, 2) = 8

addTuples :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
addTuples (a, b) (c, d) = (a + c, b + d)

-- takes coords, applies the direction given by the die, return new coords
getDestCoords :: (Integer, Integer) -> Integer -> (Integer, Integer)
getDestCoords oldCoords die = addTuples (getDeltas die) oldCoords

getDestCell :: Integer -> Integer -> Integer
getDestCell oldCell die = coordsToCell (getDestCoords (cellToCoords oldCell) die)

-- part e: -------------------------------------------------------------
coordsOutOfBounds :: (Integer, Integer) -> Bool
coordsOutOfBounds (a, b) = a < 0 || b < 0 || a > 2 || b > 2

moveOutOfBounds :: Integer -> Integer -> Bool
moveOutOfBounds cellIdx die = coordsOutOfBounds (getDestCoords (cellToCoords cellIdx) die)

-- part f: -------------------------------------------------------------

updateBoard :: [(Integer, Bean)] -> Integer -> Integer -> [(Integer, Bean)]
updateBoard [] _ _ = []
updateBoard ((cell, bean) : xs) oldCell die =
    let newCell = getDestCell oldCell die
    in if cell == oldCell 
        then (newCell, bean):updateBoard xs oldCell die
        else if cell == newCell
            then updateBoard xs oldCell die
            else (cell,bean):updateBoard xs oldCell die

-- main func: ----------------------------------------------------------
handleMove :: [(Integer, Bean)] -> Bean -> Integer -> Integer -> Result
handleMove board bean cell die
    | not (contains board cell bean) = NoBean
    | moveOutOfBounds cell die = OutOfBounds
    | isWin (updateBoard board cell die) bean = Win
    | otherwise = Valid

----------------- test cases  ----------------------------
--- to run:   ghci     > :load pwaang       >  testResults

--- 30 test cases
testResults = [
    NoBean == handleMove  emptyBoard Pinto 3 3,           -- empty: 1
    NoBean == handleMove classicInitialBoard White 4 8,   -- nothing there: 2
    NoBean == handleMove classicInitialBoard White 2 8,   -- pinto is there: 3
    NoBean == handleMove simpleBoard Pinto 2 4,      -- nothing there: 4
    OutOfBounds == handleMove simpleBoard Pinto 1 8,  -- : 5
    OutOfBounds == handleMove simpleBoard Pinto 1 7,    -- :6
    OutOfBounds == handleMove simpleBoard Pinto 1 6,    -- :7
    OutOfBounds == handleMove classicInitialBoard White 7 3,   --- : 8
    OutOfBounds == handleMove classicInitialBoard White 8 4,   --- :9
    OutOfBounds == handleMove classicInitialBoard White 6 4,   --- :10
    OutOfBounds == handleMove classicInitialBoard White 6 5,   --- :11
    OutOfBounds == handleMove classicInitialBoard White 8 8,   --- :12
    Valid == handleMove classicInitialBoard White 7 6,          -- empty : 13
    Valid == handleMove classicInitialBoard White 7 5,        -- self-capture  :14
    Valid == handleMove classicInitialBoard White 7 7,        -- empty :15
    Valid == handleMove classicInitialBoard Pinto 0 1,           -- self-capture : 16
    Valid == handleMove classicInitialBoard Pinto 0 3,            -- empty :17
    Valid == handleMove classicInitialBoard Pinto 1 2,          --- empty :18
    Valid == handleMove meleeBoard Pinto 0 3,                      -- capture :19
    Valid == handleMove meleeBoard Pinto 1 4,                       -- capture :20 *fail
    Valid == handleMove meleeBoard Pinto 8 5,                       -- capture : 21
    Valid == handleMove meleeBoard White 3 7,                       -- capture :22
    Valid == handleMove meleeBoard White 5 3,                     -- capture :23
    Valid == handleMove meleeBoard White 7 1,                     -- capture :24
    Win == handleMove winnableBoard1 White 1 3,               -- : 25   *fail 
    Win == handleMove winnableBoard1 Pinto 4 7,                -- :26 *fail 
    Win == handleMove winnableBoard2 Pinto 0 1,               -- : 27 *fail 
    Win == handleMove winnableBoard2 White  1 5,             -- :28  *fail 
    Win == handleMove winnableBoard3 Pinto  0 2,              -- : 29 *fail 
    Win == handleMove winnableBoard3 White 4 6               -- :30  *fail 
    ]



main :: IO ()
main = someFunc
