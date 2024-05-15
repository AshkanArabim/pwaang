main :: IO ()
main = putStrLn "Test suite not yet implemented"

----------  Boards for Testing  ------------------

emptyBoard = []
simpleBoard = [(1,Pinto), (7,White)]   
classicInitialBoard = [(0, Pinto), (1, Pinto), (2, Pinto), (6, White), (7, White), (8, White)]
meleeBoard = [(0, Pinto), (1, Pinto), (3, White), (5, White), (7, White), (8, Pinto)]
winnableBoard1 = [(1, White), (4, Pinto)]
winnableBoard2 = [(0, Pinto), (1, White)]
winnableBoard3 = [(0, Pinto), (4, White)]

----------------- test cases  ----------------------------
-- TODO: display these. 
    -- is there a "recommended" way to run test cases?

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
