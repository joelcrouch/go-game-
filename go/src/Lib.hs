module Lib
    ( someFunc,
      Stone,
      Board, 
      GameState,
      Group,
      runTests,
      --loop,
      initialBoard,
      --showBoard,
      boardSize,
      --initialGameState
    ) where

import Test.HUnit
import System.IO
import Data.Char
import Data.Maybe
import Data.List 
import Text.Read


data Stone = Black | White deriving (Eq, Show)

type Board = [[Maybe Stone]]

data GameState = GameState
  { board :: Board
  , currentPlayer :: Stone
  , capturedStones :: Maybe (Int, Int)
  , score :: Maybe (Int, Int)
  } deriving (Eq, Show)

type Position = (Int, Int)
type Group = [Position]

boardSize :: Int 
boardSize = 19 

someFunc :: IO ()
someFunc = putStrLn "someFunc"

runTests :: IO ()
runTests =  do
  _ <- runTestTT $ TestList [ testInitialBoard,
                              testInitialGameState,
                              testShowBoard,
                              testIsOnBoard,
                              testParseMove,
                              testGetStone,
                              testReplace,
                              testSetStone
                              ]
  return ()

-- loop :: GameState -> IO ()
-- loop gameState@(GameState board currentPlayer _ _) = do
--   putStr $ showBoard board
--   putStrLn $ "\nCurrent player: " ++ show currentPlayer ++ "\n"
--   putStr "Enter move (row,column): "
--   hFlush stdout
--   input <- getLine
--   case parseMove input of
--     Left err -> putStrLn err >> loop gameState
--     Right position ->
--       case playMove gameState position of
--         Left err -> putStrLn err >> loop gameState
--         Right newGameState -> do
--           let score' = gameScore newGameState
--           putStr $ getScore score'
--           case gameFinished newGameState of
--             Just winner -> putStrLn $ show winner ++ " wins!"
--             Nothing -> loop newGameState


initialBoard :: Int -> Board
initialBoard boardSize = replicate boardSize (replicate boardSize Nothing)

testInitialBoard :: Test
testInitialBoard = "testInitialBoard" ~: TestList  [
    TestCase $ assertEqual "board size 0" [] (initialBoard 0),
    TestCase $ assertEqual "board size 1" [[Nothing]] (initialBoard 1),
    TestCase $ assertEqual "board size 2" [[Nothing, Nothing], [Nothing, Nothing]] (initialBoard 2)
   ]
-- test that the size is boardsize and all are nothing
testInitialBoardLength :: Test
testInitialBoardLength = "testInitialBoardLength" ~: TestList [
  let boardSize = 19
      board = initialBoard boardSize
  in TestList [ TestCase $ assertEqual "board size" boardSize (length board)
              , TestCase $ assertBool "board elements" (all (\row -> length row == boardSize && all (== Nothing) row) board)
              ]
  ]

initialGameState :: Int -> GameState
initialGameState size = GameState (initialBoard size) Black (Just (0,0)) (Just (0,0))

testInitialGameState :: Test
testInitialGameState = "testInitialGameState" ~: TestCase $ do
  let expectedGameState = GameState (initialBoard 19) Black (Just (0,0)) (Just (0,0))
  let actualGameState = initialGameState 19
  assertEqual "Initial game state is incorrect" expectedGameState actualGameState

 
showBoard :: Board -> String
showBoard board =
  let
    size = boardSize
    rows = map (\r -> map (\c -> maybe "." show c) r) board
    numberedRows = zip [1..size] rows
    numberedCols = ['a'..'t']
    colNumbers = "  " ++ take size (unwords (map (\c -> " " ++ [c] ++ " ") numberedCols))
    boardRows = map (\(rowNum, row) -> (show rowNum) ++ " " ++ unwords row) numberedRows
  in
    unlines (colNumbers : boardRows)

testShowBoard :: Test
testShowBoard =  "testShowBoard " ~: TestCase $ do
  let board = [[Just Black, Just White, Nothing], [Nothing, Just Black, Just White], [Just Black, Nothing, Just White]]
      expectedOutput = unlines ["  a   b   c ", "1 B W . ", "2 . B W ", "3 B . W "]
      actualOutput = showBoard board
  assertEqual "The showBoard function should format the board correctly" expectedOutput actualOutput

isOnBoard :: Board -> Position -> Bool
isOnBoard board (x, y) = x >= 0 && y >= 0 && x < boardSize && y < boardSize

testIsOnBoard :: Test
testIsOnBoard = "testIsOnBoard" ~:
  TestList
    [ TestCase $ assertBool "Position (0, 0) should be on a 19x19 board." (isOnBoard (initialBoard 19) (0, 0))
    , TestCase $ assertBool "Position (19, 19) should not be on a 19x19 board." (not $ isOnBoard (initialBoard 19) (19, 19))
    , TestCase $ assertBool "Position (-1, 5) should not be on a 19x19 board." (not $ isOnBoard (initialBoard 19) (-1, 5))
    ]

parseMove :: Board -> String -> Either String Position
parseMove board input =
  case map readMaybe (words input) of
    [Just row, Just col]
      | isOnBoard board (row - 1, col - 1) -> Right (row - 1, col - 1)
    _ -> Left "Invalid input: position must be two integers separated by a space"

testParseMove :: Test
testParseMove = "testParseMove" ~: test
  [ "parse valid input" ~:
    parseMove (initialBoard 19) "10 10" ~?= Right (9, 9)
    
  , "parse invalid input" ~:
    parseMove (initialBoard 19) "a b" ~?= Left "Invalid input: position must be two integers separated by a space"
    
  , "parse out of board input" ~:
    parseMove (initialBoard 19) "20 20" ~?= Left "Invalid input: position must be two integers separated by a space"
  ]
  
-- -- -- attempts to play a stone at the specified position on the board and updates the game state  either uses Left or Right to denote errors or Not errors where 
-- --     --right means good
-- playMove :: GameState -> Position -> Either String GameState
-- playMove gameState@(GameState board currentPlayer _ _) position =
--   case getStone board position of
--     Just _ -> Left "There is already a stone at that position"
--     Nothing ->
--       let newBoard = setStone board position (currentPlayer)
--           newCapturedStones = captureStones newBoard (otherPlayer currentPlayer)
--           newScore = countScore newBoard  
--           newGameState = GameState newBoard (otherPlayer currentPlayer) newCapturedStones newScore
--       in Right newGameState
--------------------------------------------------------------------test needed

--  Get the stone at the given position on the board.
getStone :: Board -> Position -> Maybe Stone
getStone board (x,y)
  | x < 0 || y < 0 || x >= boardSize  || y >= boardSize  = Nothing
  | otherwise = board !! x !! y

-- this one is pretty verbose

testGetStone :: Test
testGetStone = "testGetStone" ~: TestList [
  "getStone returns Nothing for out-of-bounds position on one side" ~:
    Nothing ~?= getStone (initialBoard 19) (-1, 0),
  "getStone returns Nothing for out-of-bounds position on the other" ~:
    Nothing ~?= getStone (initialBoard 19) (0, -1),
  "getStone returns Nothing for out-of-bounds position on top with one out of bounds" ~:
    Nothing ~?= getStone (initialBoard 19) (19, 0),
  "getStone returns Nothing for out-of-bounds position the other out of bounds" ~:
    Nothing ~?= getStone (initialBoard 19) (0, 19),
  "getStone returns Just stone for a valid position" ~:
    let board = setStone (initialBoard 19) (3, 3) Black
    in Just Black ~?= getStone board (3, 3),
  "getStone returns Just stone for a valid position" ~:
    let board = setStone (initialBoard 19) (5, 5) White
    in Just White ~?= getStone board (5, 5)
  ]


--  Set the stone at the given position on the board.
setStone :: Board -> Position -> Stone -> Board
setStone board (x,y) stone = replace x newRow board
  where newRow = replace y (Just stone) (board !! x)


testSetStone :: Test
testSetStone = "testSetStone" ~: TestList [
    "setStone replaces empty cell with a stone" ~:
      expected ~=? setStone initial (1, 1) Black
  ]
  where
    initial = initialBoard 4 --replicate 4 (replicate 4 Nothing)
    expected = [ [Nothing, Nothing, Nothing, Nothing]
               , [Nothing, Just Black, Nothing, Nothing]
               , [Nothing, Nothing, Nothing, Nothing]
               , [Nothing, Nothing, Nothing, Nothing]
               ]


-- helper fucntion for setStoneReplace the element at the given index in the list.
replace :: Int -> a -> [a] -> [a]
replace index element list = take index list ++ [element] ++ drop (index+1) list

testReplace :: Test
testReplace = "testReplace" ~: TestList [
  "replace returns expected result for valid input" ~:
    [1, 5, 3, 4] ~?= replace 1 5 [1, 2, 3, 4],
  "replace returns original list if index is out of bounds" ~:
    [1, 2, 3, 4] ~?= replace 4 5 [1, 2, 3, 4],
  "replace returns original list if index is negative" ~:
    [1, 2, 3, 4] ~?= replace (-1) 5 [1, 2, 3, 4]
  ]

-----------------------------------------------------------------------------------------------------------------------------------------------test needed --------------------------------------------------------------------------------


-- Capture stones of the given player that are surrounded by the other player's stones.
-- captureStones :: Board -> Stone -> Int
-- captureStones board player = sum $ map (length . filter isCaptured) groups
--   where
--     groups = getGroups board
--     isCaptured group =
--       all (\pos -> maybe False (/= player) (getStone board pos)) (groupNeighbors group) &&
--       any (\pos -> maybe False (== player) (getStone board pos)) (groupPositions group)
    
--     --groupNeighbors group = nub $ concatMap getNeighborPositions (groupPositions group)
--     --getNeighborPositions pos = filter (isOnBoard board) (neighbors pos)
--    -- isOnBoard board (x, y) = x >= 0 && y >= 0 && x < boardSize && y < boardSize

-- -- helper function for captureStones 

-- getGroups :: Board -> [Group]
-- getGroups board =
--   let size = boardSize
--       allPositions = [(i, j) | i <- [0..size-1], j <- [0..size-1]]
--       groups = groupByColor $ sortBy (comparing $ getStone board) allPositions
--   in map (Group board) groups

-- -- Group the stones by color (Black/White)
-- groupByColor :: [Group] -> (Group, Group)
-- groupByColor groups = foldl (\(bs, ws) g -> if head g == Black then (g:bs, ws) else (bs, g:ws)) ([], []) groups

-- groupNeighbors :: Group -> [Position]
-- groupNeighbors group = nub $ concatMap getNeighborPositions (groupPositions group)
--   --where
--     --getNeighborPositions pos = filter (isOnBoard board) (neighbors pos)
--     --isOnBoard (x, y) = x >= 0 && y >= 0 && x < boardSize && y < boardSize

-- -- helper functions for the capture  functions, also used elsewhere
-- getNeighborPositions :: Position -> [Position]
-- getNeighborPositions (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]




-- groupPositions :: Group -> [Position]
-- groupPositions group = group

-- countScore :: Board -> (Int, Int)
-- countScore board = foldr countStone (0, 0) $ concat board
--   where
--     countStone Nothing (bs, ws) = (bs, ws)
--     countStone (Just Black) (bs, ws) = (bs+1, ws)
--     countStone (Just White) (bs, ws) = (bs, ws+1)

-- --extracts the score from a gamestate 
-- gameScore :: GameState -> (Int, Int)
-- gameScore (GameState _ _ _ s) = s

-- --extracts the score from a tuple and Returns a formatted string with the current score
-- getScore :: (Int, Int) -> String
-- getScore (blackScore, whiteScore) = "Black: " ++ show blackScore ++ ", White: " ++ show whiteScore ++ "\n"



-- gameFinished :: GameState -> Maybe Stone
-- gameFinished (GameState board _ _ _) =
--   let stones = catMaybes (concat board)
--       blackCaptures = sum [captures | (player, captures) <- stones, player == Black]
--       whiteCaptures = sum [captures | (player, captures) <- stones, player == White]
--       blackScore = fst (gameScore board)
--       whiteScore = snd (gameScore board)
--       totalSpaces = boardSize * boardSize
--       totalMoves = length stones
--   in if blackCaptures >= capturesToWin
--         then Just Black
--         else if whiteCaptures >= capturesToWin
--                then Just White
--                else if totalSpaces - totalMoves == 0
--                       then if blackScore > whiteScore
--                              then Just Black
--                              else Just White
--                       else Nothing

-- -- Given a player, returns the other player.
-- otherPlayer :: Stone -> Stone
-- otherPlayer Black = White
-- otherPlayer White = Black