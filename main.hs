import Data.List 
import Data.Char

data Player = Empty | P1 | P2 
                deriving (Ord, Eq, Show)

type Board  = [[Player]]
---------------------------------------------------------------------------
-- Print out the Connect4 Board 
printPlayer :: Player -> Char
printPlayer Empty   = '.'
printPlayer P1      = 'O'
printPlayer P2      = 'X'

printRow :: [Player] -> String
printRow = map printPlayer

printBoard :: Board -> IO()  -- unlines [String] -> String
printBoard board =  putStrLn (unlines((map printRow board) ++ [replicate 7 '=' ] ++ [take 7 ['0'..] ] ))

---------------------------------------------------------------------------

-- Available Boards 
emptyBoard :: Board
emptyBoard = replicate 6 (replicate 7 Empty)

---------------------------------------------------------------------------
-- Winner Checker (Find 4 same token O or X)
-- Check - 
fourInARow :: [Player] -> Player -> Bool
fourInARow listRow token
    | length listRow < 4 = False
fourInARow (w:x:y:z:zs) token
    | ( w==x && w==y && w==z && w == token ) = True
    | otherwise         = fourInARow (x:y:z:zs) token


----------------------------------------------------------------------------

getCol :: Board -> Int -> [Player]
getCol board colNum = (transpose board) !! colNum 

getRow :: Board -> Int -> [Player]
getRow board rowNum = board !! rowNum 

-- / diagonals
-- From stackoverflow https://stackoverflow.com/questions/32465776/getting-all-the-diagonals-of-a-matrix-in-haskell
rightDiagonals :: [[a]] -> [[a]]
rightDiagonals = tail . go [] where
    -- it is critical for some applications that we start producing answers
    -- before inspecting es_
    go b es_ = [h | h:_ <- b] : case es_ of
        []   -> transpose ts
        e:es -> go (e:ts) es
        where ts = [t | _:t <- b]

getRightDiag :: [[a]] -> [[a]]
getRightDiag board = filter (\x -> length x >= 4) (rightDiagonals board)

isColNotFull    :: Board -> Int -> Bool
isColNotFull board colNum = Empty `elem` (getCol board colNum)

validCol :: Int -> Board -> Bool
validCol colNum board 
    | not ( colNum >= 0) && ( colNum < 7)       = False
    | isColNotFull board colNum                 = True
    | otherwise                                 = False

-- -- get the height of the token of the column number
colTokenHeight :: [Player] -> Int
colTokenHeight col  = sum [ 1 | n <- [0..5] , (col !! n) /= Empty]

nextEmptyRowIndexSpace :: Board -> Int -> Int 
nextEmptyRowIndexSpace board colNum  =  5 -(colTokenHeight col)
    where 
        col = getCol board colNum

putTokenRow :: [Player] -> Int -> Player -> [Player]
putTokenRow row colNum token   = beforeTokenList ++ [token] ++ afterTokenList
    where
        beforeTokenList = take colNum row
        afterTokenList = drop (colNum+1) row

putToken :: Player -> Int -> Board -> Board      --add token to [x] y 
putToken token colNum board  = take nextInd board ++ [putTokenRow row colNum token ] ++ drop (nextInd+1) board
    where
        nextInd = nextEmptyRowIndexSpace board colNum
        row = getRow board nextInd 

----------------------------------------------------------------------------

-- count the number of turns of the players
turnPlayer :: Board -> Player -> Int            
turnPlayer board player = length ( filter ( == player) (listRow))
    where 
        -- concat [[a]] -> [a]
        listRow = concat board

-- know who is the current player
currPlayer      :: Board -> Player
currPlayer board  
    | isBoardFull board                                 = Empty
    | (turnPlayer board P1) > (turnPlayer board P2 )    = P2
    | otherwise                                         = P1

-- Check if board is full
isBoardFull :: Board -> Bool
isBoardFull board   
    | Empty `elem` (listRow)        = False
    | otherwise                     = True
    where 
        -- concat [[a]] -> [a]
        listRow = concat board

isWinCol :: Board -> Player -> [Bool]
isWinCol board token = [fourInARow (getCol board i) token | i <- [0..6]]

isWinRow :: Board -> Player -> [Bool]
isWinRow board token = [fourInARow (getRow board i) token | i <- [0..5]]

lengthRDiag :: Board -> Player -> Int
lengthRDiag board token = length (getRightDiag board)

isWinRDiag :: Board -> Player -> [Bool]
isWinRDiag board token = [fourInARow (getRow diag2DList i) token | i <- [0..(size-1)]] 
    where
        diag2DList = getRightDiag board
        size = lengthRDiag board token 

isWin     ::   Board -> Player -> Bool
isWin board player  
    | numOfTrueCol + numOfTrueRow + numOfTrueRDiag >= 1         = True
    | otherwise                                = False
    where
        numOfTrueCol = length ( filter (== True) (isWinCol board player))
        numOfTrueRow = length ( filter (== True) (isWinRow board player))
        numOfTrueRDiag = length ( filter (== True) (isWinRDiag board player))

play :: Board -> Int -> Player -> Board
play board colNum token = if (validCol colNum board ) then (putToken token colNum board)
                         else board
                         
info :: IO ()
info = do   putStrLn " Welcome to 368 Connect 4 "
            putStrLn "To win you have to have 4 in a row token horizontally or vertically"
            putStrLn "Player 1 ( O ) and Player 2 ( X)"
            printBoard emptyBoard
        
playerToString :: Player -> String
playerToString P1 = "Player 1 O"
playerToString P2 = "Player 2 X"

startGame   :: Board ->  IO ()
startGame board
    | isBoardFull board         = putStrLn ("Draw!")
    | isWin board whichPlayer   = putStrLn (playerToString whichPlayer ++ " is the Winner!")
    | otherwise                 = 
        do  putStrLn ((playerToString whichPlayer) ++ "'s Turn")
            putStrLn ("Enter row number")
            userInput <- getLine
            let input = (read userInput :: Int)
            
            board <- pure (play board input whichPlayer)
            printBoard board    
            startGame board 
    where 
        whichPlayer = currPlayer board 

main :: IO ()
main = do   info
            putStrLn ("---Starting game---")
            startGame emptyBoard
    