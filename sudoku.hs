module Sudoku where

import Prelude hiding (replicate)
import FPPrac.Events
import FPPrac.Graphics
import Data.List
import Debug.Trace
import Structure
import Graphics

-- Print a board. Only for debugging purposes and it's very ugly. So, please just mosey along
printBoard :: Board -> IO ()
printBoard (Board fs t) = putStrLn 
                        . unlines 
                        $ [ unwords $ [ " "++[def] | f@(Field col row sec os def)<-fs, col==c ] | c <- [0..8] ]
hLine = replicate 9 '-'

-- Sets a square at the board, without checking anything.
setSquare :: Board -> Integer -> Integer -> Char -> Board
setSquare (Board fs t) col row new = updateOptions (Board (map set fs) t)
    where
        set f@(Field c r sec os def) 
            | c == col && r == row   = (Field col row sec [] new)
            | otherwise              = f

-- Checks whether a given move is valid
validMove :: Board -> Integer -> Integer -> Char -> Bool
validMove b@(Board _ t) col row new = col >= 0 && col < 9 &&               -- col in range 
                                      row >= 0 && row < 9 &&               -- row in range
                                      new `elem` allOptions &&             -- value is valid
                                      not  (new `elem` (getRowD b row)) && -- not already in row
                                      not  (new `elem` (getColD b col)) && -- not already in column
                                      not  (new `elem` (getSecD b (secCalc col row))) && -- not already in sector
                                      (t /= Cross || not (new `elem` (getDiaD b (diaCalc col row)))) -- not already in diagonal (if board type is Cross)

-- Does a move when the move is valid, otherwise returns the unaltered board
validSet :: Board -> Integer -> Integer -> Char -> Board
validSet b col row new = if validMove b col row new 
                         then setSquare b col row new
                         else b
                         
                         
-- Several get methods
--- Get field given coordinates
getField :: Board -> Integer -> Integer -> Field
getField (Board (f@(Field c r s o d):fs) t) col row
    | c == col && r == row  = f
    | otherwise             = getField (Board fs t) col row

--- Get all fields in a specific row, column, sector or diagonal
getCol :: Board -> Integer -> [Field]
getCol (Board fs t) col = [ f | f@(Field c _ _ _ _)<-fs, c==col ]
getRow :: Board -> Integer -> [Field]
getRow (Board fs t) row = [ f | f@(Field _ r _ _ _)<-fs, r==row ]
getSec :: Board -> Integer -> [Field]
getSec (Board fs t) sec = [ f | f@(Field _ _ s _ _)<-fs, s==sec ]
---- Dia 1 is upper left to lower right, Dia 2 is upper right to lower left
getDia :: Board -> Integer -> [Field]
getDia (Board fs t) dia = [ f | f@(Field c r _ _ _)<-fs, case (t, dia) of (Cross, 1) -> c == r
                                                                          (Cross, 2) -> (c+r) == 8 
                                                                          (_,_)      -> False ]
--- Gets all definitives of the given column, row, sector or diagonal
getColD :: Board -> Integer -> [Char]
getColD (Board fs t) col = [ d | f@(Field c _ _ _ d)<-fs, c==col ]
getRowD :: Board -> Integer -> [Char]
getRowD (Board fs t) row = [ d | f@(Field _ r _ _ d)<-fs, r==row ]
getSecD :: Board -> Integer -> [Char]
getSecD (Board fs t) sec = [ d | f@(Field _ _ s _ d)<-fs, s==sec ]
getDiaD :: Board -> Integer -> [Char]
getDiaD (Board fs t) dia = [ d | f@(Field c r _ _ d)<-fs, case (t, dia) of (Cross, 1) -> c == r
                                                                           (Cross, 2) -> (c+r) == 8 
                                                                           (_,_)      -> False ]

-- Union on all field options of the given fields
aggOptions :: [Field] -> [Char]
aggOptions [] = []
aggOptions ((Field c r sec os def):fs) = os `union` (aggOptions fs)
 
-- Updates all options of the board
updateOptions :: Board -> Board
updateOptions b@(Board fs _)
    | b == b'   = b'
    | otherwise = updateOptions b'
    where
        --fs' = (map (updateFields b) fs)
        --b'  = (Board fs')
        
        b' = updateFields b fs

-- hard +singleton: 55s 
-- hard -singleton: 45s
-- Updates all options of all fields. The Board is gradually updated; the list of fields is the list to be updated, so this will be empty when finished.
updateFields :: Board -> [Field] -> Board
updateFields b [] = b

--- the following is only for empty fields
updateFields b@(Board cfs t) (f@(Field c r s os ' '):fs) = updateFields (Board cfs' t) fs 
    where
        -- Basic exclusions Haal alle opties weg die al in de kolom; rij of box staan.
        os1 = possibles b f os
        -- Basic singleton (+23s op hardest...) Als er maar 1 plek is voor een getal, maar er ook andere opties staan, moet dit getal w
        os2 = singles (delete f (getSec b s)) os1
        os3 = singles (delete f (getCol b c)) os2
        os4 = singles (delete f (getRow b r)) os3
        os' = if t == Cross then singles (delete f (getDia b (diaCalc c r))) os4 else os4
        
        -- Update board with new field
        f' = (Field c r s os' ' ') 
        cfs' = (delete f cfs) ++ [f']

--- all non-empty fields will have no options
updateFields b@(Board cfs t) (f@(Field c r s _ def):fs) = updateFields (Board cfs' t) fs 
    where
        cfs' = (delete f cfs) ++ [Field c r s [] def]

-- Basic exclusions based on Sudoku rules. 'os' is current list of options, 
-- board is given only because this contains some rules
possibles :: Board -> Field -> [Char] -> [Char]
possibles b@(Board _ t) f@(Field c r s _ _) os
    | t == Cross  = os \\ (getColD b c `union` (getRowD b r) `union` (getSecD b s) `union` (getDiaD b (diaCalc c r))) 
    | otherwise   = os \\ (getColD b c `union` (getRowD b r) `union` (getSecD b s)) 
        
-- When one option in the list of options of a field (os) does not occur in the options 
-- of other fields (fs), that option is the option for this field
singles :: [Field] -> [Char] -> [Char]
singles fs os
    | (length os) > 1 && not (null os') = os'
    | otherwise                         = os
    where
        secOs = aggOptions fs
        os'   = (os \\ secOs)
              
           
    

-- Solves a board
-- May backtrack in the case that there are more then one option for a particular field
-- (which is the otherwise) and returns all possible options.
solve :: Board -> [Board]
solve b@(Board fs t)
    | completeBoard fs = [b]
    | unsolvable fs    = []
    | length os == 1   = solve (validSet b col row (head os))
    | otherwise        = concat $ map (solve . (validSet b col row)) os
    where
        fs' = sort(fs)
        (Field col row _ os _) = findFirstUnsolved fs'

-- Gets a hint; returns the board where the hint is set (or the same board if no solutions are found)
hint :: Board -> Board
hint b@(Board fs t)
    | completeBoard fs || unsolvable fs    = b
    | length os == 1                       = validSet b col row (head os)
    | otherwise                            = validSet b colb rowb defb
    where
        fs' = sort(fs)
        -- Check for only one option
        (Field col row _ os _) = findFirstUnsolved fs'
        -- Otherwise: get first field that is solved by the solver (possibly using backtracking)
        (Field colb rowb _ _ defb) = getField (head $ solve b) col row

-- Returns the first unsolved field
findFirstUnsolved :: [Field] -> Field
findFirstUnsolved (f@(Field _ _ _ os _):fs)
    | (not (null os)) = f
    | otherwise       = findFirstUnsolved fs

-- Check whether the board has been completed
completeBoard :: [Field] -> Bool
completeBoard [] = True
completeBoard ((Field _ _ _ _ def):fs) = not (def == ' ') && (completeBoard fs)

-- Check whether the board is solvable
unsolvable :: [Field] -> Bool
unsolvable [] = False
unsolvable ((Field _ _ _ os def):fs)   =  (os == [] && def == ' ') || (unsolvable fs)

-- Reset options of all fields to all options
resetOptions :: Board -> Board
resetOptions b@(Board fs t) = (Board [ (Field c r s allOptions def) | (Field c r s _ def) <- fs ] t)

------------------------------------------------
-- End of game logic, start of application logic
------------------------------------------------
main = doShow sudokuHandler

-- Event handler
sudokuHandler :: Store -> Input -> (Store,[Output])

--- Solve
sudokuHandler store (KeyIn 'o') = (store', [DrawPicture $ redraw store'])
    where
        Store {board=board} = store
        sB = solve board
        board' = if not (null sB) then head sB else board 
        err = if not (null sB) then if null . tail $ sB then "" else "There's more than one solution." else "There is no solution."
        store' = store {board=board', process=DoingNothing, errorMsg=err} 

--- Hint
sudokuHandler store (KeyIn 'h') = (store', [DrawPicture $ redraw store'])
    where
        Store {board=board@(Board fs t)} = store
        board' = hint board
        err = if unsolvable fs || completeBoard fs then "There is no hint." else ""
        store' = store {board=board', process=DoingNothing, errorMsg=err} 
        
--- Toggle X-Sudoku
sudokuHandler store (KeyIn 'x') = (store', [DrawPicture $ redraw store'])
    where
        Store {board=(Board fs t)} = store
        board' = updateOptions . resetOptions $ Board fs (if t == Cross then Normal else Cross)
        store' = store {board=board', process=DoingNothing, errorMsg=""} 

--- Handle entered value
sudokuHandler store@(Store {process=EnteringValue}) (KeyIn newVal)
    | clickedField /= Nothing && newVal `elem` (' ':allOptions) = (store', [DrawPicture $ redraw store'])
    | otherwise                                                 = (store'', [DrawPicture $ drawBottomLine store''])
    where
        Store {board=board, clickedField=clickedField} = store
        Just (Field x y s os v) = clickedField       
        
        board' = updateOptions . resetOptions $ (if newVal == ' ' then setSquare board x y ' ' else validSet board x y newVal)
        store' = if board == board'
                 then store {board=board', clickedField=Nothing, process=DoingNothing, errorMsg="Invalid move: " ++ [newVal]}
                 else store {board=board', clickedField=Nothing, process=DoingNothing, errorMsg=""}
        
        store'' = store {clickedField=Nothing, process=DoingNothing, errorMsg="Invalid value: " ++ [newVal]}
  
--- Handle file load
sudokuHandler store (File filename (TXTFile input))
    | input /= "" = (store', [DrawPicture $ redraw store'])
    | otherwise   = (store,[])
    where
        board' = updateOptions $ readBoard input
        store' = store {board=board', name=filename, process=DoingNothing, errorMsg=""}

--- Unhandled event handler
sudokuHandler store _ = (store,[])