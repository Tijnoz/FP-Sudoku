module Sudoku where

import Prelude hiding (replicate)
import FPPrac.Events
import FPPrac.Graphics
import Data.List
import Debug.Trace
import Structure
import Graphics

printBoard :: Board -> IO ()
printBoard (Board fs t) = putStrLn 
                        . unlines 
                        $ [ unwords $ [ " "++[def] | f@(Field col row sec os def)<-fs, col==c ] | c <- [0..8] ]
hLine = replicate 9 '-'

-- Sets a square at the board, without checking anything.
setSquare (Board fs t) col row new = updateOptions (Board (map set fs) t)
    where
        set f@(Field c r sec os def) 
            | c == col && r == row
            = (Field col row sec [] new)
            | otherwise
            = f
            


-- Checks whether a given move is valid
validMove b@(Board _ t) col row new = 
            col >= 0 && col < 9 &&
            row >= 0 && row < 9 &&
            new `elem` allOptions &&
            not  (new `elem` (getRowD b row)) &&
            not  (new `elem` (getColD b col)) &&
            not  (new `elem` (getSecD b (secCalc col row))) &&
            (t /= Cross || not (new `elem` (getDiaD b (diaCalc col row))))

-- Does a move when the move is valid, otherwise returns the unaltered board
validSet b col row new = if validMove b col row new 
                         then setSquare b col row new
                         else b
                         
                         
-- Several get methods
getCol (Board fs t) col = [ f | f@(Field c _ _ _ _)<-fs, c==col ]
getRow (Board fs t) row = [ f | f@(Field _ r _ _ _)<-fs, r==row ]
getSec (Board fs t) sec = [ f | f@(Field _ _ s _ _)<-fs, s==sec ]
--- Dia 1 is upper left to lower right, Dia 2 is upper right to lower left
getDia (Board fs t) dia = [ f | f@(Field c r _ _ _)<-fs, case (t, dia) of (Cross, 1) -> c == r
                                                                          (Cross, 2) -> (c+r) == 8 
                                                                          (_,_)      -> False ]
getField (Board (f@(Field c r s o d):fs) t) col row
    | c == col && r == row  = f
    | otherwise             = getField (Board fs t) col row

-- Gets all definitives of the given options
getColD (Board fs t) col = [ d | f@(Field c _ _ _ d)<-fs, c==col ]
getRowD (Board fs t) row = [ d | f@(Field _ r _ _ d)<-fs, r==row ]
getSecD (Board fs t) sec = [ d | f@(Field _ _ s _ d)<-fs, s==sec ]
getDiaD (Board fs t) dia = [ d | f@(Field c r _ _ d)<-fs, case (t, dia) of (Cross, 1) -> c == r
                                                                           (Cross, 2) -> (c+r) == 8 
                                                                           (_,_)      -> False ]

-- Union on all field options of the given fields
aggOptions [] = []
aggOptions ((Field c r sec os def):fs) = os `union` (aggOptions fs)
 
-- Updates all options of the board
updateOptions b@(Board fs _)
    | b == b'   = b'
    | otherwise = updateOptions b'
    where
        --fs' = (map (updateFields b) fs)
        --b'  = (Board fs')
        
        b' = updateFields b fs

-- hard +singleton: 55s 
-- hard -singleton: 45s
-- Updates all options of all fields. The Board is gradually updated; the list of fields is the list to be updated.
updateFields b [] = b
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
 
updateFields b@(Board cfs t) (f@(Field c r s _ def):fs) = updateFields (Board cfs' t) fs
    where
        cfs' = (delete f cfs) ++ [Field c r s [] def]

-- Basic exclusions based on Sudoku rules
possibles b@(Board _ t) f@(Field c r s _ _) os
    | t == Cross  = os \\ (getColD b c `union` (getRowD b r) `union` (getSecD b s) `union` (getDiaD b (diaCalc c r))) 
    | otherwise   = os \\ (getColD b c `union` (getRowD b r) `union` (getSecD b s)) 
        
-- When one option in the list of options of a field (os) does not occur in the options of other fields (fs), that option is the option for this field
singles fs os
    | (length os) > 1 && not (null os') = os'
    | otherwise                         = os
    where
        secOs = aggOptions fs
        os'   = (os \\ secOs)
              
           
    

-- Solves a board
solve :: Board -> [Board]
solve b@(Board fs t)
    | completeBoard fs = [b]
    | unsolvable fs    = []
    | length os == 1   = solve (validSet b col row (head os))
    | otherwise        = concat $ map (solve . (validSet b col row)) os
    where
        fs' = sort(fs)
        (Field col row _ os _) = findFirstUnsolved fs'

-- Gets a hint; returns the board where the hint is set
hint :: Board -> Board
hint b@(Board fs t)
    | completeBoard fs = b
    | unsolvable fs    = b
    | length os == 1   = validSet b col row (head os)
    | otherwise        = validSet b colb rowb defb
    where
        fs' = sort(fs)
        (Field col row _ os _) = findFirstUnsolved fs'    
        (Field colb rowb _ _ defb) = getField (head $ solve b) col row
      
findFirstUnsolved (f@(Field _ _ _ os _):fs)
    | (not (null os)) = f
    | otherwise       = findFirstUnsolved fs

completeBoard [] = True
completeBoard ((Field _ _ _ _ def):fs) = not (def == ' ') && (completeBoard fs)

unsolvable [] = False
unsolvable ((Field _ _ _ os def):fs)   =  (os == [] && def == ' ') || (unsolvable fs)

resetOptions b@(Board fs t) = (Board [ (Field c r s allOptions def) | (Field c r s _ def) <- fs ] t)

------------------------------------------------
-- End of game logic, start of application logic
------------------------------------------------
main = doShow sudokuHandler

-- Event handler
sudokuHandler :: Store -> Input -> (Store,[Output])

sudokuHandler store (KeyIn 'o') = (store', [DrawPicture $ redraw store'])
    where
        Store {board=board} = store
        sB = solve board
        board' = if not (null sB) then head sB else board 
        err = if not (null sB) then "" else "There is no solution."
        store' = store {board=board', process=DoingNothing, errorMsg=err} 

sudokuHandler store (KeyIn 'h') = (store', [DrawPicture $ redraw store'])
    where
        Store {board=board@(Board fs t)} = store
        board' = hint board
        err = if unsolvable fs || completeBoard fs then "There is no hint." else ""
        store' = store {board=board', process=DoingNothing, errorMsg=err} 

sudokuHandler store (KeyIn 'x') = (store', [DrawPicture $ redraw store'])
    where
        Store {board=(Board fs t)} = store
        board' = updateOptions . resetOptions $ Board fs (if t == Cross then Normal else Cross)
        store' = store {board=board', process=DoingNothing, errorMsg=""} 
        
sudokuHandler store (MouseUp p)
    | oF == Nothing = (store, [])
    | otherwise     = (store', [DrawPicture $ drawBottomLine store']) 
                                   --[GraphPrompt ("Enter new value", "Value for field " ++ show x ++ "," ++ show y)])
    where
        Store {board=board} = store
        Board fields t = board
        oF = onField fields p
        Just (Field x y s os v) = oF
        store' = store {clickedField=onField fields p, process=EnteringValue} 
        
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
  
--sudokuHandler store (Prompt ("Enter new value", newVal))
--    | clickedField == Nothing = (store, [])
--    | newVal /= ""            = (store', [DrawPicture $ redraw store'])
--    | otherwise               = (store {clickedField=Nothing}, [])
--    where
--        Store {board=board, clickedField=clickedField} = store
--        Just (Field x y s os v) = clickedField
--        
--        board' = validSet board x y (newVal !! 0)
--        store' = store {board=board', clickedField=Nothing, process=DoingNothing, errorMsg=""}
        
sudokuHandler store (File filename (TXTFile input))
    | input /= "" = (store', [DrawPicture $ redraw store'])
    | otherwise   = (store,[])
    where
        board' = updateOptions $ readBoard input
        store' = store {board=board', name=filename, process=DoingNothing, errorMsg=""}

--- Unhandled event handler
sudokuHandler store _ = (store,[])