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

setSquare (Board fs t) col row new = updateOptions (Board (map set fs) t)
    where
        set f@(Field c r sec os def) 
            | c == col && r == row
            = (Field col row sec [] new)
            | otherwise
            = f
            
updateOptions b@(Board fs _)
    | b == b'   = b'
    | otherwise = updateOptions b'
    where
        --fs' = (map (updateFields b) fs)
        --b'  = (Board fs')
        
        b' = updateFields b fs

-- hard +singleton: 55s 
-- hard -singleton: 45s
updateFields b [] = b
updateFields b@(Board cfs t) (f@(Field c r s os ' '):fs) = updateFields (Board cfs' t) fs
    where
        -- Basic exclusions Haal alle opties weg die al in de kolom; rij of box staan.
        os1 = os \\ (getColD b c `union` (getRowD b r) `union` (getSecD b s)) 
        -- Basic singleton (+23s op hardest...) Als er maar 1 plek is voor een getal, maar er ook andere opties staan, moet dit getal w
        os2 = checkSingleton (delete f (getSec b s)) os1
        os3 = checkSingleton (delete f (getCol b c)) os2
        os' = checkSingleton (delete f (getRow b r)) os3
        
        -- Update board with new field
        f' = (Field c r s os' ' ') 
        cfs' = (delete f cfs) ++ [f']
 
updateFields b@(Board cfs t) (f@(Field c r s _ def):fs) = updateFields (Board cfs' t) fs
    where
        cfs' = (delete f cfs) ++ [Field c r s [] def]

checkSingleton fs os
    | (length os) > 1 && not (null os') = os'
    | otherwise                         = os
    where
        secOs = aggOptions fs
        os'   = (os \\ secOs)
              
           
    
-- union on all fields
aggOptions [] = []
aggOptions ((Field c r sec os def):fs) = os `union` (aggOptions fs)
        
validMove b col row new = 
            col >= 0 && col < 9 &&
            row >= 0 && row < 9 &&
            new `elem` allOptions &&
            not  (new `elem` (getRowD b row)) &&
            not  (new `elem` (getColD b col)) &&
            not  (new `elem` (getSecD b (secCalc col row))) 

validSet b col row new = if validMove b col row new 
                         then setSquare b col row new
                         else b
                         
getRow (Board fs t) row = [ f | f@(Field c r s o d)<-fs, r==row ]
getCol (Board fs t) col = [ f | f@(Field c r s o d)<-fs, c==col ]
getSec (Board fs t) sec = [ f | f@(Field c r s o d)<-fs, s==sec ]
getField (Board (f@(Field c r s o d):fs) t) col row
    | c == col && r == row  = f
    | otherwise             = getField (Board fs t) col row
            
getRowD (Board fs t) row = [ d | f@(Field c r s o d)<-fs, r==row ]
getColD (Board fs t) col = [ d | f@(Field c r s o d)<-fs, c==col ]
getSecD (Board fs t) sec = [ d | f@(Field c r s o d)<-fs, s==sec ]


solve :: Board -> [Board]
solve b@(Board fs t)
    | completeBoard fs = [b]
    | unsolvable fs    = []
    | length os == 1   = solve (validSet b col row (head os))
    | otherwise        = concat $ map (solve . (validSet b col row)) os
    where
        fs' = sort(fs)
        (Field col row _ os _) = findFirstUnsolved fs'
        
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

resetOptions b@(Board fs t) = (Board [ (Field c r s (if def == ' ' then [] else allOptions) def) | (Field c r s _ def)<-fs ] t)

---meuk
main = doShow sudokuHandler

-- Event handler
sudokuHandler :: Store -> Input -> (Store,[Output])

sudokuHandler store (KeyIn 'o') = (store', [DrawPicture $ redraw store'])
    where
        Store {board=board} = store
        sB    = solve board
        board' = if not (null sB) then head sB else board 
        err = if not (null sB)  then "" else "There is no solution."
        store' = store {board=board', process=DoingNothing, errorMsg=err } 

sudokuHandler store (KeyIn 'h') = (store', [DrawPicture $ redraw store'])
    where
        Store {board=board@(Board fs t)} = store
        board'   = hint board
        err = if unsolvable fs || completeBoard fs then "There is no hint." else ""
        store' = store {board=board', process=DoingNothing, errorMsg=err } 
        
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
    | clickedField /= Nothing && newVal `elem` allOptions = (store', [DrawPicture $ redraw store'])
    | otherwise                                           = (store'', [DrawPicture $ drawBottomLine store''])
    where
        Store {board=board, clickedField=clickedField} = store
        Just (Field x y s os v) = clickedField       
        board' = resetOptions $ validSet board x y newVal
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