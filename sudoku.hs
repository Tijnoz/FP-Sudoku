module Sudoku where

import Prelude hiding (replicate)
import FPPrac.Events
import FPPrac.Graphics
import Data.List
import Debug.Trace
import Structure
import Graphics

printBoard :: Board -> IO ()
printBoard (Board fs) = putStrLn 
                        . unlines 
                        $ [ unwords $ [ " "++[def] | f@(Field col row sec os def)<-fs, col==c ] | c <- [0..8] ]
hLine = replicate 9 '-'

setSquare (Board fs) col row new = updateOptions (Board (map set fs))
    where
        set f@(Field c r sec os def) 
            | c == col && r == row
            = (Field col row sec [] new)
            | otherwise
            = f
            
updateOptions b@(Board fs) = (Board (map (updateOption b) fs))
updateOption b (Field c r sec os def) = (Field c r sec os' def) 
    where
        os' = allOptions \\ (getCol b c `union` (getRow b r) `union` (getSec b sec))
        
validMove b col row new = 
            col >= 0 && col < 9 &&
            row >= 0 && row < 9 &&
            new `elem` allOptions &&
            not  (new `elem` (getRow b row)) &&
            not  (new `elem` (getCol b col)) &&
            not  (new `elem` (getSec b (secCalc col row))) 

validSet b col row new = if validMove b col row new 
                         then setSquare b col row new
                         else b
            
getRow (Board fs) row = [ d | f@(Field c r s o d)<-fs, r==row ]
getCol (Board fs) col = [ d | f@(Field c r s o d)<-fs, c==col ]
getSec (Board fs) sec = [ d | f@(Field c r s o d)<-fs, s==sec ]

solve b@(Board fs) = newBoard
    where
        fs' = sort(fs)
        fst = findFirstUnsolved
        newBoard = (Board fs')

findFirstUnsolved (f@(Field _ _ _ os _):fs) = if (not (null os)) then f else findFirstUnsolved fs
---meuk
main = doShow sudokuHandler

-- Event handler
sudokuHandler :: Store -> Input -> (Store,[Output])

sudokuHandler store (MouseUp p)
    | oF == Nothing = (store, [])
    | otherwise     = (store', [DrawPicture $ drawBottomLine store']) 
                                   --[GraphPrompt ("Enter new value", "Value for field " ++ show x ++ "," ++ show y)])
    where
        Store {board=board} = store
        Board fields = board
        oF = onField fields p
        Just (Field x y s os v) = oF
        store' = store {clickedField=onField fields p, process=EnteringValue} 
        
sudokuHandler store@(Store {process=EnteringValue}) (KeyIn newVal)
    | clickedField /= Nothing && newVal `elem` allOptions = (store', [DrawPicture $ redraw store'])
    | otherwise                                           = (store'', [DrawPicture $ drawBottomLine store''])
    where
        Store {board=board, clickedField=clickedField} = store
        Just (Field x y s os v) = clickedField       
        board' = validSet board x y newVal
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
        board' = readBoard input
        store' = store {board=board', name=filename, process=DoingNothing, errorMsg=""}

--- Unhandled event handler
sudokuHandler store _ = (store,[])