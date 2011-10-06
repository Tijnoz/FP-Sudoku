module Sudoku where

<<<<<<< HEAD
import FPPrac
import FPPrac.Events
=======
import FPPrac hiding (replicate)
import FPPrac.Events
import FPPrac.Graphics
import Data.List
>>>>>>> bf7e431d93f88f3310e3c0ad92f7fa3b852307e9
import Debug.Trace
import Structure
import Graphics

<<<<<<< HEAD
    

printBoard (Board fs) = [ [ fs !! ((x)+(s*3)+(i*9)) | s <-[0..2], x <- [1..3]] | i <- [0..9] ]


--setSquare (Board fs) sector position new = 

--getRow (Board fs) row

--getColomn (Board fs) col

--getSector (Board fs) sec


=======
printBoard :: Board -> IO ()
printBoard (Board fs) = putStrLn 
                        . unlines 
                        $ [ unwords $ [ " "++[def] | f@(Field col row sec os def)<-fs, col==c ] | c <- [0..8] ]
hLine = replicate 9 '-'

setSquare (Board fs) col row new = Board (map set fs)
    where
        set f@(Field c r sec os def) 
            | c == col && r == row
            = (Field col row sec [] new)
            | c == col
            = (Field c r sec (delete new os) def)
            | r == row
            = (Field c r sec (delete new os) def)
            | sec == (secCalc col row)
            = (Field c r sec (delete new os) def)
            | otherwise
            = f
        
validMove b col row new = 
            col > 0 && col < 9 &&
            row > 0 && row < 9 &&
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

secCalc col row = (col `div` 3)+(row `div` 3)*3


 
---meuk
>>>>>>> bf7e431d93f88f3310e3c0ad92f7fa3b852307e9

main = doShow sudokuHandler

-- Event handler
sudokuHandler :: Store -> Input -> (Store,[Output])

--- Unhandled event handler
sudokuHandler store _ = (store,[])