module Sudoku where

import FPPrac
import FPPrac.Events
import Debug.Trace
import Structure
import Graphics

    

printBoard (Board fs) = [ [ fs !! ((x)+(s*3)+(i*9)) | s <-[0..2], x <- [1..3]] | i <- [0..9] ]


--setSquare (Board fs) sector position new = 

--getRow (Board fs) row

--getColomn (Board fs) col

--getSector (Board fs) sec



main = doShow sudokuHandler

-- Event handler
sudokuHandler :: Store -> Input -> (Store,[Output])

--- Unhandled event handler
sudokuHandler store _ = (store,[])