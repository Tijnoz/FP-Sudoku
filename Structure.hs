module Structure where

import Prelude

-- The Type of a Board
data Taip = Normal | Cross
    deriving (Show, Eq)

-- The board
data Board = Board [Field] Taip
    deriving (Show, Eq)

-- A field. The Sector indice is calculated from the col and row and is legacy.
-- Definite is a space when there's no value present.
--                 Col     Row    Sector  Options Definite
data Field = Field Integer Integer Integer [Char] Char  
    deriving (Show, Eq)
--instance Show Field where
 -- show (Field col row sec os def) = show def
instance Ord Field where  -- Order fields based on their options length
  compare (Field ca ra _ osa _) (Field cb rb _ osb _) = if length osa == (length osb)
                                                    then ra `compare` rb 
                                                    else length osa `compare` (length osb)
  
-- Defaults: allOptions contains all options and emptyBoard is the empty board
allOptions = ['1','2','3','4','5','6','7','8','9'] 
emptyBoard = Board [ Field col row (secCalc col row) allOptions ' ' | col <- [0..8], row <- [0..8]] Normal

-- Retrieve the sector based on a col and a row number
secCalc :: Integer -> Integer -> Integer
secCalc col row = (col `div` 3)+(row `div` 3)*3

-- Retrieve the diagonal based on a col and a row number
-- Left-to-Right = 1, Right-To-Left = 2, otherwise = 0
diaCalc :: Integer -> Integer -> Integer
diaCalc col row = if col == row then 1 else if (col+row) == 8 then 2 else 0
bothDia :: Integer -> Integer -> Bool -- True when two diagonals are applicable
bothDia col row = (col == 4 && row == 4)