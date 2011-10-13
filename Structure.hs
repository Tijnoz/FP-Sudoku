module Structure where

import Prelude

data Taip = Normal | Cross
    deriving (Show, Eq)

data Board = Board [Field] Taip
    deriving (Show, Eq)

--                 Col    Row    Sector  Options  Definite
data Field = Field Integer Integer Integer [Char] Char  
    deriving (Show, Eq)
--instance Show Field where
 -- show (Field col row sec os def) = show def
instance Ord Field where
  compare (Field ca ra _ osa _) (Field cb rb _ osb _) = if length osa == (length osb)
                                                    then ra `compare` rb 
                                                    else length osa `compare` (length osb)
  
  
allOptions = ['1','2','3','4','5','6','7','8','9'] 
emptyBoard = Board [ Field col row (secCalc col row) allOptions ' ' | col <- [0..8], row <- [0..8]] Normal

secCalc col row = (col `div` 3)+(row `div` 3)*3