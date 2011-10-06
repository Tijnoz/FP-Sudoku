module Structure where

import FPPrac

data Board = Board [Field]
    deriving (Show)

--                 Col    Row    Sector  Options  Definite
data Field = Field Number Number Number [Char] Char  
instance Show Field where
  show (Field col row sec os def) = show def
  
  
allOptions = ['1','2','3','4','5','6','7','8','9'] 
emptyBoard = Board [ Field col row ((col/3)+(row/3)*3) allOptions '.' | col <- [0..8], row <- [0..8]]