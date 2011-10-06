module Graphics where

import FPPrac
import FPPrac.Graphics
import FPPrac.Events
import System.FilePath (splitPath, dropExtension)
import Structure
import Data.List

data Store = Store
  { board             :: Board
  , additionalHandler :: Store -> Input -> (Store,[Output])
  }

boardTop = 250
boardLeft = (-230)
squareTop y = (y * (-50) + boardTop)
squareLeft x = (x * 50 + boardLeft)
  
-----------------------------------------------------------
-- Draws one field
drawBoard :: Board -> Picture
drawBoard (Board fields)
    = Pictures $
        Color white (rectangleSolid 800 564)
      : drawLines
      : (map drawField fields)

drawLines :: Picture
drawLines
    = Pictures $
        [ Line [(squareLeft x, boardTop), (squareLeft x, boardTop-9*50)] | x <- [0..9] ]
      ++ [ Line [(boardLeft, squareTop y), (boardLeft+9*50, squareTop y)] | y <- [0..9] ]
      ++ [ Line [(squareLeft (x*3) - 1, boardTop), (squareLeft (x*3) - 1, boardTop-9*50)] | x <- [1..2] ]
      ++ [ Line [(boardLeft, squareTop (y*3) + 1), (boardLeft+9*50, squareTop (y*3) + 1)] | y <- [1..2] ]
      
drawField :: Field -> Picture
drawField f@(Field x y s os v)
    = Pictures
        [ Translate (x'+17) (y'-40) $ Scale 0.2 0.2 $ Text [v]
        , Translate (x'+1) (y'-10) $ Scale 0.07 0.07 $ Text os
        ]
    where
        x' = fromIntegral $ squareLeft x
        y' = fromIntegral $ squareTop y
        
drawBottomLine :: Store -> Picture
drawBottomLine store 
    = Pictures []
        
-----------------------------------------------------------
-- Event handler
handleEvent :: Store -> Input -> (Store, [Output])

--- Unhandled event handler
handleEvent store input = ah store input
    where
        Store {additionalHandler=ah} = store

-----------------------------------------------------------
initStore ah = Store { board = emptyBoard
                     , additionalHandler = ah
                     }

doShow ah = installEventHandler "U Kudos (tm)" handleEvent store startPic 10
    where
        store = initStore ah
        Store {board=board} = store
        startPic = Pictures
          [ drawBoard board
          , drawBottomLine store
          ]
