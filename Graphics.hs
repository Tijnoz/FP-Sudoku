module Graphics where

import Prelude
import FPPrac.Graphics
import FPPrac.Events
import System.FilePath (splitPath, dropExtension)
import Structure
import Data.List

data Store = Store
  { board             :: Board
  , additionalHandler :: Store -> Input -> (Store,[Output])
  , clickedField      :: Maybe Field
  }

boardTop = 250
boardLeft = (-230)
squareTop y = (y * (-50) + boardTop)
squareLeft x = (x * 50 + boardLeft)

initStore ah = Store { board = emptyBoard
                     , additionalHandler = ah
                     , clickedField = Nothing
                     }
  
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
        [ Translate (x'+17) (y'-38) $ Scale 0.2 0.2 $ Text [v]
        , Translate (x'+1) (y'-10) $ Scale 0.07 0.07 $ Text os
        ]
    where
        x' = fromIntegral $ squareLeft x
        y' = fromIntegral $ squareTop y
        
drawBottomLine :: Store -> Picture
drawBottomLine store 
    = Pictures []

-----------------------------------------------------------

inField :: Field -> (Float, Float) -> Bool
inField f@(Field fx fy s os v) (x,y) = xmin <= x && x < xmax && ymin <= y && y < ymax
    where
        xmin = fromIntegral $ squareLeft fx
        xmax = xmin + 50
        ymax = fromIntegral $ squareTop fy
        ymin = ymax - 50

onField :: [Field] -> (Float, Float) -> Maybe Field
onField [] p = Nothing
onField (f:fs) p
    | inField f p = Just f
    | otherwise   = onField fs p

-----------------------------------------------------------
-- Event handler
handleEvent :: Store -> Input -> (Store, [Output])

--handleEvent store (MouseUp p) = (store, [infoPanel r, infoPanelVisible])
--    where
--        Store {board=board} = store
--        Board fields = board
--        oF = onField fields p
--        Just (Field x y s os v) = onField fields p
--       
--        r = if oF == Nothing then "Nothing"
--        else "Field " ++ (show x) ++ "," ++ (show y) ++ " sector " ++ (show s) ++ " value " ++ (show v)
--        
--handleEvent store (Panel 1000 _) = (store,[infoPanelInvisible])

--- Unhandled event handler
handleEvent store input = ah store input
    where
        Store {additionalHandler=ah} = store

-----------------------------------------------------------

-- Panel
infoPanel x
  = PanelCreate 
    (x, 260, 45
    ,[]
    ,[ (1000 , "OK"      , Button     , 0  , 0 , 60, 20)
     ]
    )
    
infoPanelVisible   = PanelUpdate True  []
infoPanelInvisible = PanelUpdate False []


-----------------------------------------------------------

doShow ah = installEventHandler "U Kudos (tm)" handleEvent store startPic 10
    where
        store = initStore ah
        Store {board=board} = store
        startPic = Pictures
          [ drawBoard board
          , drawBottomLine store
          ]
