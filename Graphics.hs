module Graphics where

import Prelude
import FPPrac.Graphics
import FPPrac.Events
import System.FilePath (splitPath, dropExtension)
import Structure
import Data.List
import Debug.Trace

data Store = Store
  { board             :: Board
  , name              :: String
  , additionalHandler :: Store -> Input -> (Store,[Output])
  , clickedField      :: Maybe Field
  }

boardTop = 250
boardLeft = (-230)
squareTop y = (y * (-50) + boardTop)
squareLeft x = (x * 50 + boardLeft)

initStore ah = Store { board = emptyBoard
                     , name = ""
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

readBoard :: String -> Board
readBoard vs = Board (readBoardR vs 0 0)

readBoardR :: String -> Integer -> Integer -> [Field]
readBoardR []        _ _ = []
readBoardR ('\n':vs) _ r = readBoardR vs 0 (r+1)  -- \n is newline => new row
readBoardR ('\r':vs) c r = readBoardR vs c r      -- ignore \r, to be sure
readBoardR (v:vs)    c r = (Field c r (secCalc c r) allOptions v) : (readBoardR vs (c+1) r)


writeBoard :: Board -> String
writeBoard (Board fs) = unlines $ [
                            concat $ [
                                [ v | f@(Field c r s os v) <- fs, c == cs, r == rs ]
                                | cs <- [0,1..m] 
                            ]
                            | rs <- [0,1..m]
                        ]
    where
        m = ceiling (sqrt (fromIntegral . length $ fs)) - 1

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

--- Create new Sudoku board
handleEvent store (KeyIn 'n')
    = (store', [DrawPicture $ Pictures [drawBoard board', drawBottomLine store]])
    where
        board' = emptyBoard
        store' = store {board=board'}

--- Load Sudoku board
handleEvent store (KeyIn 'l')
    = (store,[GraphPrompt ("Load game", "filename")])

handleEvent store (Prompt ("Load game", filename))
    | filename /= "" = (store, [ReadFile filename (TXTFile "")])
    | otherwise      = (store, [])

--- Actual loading handled by Sudoku Handler
--handleEvent store (File filename (TXTFile input))
--    | input /= "" = (store', [DrawPicture $ Pictures [drawBoard board, drawBottomLine store]])
--    | otherwise   = (store,[])
--    where
--        board' = readBoard input
--        store' = store {board=board',name=filename}

--- Save Sudoku Board
handleEvent store (KeyIn 's')
    | name /= ""  = (store, [SaveFile name (TXTFile savetext)])
    | otherwise   = (store, [GraphPrompt ("Save as", "filename")])
    where
        Store {board=board, name=name} = store
        savetext = writeBoard board

handleEvent store (KeyIn 'a')
    = (store, [GraphPrompt ("Save as", "filename")])

handleEvent store (Prompt ("Save as", filename))
    = (store', [SaveFile filename (TXTFile savetext), DrawPicture $ drawBottomLine store'])
    where
        Store {board=board} = store
        savetext = writeBoard board
        
        store' = store {name=filename}

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
--infoPanel x
--  = PanelCreate 
--    (x, 260, 45
--    ,[]
--    ,[ (1000 , "OK"      , Button     , 0  , 0 , 60, 20)
--     ]
--    )
--    
--infoPanelVisible   = PanelUpdate True  []
--infoPanelInvisible = PanelUpdate False []


-----------------------------------------------------------

doShow ah = installEventHandler "U Kudos (tm)" handleEvent store startPic 10
    where
        store = initStore ah
        Store {board=board} = store
        startPic = Pictures
          [ drawBoard board
          , drawBottomLine store
          ]
