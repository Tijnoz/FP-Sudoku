module Graphics where

import Prelude
import FPPrac.Graphics
import FPPrac.Events
import System.FilePath (splitPath, dropExtension)
import Structure
import Data.List
import Debug.Trace

data Process = EnteringValue | DoingNothing deriving (Eq, Show)

data Store = Store
  { board             :: Board
  , name              :: String
  , additionalHandler :: Store -> Input -> (Store,[Output])
  , clickedField      :: Maybe Field
  , process           :: Process
  , errorMsg          :: String
  }

boardTop = 250
boardLeft = (-230)
squareTop y = (y * (-50) + boardTop)
squareLeft x = (x * 50 + boardLeft)

initStore ah = Store { board = emptyBoard
                     , name = ""
                     , additionalHandler = ah
                     , clickedField = Nothing
                     , process = DoingNothing
                     , errorMsg = ""
                     }
  
-----------------------------------------------------------
redraw :: Store -> Picture
redraw store = Pictures $ [drawBoard (board store), drawBottomLine store]

-- Draws the entire board
drawBoard :: Board -> Picture
drawBoard (Board fields)
    = Pictures $
        Color white (rectangleSolid 800 564)
      : drawLines
      : (map drawField fields)

-- Draws all outer lines
drawLines :: Picture
drawLines
    = Pictures $
        [ Line [(squareLeft x, boardTop), (squareLeft x, boardTop-9*50)] | x <- [0..9] ]
      ++ [ Line [(boardLeft, squareTop y), (boardLeft+9*50, squareTop y)] | y <- [0..9] ]
      ++ [ Line [(squareLeft (x*3) - 1, boardTop), (squareLeft (x*3) - 1, boardTop-9*50)] | x <- [1..2] ]
      ++ [ Line [(boardLeft, squareTop (y*3) + 1), (boardLeft+9*50, squareTop (y*3) + 1)] | y <- [1..2] ]

-- Draws the contents of one field
drawField :: Field -> Picture
drawField f@(Field x y s os v)
    = Pictures
        [ Translate (x'+17) (y'-38) $ Scale 0.2 0.2 $ Text [v]
        , Translate (x'+1) (y'-10) $ Scale 0.07 0.07 $ Text os
        ]
    where
        x' = fromIntegral $ squareLeft x
        y' = fromIntegral $ squareTop y

-- Draws the bottom line
bottomLineHeight = 25
bottomTextHeight = 10
        
drawBottomLine :: Store -> Picture
drawBottomLine store 
    = Pictures 
      [ Translate 0 (-300 + bottomLineHeight / 2) $ Color white $ rectangleSolid 800 bottomLineHeight
      , Color black $ Line [(-400,height1),(400,height1)]
      , Color black $ Line [(-240,height1),(-240,-300)]
      , Color black $ Line [(100,height1),(100,-300)]
      , Translate (-392) height2 $ Color red   $ Scale 0.11 0.11 $ Text $ (name store)
      , Translate (-235) height2 $ Color black $ Scale 0.11 0.11 $ Text "[n]ew; [s]ave; [S]ave as; [l]oad"
      , if (process store) == DoingNothing && not (null (errorMsg store))
           then Translate 120 height2 $ Color red   $ Scale 0.11 0.11 $ Text (errorMsg store)
           else Translate 120 height2 $ Color black $ Scale 0.11 0.11 $ Text actionText
      ]
    where
        height1 = -300 + bottomLineHeight
        height2 = -300 + bottomTextHeight
        
        Just (Field x y s os v) = (clickedField store)
        actionText = (case (process store) of
                      DoingNothing -> "Click field to edit";
                      EnteringValue -> "Enter new value for " ++ (show y) ++ "," ++ (show x)
                     )
-----------------------------------------------------------

-- Reads a board from a string
readBoard :: String -> Board
readBoard vs = Board (readBoardR vs 0 0)

readBoardR :: String -> Integer -> Integer -> [Field]
readBoardR []        _ _ = []
readBoardR ('\n':vs) _ r = readBoardR vs 0 (r+1)  -- \n is newline => new row
readBoardR ('\r':vs) c r = readBoardR vs c r      -- ignore \r, to be sure
readBoardR (v:vs)    c r = (Field c r (secCalc c r) allOptions v) : (readBoardR vs (c+1) r)

-- Normalizes a board to a string
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

-- Returns true when the given coordinates are within the field
inField :: Field -> (Float, Float) -> Bool
inField f@(Field fx fy s os v) (x,y) = xmin <= x && x < xmax && ymin <= y && y < ymax
    where
        xmin = fromIntegral $ squareLeft fx
        xmax = xmin + 50
        ymax = fromIntegral $ squareTop fy
        ymin = ymax - 50

-- Returns either Just Field when the given coordinates are within that field, or Nothing
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
    = (store', [DrawPicture $ (redraw store')])
    where
        board' = emptyBoard
        store' = store {board=board', process=DoingNothing, errorMsg=""}

--- Load Sudoku board
handleEvent store (KeyIn 'l')
    = (store {process=DoingNothing, errorMsg=""}, [GraphPrompt ("Load game", "filename")])

handleEvent store (Prompt ("Load game", filename))
    | filename /= "" = (store {process=DoingNothing, errorMsg=""}, [ReadFile filename (TXTFile "")])
    | otherwise      = (store {process=DoingNothing, errorMsg=""}, [])

--- Actual loading handled by Sudoku Handler
--handleEvent store (File filename (TXTFile input))
--    | input /= "" = (store', [DrawPicture $ Pictures [drawBoard board, drawBottomLine store]])
--    | otherwise   = (store,[])
--    where
--        board' = readBoard input
--        store' = store {board=board',name=filename}

--- Save Sudoku Board
handleEvent store (KeyIn 's')
    | name /= ""  = (store {process=DoingNothing, errorMsg=""}, [SaveFile name (TXTFile savetext)])
    | otherwise   = (store {process=DoingNothing, errorMsg=""}, [GraphPrompt ("Save as", "filename")])
    where
        Store {board=board, name=name} = store
        savetext = writeBoard board

handleEvent store (KeyIn 'S')
    = (store {process=DoingNothing, errorMsg=""}, [GraphPrompt ("Save as", "filename")])

handleEvent store (Prompt ("Save as", filename))
    = (store', [SaveFile filename (TXTFile savetext), DrawPicture $ drawBottomLine store'])
    where
        Store {board=board} = store
        savetext = writeBoard board
        
        store' = store {name=filename, process=DoingNothing, errorMsg=""}

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

-- Draw the screen and install the event handler
doShow ah = installEventHandler "U Kudos (tm)" handleEvent store startPic 10
    where
        store = initStore ah
        Store {board=board} = store
        startPic = Pictures
          [ drawBoard board
          , drawBottomLine store
          ]
