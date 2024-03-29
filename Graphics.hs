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
  , showOptions       :: Bool
  , useBacktracking   :: Bool
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
                     , showOptions = False
                     , useBacktracking = True
                     }
  
-----------------------------------------------------------
-- Redraws the entire window (i.e. a drawBoard and a drawBottomLine)
redraw :: Store -> Picture
redraw store = Pictures $ [drawBoard (showOptions store) (board store), drawBottomLine store]

-- Draws the entire board. The boolean indicates whether all options must be shown
drawBoard :: Bool -> Board -> Picture
drawBoard h board@(Board fields t)
    = Pictures $
        Color white (rectangleSolid 800 564)
      : (map (drawField h board) fields)
      ++ [drawLines]

-- Draws all outer lines. The boolean indicates whether all options must be shown
drawLines :: Picture
drawLines
    = Pictures $
        [ Line [(fromIntegral (squareLeft x), fromIntegral boardTop), (fromIntegral (squareLeft x), fromIntegral (boardTop-9*50))] | x <- [0..9] ]
      ++ [ Line [(fromIntegral boardLeft, fromIntegral (squareTop y)), (fromIntegral (boardLeft+9*50), fromIntegral (squareTop y))] | y <- [0..9] ]
      ++ [ Line [(fromIntegral (squareLeft (x*3) - 1), fromIntegral boardTop), (fromIntegral (squareLeft (x*3) - 1), fromIntegral (boardTop-9*50))] | x <- [1..2] ]
      ++ [ Line [(fromIntegral boardLeft, fromIntegral (squareTop (y*3) + 1)), (fromIntegral (boardLeft+9*50), fromIntegral (squareTop (y*3) + 1))] | y <- [1..2] ]

-- Draws the contents of one field
drawField :: Bool -> Board -> Field -> Picture
drawField h b@(Board _ t) f@(Field x y s os v)
    = Pictures
        [ Translate (x'+25) (y'-25) $ Color (if t == Cross && (x == y || (x+y)==8)
                                             then (makeColor 0.8 0.8 0.8 1.0) 
                                             else (if s `mod` 2 == 0 
                                                   then makeColor 0.95 0.95 0.95 1.0 
                                                   else white))
                                            $ rectangleSolid 50 50
        , Translate (x'+17) (y'-38) $ Scale 0.2 0.2 $ Text [v]
        , Translate (x'+1) (y'-10) $ Scale 0.07 0.07 $ Text (if h then os else "")
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
      , Color black $ Line [(-400,height1),(400,height1)] -- top
      , Color black $ Line [(-300,height1),(-300,-300)] -- left
      , Color black $ Line [(145,height1),(145,-300)] -- right
      , Translate (-394) height2 $ Color blue  $ Scale 0.11 0.11 $ Text $ (name store)
      , Translate (-290) height2 $ Color black $ Scale 0.09 0.09 $ Text "[n]ew [s]ave (a[S]) [l]oad s[o]lve [h]int [x]sudoku op[t]ions [b]acktracking"
      , Translate (61)   height2 $ Color red   $ Scale 0.09 0.09 $ Text (if (useBacktracking store) then "" else  "[b]acktracking") --make backtracking red
      , if not (null (errorMsg store)) -- (process store) == DoingNothing && 
           then Translate 155 height2 $ Color red   $ Scale 0.11 0.11 $ Text (errorMsg store)
           else Translate 155 height2 $ Color black $ Scale 0.11 0.11 $ Text actionText
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
readBoard vs = Board (readBoardR (tail . dropWhile (/='\n') $ vs) 0 0) (if (head . lines $ vs) == "cross" then Cross else Normal)

-- String is remainder, c is next column, r is next row
readBoardR :: String -> Integer -> Integer -> [Field]
readBoardR []        _ _ = []
readBoardR ('\n':vs) _ r = readBoardR vs 0 (r+1)  -- \n is newline => new row
readBoardR ('\r':vs) c r = readBoardR vs c r      -- ignore \r, to be sure
readBoardR (v:vs)    c r = (Field c r (secCalc c r) allOptions v) : (readBoardR vs (c+1) r)

-- Normalizes a board to a string
writeBoard :: Board -> String
writeBoard (Board fs t) = (if t == Cross then "cross\n" else "normal\n")
                          ++
                          (unlines $ [
                              concat $ [
                                  [ v | f@(Field c r s os v) <- fs, c == cs, r == rs ]
                                  | cs <- [0,1..m] 
                              ]
                              | rs <- [0,1..m]
                          ])
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

--- Toggle options
handleEvent store (KeyIn 't') = (store', [DrawPicture $ redraw store'])
    where
        Store {showOptions=showOptions} = store
        store' = store {process=DoingNothing, errorMsg="", showOptions=(not showOptions)}
        
--- Toggle backtracking
handleEvent store (KeyIn 'b') = (store', [DrawPicture $ drawBottomLine store'])
    where
        Store {useBacktracking=useBacktracking} = store
        store' = store {process=DoingNothing, errorMsg="", useBacktracking=(not useBacktracking)}


--- Create new Sudoku board
handleEvent store (KeyIn 'n')
    = (store', [DrawPicture $ (redraw store')])
    where
        Store {board=(Board _ t)} = store
        (Board fs _) = emptyBoard
        board' = Board fs t
        store' = store {board=board', name="", process=DoingNothing, errorMsg=""}

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

--- Mouse click on field
handleEvent store (MouseUp p)
    | oF == Nothing = (store, [])
    | otherwise     = (store', [DrawPicture $ drawBottomLine store']) 
                                   --[GraphPrompt ("Enter new value", "Value for field " ++ show x ++ "," ++ show y)])
    where
        Store {board=board} = store
        Board fields t = board
        oF = onField fields p
        Just (Field x y s os v) = oF
        store' = store {clickedField=onField fields p, process=EnteringValue, errorMsg=""} 

--- Unhandled event handler
handleEvent store input = ah store input
    where
        Store {additionalHandler=ah} = store
        
-----------------------------------------------------------

-- Draw the screen and install the event handler
doShow ah = installEventHandler "Sudo Ku!" handleEvent store startPic 10
    where
        store = initStore ah
        Store {board=board} = store
        startPic = Pictures
          [ drawBoard False board
          , drawBottomLine store
          ]
