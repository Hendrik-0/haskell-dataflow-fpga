{-# LANGUAGE OverloadedStrings #-}
module SVGWriter where

import Graphics.Svg

import Data.Text.Internal
import qualified Data.Map as M
import Data.String
import Data.Ratio
import Data.Maybe

import Hardware
import DataFlow
import Graph

scalar = 20
leftMargin = 100
topMargin = 50
asOffset = 4 -- the space between the different actor schedules in the graph

svg :: RealFloat a => a -> a -> Element -> Element
svg w h content =
     doctype
  <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- toText w, Height_ <<- toText h]

contents :: Element
contents =
     --rect_   [ X_ <<- "20", Y_ <<- "20",  Width_ <<- "10", Height_ <<- "10", "blue" ->> Fill_]
     periodicRects 0 0  (0  ,50 ) 10 10 1200
--  <> periodicRects 0 20 (160,200) 40 10 1200
--  <> circle_ [ Cx_ <<- "150", Cy_ <<- "100", R_ <<- "80", Fill_ <<- "green"]
--  <> text_   [ X_ <<- "150", Y_ <<- "125", Font_size_ <<- "60", Text_anchor_ <<- "middle", Fill_ <<- "white"] "SVG"
--  <> txt 0 10 10 "habla"
--  <> txt 0 60 50 "test"
--  <> actor 10 50 "*" (0,50) 10 15 1200


-- rect arguments
-- x: x-coordinate of top left corner
-- y: y-coordinate of top left corner
-- w: width
-- h: height
-- c: color
rect :: RealFloat a => a -> a -> a -> a -> Data.Text.Internal.Text -> Element
rect x y w h c = rect_ [X_ <<- toText x
                      , Y_ <<- toText y
                      , Width_ <<- toText w
                      , Height_ <<- toText h
                      , Stroke_ <<- "black"
                      , c ->> Fill_
                      ]

-- periodicRects arguments
-- x: x-coordinate
-- y: y-coordinate
-- s: start time
-- p: period
-- w: width of rectangle
-- h: hight of rectangle
-- m: length of pattern
periodicRects :: (Enum a, RealFloat a) => a -> a -> (a, a) -> a -> a -> a -> Element
periodicRects x y (s,p) w h m = mconcat [rect x' y w h "green"| x' <- [s',(s'+p)..m]]
  where
    s' = x + s

-- txt arguments (anchor point is left down cornor)
-- x: x-coordinate of the anchor point
-- y: y-coordinate of the anchor point
-- fontSize: size of text font
-- t: String to print
txt :: (RealFloat a) => a -> a -> a -> String -> Element
txt x y fontSize t = text_ [ X_ <<- toText x
                           , Y_ <<- toText y
                           , Font_size_ <<- toText fontSize
                           , Text_anchor_ <<- "start"
                           ] (toElement t)

-- line arguments (line between point 1 and 2)
-- x1: x-coordinate of point 1
-- y1: y-coordinate of point 1
-- x2: x-coordinate of point 2
-- y2: y-coordinate of point 2
-- s: stroke pattern
-- c: color of line
line :: (RealFloat a) => a -> a -> a -> a -> a -> Data.Text.Internal.Text -> Element
line x1 y1 x2 y2 s c = line_ [X1_ <<- toText x1
                            , Y1_ <<- toText y1
                            , X2_ <<- toText x2
                            , Y2_ <<- toText y2
                            , Stroke_ <<- c
                            , Stroke_width_ <<- "2"
                            , Stroke_dasharray_ <<- toText s
                            ]

-- vline arguments
-- x: x-coordinate of left upper corner of schedule (excluding text)
-- y: y-coordinate of left upper corner of schedule (excluding text)
-- l: length of lines
-- s: stroke pattern
-- m: length of schedule pattern
-- p: period between lines
-- c: color
vlines :: (Enum a, RealFloat a) => a -> a -> a -> a -> a -> a -> Data.Text.Internal.Text -> Element
vlines x y l s m p c = mconcat [line x' y1 x' y2 s c
  | let y1 = y
  , let y2 = y+l
  , x' <- [x,x+p..m]]

-- actor arguments
-- x: x-coordinate
-- y: y-coordinate
-- la: actor label
-- s: start time
-- p: period
-- w: execution time (width of rectangle)
-- fontSize: fontSize
-- m: length of pattern
actor :: (RealFloat a, Enum a) => a -> a -> String -> (a,a) -> a -> a -> a -> Element
actor x y la (s,p) w h m = 
  txt x' (y+0.8*fontSize) fontSize la -- y + fontsize because anchor point is left down corner
  <> periodicRects x y (s,p) w h m
  where
    x' = x - fontSize * (fromIntegral $ length la)
    fontSize = h


-- actors arguments  
-- x: x-coordinate of left upper corner of schedule
-- y: y-coordinate of left upper corner of schedule
-- h: hight of schedule blocks
-- m: length of schedule pattern
-- mmap: strict periodic schedule as M.Map with node label as key, and (start time, period, execution time) for each node as element
actors :: (Show l, RealFloat a, Enum a) => a -> a -> a -> a -> M.Map l (Ratio Integer, Ratio Integer, Integer) -> Element
actors x y h m mmap = mconcat [actor x y' (show l) (s',p') ex' h m
  | ((l,(s,p,ex)),y') <- zip (M.toList mmap) [y,(y+h+ao)..]
  , let s'  = scal * ((fromInteger $ numerator s) / (fromInteger $ denominator s))
  , let p'  = scal * ((fromInteger $ numerator p) / (fromInteger $ denominator p))
  , let ex' = if ex == 0 then 1 else scal * (fromInteger ex) -- if execution time is 0, print a small line (1)
  ]
  where
    ao = fromIntegral asOffset
    scal = fromIntegral scalar


--svgSchedule Nothing     = writeFile "../schedules/svg.svg" (show $ svg $ clocklines 50 100)
--svgSchedule (Just mmap) = writeFile "../schedules/svg.svg" (show $ svg $ (clocklines 50 1200) <> (actors 60 60  1200 mmap))

svgSchedule graph
  | isJust mmap' = writeFile path (show $ svg m l $ actors x y h m mmap <> clocklines <> periodLines )
  | otherwise    = writeFile path (show $ svg m l $ txt 0 50 50 "No schedule")
  where
    clocklines = vlines x y l 5.0 m clkP "red"
    periodLines = vlines x y l 2.0 m tP  "blue"

    l = (fromIntegral $ (asOffset + length mmap))*h
    h = 30
    x = leftMargin*2
    y = topMargin
    m = 1920
    mmap' = strictlyPeriodicScheduleWithExTime graph
    mmap = fromJust mmap'
    Just (clkP',tP') = clock graph
    clkP = (fromIntegral scalar) * (fromInteger $ numerator clkP') / (fromInteger $ denominator clkP')
    tP   = (fromIntegral scalar) * (fromInteger $ numerator tP'  ) / (fromInteger $ denominator tP'  )
    path = "../schedules/svg.svg"
--    Just (mcr, _) = clock graph


--main :: IO ()
--main = do
--  print $ svg contents