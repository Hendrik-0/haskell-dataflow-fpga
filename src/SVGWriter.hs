{-# LANGUAGE OverloadedStrings #-}
module SVGWriter where

import Graphics.Svg

import Data.Text.Internal
import qualified Data.Map as M
import qualified Data.List as L
import Data.String
import Data.Ratio
import Data.Maybe

import Hardware
import DataFlow
import Graph

import Debug.Trace

scalar :: RealFloat a => a
scalar = 15

svg :: RealFloat a => a -> a -> Element -> Element
svg w h content =
     doctype
  <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- toText w, Height_ <<- toText h]

-- contents :: Element
-- contents =
--      --rect_   [ X_ <<- "20", Y_ <<- "20",  Width_ <<- "10", Height_ <<- "10", "blue" ->> Fill_]
--      periodicRects 0 0  (0  ,50 ) 10 10 1200
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
-- rect :: RealFloat a => a -> a -> a -> a -> Data.Text.Internal.Text -> Element
rect x y w h c = rect_ [X_ <<- toText (scalar * x)
                      , Y_ <<- toText (scalar * y)
                      , Width_ <<- toText (scalar * w)
                      , Height_ <<- toText (scalar * h)
                      , Stroke_ <<- "black"
                      , Stroke_width_ <<- toText (h/15*scalar)
                      , c ->> Fill_
                      ]


-- periodicRects arguments
-- x: x-coordinate
-- y: y-coordinate
-- s: start time
-- p: period
-- w: width of rectangle
-- h: hight of rectangle
-- endX: length of pattern
-- color: color of rectangles
-- periodicRects :: (Enum a, RealFloat a) => a -> a -> (a, a) -> a -> a -> a -> Data.Text.Internal.Text -> Element
periodicRects x y (s,p) w h endX color
  = mconcat [rect x' y w h color | x' <- [s',(s' + p)..endX]]
  where
    s' = (x + s) -- start x + the start time is the start of the printing

-- txt arguments (anchor point is left down cornor)
-- x: x-coordinate of the anchor point
-- y: y-coordinate of the anchor point
-- fontSize: size of text font
-- text: String to print
-- anchor = anchor in this case is as follows:
-- "start" : upper left corner
-- "middle" : top middle
-- "end": upper right corner
txt :: (RealFloat a) => a -> a -> a -> Data.Text.Internal.Text -> String -> Element
txt x y fontSize anchor text
  = text_ [ X_ <<- toText (scalar * x)
          , Y_ <<- toText (scalar * y + fontSize)
          , Font_size_ <<- toText fontSize
          , Text_anchor_ <<- anchor
          ] (toElement text)


-- line arguments (line between point 1 and 2)
-- x1: x-coordinate of point 1
-- y1: y-coordinate of point 1
-- x2: x-coordinate of point 2
-- y2: y-coordinate of point 2
-- s: stroke pattern
-- c: color of line
line :: (RealFloat a) => a -> a -> a -> a -> a -> Data.Text.Internal.Text -> Element
line x1 y1 x2 y2 s c = line_ [X1_ <<- toText (scalar * x1)
                            , Y1_ <<- toText (scalar * y1)
                            , X2_ <<- toText (scalar * x2)
                            , Y2_ <<- toText (scalar * y2)
                            , Stroke_ <<- c
                            , Stroke_width_ <<- toText 2
                            , Stroke_dasharray_ <<- toText s
                            ]


-- lineWithText (line between point 1 and 2 with text at the bottom)
-- x1: x-coordinate of point 1
-- y1: y-coordinate of point 1
-- x2: x-coordinate of point 2
-- y2: y-coordinate of point 2
-- s: stroke pattern
-- c: color of line
-- fontSize: size of text font
-- t: String to print
lineWithText :: RealFloat a => a -> a -> a -> a -> a -> Data.Text.Internal.Text -> a -> String -> Element
lineWithText x1 y1 x2 y2 s c fontSize text =
  txt x1 y2 fontSize "middle" text
  <> line x1 y1 x2 y2 s c


-- columnLines: Lines with numbers spanning between start and end
-- x: the X of the upper left corner of the raster
-- y: the Y of the upper left corner of the raster
-- h: the height of the raster (length of lines
-- endX: the X of the end of the raster
-- scale: scale the visual space between the raster lines, and the height of the raster
-- startCount: the starting number of the columnLines
-- stepSize: the step size between the numbers
columnLines :: (Enum a, RealFloat a) => a -> a -> a -> a -> a -> a -> (a, Element)
columnLines x y h endX startCount stepSize
  = (y+h+1, -- line has length h, starts at y, and fontsize, so +1
    mconcat [lineWithText x1 y1 x2 y2 s c fontSize t'
            | (x,t) <- zip [x, (x + stepSize)..endX] [startCount, stepSize..]
            , let x1 = x
            , let x2 = x1
            , let y1 = y
            , let y2 = y + h
            , let fontSize = 0.8*scalar
            , let c = "black"
            , let s = (scalar / 15)*2
            , let t' = show (round t)
            ]
  )

{-
-- vline arguments
-- x: x-coordinate of left upper corner of schedule (excluding text)
-- y: y-coordinate of left upper corner of schedule (excluding text)
-- l: length of lines
-- s: stroke pattern
-- m: length of schedule pattern
-- p: period between lines
-- c: color
vlines :: (Enum a, RealFloat a) => a -> a -> a -> a -> a -> a -> Data.Text.Internal.Text -> Element
vlines x y l s m p c = mconcat  [ line x' y1 x' y2 s c
                                | let y1 = y
                                , let y2 = y+l
                                , x' <- [x,x+p..m]
                                ]
-}


-- actorST arguments, ST because of sim table
-- tx: start x of text column
-- y: y-coordinate
-- sx: start x coordinate of the scheduling printing blocks
-- h: height of row
-- endX: length of text+pattern
-- text: actor label
-- firings: list of tuples with (startTime, endTime) of every fire instance
-- produces an Element containing the textbox with label, and one or more lanes with the schedule.
-- because the drawing could be on multiple lanes, the function also provides the amount of lanes needed
actorST :: (RealFloat a, Show l) => a -> a -> a -> a -> l -> [(a, a)] -> (a, Element)
actorST tx y sx h text firings
  = ( lastY
    , txt tx y (0.8*h*scalar) "start" (show text)
  <> mconcat [rect x y' w' h "green" | (st,et,laneNr) <- coordinateList       -- start time, end time, lane number from coordinate list
                                    , let x = sx + st                         -- start x of schedule + start time actor
                                    , let y' = y + h * (fromIntegral laneNr)  -- y of schedule + the number of parallel firings at that specific instance
                                    , let w = et - st                         -- width = end time - start time actor
                                    , let w' = if w == 0 then 1/scalar else w -- if execution time is 0, print small line
                                    ]
    )
  where
    coordinateList = foldl findFirstAvailLaneNr [] firings            -- list with tuple containg start time, end time, and lane number of all the actor firings
    largestLaneNr = maximum $ map (\(_,_, ln) -> ln) coordinateList   -- the amount of lanes used for this actor
    lastY = y + h * (1+fromIntegral largestLaneNr) -- the amount of lanes needed for this actor + 1 times the heigth is the lastY

    findFirstAvailLaneNr simTable (cst, cet) = simTable ++ [(cst, cet, firstLaneNotTaken)]
      where
        activeFirings = filter (\(st, et, ln) -> cst >= st && cst < et) simTable  -- alread active firings
        takenLanes = map (\(_,_, ln) -> ln) activeFirings                         -- numbers of the lanes that are already taken
        firstLaneNotTaken = head ([0..] L.\\ takenLanes)                          -- remove the taken lanes from the list of all lanes


-- actorST' arguments, helper function, ST = sim table
-- tx: start of the text column
-- h: height of each row
-- simTable: simulation table, continging a list of tuples with (label, startTime, endTime)
-- y: starting coordinate, if schedule spans over multiple lanes, the y' is the resulting y coordinate
-- node: node to draw the schedule for
actorST' :: (RealFloat a, Nodes n, Eq l, Show l) => a -> a -> a -> [(l, a, a)] -> a -> n l -> (a, Element)
actorST' tx sx h simTable y node = (y', element)
  where
    (lastY, element) = actorST tx y sx h (label node) firings'
    y' = lastY -- y + (maxAi+1) * h
    firings = filter (\(lbl,_,_) -> lbl == (label node)) simTable   -- filter all firings from the current node
    firings' = map (\(_,st,et) -> (st, et)) firings                 -- get only start and end times of every firing


-- actorsST converts a schedule from the simTable to an SVG element, if one actor fires multiple times in parallel, different lanes are used to print it
-- x: x-coordinate of upper left corner of the schedule, this includes the text column with node labels
-- y: y-coordinate of upper left corner of the schedule, this includes the text column with node labels
-- h: height of each row
-- endX: x-coordinate of end, nodes with the start time before endX will be drawn
-- simTable: the simulation table for which a schedule must be drawn, containing tuples with: (label, startTime, endTime) of every firing instance
-- ns : nodes of the graph
-- clStepSize: step size of the column lines.
actorsST :: (Show k, Enum a, RealFloat a, DFNodes n, Eq l, Show l) => a -> a -> a -> a -> [(l, a, a)] -> M.Map k (n l) -> a -> (a, Element)
actorsST x y h endX simTable ns clStepSize = (lastY, element)
  where
    (lastYColumnLines, columnElem) = columnLines sx y th endX 0 clStepSize
    (lastYSchedule, actorElems) = L.mapAccumL (actorST' x sx h simTable) y (M.elems ns)
    element = mconcat (columnElem:actorElems)
    th = lastYSchedule + h - y
    lastY = lastYColumnLines
    sx = x + (fromIntegral $ maximum $ map (length . show) (M.keys ns))     -- start periods at x + maximum label length


-- converts a ratio to a Fractional
ratioToFrac :: Fractional a => Ratio Integer -> a
ratioToFrac rt = (fromInteger $ numerator rt) / (fromInteger $ denominator rt)


-- converts a mmap to a simTable
-- mmap: (an sps schedule), containing (startTime, period, execution time) as parametes with node label as key.
spsMmapToSimTable :: (Enum a, Ord a, Fractional a)
  => M.Map k (Ratio Integer, Ratio Integer, Integer) -> a -> [(k, a, a)]
spsMmapToSimTable mmap endX = simTable
  where
    simTable = concat $ M.elems $ M.mapWithKey nodeFirings mmap
    nodeFirings lbl (startTime, period, exTime)
      = [(lbl, st, et)  | st <- [sx, (sx + p)..endX], let et = st + (fromInteger exTime), st >= 0]
      where
        -- endX' = scalar * endX
        p  = ratioToFrac period --  (fromInteger $ numerator period) / (fromInteger $ denominator period)
        sx = ratioToFrac startTime -- (fromInteger $ numerator startTime) / (fromInteger $ denominator startTime)


-- creates an svg Element and the lastY
-- x: x-coordinate of upper left corner of schedule element (including the text)
-- y: y-coordinate of upper left corner of schedule element (including the text)
-- rowHeight: the height of each row in the schedule
-- endX: x-coordinate of right side of the schedule
-- graph: the graph
svgStrictlyPeriodicSchedule :: (DFEdges e, DFNodes n, Enum a, RealFloat a, Show l, Ord l, Eq (e l))
  => a -> a -> a -> a -> Graph (M.Map l (n l)) [e l] -> (a, Element)
svgStrictlyPeriodicSchedule x y rowHeight endX graph
  | isNothing mmap' = (fontSize, txt x y fontSize "start" "No Strictly Periodic Schedule")
  | otherwise =       (lastY,    txt x y fontSize "start" "Strictly Periodic Schedule"
                                 <> scheduleElement
                      )
  where
    (lastY, scheduleElement) = actorsST startX startY rowHeight endX simTable (nodes graph) clStepSize
    fontSize = 0.8 * scalar*rowHeight
    startX = x
    startY = y + rowHeight -- after text

    mmap' = strictlyPeriodicScheduleWithExTime graph
    mmap = fromJust mmap'
    simTable = spsMmapToSimTable mmap (endX / scalar)      -- convert the mmap to simTable, we only need to schedule up to the width of the canvas
    clStepSize = maximum $ M.elems $ M.map (\(_,p,_) -> ratioToFrac p) mmap -- step size of the column lines is largest period (they are probably all the same)


-- creates an svg Element and the lastY
-- x: x-coordinate of upper left corner of schedule element (including the text)
-- y: y-coordinate of upper left corner of schedule element (including the text)
-- rowHeight: the height of each row in the schedule
-- endX: x-coordinate of right side of the schedule
-- graph: the graph
svgSelfTimedSchedule :: (Enum a, RealFloat a, DFNodes n, Show l, Ord l)
  => a -> a -> a -> a -> Graph (M.Map l (n l)) [DFEdge l] -> (a, Element)
svgSelfTimedSchedule x y rowHeight endX graph
  | isNothing mcr = (fontSize, txt x y fontSize "start" "Deadlock")
  | otherwise =     (lastY,    txt x y fontSize "start" "Self Timed Schedule"
                               <> scheduleElement
                    )
  where
    ns = nodes graph
    (lastY, scheduleElement) = actorsST startX startY rowHeight endX simTable ns clStepSize
    fontSize = 0.8 * scalar*rowHeight
    startX = x
    startY = y + rowHeight -- after text

    (mcr, _) = maxCycleRatio $ singleRateApx graph
    ticks = div (fromIntegral $ round endX) (fromIntegral $ round scalar)  -- number of ticks to be sufficient to fill the entire canvassize
    simTableST = concat $ snd $ selfTimedSchedule graph ticks                     -- simTable comming from selfTimedSchedule function
    simTable = map (\(lbl, _, stI, etI) -> (lbl, fromInteger stI, fromInteger etI)) simTableST -- remove periodCount, change Integers to RealFrac
    clStepSize = max 1 $ fromIntegral $ maximum $ concat $ map wcet (M.elems ns)


drawST :: (DFNodes n, Show l, Ord l)
  => Graph (M.Map l (n l)) [DFEdge l] -> IO ()
drawST graph = writeFile path (show $ svg canvasWidth canvasHeight stElem)
  where
    canvasWidth = 1920
    canvasHeight = lastY*scalar
    endX = canvasWidth
    startX = 4
    startY = 4
    rowHeight = 2
    path = "../schedules/svg.svg"
    (lastY, stElem) = svgSelfTimedSchedule startX startY rowHeight endX graph


drawSPS :: (DFEdges e, DFNodes n, Show l, Ord l, Eq (e l))
  => Graph (M.Map l (n l)) [e l] -> IO ()
drawSPS graph = writeFile path (show $ svg canvasWidth canvasHeight spsElem)
  where
    canvasWidth = 1920
    canvasHeight = lastY*scalar
    endX = canvasWidth
    startX = 4
    startY = 4
    rowHeight = 2
    path = "../schedules/svg.svg"
    (lastY, spsElem) = svgStrictlyPeriodicSchedule startX startY rowHeight endX graph


drawSchedules :: (DFNodes n, Show l, Ord l)
  => Graph (M.Map l (n l)) [DFEdge l] -> IO ()
drawSchedules graph = writeFile path (show $ svg canvasWidth canvasHeight elem)
  where
    canvasWidth = 1920
    canvasHeight = lastYST*scalar
    endX = canvasWidth
    startX = 4
    startYSPS = 4
    startYST = lastYSPS + rowHeight
    rowHeight = 2
    path = "../schedules/svg.svg"
    (lastYSPS, spsElem) = svgStrictlyPeriodicSchedule startX startYSPS rowHeight endX graph
    (lastYST , stElem) = svgSelfTimedSchedule startX startYST rowHeight endX graph
    elem = spsElem <> stElem


{- Below is the previous code for printing an SVG schedule of strictly periodic graphs, but the schedule is never printed in different lanes, meaning that firings will overlap if multiple firings are happening in parrallel

-- actorP arguments, P because periodic actor
-- tx: start x of text box
-- y: y-coordinate
-- sx: start x coordinate of the scheduling printing blocks
-- h: height of row
-- text: actor label
-- startTime: start time of actor
-- period: period of actor
-- exTime:  execution time (width of rectangle)
-- endX: length of text+pattern
actorP :: (RealFloat a, Enum a) => a -> a -> a -> a -> a -> String -> (a,a) -> a -> Element
actorP tx y sx h endX text (startTime,period) exTime
  =  txt tx y (0.8*h*scalar) "start" text
  <> periodicRects sx y (startTime,period) exTime h endX "green"


-- actorsP arguments, P because periodic actors
-- x: x-coordinate of left upper corner of schedule
-- y: y-coordinate of left upper corner of schedule
-- h: hight of schedule blocks
-- endX: length of schedule pattern
-- mmap: strict periodic schedule as M.Map with node label as key, and (start time, period, execution time) for each node as element
actorsP :: (Show l, RealFloat a, Enum a) => a -> a -> a -> a -> M.Map l (Ratio Integer, Ratio Integer, Integer) -> Element
actorsP x y h endX mmap
  = columnLines sx y th endX 0 tp'
  <> mconcat [actorP x y' sx h endX (show l) (st,p') ex'
  | ((l,(s,p,ex)),y') <- zip (M.toList mmap) [y,(y+h)..]
  , let p'  = ((fromInteger $ numerator p) / (fromInteger $ denominator p))
  , let s'  = ((fromInteger $ numerator s) / (fromInteger $ denominator s))
  , let st = if s' < 0 then s' + p' else s'
  , let ex' = if ex == 0 then 1 else (fromInteger ex) -- if execution time is 0, print a small line (1) TODO: this is not small line anymore
  ]
  where
    sx = x + (fromIntegral $ maximum $ map (length . show) (M.keys mmap)) -- start periods at x + maximum label length
    th = h * (fromIntegral $ M.size mmap + 1) -- size +1 so some extra space beneath
    tp = maximum $ map (\(_,p,_) -> p) $ M.elems mmap -- max period of all actors
    tp' = (fromInteger $ numerator tp) / (fromIntegral $ denominator tp) -- from Ratio to Rational


svgStrictlyPeriodicSchedule :: (Show l, DFEdges e, Ord l, DFNodes n, Eq (e l))
  => Graph (M.Map l (n l)) [e l] -> IO ()
svgStrictlyPeriodicSchedule graph
  | isNothing mmap' = writeFile path (show $ svg canvasWidth canvasHeight $ txt startX startY 40 "start" "No schedule")
  | otherwise = writeFile path (show $ svg canvasWidth canvasHeight $ actorsP startX startY height endX mmap)
  where
    -- Important: everything is scaled by the scaler defined in this file
    canvasHeight = 1000
    canvasWidth = 1920
    height = 2 -- height of each row
    startX = 4
    startY = 4
    endX = canvasWidth

    mmap' = strictlyPeriodicScheduleWithExTime graph
    mmap = fromJust mmap'

    path = "../schedules/svg.svg"
-}