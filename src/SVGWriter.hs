{-# LANGUAGE OverloadedStrings #-}
module SVGWriter where

import Graphics.Svg

import Data.Text.Internal
import qualified Data.Map as M
import qualified Data.List as L
import Data.String
import Data.Ratio
import Data.Maybe

-- import System.IO
-- import System.Directory
import System.FilePath.Posix

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
actorST' :: (RealFloat a, Eq l, Show l)
  => a -> a -> a
  -> [(l, a, a)]
  -> a
  -> DFNode l
  -> (a, Element)
actorST' tx sx h simTable y node = (y', element)
  where
    nlabel = label node
    (lastY, element) = actorST tx y sx h nlabel firings'
    y' = lastY -- y + (maxAi+1) * h
    firings = filter (\(lbl,_,_) -> lbl == nlabel) simTable   -- filter all firings from the current node
    firings' = map (\(_,st,et) -> (st, et)) firings                 -- get only start and end times of every firing


-- actorsST converts a schedule from the simTable to an SVG element, if one actor fires multiple times in parallel, different lanes are used to print it
-- x: x-coordinate of upper left corner of the schedule, this includes the text column with node labels
-- y: y-coordinate of upper left corner of the schedule, this includes the text column with node labels
-- h: height of each row
-- endX: x-coordinate of end, nodes with the start time before endX will be drawn
-- simTable: the simulation table for which a schedule must be drawn, containing tuples with: (label, startTime, endTime) of every firing instance
-- ns : nodes of the graph
-- clStepSize: step size of the column lines.
actorsST :: (Enum a, RealFloat a, Eq l, Show l) => a -> a -> a -> a
  -> [(l, a, a)]
  -> [DFNode l]
  -> a
  -> (a, Element)
actorsST x y h endX simTable ns clStepSize = (lastY, element)
  where
    (lastYColumnLines, columnElem) = columnLines sx y th endX 0 clStepSize
    (lastYSchedule, actorElems) = L.mapAccumL (actorST' x sx h simTable) y ns
    element = mconcat (columnElem:actorElems)
    th = lastYSchedule + h - y
    lastY = lastYColumnLines
    sx = x + (fromIntegral $ maximum $ map (length . show) (map label ns))     -- start periods at x + maximum label length


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
svgStrictlyPeriodicSchedule :: (Enum a, RealFloat a, Show l, Ord l)
  => a -> a -> a -> a
  -> Graph [DFNode l] [DFEdge l]
  -> (a, Element)
svgStrictlyPeriodicSchedule x rowHeight endX y graph
  | isNothing mmap' = (fontSize, txt x y fontSize "start" (name graph ++ " - No Strictly Periodic Schedule"))
  | otherwise =       (lastY,    txt x y fontSize "start" (name graph ++ " - Strictly Periodic Schedule")
                                 <> scheduleElement
                      )
  where
    (lastY, scheduleElement) = actorsST startX startY rowHeight endX simTable (nodes graph) clStepSize
    fontSize = 0.8 * scalar * rowHeight
    startX = x
    startY = y + rowHeight -- after text

    mmap' = strictlyPeriodicScheduleWithExTime (graphMap graph)
    mmap = fromJust mmap'
    simTable = spsMmapToSimTable mmap (endX / scalar)      -- convert the mmap to simTable, we only need to schedule up to the width of the canvas
    clStepSize = maximum $ M.elems $ M.map (\(_,p,_) -> ratioToFrac p) mmap -- step size of the column lines is largest period (they are probably all the same)


-- creates an svg Element and the lastY
-- x: x-coordinate of upper left corner of schedule element (including the text)
-- y: y-coordinate of upper left corner of schedule element (including the text)
-- rowHeight: the height of each row in the schedule
-- endX: x-coordinate of right side of the schedule
-- graph: the graph
svgSelfTimedSchedule :: (Enum a, RealFloat a, Show l, Ord l)
  => a -> a -> a -> a
  -> Graph [DFNode l] [DFEdge l]
  -> (a, Element)
svgSelfTimedSchedule x rowHeight endX y graph'
  | isNothing mcr = (fontSize, txt x y fontSize "start" (name graph' ++ " - Deadlock"))
  | otherwise =     (lastY,    txt x y fontSize "start" (name graph' ++ " - Self Timed Schedule")
                               <> scheduleElement
                    )
  where
    -- Map graph to mapped graph
    graph = graphMap graph'

    ns = nodes graph'
    (lastY, scheduleElement) = actorsST startX startY rowHeight endX simTable ns clStepSize
    fontSize = 0.8 * scalar * rowHeight
    startX = x
    startY = y + rowHeight -- after text

    (mcr, _) = maxCycleRatio $ singleRateApx graph
    ticks = div (fromIntegral $ round endX) (fromIntegral $ round scalar)  -- number of ticks to be sufficient to fill the entire canvassize
    simTableST = concat $ snd $ selfTimedSchedule graph ticks                     -- simTable comming from selfTimedSchedule function
    simTable = map (\(lbl, _, stI, etI) -> (lbl, fromInteger stI, fromInteger etI)) simTableST -- remove periodCount, change Integers to RealFrac
    clStepSize = max 1 $ fromIntegral $ maximum $ concat $ map wcet ns


drawST :: (Show l, Ord l)
  => String
  -> [Graph [DFNode l] [DFEdge l]]
  -> IO ()
drawST dirname graphs = do
  writeFile (joinPath [dirname, filename]) (show $ svg canvasWidth canvasHeight stElem)
  where
    canvasWidth = 1920
    canvasHeight = lastY * scalar
    endX = canvasWidth
    startX = 4
    startY = 4
    rowHeight = 2
    filename = "svg.svg"

    (lastY, stElems) = L.mapAccumL (svgSelfTimedSchedule startX rowHeight endX) startY graphs
    stElem = mconcat stElems

drawSPS :: (Show l, Ord l)
  => String
  -> [Graph [DFNode l] [DFEdge l]]
  -> IO ()
drawSPS dirname graphs = do
  writeFile (joinPath [dirname, filename]) (show $ svg canvasWidth canvasHeight spsElem)
  where
    canvasWidth = 1920
    canvasHeight = lastY*scalar
    endX = canvasWidth
    startX = 4
    startY = 4
    rowHeight = 2
    filename = "svg.svg"

    (lastY, spsElems) = L.mapAccumL (svgStrictlyPeriodicSchedule startX rowHeight endX) startY graphs
    spsElem = mconcat spsElems

scheduleElems :: (Enum a, RealFloat a, Show l, Ord l)
  => a -> a -> a -> a
  -> Graph [DFNode l] [DFEdge l]
  -> (a, Element)
scheduleElems startX rowHeight endX startY graph = (lastY, spsElem <> stElem)
  where
    (lastYSPS, spsElem) = svgStrictlyPeriodicSchedule startX rowHeight endX startY graph
    (lastY   , stElem ) = svgSelfTimedSchedule startX rowHeight endX lastYSPS graph


drawSchedules :: (Show l, Ord l)
  => String
  -> [Graph [DFNode l] [DFEdge l]]
  -> IO ()
drawSchedules dirname graphs = do
  writeFile (joinPath [dirname, filename]) (show $ svg canvasWidth canvasHeight elem)
  where
    canvasWidth = 1920
    canvasHeight = lastY*scalar
    endX = canvasWidth
    startX = 4
    startY = 4
    rowHeight = 2
    filename = "svg.svg"

    (lastY, elems) = L.mapAccumL (scheduleElems startX rowHeight endX) startY graphs
    elem = mconcat elems

-- For translation between listed and mapped graphs
graphMap :: (Ord l)
  => Graph [DFNode l]           [DFEdge l]
  -> Graph (M.Map l (DFNode l)) [DFEdge l]
graphMap (Graph name ns e) = Graph name (M.fromList $ map (\n -> (label n, n)) ns) e

