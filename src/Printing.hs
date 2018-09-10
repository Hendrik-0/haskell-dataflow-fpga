module Printing where
-- not used yet
import qualified Data.Map as M
import Data.Ratio
import Lucid.Html5

--printSchedule :: Show l => Maybe (M.Map l (Ratio Integer, Ratio Integer)) -> IO ()

--printHTML :: Show l => Maybe (M.Map l (Ratio Integer, Ratio Integer)) -> String
printHTML Nothing = [""]
printHTML (Just mmap) = concat $ replicate nrRows col where
  col = "\t<tr>":replicate nrCols "\t\t<th></th>" ++ ["\t</tr>"]
  nrRows = 3  -- length mmap
  nrCols = 4  -- ceiling $ maximum $ map snd $ M.elems mmap

path = "./tmp/test1.html"

createHTML mmap = writeFile path file where
  fileHeader = ["<!DOCTYPE html>","<html>","<body>", "<table>"]
  fileBody = printHTML mmap
  fileEnd = ["</table>", "</body>", "</html>"]
  file = unlines $ fileHeader ++ fileBody ++ fileEnd




--l = table_ [rows_ "2"] (tr_ (do td_ [class_ "top",colspan_ "2",style_ "color:red"] (p_ "Hello, attributes!") td_ "yay!"))

