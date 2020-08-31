module Drawing.Output
    ( canvasToPpm    
    ) where

import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Drawing (Canvas (..), Color (..))

maxLineLength = 70

canvasToPpm :: Canvas -> String
canvasToPpm canvas = buildPpmHeader canvas ++ buildPpmData canvas

buildPpmHeader :: Canvas -> String
buildPpmHeader (Canvas width height _) =
    "P3\n" ++ show width  ++ " " ++ show height ++ "\n255\n"

buildPpmData :: Canvas -> String
buildPpmData (Canvas width _ pixels) =
    let canvasPixelsRows = chunksOf width pixels
        maxLineChunks = map (intercalate "\n" . map unwords . lineChunks . concatMap colorToComponentsTexts) canvasPixelsRows
    in intercalate "\n" maxLineChunks ++ "\n"

lineChunks :: [String] -> [[String]]
lineChunks [] = []
lineChunks xs = 
    let (chunk, rest) = lineChunk' xs
    in chunk : lineChunks rest

lineChunk :: [String] -> [String] -> Int -> ([String], [String])
lineChunk [] chunk len = (chunk, [])
lineChunk (x:xs) chunk len =
    let nextLen = len + length x
    in if nextLen > maxLineLength
       then (chunk, x:xs)
       else lineChunk xs (chunk ++ [x]) (len + length x + 1)

lineChunk' :: [String] -> ([String], [String])
lineChunk' xs = lineChunk xs [] 0

colorToComponentsTexts :: Color -> [String]
colorToComponentsTexts (Color r g b) =
    map componentText [r, g, b]

componentText :: Float -> String
componentText = show . componentValue

componentValue :: Float -> Int
componentValue c = round . min 255 . max 0 $ c * 255
