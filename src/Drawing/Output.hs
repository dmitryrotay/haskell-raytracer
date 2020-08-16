module Drawing.Output
    ( canvasToPpm    
    ) where

import qualified Data.Text as T
import Drawing

maxLineLength = 70

canvasToPpm :: Canvas -> String
canvasToPpm c = T.unpack $ T.append (buildPpmHeader c) (buildPpmData c)

buildPpmHeader :: Canvas -> T.Text
buildPpmHeader (Canvas w h _) =
    T.pack "P3\n" `T.append`
    (T.pack . show $ w) `T.append`
    T.pack " " `T.append`
    (T.pack . show $ h) `T.append`
    T.pack "\n255\n"

buildPpmData :: Canvas -> T.Text
buildPpmData (Canvas w _ ps) = buildPpmPixels w ps

buildPpmPixels :: Int -> [Color] -> T.Text
buildPpmPixels w ps = 
    let (text, _, _) = foldl (\(t, writtenChars, i) ct -> let (t', l, newLine) = formattedComponentText ct writtenChars w i
                                                              writtenChars' = if newLine then l else writtenChars + l
                                                          in (t `T.append` t', writtenChars', i + 1)) (T.pack "", 0, 0) (concat [colorToComponentsTexts p | p <- ps])
    in text `T.append` T.pack "\n"

formattedComponentText :: T.Text -> Int -> Int -> Int -> (T.Text, Int, Bool)
formattedComponentText text writtenChars w componentIndex =
    let first = componentIndex == 0
        newLineByWidth = (componentIndex `mod` (w * 3) == 0 && not first)
        spacePrefix = not first && not newLineByWidth
        extraLength = if spacePrefix then 1 else 0
        newLineByMaxLimit = writtenChars + T.length text + extraLength > maxLineLength
        prefix = if newLine then "\n" else if first then "" else " "
        text' = T.pack prefix `T.append` text
        newLine = newLineByWidth || newLineByMaxLimit
    in (T.pack prefix `T.append` text, T.length text' - if newLine then 1 else 0, newLine)

colorToComponentsTexts :: Color -> [T.Text]
colorToComponentsTexts (Color r g b) =
    map componentText [r, g, b]

componentText :: Float -> T.Text
componentText c = T.pack . show . componentValue $ c

componentValue :: Float -> Int
componentValue c = round . min 255 . max 0 $ c * 255
