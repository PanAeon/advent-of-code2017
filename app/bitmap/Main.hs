{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Lib
import Codec.Picture
import Codec.Picture.Types
-- import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Storable as V
import qualified Data.Vector as DV
import Control.Monad(void)
-- import System.Process
import Day21

greyScaleWitness ::  Image Pixel8
greyScaleWitness  = img 2187 2187
    where img w h = Image w h $ V.fromListN (w * h) $ pixels w h
          pixels w h = [ pixel ch | ch <- DV.toList (iterateN 18) ]
          pixel '#' =  255
          pixel '.' =  0



-- exportBmpWitness :: IO ()
-- exportBmpWitness = writeBitmap "wintess.bmp" $ img 232 241
--     where img w h = array ((0,0), (w - 1, h - 1)) $ pixels w h
--           pixels w h = [((x,y), pixel x y) | y <- [0 .. h-1], x <- [0 .. w-1] ]
--           pixel x y = PixelRGBA8 128 (fromIntegral x) (fromIntegral y) 255
main :: IO ()
main = do
         writeBitmap "foo.bmp" $ (greyScaleWitness)
         -- openViewer "foo.bmp"
