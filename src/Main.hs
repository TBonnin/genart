{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow
import Control.Monad.Random
import Control.Monad.Reader
import Data.Foldable (for_)
import Data.List ()
import Data.Time.Clock.POSIX
import Graphics.Rendering.Cairo
import Linear.V2
import qualified Numeric.Noise.Perlin as P

data World = World
  { worldWidth :: Int
  , worldHeight :: Int
  , worldSeed :: Int
  , worldScale :: Double
  }

data Quad = Quad
  { aa :: V2 Double
  , ab :: V2 Double
  , ba :: V2 Double
  , bb :: V2 Double
  , ca :: V2 Double
  , cb :: V2 Double
  , da :: V2 Double
  , db :: V2 Double
  , alpha :: Double
  } deriving (Eq, Ord)

type Generate a = RandT StdGen (ReaderT World Render) a

-- Lift a Cairo action into a Generate action
cairo :: Render a -> Generate a
cairo = lift . lift

getSize :: Num a => Generate (a, a)
getSize = do
  (w, h) <- asks (worldWidth &&& worldHeight)
  pure (fromIntegral w, fromIntegral h)

getScale :: Num a => Generate a
getScale = do
  s <- asks worldWidth
  pure (fromIntegral s)

fillScreen :: (Double -> Render a) -> Double -> Generate ()
fillScreen color opacity = do
  (w, h) <- getSize
  cairo $ do
    rectangle 0 0 w h
    color opacity *> fill

rgb :: Double -> Double -> Double -> Double -> Render ()
rgb r g b = setSourceRGBA (r / 256) (g / 256) (b / 256)

background :: Double -> Render ()
background = rgb 255 255 255

black :: Double -> Render ()
black = rgb 0 0 0

renderLines :: [V2 Double] -> Render ()
renderLines (V2 x y:V2 x' y':vs) = do
  moveTo x y
  lineTo x' y'
  renderLines vs
renderLines [(V2 x y)] = do
  lineTo x y
renderLines [] = pure ()

quadAlpha :: Quad -> Double
quadAlpha Quad {..} = alpha

renderQuad :: Quad -> Render ()
renderQuad Quad {..} = renderLines [aa, ab, ba, bb, ca, cb, da, db]

genQuads :: Int -> Generate [Quad]
genQuads n =
  replicateM n $ do
    (w, h) <- getSize @Double
    let margin = 20
        minSize = 25
        maxSize = 40
    x <- getRandomR (0, (w - minSize))
    y <- getRandomR (0, (h - minSize))
    a <- getRandomR (0.1, 1.0 :: Double)
    size <- getRandomR (minSize, min (w - (max x y) - margin) maxSize)
    pure $
      Quad
        (V2 x y)
        (V2 (x + size) y)
        (V2 (x + size) y)
        (V2 (x + size) (y + size))
        (V2 (x + size) (y + size))
        (V2 x (y + size))
        (V2 x (y + size))
        (V2 x y)
        a

quadStrokes :: Quad -> Generate [Quad]
quadStrokes Quad {..} =
  replicateM 8 $ do
    offsets <- getRandomRs (-2, 2)
    alphaOffset <- getRandomR ((-0.2), 0.2 :: Double)
    pure $
      Quad
        (aa + V2 (offsets !! 0) (offsets !! 1))
        (ab + V2 (offsets !! 2) (offsets !! 3))
        (ba + V2 (offsets !! 4) (offsets !! 5))
        (bb + V2 (offsets !! 6) (offsets !! 7))
        (ca + V2 (offsets !! 8) (offsets !! 9))
        (cb + V2 (offsets !! 10) (offsets !! 11))
        (da + V2 (offsets !! 12) (offsets !! 13))
        (db + V2 (offsets !! 14) (offsets !! 15))
        (alpha + alphaOffset)

draw :: Generate ()
draw = do
  fillScreen background 1.0
  c <- getRandomR (50, 80)
  quads <- fmap concat (genQuads c >>= traverse quadStrokes)
  cairo $ do
    setLineWidth 0.5
    for_ quads $ \quad -> do
      renderQuad quad
      black (quadAlpha quad) *> stroke

main :: IO ()
main = do
  seed <- round . (* 1000) <$> getPOSIXTime
  let stdGen = mkStdGen seed
      height = 300
      width = 300
      scaleAmount = 4
      scaledWidth = round $ fromIntegral width * scaleAmount
      scaledHeight = round $ fromIntegral height * scaleAmount
  let world = World width height seed scaleAmount
  surface <- createImageSurface FormatARGB32 scaledWidth scaledHeight
  renderWith surface . flip runReaderT world . flip runRandT stdGen $ do
    cairo $ scale scaleAmount scaleAmount
    draw
  putStrLn "Generating..."
  surfaceWriteToPNG surface $
    "./images/" <> show seed <> "-" <> show (round scaleAmount :: Int) <> ".png"
  surfaceWriteToPNG surface "./images/latest.png"
