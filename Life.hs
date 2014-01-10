{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Array.Repa                    hiding ((++), map)
import qualified Data.Array.Repa                    as R
import qualified Data.Vector.Unboxed                as V
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Graphics.Gloss.Raster.Array
import           System.Random.MWC

data Hole = Hole

newtype World a = World { getWorld :: Array U DIM2 a }

type LifeWorld = World Bool

data LifeCast = LifeCast
              { _running :: Bool
              , _world   :: LifeWorld
              }

-- Settings

worldWidth :: Int
worldWidth = 500

worldHeight :: Int
worldHeight = 309

-- Lenses

world :: Lens' LifeCast LifeWorld
world f (LifeCast r w) = fmap (\w' -> LifeCast r w') (f w)

running :: Lens' LifeCast Bool
running f (LifeCast r w) = fmap (\r' -> LifeCast r' w) (f r)

-- | Moore neighborhood

leftCol :: DIM2 -> DIM2 -> Int
leftCol (Z :. _ :. w') (Z :. _ :. w)
    | w == 0    = w' - 1
    | otherwise = w  - 1

rightCol :: DIM2 -> DIM2 -> Int
rightCol (Z :. _ :. w') (Z :. _ :. w)
    | w == (w' - 1) = 0
    | otherwise     = w + 1

upRow :: DIM2 -> DIM2 -> Int
upRow (Z :. h' :. _) (Z :. h :. _)
    | h == 0    = h' - 1
    | otherwise = h  - 1

downRow :: DIM2 -> DIM2 -> Int
downRow (Z :. h' :. _) (Z :. h :. _)
    | h == (h' - 1) = 0
    | otherwise     = h + 1

mooreNeighborhood :: DIM2 -> DIM2 -> [DIM2]
mooreNeighborhood extent pos =
    let [xp, yp] = listOfShape pos
        x1       = leftCol extent pos
        x2       = rightCol extent pos
        y1       = upRow extent pos
        y2       = downRow extent pos
    in  map (uncurry ix2) [ (y1, x1), (yp, x1), (y2, x1)
                          , (y1, xp),           (y2, xp)
                          , (y1, x2), (yp, x2), (y2, x2)
                          ]

-- | Display and colors

cellColor :: Bool -> Color
cellColor True  = dim red
cellColor False = light black

colorWorld :: LifeWorld -> Array D DIM2 Color
colorWorld = R.map cellColor . getWorld

-- | Generating the next generation

step :: LifeWorld -> LifeWorld
step w = World . computeS . R.traverse (getWorld w) id $ \getter pos ->
      conway (getter pos). length . filter id . map getter
    $ mooreNeighborhood worldExtent pos
    where worldExtent = extent $ getWorld w

stepCast :: LifeCast -> LifeCast
stepCast lc@(LifeCast True  _) = over world step lc
stepCast lc@(LifeCast False _) = lc

-- Rules
conway :: Bool -> Int -> Bool
conway current 2 = current
conway _       3 = True
conway _       _ = False

randomWorld :: Int -> Int -> IO LifeWorld
randomWorld w h = withSystemRandom . asGenIO $ \gen ->
        World . fromUnboxed (ix2 h w)
    <$> (uniformVector gen (w * h) :: IO (V.Vector Bool))

onEvent :: Event -> LifeCast -> LifeCast
onEvent (EventKey (SpecialKey KeySpace) Up _ _) = over running not
onEvent _                                       = id

main :: IO ()
main = do
    initial <- LifeCast False <$> randomWorld worldWidth worldHeight
    playArray (InWindow "Life" (tow worldWidth, tow worldHeight) (0, 0))
              (2, 2)
              5
              initial
              (colorWorld . _world)
              onEvent
              (const stepCast)
    where tow = (*2)

