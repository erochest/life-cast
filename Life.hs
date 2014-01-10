{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Lens
import           Data.Array.Repa hiding (map)
import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed as V
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
leftCol (Z :. w' :. _) (Z :. w :. _)
    | w == 0    = w' - 1
    | otherwise = w  - 1

rightCol :: DIM2 -> DIM2 -> Int
rightCol (Z :. w' :. _) (Z :. w :. _)
    | w == (w' - 1) = 0
    | otherwise     = w + 1

upRow :: DIM2 -> DIM2 -> Int
upRow (Z :. _ :. h') (Z :. _ :. h)
    | h == 0    = h' - 1
    | otherwise = h  - 1

downRow :: DIM2 -> DIM2 -> Int
downRow (Z :. _ :. h') (Z :. _ :. h)
    | h == (h' - 1) = 0
    | otherwise     = h + 1

mooreNeighborhood :: DIM2 -> DIM2 -> [DIM2]
mooreNeighborhood extent pos = do
    x <- [leftCol extent pos .. rightCol extent pos]
    y <- [upRow extent pos   .. downRow extent pos]
    let pos' = ix2 x y
    guard (pos' /= pos)
    return pos'

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
    $ mooreNeighborhood (extent $ getWorld w) pos

stepCast :: LifeCast -> LifeCast
stepCast lc@(LifeCast True  w) = over world step lc
stepCast lc@(LifeCast False _) = lc

-- Rules
conway :: Bool -> Int -> Bool
conway current alive
    | alive == 2 = current
    | alive == 3 = True
    | otherwise  = False

randomWorld :: Int -> Int -> IO LifeWorld
randomWorld w h = withSystemRandom . asGenIO $ \gen ->
        World . fromUnboxed (ix2 w h)
    <$> (uniformVector gen (w * h) :: IO (V.Vector Bool))

onEvent :: Event -> LifeCast -> LifeCast
onEvent (EventKey (SpecialKey KeySpace) Up _ _) = over running not
onEvent _                                       = id

main :: IO ()
main = do
    initial <- LifeCast False <$> randomWorld worldWidth worldHeight
    playArray (InWindow "Life" (worldWidth * 2, worldHeight * 2) (0, 0))
              (2, 2)
              2
              initial
              (colorWorld . _world)
              onEvent
              (const stepCast)

