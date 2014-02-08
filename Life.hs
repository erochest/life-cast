{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Array.Repa                    hiding (map, (++))
import qualified Data.Array.Repa                    as R
import           Data.Array.Repa.Repr.Vector
import qualified Data.Vector                        as V
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Graphics.Gloss.Raster.Array
import           System.Random.MWC

data Hole = Hole

newtype World a = World { getWorld :: Array V DIM2 a }

data WorldState = Off
                | On
                | Dying
                deriving (Show, Enum, Bounded)

scaleFromInt :: Int -> WorldState
scaleFromInt = toEnum . (`mod` 3)

instance Variate WorldState where
    uniform = return . scaleFromInt <=< uniform
    uniformR (f, t) = return . scaleFromInt <=< uniformR (fromEnum f, fromEnum t)

type LifeWorld = World WorldState

data LifeCast = LifeCast
              { _running :: Bool
              , _world   :: LifeWorld
              }

-- Settings

worldScale :: Int
worldScale = 2

worldWidth :: Int
worldWidth = 500

worldHeight :: Int
worldHeight = 309

-- Lenses

world :: Lens' LifeCast LifeWorld
world f (LifeCast r w) = fmap (LifeCast r) (f w)

running :: Lens' LifeCast Bool
running f (LifeCast r w) = fmap (`LifeCast` w) (f r)

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

cellColor :: WorldState -> Color
cellColor On    = dim red
cellColor Off   = light black
cellColor Dying = dim $ dim blue

colorWorld :: LifeWorld -> Array D DIM2 Color
colorWorld = R.map cellColor . getWorld

-- | Generating the next generation

step :: LifeWorld -> IO LifeWorld
step w = fmap World . computeP . R.traverse (getWorld w) id $ \getter pos ->
      conway (getter pos) . length . filter isAlive . map getter
    $ mooreNeighborhood worldExtent pos
    where worldExtent = extent $ getWorld w

stepCast :: LifeCast -> IO LifeCast
stepCast lc@(LifeCast True  w) = LifeCast True <$> step w
stepCast lc@(LifeCast False _) = return lc

-- Rules

conway :: WorldState -> Int -> WorldState
conway Dying   2 = Off
conway current 2 = current
conway _       3 = On
conway On      _ = Dying
conway _       _ = Off

isAlive :: WorldState -> Bool
isAlive On    = True
isAlive Off   = False
isAlive Dying = False

randomWorld :: Int -> Int -> IO LifeWorld
randomWorld w h = withSystemRandom . asGenIO $ \gen ->
        World . fromVector (ix2 h w)
    <$> (uniformVector gen (w * h) :: IO (V.Vector WorldState))

onEvent :: Event -> LifeCast -> IO LifeCast
onEvent (EventKey (SpecialKey KeySpace) Up _ _) = return . over running not
onEvent _                                       = return

main :: IO ()
main = do
    initial <- LifeCast False <$> randomWorld worldWidth worldHeight
    playArrayIO (InWindow "Life" (tow worldWidth, tow worldHeight) (0, 0))
                (worldScale, worldScale)
                7
                initial
                (return . colorWorld . _world)
                onEvent
                (const stepCast)
    where tow = (*worldScale)

