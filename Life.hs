{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- Hi. Today I want to live code a version of Conway's Life.
--
-- This is meant to illustrate one of the more interesting aspects of
-- coding in Haskell: some of the different ways to code that its
-- type system enables. This is sometimes referred to as TDD ---
-- which here means *type-driven development*. I'll also practice
-- a little of what's called *hole-driven development*. This involves
-- leaving holes in my program and relying on the compiler to tell
-- me how to fill them. This will be a lot easier when GHC 7.8 has
-- been released (it'll include the TypeHoles extension), but it's
-- still possible to do now.
--
-- For type-driven development, you start with your types. You
-- try to express as many of your program's requirements in the
-- type system so that you can rely on the compiler to check them
-- and to make sure you're abiding by those requirements. I don't
-- expect this program to really showcase that well, but it can
-- still be useful to start thinking about your program from the
-- types that you'll need, and working outward from there.
--
-- Initially, I'll probably define the types of the functions and
-- just stub them out using undefined. This is a value in Haskell
-- that has two properties: first, it can be any time, so the
-- compiler will be happy. However, when it's executed, it causes
-- an exception. So it's kind of a combination of null from other
-- languages and a bomb.
--
-- Once the compiler is happy with the types, then I'll fill in the
-- implementation.
--
-- Let's get started.

-- Wait. Is that it? Apparently so. Let's give it a try.
--
-- Sorry you won't be able to see this. I'll report back here, and
-- I hope to make a screencast of the working program.
--
-- It appears to work. It's a little big though.
--
-- Mostly works. I appear to have gotten the height and width
-- swapped around at some point (it's square). Also, it's a little
-- slow, so I'll probably try to parallelize it.
--
-- Since I don't use Repa that much, there's almost certainly a 
-- better/faster way to do it.
--
-- Anyway, that's enough for right now. You can check out the
-- github repo for this at github.com/erochest/life-cast for
-- the latest copy.

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

-- First, we need to think about what the world will look like.
-- This defines a light-weight, but type-safe wrapper type around
-- a Repa array. This means that we should get a little automatic
-- parallelization. We'll see.
--
-- Let's try something. (Actually, I don't have a lot of experience with
-- Repa, so we're going to be trying lots of things. Deal with it. :) )
-- The world should be 2-dimensional. I think that Repa lets us enforce
-- that at the type level.
--
-- I was trying to be all fancy and general. Let's simplify a little.
-- We won't need that anyway.
--
-- That took care of it.
newtype World a = World { getWorld :: Array U DIM2 a }

-- There we go. Now we have a two-dimensional world, enforced at the
-- type level by the compiler.

-- The `World` type has the feature that it doesn't actually specify
-- what populates the world. We could use this type for extensions of
-- Conway's life that involves more than two possible states per cell.
-- However, we won't do that here, so we'll create a light-weight
-- type alias for a World populated by booleans. Each cell can be either
-- on or off.
type LifeWorld = World Bool

-- We'll want a little more state for the actual application. We'll let
-- the user turn the simulation on and off, so we'll need something
-- to track whether the simulation is running or not. Because this
-- is a little more stateful than the rest, we'll define this using
-- the lens library also. (However, because this is a very simple
-- data type and I don't want to enable TemplateHaskell, I'll define
-- the lenses myself.)
data LifeCast = LifeCast
              { _running :: Bool
              , _world   :: LifeWorld
              }

-- Oops. That's a great example of why refactoring in Haskell is
-- so great. The compiler just told me that I need to swap the
-- order of the arguments in these two functions. I swapped
-- how they're defined in the data structure and their order
-- in the constructor. Now I need to change how they're used.
world :: Lens' LifeCast LifeWorld
world f (LifeCast r w) = fmap (\w' -> LifeCast r w') (f w)

running :: Lens' LifeCast Bool
running f (LifeCast r w) = fmap (\r' -> LifeCast r' w) (f r)

-- Hmm. Having the World newtype and the world here may be awkward.
-- I may need to revisit that decision later. Fortunately,
-- refactoring like that is pretty easy.

-- The next thing that we need to worry about is the shape of the
-- world. Specifically, how do we handle the edges. There are several
-- reasonable ways to do that. For this, we'll say that the world
-- is a torus. This means that the top edge wraps around to the bottom,
-- and the left edge wraps around to the right. Let's define some
-- functions that will do that.
--
-- Each of these functions will take a cell and index of the col or row
-- that neighbors it in that direction.

-- This will be a little complicated. Well, maybe not.
-- A little destructuring, though.

-- Oops. We need to have more information before we can answer
-- these questions. Such as, what's the size of the world.

-- Yep, just need to make sure the type checker was enforcing
-- everything.

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

-- We're going to get a little fancy. The List monad is meant for
-- computations that can return multiple values. That's exactly
-- what this does.
mooreNeighborhood :: DIM2 -> DIM2 -> [DIM2]
mooreNeighborhood extent pos = do
    x <- [leftCol extent pos .. rightCol extent pos]
    y <- [upRow extent pos   .. downRow extent pos]
    let pos' = ix2 x y
    guard (pos' /= pos)
    return pos'

-- We'll that's cheating, but we'll fill it in later.

-- We'll also need something that will map from the Bool's to a Color.
-- So that the contrast isn't quite so stark, we'll use gloss's
-- functions to munge the colors a little. (Oh, I don't think I've
-- mentioned that we'll use gloss for the graphics. I was going to
-- try to do something on the console, but gloss is just so easy.)
--
-- The implementation of this seems pretty straightforward, so I'll
-- go ahead and fill it in now.
cellColor :: Bool -> Color
cellColor True  = dim red
cellColor False = light black

colorWorld :: LifeWorld -> Array D DIM2 Color
colorWorld = R.map cellColor . getWorld

-- Now we're getting down to the nitty-gritty. The function that
-- computes the next generation we'll just call step. It basically
-- transforms a world.
--
-- We could parameterize this pretty heavily, but we'll save that
-- until we actually need it.
--
-- This is the heart of the program. Let's break down what needs
-- to happen.
step :: LifeWorld -> LifeWorld
step w = World . computeS . R.traverse (getWorld w) id $ \getter pos ->
      conway (getter pos). length . filter id . map getter
    $ mooreNeighborhood (extent $ getWorld w) pos

-- That's not in parallel. To do that, we'll need to move it into
-- the IO monad. Maybe in a minute.

-- Sorry. Thinking.
-- I need a spinner.

    -- 1. x walk over the world, for each cell
        -- a. get the neighborhood
        -- b. count the number of activated cells
        -- c. set the cell based on the rules of the game.

-- traverse returns a delayed array, now we need to force evaluation.
-- Hang on.

-- Let's do the rules.
conway :: Bool -> Int -> Bool
conway current alive
    | alive == 2 = current
    | alive == 3 = True
    | otherwise  = False

-- OK. What this does depends on the value of _running.
-- Crap. Naming bites again.
stepCast :: LifeCast -> LifeCast
stepCast lc@(LifeCast True  w) = over world step lc
stepCast lc@(LifeCast False _) = lc

-- I think that we have all the types stubbed out. Let's wire up
-- the (undefined) functions to Gloss and see how that works out.

-- We'll need some values first.
worldWidth :: Int
worldWidth = 500

worldHeight :: Int
worldHeight = 309

-- We'll fill this in momentarily.
--
-- This will get minorly complicated. I could work it out in my
-- head as I go, but that's why we have computers. Let's use a bit
-- of Hole-driven developement. First (since I'm still using GHC 7.6)
-- I'll need a type to help, of course.
randomWorld :: Int -> Int -> IO LifeWorld
randomWorld w h = withSystemRandom . asGenIO $ \gen ->
        World . fromUnboxed (ix2 w h)
    <$> (uniformVector gen (w * h) :: IO (V.Vector Bool))

-- I know I want to use uniformVector to create the vector.
--
-- I'll be pulling the value out of the monad that it's in, transforming
-- it, and putting it back into the monad so I can return it. fmap
-- does that.
--
-- What returns a `LifeWorld`? Just `World`.
--
-- Now I want a function that returns an Array. `fromUnboxed` should
-- work nicely. Oh, wait. I forgot its parameters.
--
-- I think I need an explicit type to help the compiler. This doesn't
-- happen often.
--
-- Finally, I'll clean it up a little.

-- OK. That all compiles. Let's see what types and functions I'll
-- need to work with to get this responding to events. Hang on
-- while I check the documentation :O
--
-- That was pretty terse. Lens helped a lot here. Basically, this just
-- toggles the _running property whenever the space bar is let up.
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

-- Where should I write now?
--
-- The types are now in place. Let's go in and start implementing some of
-- the smaller, easier parts.
