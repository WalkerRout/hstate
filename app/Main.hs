module Main where

import Control.Monad.State
import Control.Monad.Trans.Reader

newtype World = World { counter :: Int } deriving (Show, Eq, Ord)

readerComp :: ReaderT World (State World) Int
readerComp = do
  world <- ask
  return $ (counter world) + 1
  
stateComp ::  State World Int
stateComp = do
  world <- get
  put $ world { counter = (counter world) + 1}
  world <- get
  value <- runReaderT readerComp world
  return $ value + 1

main :: IO ()
main = do
  let initWorld = World {counter = 0}
  print $ runState stateComp initWorld

{-
 
stateComp ::  State World Int
stateComp = do
  world <- get
  put $ world { counter = (counter world) + 1}
  world <- get
  value <- (flip runReaderT) world $ do
    world <- ask
    return $ (counter world) + 1
  return $ value + 1

-}
