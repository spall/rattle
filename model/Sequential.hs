{-# LANGUAGE RecordWildCards, TupleSections #-}

module Sequential(seqSched) where

import Types
import Scheduler
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import Data.List
import Data.Functor.Identity
import Debug.Trace as Trace

-- Decides whether to start a cmd or finish a cmd
seqOracle :: State -> Identity Action
seqOracle (State tr pr r _ t) = if isSomethingDone r t
                                then return Finished 
                                else return Wait

pickCmd :: State -> Cmd
pickCmd State{..} = f toRun running $ fst done
  where f [] xs d = error $ "state: " ++ show d
        f (t:ts) [] d | inTree t d = f ts [] d
                      | otherwise = t
        f (t:ts) (x:xs) d | t == x = f ts xs d
                          | inTree t d = f ts (x:xs) d
                          | otherwise = t

-- continues to take a step until a hazard is encountered or the build is done; nothing running or to run
seqSched :: State -> Identity (Either State Hazard)
seqSched = sched seqOracle pickCmd
