
module Original(originalSched) where

import Sequential
import Speculate
import Types
import Data.Functor.Identity
-- rattle's original scheduling policy

-- this scheduler is a combination of 2 schedulers.
-- What to try first; then what to try in case of failure

originalSched :: State -> IO State
originalSched t = do
  st <- speculateSched t
  case st of
    (State _ _ _ (Hazard h _) _) -> case seqSched t of -- throw away failed speculatedrun
                                   (Identity st) -> return st
    x -> return x