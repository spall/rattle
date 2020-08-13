{-# LANGUAGE ScopedTypeVariables #-}

module Test.Type(
    root,
    ignoreIO,
    assertWithin,
    (===),
    meetup,
    testGit
    ) where

import Development.Rattle
import Development.Shake.Command
import System.Time.Extra
import Control.Monad
import Control.Concurrent.QSemN
import Control.Exception.Extra
import System.Directory


root :: FilePath
root = "../.."

ignoreIO :: IO () -> IO ()
ignoreIO act = catch act $ \(_ :: IOException) -> pure ()


assertWithin :: Seconds -> IO a -> IO a
assertWithin n act = do
    t <- timeout n act
    case t of
        Nothing -> assertFail $ "Expected to complete within " ++ show n ++ " seconds, but did not"
        Just v -> pure v

assertFail :: String -> IO a
assertFail msg = errorIO $ "ASSERTION FAILED: " ++ msg

assertBool :: Bool -> String -> IO ()
assertBool b msg = unless b $ assertFail msg

infix 4 ===

(===) :: (Show a, Eq a) => a -> a -> IO ()
a === b = assertBool (a == b) $ "failed in ===\nLHS: " ++ show a ++ "\nRHS: " ++ show b


-- | The action blocks until N people have reached, then everyone unblocks
meetup :: Int -> IO (IO ())
meetup n = do
    sem <- newQSemN 0
    pure $ do
        signalQSemN sem 1
        waitQSemN sem n
        signalQSemN sem n


testGit :: String -> Run () -> IO ()
testGit url run = do
    b <- doesDirectoryExist ".git"
    if b then cmd "git fetch" else cmd_ "git clone" url "."
    forM_ [10,9..0] $ \i -> do
        cmd_ "git reset --hard" ["origin/master~" ++ show i]
        rattleRun rattleOptions run
