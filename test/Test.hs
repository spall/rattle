{-# LANGUAGE TupleSections #-}

module Test(main) where

import Control.Monad
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import General.Paths

import qualified Test.Limit
import qualified Test.Simple

tests =
    ["limit" * Test.Limit.main
    ,"simple" * Test.Simple.main
    ]
    where
        name * act = (name,) $ do
            let dir = "output" </> name
            createDirectoryIfMissing True dir
            withCurrentDirectory dir act

main = do
    initDataDirectory
    args <- getArgs
    case args of
        [] ->
            forM_ tests $ \(name, act) -> do
                putStrLn $ "\n# Test " ++ name
                act
        name:args
            | Just act <- lookup name tests -> do
                putStrLn $ "\n# Test " ++ name
                withArgs args act
        _ -> do
            putStrLn $ "Unknown arguments, expected one of\n  " ++ unwords (map fst tests)
            exitFailure
