{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

module Benchmark.VsMake(
    VsMake(..), vsMake, Args(..),
    ) where

import Benchmark.Args
import System.Directory.Extra
import Development.Rattle
import Development.Shake.Command
import System.Time.Extra
import Control.Exception
import System.FilePath
import Data.List
import Data.IORef
import Data.Maybe
import System.IO.Extra
import Control.Monad.Extra
import qualified Data.List as List


data VsMake = VsMake
    {repo :: String
    ,generate :: IO String
    ,generateVersion :: Int
    ,clean :: IO ()
    ,broken :: [String]
    ,configure :: Maybe CmdArgument
    ,make :: CmdArgument
    ,rattle :: CmdArgument
    ,master :: String
    }

gitCheckoutCommit :: VsMake -> String -> IO String
gitCheckoutCommit VsMake{..} c = do
  Stdout x <- cmd "git reset --hard" [c]
  return $ words x !! 4

gitCheckout :: VsMake -> Int -> IO String
gitCheckout VsMake{..} i = do
    Stdout x <- cmd "git reset --hard" ["origin/" ++ master ++ "~" ++ show i]
    -- HEAD is now at 41fbba1 Warning
    pure $ words x !! 4


generateName :: VsMake -> String -> IO FilePath
generateName VsMake{..} commit = do
    tdir <- getTemporaryDirectory
    pure $ tdir </> takeBaseName repo ++ "." ++ commit ++ "." ++ show generateVersion ++ ".txt"


timed :: IORef Seconds -> String -> Int -> String -> IO () -> IO ()
timed ref msg j commit act = do
    (t, _) <- duration act
    appendFile ("/home/spall/rattle-stuff/" ++ msg ++ ".vsmake.log") $ intercalate "\t" [msg, show j, show commit, show t] ++ "\n"
    putStrLn $ msg ++ " " ++ show j ++ " (" ++ show commit ++ ") = " ++ showDuration t
    modifyIORef' ref (+ t)

vsMake :: VsMake -> Args -> IO ()
vsMake vs@VsMake{..} Args{..} = withTempDir $ \dir -> do
    let proj = takeBaseName repo
    let counted = maybe id take count
    let commitList = reverse [0..fromMaybe 10 commits]

    let checkout i act = do
            commit <- gitCheckout vs i
            when (commit `notElem` broken) $
                flip onException (putStrLn $ "AT COMMIT " ++ commit) $
                    act commit
    let stderr = [EchoStderr False | no_stderr]

    let checkoutCommit c act = do
          commit <- gitCheckoutCommit vs c
          when (commit `notElem` broken) $
                flip onException (putStrLn $ "AT COMMIT " ++ commit) $
                    act commit
    
    withCurrentDirectory dir $ do
        -- don't pass a depth argument, or git changes the length of the shown commit hashes
        -- which messes up caching and broken hashes
        cmd_ "git clone" repo "."

        -- generate all the Rattle files
        when ("generate" `elemOrNull` step) $ do
            putStrLn "GENERATING RATTLE SCRIPTS"
            if isJust commitsList
              then do
              forM_ (counted $ fromJust commitsList) $ \c -> do
                putChar '.' >> hFlush stdout
                checkoutCommit c $ \commit -> do
                  file <- generateName vs commit
                  unlessM (doesFileExist file) $ do
                    res <- generate
                    evaluate $ length res
                    writeFile file res
              putStrLn ""
              else do
              forM_ (counted commitList) $ \i -> do
                putChar '.' >> hFlush stdout
                checkout i $ \commit -> do
                  file <- generateName vs commit
                  unlessM (doesFileExist file) $ do
                    res <- generate
                    evaluate $ length res
                    writeFile file res
              putStrLn ""


        -- for different levels of parallelism
        forM_ (threads `orNull` [1..4]) $ \j -> do
            makeTime <- newIORef 0
            rattleTime <- newIORef 0

            let buildMake f ls = forM_ ls $ \x ->
                  f x $ \i ->
                  timed makeTime (proj ++ " make") j i $ cmd_ make ["-j" ++ show j] (EchoStdout False) stderr


            let buildRattle f ls = forM_ ls $ \x ->
                  f x $ \commit -> do
                  file <- generateName vs commit
                  cmds <- lines <$> readFile' file
                  let opts = rattleOptions{rattleProcesses=j, rattleUI=Just RattleQuiet, rattleNamedDirs=[], rattleShare=False}
                  timed rattleTime (proj ++ " rattle") j commit $ rattleRun opts $ forM_ cmds $ cmd rattle stderr

            let deleteFiles = do
                  (Stdout ls) :: (Stdout String)
                              <- cmd "ls -a"
                  cmd_ Shell "rm -rf " $ List.delete ".." $ List.delete "." $ words ls

            -- first build with make
            when ("make" `elemOrNull` step) $ do
              putStrLn "BUILDING WITH MAKE"
              clean
              if isJust commitsList
                then do
                let ls = counted $ fromJust commitsList
                if isJust configure
                  then do
                  checkoutCommit (head ls) $ \i -> do
                    cmd_ configure (EchoStdout False) stderr
                    timed makeTime (proj ++ " make") j i $ cmd_ make ["-j" ++ show j] (EchoStdout False) stderr
                  buildMake checkoutCommit $ tail ls
                  else do
                  buildMake checkoutCommit ls
                else do
                let ls = counted $ commitList ++ [0]
                if isJust configure
                  then do
                  checkout (head ls) $ \i -> do
                    cmd_ configure (EchoStdout False) stderr
                    timed makeTime (proj ++ " make") j i $ cmd_ make ["-j" ++ show j] (EchoStdout False) stderr
                  buildMake checkout $ tail ls
                  else do
                  buildMake checkout ls

            -- now with rattle
            when ("rattle" `elemOrNull` step) $ do
                putStrLn "BUILDING WITH RATTLE"
                cmd_ Shell "rm -rf ./*"
                deleteFiles
                cmd_ "git clone" repo "."

                if isJust commitsList
                  then do
                  let ls = counted $ fromJust commitsList
                  if isJust configure
                    then do
                    checkoutCommit (head ls) $ \commit -> do
                      cmd_ configure (EchoStdout False) stderr
                      file <- generateName vs commit
                      cmds <- lines <$> readFile' file
                      let opts = rattleOptions{rattleProcesses=j, rattleUI=Just RattleQuiet, rattleNamedDirs=[], rattleShare=False}
                      timed rattleTime (proj ++ " rattle") j commit $ rattleRun opts $ forM_ cmds $ cmd rattle stderr
                    buildRattle checkoutCommit $ tail ls
                    else do
                    buildRattle checkoutCommit ls
                  else do
                  let ls = counted $ commitList ++ [0]
                  if isJust configure
                    then do
                    checkout (head ls) $ \commit -> do
                      cmd_ configure (EchoStdout False) stderr
                      file <- generateName vs commit
                      cmds <- lines <$> readFile' file
                      let opts = rattleOptions{rattleProcesses=j, rattleUI=Just RattleQuiet, rattleNamedDirs=[], rattleShare=False}
                      timed rattleTime (proj ++ " rattle") j commit $ rattleRun opts $ forM_ cmds $ cmd rattle stderr

                    buildRattle checkout $ tail ls
                    else do
                    buildRattle checkout ls

            make <- readIORef makeTime
            rattle <- readIORef rattleTime
            putStrLn $ "TOTALS: make = " ++ showDuration make ++ ", rattle = " ++ showDuration rattle
