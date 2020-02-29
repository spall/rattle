{-# LANGUAGE RecordWildCards #-}

module Benchmark.Tmux(main) where

import Benchmark.VsMake
import Development.Shake.Command
import Data.List.Extra
import System.IO.Extra

main :: Args -> IO ()
main args = vsMake VsMake{..} args{commits=Just 40, commitsList = Just commits}
  where
    broken = ["ee3d3db"]
    repo = "https://github.com/tmux/tmux"
    master = "master"
    configure = Just $ cmd Shell "sh autogen.sh && ./configure"
    make = cmd "make"
    rattle = cmd Shell
    generateVersion = 1
    
    commits1=["ed16f51", "61b075a", "e9b1294", "3e70130", "8457f54", "a01c9ff", "cdf1383"
             , "74b4240", "0eb7b54", "f3ea318", "7cdf5ee", "ee3d3db"]
    -- 685 fails because speculation caused a failed cmd
    commits2=["685eb38", "60ab714", "7eada28", "7f3feb1", "8b22da6", "bc36700", "32be954"
             , "6f0241e", "19d5f4a", "43b3675", "0bf153d"]
    commits3=["4822130"]
    commits4=["47174f5", "c915cfc", "5455390", "400750b", "470cba3", "a4d8437", "6c28d0d"
             , "24cd726", "9900ccd", "c391d50" ,"0c6c8c4" ,"fdbc111" ,"37919a6", "22e9cf0"
             , "ba542e4", "4694afb"]
    commits = commits1 ++ commits2 ++ commits3 ++ commits4

    getScript :: String -> FilePath
    getScript commit | elem commit commits1 = "tmux." ++ head commits1 ++ ".txt"
                     | elem commit commits2 = "tmux." ++ head commits2 ++ ".txt"
                     | elem commit commits3 = "tmux." ++ head commits3 ++ ".txt"
                     | elem commit commits4 = "tmux." ++ head commits4 ++ ".txt"
                     | otherwise = error "No script created for commit: " ++ commit

    generate :: IO String
    generate = do
      -- find out the commit then just read from the appropriate file that already exists
      Stdout cStr <- cmd "git show --oneline -s" -- command is first thing  
      let commit = head $ words cStr
      readFile' $ "/data/home.local/sjspall/icfp/rattle-papers/project-data/tmux/machine_hive/scripts/" ++ (getScript commit)

    clean :: IO ()
    clean = cmd_ "echo no" (EchoStdout False)
