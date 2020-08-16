{-# LANGUAGE RecordWildCards #-}

module Benchmark.Tmux(main) where

import Benchmark.VsMake
import Development.Shake.Command
import Data.List.Extra
import System.IO.Extra

main :: Args -> IO ()
main args = vsMake VsMake{..} args{commits=Just 40, commitsList = Just commits}
  where
    broken = ["ee3d3db3"]
    repo = "https://github.com/tmux/tmux"
    master = "master"
    configure = Just $ cmd Shell "sh autogen.sh && ./configure"
    make = cmd "make"
    rattle = cmd Shell
    generateVersion = 1
    
    commits0=["ed16f51e", "61b075a2", "e9b12943", "3e701309", "8457f54e", "a01c9ffc", "cdf13837"
             , "74b42407", "0eb7b547", "f3ea318a", "7cdf5ee9"]
    commits1=["ee3d3db3"]
    -- 685 fails because speculation caused a failed cmd
    commits2=["685eb381", "60ab7144", "7eada28f", "7f3feb18", "8b22da69", "bc36700d", "32be954b"
             , "6f0241e6", "19d5f4a0", "43b36752", "0bf153da"]
    commits3=["4822130b"]
    commits4=["47174f51", "c915cfc7", "54553903", "400750bb", "470cba35", "a4d8437b", "6c28d0dd"
             , "24cd726d", "9900ccd0", "c391d50c" ,"0c6c8c4e" ,"fdbc1116" ,"37919a6b", "22e9cf04"
             , "ba542e42", "4694afbe"]
    commits = commits0 ++ commits1 ++ commits2 ++ commits3 ++ commits4

    getScript :: String -> FilePath
    getScript commit | elem commit commits0 = "tmux." ++ head commits0 ++ ".txt"
		     | elem commit commits1 = "tmux." ++ head commits1 ++ ".txt"
                     | elem commit commits2 = "tmux." ++ head commits2 ++ ".txt"
                     | elem commit commits3 = "tmux." ++ head commits3 ++ ".txt"
                     | elem commit commits4 = "tmux." ++ head commits4 ++ ".txt"
                     | otherwise = error "No script created for commit: " ++ commit

    generate :: IO String
    generate = do
      -- find out the commit then just read from the appropriate file that already exists
      Stdout cStr <- cmd "git show --oneline -s" -- command is first thing  
      let commit = head $ words cStr
      readFile' $ "/home/spall/rattle-stuff/rattle-papers/tmux_scripts/" ++ (getScript commit)

    clean :: IO ()
    clean = cmd_ "echo no" (EchoStdout False)
