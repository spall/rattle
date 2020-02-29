{-# LANGUAGE RecordWildCards #-}

module Benchmark.Node(main) where

import Benchmark.VsMake
import Development.Shake.Command
import Data.List.Extra

import System.IO.Extra

main :: Args -> IO ()
main args = vsMake VsMake{..} args{commits=Just 38, commitsList = Just commits}
  where
    broken = []
    repo = "https://github.com/nodejs/node"
    master = "master"
    configure = Just $ cmd "./configure"
    make = cmd "make"
    rattle = cmd Shell
    generateVersion = 1

    commits7=["ab9e894", "2272489", "0fe8101", "d80c400"]
    commits6=["54c1a09", "be65963", "023ecbc", "d10927b", "cb21011"]
    commits5=["43fb6ff", "13fe56b", "25c3f7c", "470511a"]
    commits4=["64161f2", "2cd9892", "24e81d7", "d65e6a5", "5cf789e", "d227d22", "1d95111"
             ,"38aa315", "d4c81be", "9225939", "abe6a2e", "dd4c62e", "a171314"]
    commits3=["59a1981", "f2ec64f", "3d456b1", "70c32a6", "b851d7b", "2462a2c", "32f63fc"
             ,"2170259", "0f89419"]
    commits2=["7b7e7bd"]
    commits1=["a5d4a39", "78743f8"]

    commits = commits1 ++ commits2 ++ commits3 ++ commits4 ++ commits5 ++ commits6 ++ commits7

    getScript :: String -> FilePath
    getScript commit | elem commit commits1 = "node." ++ head commits1 ++ ".txt"
                     | elem commit commits2 = "node." ++ head commits2 ++ ".txt"
                     | elem commit commits3 = "node." ++ head commits3 ++ ".txt"
                     | elem commit commits4 = "node." ++ head commits4 ++ ".txt"
                     | elem commit commits5 = "node." ++ head commits5 ++ ".txt"
                     | elem commit commits6 = "node." ++ head commits6 ++ ".txt"
                     | elem commit commits7 = "node." ++ head commits6 ++ ".txt"
                     | otherwise = error "No script created for commit: " ++ commit


    generate :: IO String
    generate = do
      -- find out the commit then just read from the appropriate file that already exists
      Stdout cStr <- cmd "git show --oneline -s" -- command is first thing  
      let commit = head $ words cStr
      readFile' $ "/data/home.local/sjspall/icfp/rattle-papers/project-data/node/machine_hive/scripts/" ++ (getScript commit)

    clean :: IO ()
    clean = cmd_ "make clean" (EchoStdout False)

