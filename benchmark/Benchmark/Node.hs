{-# LANGUAGE RecordWildCards #-}

module Benchmark.Node(main) where

import Benchmark.VsMake
import Development.Shake.Command
import Data.List.Extra

import System.IO.Extra

main :: Args -> IO () -- 38
main args = vsMake VsMake{..} args{commits=Just 38, commitsList = Just commits}
  where
    broken = []
    repo = "https://github.com/nodejs/node"
    master = "master"
    configure = Just $ cmd "./configure"
    make = cmd "make"
    rattle = cmd Shell
    generateVersion = 1

    commits2=["7b7e7bd", "59a1981", "f2ec64f", "3d456b1", "70c32a6", "b851d7b", "2462a2c", "32f63fc"
             ,"2170259", "0f89419", "64161f2", "2cd9892", "24e81d7", "d65e6a5", "5cf789e", "d227d22", "1d95111"
             ,"38aa315", "d4c81be", "9225939", "abe6a2e", "dd4c62e", "a171314", "43fb6ff", "13fe56b", "25c3f7c", "470511a"
	     ,"54c1a09", "be65963", "023ecbc", "d10927b", "cb21011", "ab9e894", "2272489", "0fe8101", "d80c400"]
    commits1=["a5d4a39", "78743f8"]

    commits = commits1 ++ commits2

    getScript :: String -> FilePath
    getScript commit | elem (take 7 commit) commits2 = "node." ++ head commits2 ++ ".cmds"
                     | elem (take 7 commit) commits1 = "node." ++ head commits1 ++ ".cmds"
                     | otherwise = error "No script created for commit: " ++ (take 7 commit)


    generate :: IO String
    generate = do
      -- find out the commit then just read from the appropriate file that already exists
      Stdout cStr <- cmd "git show --oneline -s" -- command is first thing  
      let commit = head $ words cStr
      putStrLn commit
      readFile' $ "/home/spall/rattle-stuff/rattle-papers/node_scripts/" ++ (getScript commit)

    clean :: IO ()
    clean = cmd_ "make clean" (EchoStdout False)

