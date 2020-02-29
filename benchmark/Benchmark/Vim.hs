{-# LANGUAGE RecordWildCards #-}

module Benchmark.Vim(main) where

import Benchmark.VsMake
import Development.Shake.Command
import Data.List.Extra
import System.IO.Extra

main :: Args -> IO ()
main args = vsMake VsMake{..} args{commits=Just 35, commitsList = Just commits}
  where
    broken = []
    repo = "https://github.com/vim/vim"
    master = "master"
    configure = Nothing
    make = cmd "make"
    rattle = cmd Shell
    generateVersion = 1

    commits1=["4b96df5"]

    commits2=["318e7a9", "3f169ce", "50985eb", "89bfc82", "f8ddb25", "7d8ea0b", "272ca95"]

    commits3=["842931c", "5b18c24", "07da94b", "58ceca5", "97a2af3", "8cbd6df", "a6d5368"
             ,"9a5e5a3", "4549ece", "0ff6aad", "5d98dc2", "db661fb", "e258368", "70b3e70"
             ,"df54382"]

    commits4=["b099202", "0c3064b", "2110927", "9f2d020", "5feabe0", "92be6e3", "a259d8d"
             ,"705724e", "0b76ad5", "7f829ca", "7cc9692", "ab067a2"]

    commits = commits1 ++ commits2 ++ commits3 ++ commits4

    getScript :: String -> FilePath
    getScript commit | elem commit commits1 = "vim." ++ head commits1 ++ ".txt"
                     | elem commit commits2 = "vim." ++ head commits2 ++ ".txt"
                     | elem commit commits3 = "vim." ++ head commits3 ++ ".txt"
                     | elem commit commits4 = "vim." ++ head commits4 ++ ".txt"
                     | otherwise = error "No script created for commit: " ++ commit

    generate :: IO String
    generate = do
      Stdout cStr <- cmd "git show --oneline -s" -- command is first thing
      let commit = head $ words cStr
      readFile' $ "/data/home.local/sjspall/icfp/rattle-papers/project-data/vim/machine_hive/scripts/" ++ (getScript commit)

    clean :: IO ()
    clean = cmd_ "make clean" (EchoStdout False)
