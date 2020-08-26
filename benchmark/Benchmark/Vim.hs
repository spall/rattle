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

    commits1=["4b96df5a0"] 
    commits2=["318e7a9c0"]
    commits3=["3f169ce17"]
    commits4=["50985eb1f", "89bfc8218"] 
    commits5=["f8ddb2578"]
    commits6=["7d8ea0b24", "272ca95fc"]

    commits7=["842931cd7", "5b18c248d"]
    commits8=["07da94b0f", "58ceca5ca", "97a2af39c", "8cbd6dfc0", "a6d536829", "9a5e5a3e3"]
    commits9=["4549ece47"]
    commits10=["0ff6aad39"]
    commits11=["5d98dc2a4"]
    commits12=["db661fb95"]
    commits13=["e258368b4", "70b3e706b", "df54382ea"]

    commits14=["b09920203"]
    commits15=["0c3064b39", "21109272f", "9f2d020d3", "5feabe00c", "92be6e3f4", "a259d8d30"
             ,"705724e43", "0b76ad53b", "7f829cab3", "7cc96923c", "ab067a21b"]

    commits = commits1 ++ commits2 ++ commits3 ++ commits4 ++ commits5 ++ commits6 ++ commits7
    	    ++ commits8 ++ commits9 ++ commits10 ++ commits11 ++ commits12 ++ commits13
	    ++ commits14 ++ commits15

    getScript :: String -> FilePath
    getScript commit = "vim." ++ commit ++ ".cmds"

    generate :: IO String
    generate = do
      Stdout cStr <- cmd "git show --oneline -s" -- command is first thing
      let commit = head $ words cStr
      readFile' $ "/home/spall/rattle-stuff/rattle-papers/vim_scripts/" ++ (getScript commit)

    clean :: IO ()
    clean = cmd_ "make clean" (EchoStdout False)
