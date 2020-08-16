{-# LANGUAGE RecordWildCards #-}

module Benchmark(main) where

import Benchmark.Args
import qualified Benchmark.FSATrace
import qualified Benchmark.Redis
import qualified Benchmark.Micro
import qualified Benchmark.Intro
import qualified Benchmark.Vim
import qualified Benchmark.Tmux
import qualified Benchmark.Node
import Control.Monad


benchmarks =
    ["fsatrace" * Benchmark.FSATrace.main
    ,"redis" * Benchmark.Redis.main
    ,"micro" * Benchmark.Micro.main
    ,"intro" * Benchmark.Intro.main
    ,"vim" * Benchmark.Vim.main
    ,"tmux" * Benchmark.Tmux.main
    ,"node" * Benchmark.Node.main
    ]
    where (*) = (,)


main :: IO ()
main = do
    args@Args{..} <- getArguments
    when (null names) $
        error $ "Specify which benchmarks to run, from: " ++ show (map fst benchmarks)
    forM_ names $ \name -> do
        let Just m = lookup name benchmarks
        m args
