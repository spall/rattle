{-# LANGUAGE ScopedTypeVariables, RecordWildCards, TupleSections, ViewPatterns, LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs #-}

module Development.Rattle.Server(
    Rattle, withRattle, Run(..),
    Hazard(..), Recoverable(..),
    addCmdOptions, cmdRattle
    ) where

import Control.Monad.Extra
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import General.Pool
import Development.Rattle.Types
import Development.Rattle.UI
import Development.Rattle.Shared
import Development.Rattle.Options
import Development.Rattle.Hash
import Development.Rattle.CmdOption
import Control.Exception.Extra
import Control.Concurrent.Extra
import General.Extra
import Data.Either
import Data.Maybe
import System.Directory
import System.FilePath
import System.FilePattern
import System.IO.Extra
import System.IO.Unsafe(unsafeInterleaveIO)
import qualified Development.Shake.Command as C
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.IORef
import Data.Hashable
import Data.List.Extra
import Data.Tuple.Extra
import System.Time.Extra


-- | Type of actions to run. Executed using 'rattle'.
newtype Run a = Run {fromRun :: ReaderT Rattle IO a}
    deriving (Functor, Applicative, Monad, MonadIO)

instance a ~ () => C.CmdArguments (Run a) where
    cmdArguments (C.CmdArgument x) = do
        let (opts, args) = partitionEithers x
        r <- Run ask
        liftIO $ cmdRattle r opts args



data ReadOrWrite = Read | Write deriving (Show,Eq)

data S = S
    {timestamp :: !T
        -- ^ The current timestamp we are on
    ,started :: Map.HashMap Cmd (NoShow (IO ()))
        -- ^ Things that have got to running - if you find a duplicate just run the IO
        --   to wait for it.
    ,running :: [(T, Cmd, [Trace FilePath])]
        -- ^ Things currently running, with the time they started,
        --    and an amalgamation of their previous Trace (if we have any)
    ,hazard :: Map.HashMap FilePath (ReadOrWrite, T, Cmd)
        -- ^ Things that have been read or written, at what time, and by which command
        --   Used to detect hazards.
        --   Read is recorded as soon as it can, Write as late as it can, as that increases hazards.
    ,pending :: [(T, Cmd, Trace (FilePath, Hash))]
        -- ^ Things that have completed, and would like to get recorded, but have to wait
        --   to confirm they didn't cause hazards
    ,required :: [Cmd]
        -- ^ Things what were required by the user calling cmdRattle, not added due to speculation.
        --   Will be the 'speculate' list next time around.
    } deriving Show


data Problem
    = Finished
    | Hazard Hazard

throwProblem :: Problem -> IO a
throwProblem Finished = fail "Finished, but still trying to do stuff"
throwProblem (Hazard h) = throwIO h

-- | Type of exception thrown if there is a hazard when running the build system.
data Hazard
    = ReadWriteHazard FilePath Cmd Cmd Recoverable
    | WriteWriteHazard FilePath Cmd Cmd Recoverable
      deriving Show
instance Exception Hazard

data Recoverable = Recoverable | NonRecoverable | Restartable deriving (Show,Eq)

data Rattle = Rattle
    {options :: RattleOptions
    ,speculate :: [(Cmd, [Trace FilePath])] -- ^ Things that were used in the last speculation with this name
    ,runNum :: !T -- ^ Run# we are on
    ,state :: Var (Either Problem S)
    ,speculated :: IORef Bool
    ,pool :: Pool
    ,ui :: UI
    ,shared :: Shared
    }

addCmdOptions :: [C.CmdOption] -> Rattle -> Rattle
addCmdOptions new r@Rattle{options=o@RattleOptions{rattleCmdOptions=old}} =
    r{options = o{rattleCmdOptions = old ++ new}}


withRattle :: RattleOptions -> (Rattle -> IO a) -> IO a
withRattle options@RattleOptions{..} act = withUI rattleFancyUI (return "Running") $ \ui -> withShared rattleFiles $ \shared -> do
    options@RattleOptions{..} <- rattleOptionsExplicit options

    speculate <- maybe (return []) (getSpeculate shared) rattleSpeculate
    speculate <- fmap (takeWhile (not . null . snd)) $ -- don't speculate on things we have no traces for
        forM speculate $ \x ->
            (x,) . map (fmap fst) <$> unsafeInterleaveIO (map (fmap $ first $ expand rattleNamedDirs) <$> getCmdTraces shared x)
    speculated <- newIORef False

    runNum <- nextRun shared rattleMachine
    let s0 = Right $ S t0 Map.empty [] Map.empty [] []
    state <- newVar s0

    let saveSpeculate state =
            whenJust rattleSpeculate $ \name ->
                whenRightM (readVar state) $ \v ->
                    setSpeculate shared name $ reverse $ required v

    -- first try and run it
    let attempt1 = withPool rattleProcesses $ \pool -> do
            let r = Rattle{..}
            runSpeculate r
            (act r <* saveSpeculate state) `finally` writeVar state (Left Finished)
    attempt1 `catch` \(h :: Hazard) -> do
        if not (recoverableHazard h || restartableHazard h) then throwIO h
          else if restartableHazard h
               then do
          putStrLn "Warning: Speculation lead to a hazard, retrying without speculation"
          print h
          state <- newVar s0
          withPool rattleProcesses $ \pool -> do
            let r = Rattle{speculate=[], ..}
            (act r <* saveSpeculate state) `finally` writeVar state (Left Finished)
               else error $ "Caught Recoverable hazard [" ++ show h ++ "] at top level."

recoverableHazard :: Hazard -> Bool
recoverableHazard WriteWriteHazard{} = False
recoverableHazard (ReadWriteHazard _ _ _ r) = r == Recoverable

restartableHazard :: Hazard -> Bool
restartableHazard (WriteWriteHazard _ _ _ r) = r == Restartable
restartableHazard (ReadWriteHazard _ _ _ r) = r == Restartable

runSpeculate :: Rattle -> IO ()
runSpeculate rattle@Rattle{..} = void $ forkIO $ void $ runPoolMaybe pool $
    -- speculate on a process iff it is the first process in speculate that:
    -- 1) we have some parallelism free
    -- 2) it is the first eligible in the list
    -- 3) not already been started
    -- 4) no read/write conflicts with anything completed
    -- 5) no read conflicts with anything running or any earlier speculation
    join $ modifyVar state $ \s -> case s of
        Right s | Just cmd <- nextSpeculate rattle s -> do
            writeIORef speculated True
            cmdRattleStarted rattle cmd s ["speculative"]
        _ -> return (s,  return ())


nextSpeculate :: Rattle -> S -> Maybe Cmd
nextSpeculate Rattle{..} S{..}
    | any (null . thd3) running = Nothing
    | otherwise = step (addTrace (Set.empty, Set.empty) $ mconcat $ concatMap thd3 running) speculate
    where
        addTrace (r,w) Trace{..} = (f r tRead, f w tWrite)
            where f set xs = Set.union set $ Set.fromList xs

        step _ [] = Nothing
        step rw ((x,_):xs)
            | x `Map.member` started = step rw xs -- do not update the rw, since its already covered
        step rw@(r, w) ((x, mconcat -> t@Trace{..}):xs)
            | not $ any (\v -> v `Set.member` r || v `Set.member` w || v `Map.member` hazard) tWrite
                -- if anyone I write has ever been read or written, or might be by an ongoing thing, that would be bad
            , not $ any (`Set.member` w) tRead
                -- if anyone I read might be being written right now, that would be bad
                = Just x
            | otherwise
                = step (addTrace rw t) xs


cmdRattle :: Rattle -> [C.CmdOption] -> [String] -> IO ()
cmdRattle rattle opts args = cmdRattleRequired rattle $ Cmd (rattleCmdOptions (options rattle) ++ opts) args

cmdRattleRequired :: Rattle -> Cmd -> IO ()
cmdRattleRequired rattle@Rattle{..} cmd = runPool pool $
  catchJust (\(h :: Hazard) -> if recoverableHazard h then Just h else Nothing)
  (do
      modifyVar_ state $ return . fmap (\s -> s{required = cmd : required s})
      cmdRattleStart rattle cmd)
  handler
  where handler h@(ReadWriteHazard f c1 c2 Recoverable)
          | c1 == cmd = return () -- cmd is writer
          | c2 == cmd = putStrLn "Caught recoverable hazard locally" >> cmdRattleRestart rattle cmd
          | otherwise = error $ "Caught recoverable hazard [" ++ show h ++ "], but required cmd [" ++ show cmd ++ "] which caught it is not involved."

cmdRattleRestart :: Rattle -> Cmd -> IO ()
cmdRattleRestart rattle@Rattle{..} cmd = join $ modifyVar state $ \case
  Left e -> throwProblem e
  Right s -> cmdRattleRestarted rattle cmd s []

cmdRattleRestarted :: Rattle -> Cmd -> S -> [String] -> IO (Either Problem S, IO ())
cmdRattleRestarted rattle@Rattle{..} cmd s msgs = do
  let start = timestamp s
  s <- return s{timestamp = succ $ timestamp s}
  hist <- unsafeInterleaveIO $ map (fmap $ first $ expand $ rattleNamedDirs options) <$> getCmdTraces shared cmd
  go <- once $ cmdRattleRun rattle cmd start hist msgs
  s <- return s{running = (start, cmd, map (fmap fst) hist) : running s}
  return (Right s, runSpeculate rattle >> go >> runSpeculate rattle)

cmdRattleStart :: Rattle -> Cmd -> IO ()
cmdRattleStart rattle@Rattle{..} cmd = join $ modifyVar state $ \case
    Left e -> throwProblem e
    Right s -> cmdRattleStarted rattle cmd s []

cmdRattleStarted :: Rattle -> Cmd -> S -> [String] -> IO (Either Problem S, IO ())
cmdRattleStarted rattle@Rattle{..} cmd s msgs = do
    let start = timestamp s
    s <- return s{timestamp = succ $ timestamp s}
    case Map.lookup cmd (started s) of
        Just (NoShow wait) -> return (Right s, wait)
        Nothing -> do
            hist <- unsafeInterleaveIO $ map (fmap $ first $ expand $ rattleNamedDirs options) <$> getCmdTraces shared cmd
            go <- once $ cmdRattleRun rattle cmd start hist msgs
            s <- return s{running = (start, cmd, map (fmap fst) hist) : running s}
            s <- return s{started = Map.insert cmd (NoShow go) $ started s}
            return (Right s, runSpeculate rattle >> go >> runSpeculate rattle)


-- either fetch it from the cache or run it)
cmdRattleRun :: Rattle -> Cmd -> T -> [Trace (FilePath, Hash)] -> [String] -> IO ()
cmdRattleRun rattle@Rattle{..} cmd@(Cmd opts args) start hist msgs = do
    hasher <- memoIO hashFileForward
    let match (fp, h) = (== Just h) <$> hasher fp
    histRead <- filterM (allM match . tRead) hist
    histBoth <- filterM (allM match . tWrite) histRead
    case histBoth of
        t:_ ->
            -- we have something consistent at this point, no work to do
            -- technically we aren't writing to the tWrite part of the trace, but if we don't include that
            -- skipping can turn write/write hazards into read/write hazards
            cmdRattleFinished rattle start cmd t False
        [] -> do
            -- lets see if any histRead's are also available in the cache
            fetcher <- memoIO $ getFile shared
            let fetch (fp, h) = do v <- fetcher h; case v of Nothing -> return Nothing; Just op -> return $ Just $ op fp
            download <- if not (rattleShare options)
                then return Nothing
                else firstJustM (\t -> fmap (t,) <$> allMaybeM fetch (tWrite t)) histRead
            case download of
                Just (t, download) -> do
                    display ["copying"] $ sequence_ download
                    cmdRattleFinished rattle start cmd t False
                Nothing -> do
                    (time, (opts2, c)) <- duration $ display [] $ cmdRattleRaw ui opts args
                    t <- fsaTrace time runNum c
                    checkHashForwardConsistency t
                    let pats = matchMany [((), x) | Ignored xs <- opts2, x <- xs]
                    let skip x = "/dev/" `isPrefixOf` x || hasTrailingPathSeparator x || pats [((),x)] /= []
                    let f hasher xs = mapMaybeM (\x -> fmap (x,) <$> hasher x) $ filter (not . skip) xs
                    t <- Trace (tTime t) (tRun t) <$> f hashFileForward (tRead t) <*> f hashFile (tWrite t)
                    x <- generateHashForwards cmd [x | HashNonDeterministic xs <- opts2, x <- xs] t
                    when (rattleShare options) $
                        forM_ (tWrite t) $ \(fp, h) ->
                            setFile shared fp h ((== Just h) <$> hashFile fp)
                    cmdRattleFinished rattle start cmd t True
    where
        display :: [String] -> IO a -> IO a
        display msgs2 = addUI ui (head $ overrides ++ [cmdline]) (unwords $ msgs ++ msgs2)
        overrides = [x | C.Traced x <- opts] ++ [x | C.UserCommand x <- opts]
        cmdline = unwords $ ["cd " ++ x ++ " &&" | C.Cwd x <- opts] ++ args


cmdRattleRaw :: UI -> [C.CmdOption] -> [String] -> IO ([CmdOption2], [C.FSATrace])
cmdRattleRaw ui opts args = do
    (opts, opts2) <- return $ partitionEithers $ map fromCmdOption opts
    case [x | WriteFile x <- opts2] of
        [] -> do
            let optsUI = if isControlledUI ui then [C.EchoStdout False,C.EchoStderr False] else []
            res <- C.cmd (opts ++ optsUI) args
            return (opts2, res)
        files -> do
            forM_ files $ \file -> do
                createDirectoryIfMissing True $ takeDirectory file
                writeFileUTF8 file $ concat args
            return (opts2, map C.FSAWrite files)

checkHashForwardConsistency :: Trace FilePath -> IO ()
checkHashForwardConsistency Trace{..} = do
    -- check that anyone who is writing forwarding hashes is writing the actual file
    let sources = mapMaybe fromHashForward tWrite
    let bad = sources \\ tWrite
    when (bad /= []) $
        fail $ "Wrote to the forwarding file, but not the source: " ++ show bad

    -- and anyone writing to a file with a hash also updates it
    forwards <- filterM doesFileExist $ mapMaybe toHashForward tWrite
    let bad = forwards \\ tWrite
    when (bad /= []) $
        fail $ "Wrote to the source file which has a forwarding hash, but didn't touch the hash: " ++ show bad


-- | If you hae been asked to generate a forwarding hash for writes
generateHashForwards :: Cmd -> [FilePattern] -> Trace (FilePath, Hash) -> IO (Trace (FilePath, Hash))
generateHashForwards cmd ms t = do
    let match = matchMany $ map ((),) ms
    let (normal, forward) = partition (\(x, _) -> isJust (toHashForward x) && null (match [((), x)])) $ tWrite t
    let Hash hash = hashString $ show (cmd, tRead t, normal)
    let hhash = hashHash $ Hash hash
    forward <- forM forward $ \(x,_) -> do
        let Just x2 = toHashForward x -- checked this is OK earlier
        writeFile x2 hash
        return (x2, hhash)
    return t{tWrite = tWrite t ++ forward}

-- | I finished running a command
cmdRattleFinished :: Rattle -> T -> Cmd -> Trace (FilePath, Hash) -> Bool -> IO ()
cmdRattleFinished rattle@Rattle{..} start cmd trace@Trace{..} save = join $ modifyVar state $ \case
    Left e -> throwProblem e
    Right s -> do
        -- update all the invariants
        let stop = timestamp s
        s <- return s{timestamp = succ $ timestamp s}
        s <- return s{running = filter ((/= start) . fst3) $ running s}

        -- look for hazards
        -- push writes to the end, and reads to the start, because reads before writes is the problem
        let newHazards = Map.fromList $ map ((,(Write,stop ,cmd)) . fst) tWrite ++
                                        map ((,(Read ,start,cmd)) . fst) tRead
        case unionWithKeyEithers (mergeFileOps (required s) (map fst speculate)) (hazard s) newHazards of
            (ps@(p@(ReadWriteHazard f c1 c2 Recoverable):_), hazard2) -> do
              s <- return s{hazard = if cmd == c1 -- writer
                                     then let Just (Write, t2, cmd2) = Map.lookup f newHazards in
                                            Map.insert f (Write, t2, cmd2) hazard2
                                     else hazard2}
              return (Right s, print ps >> throwIO p)
            (ps@(p:_), hazard2) -> do
              s <- return s{pending = [(stop, cmd, trace) | save] ++ pending s} -- for consistency
              return (Left $ Hazard p, print ps >> throwIO p)
            ([], hazard2) -> do
                s <- return s{pending = [(stop, cmd, trace) | save] ++ pending s}
                s <- return s{hazard = hazard2}

                -- move people out of pending if they have survived long enough
                let earliest = minimum $ succ stop : map fst3 (running s)
                (safe, pending) <- return $ partition (\x -> fst3 x < earliest) $ pending s
                s <- return s{pending = pending}
                return (Right s, forM_ safe $ \(_,c,t) -> addCmdTrace shared c $ fmap (first $ shorten (rattleNamedDirs options)) t)

-- r is required list; s is speculate list
mergeFileOps :: [Cmd] -> [Cmd] -> FilePath -> (ReadOrWrite, T, Cmd) -> (ReadOrWrite, T, Cmd) -> Either Hazard (ReadOrWrite, T, Cmd)
mergeFileOps r s x (Read, t1, cmd1) (Read, t2, cmd2)
  | cmd1 == cmd2 = Right (Read, max t1 t2, cmd1) -- because the first time is defunct and was redone
  | otherwise = Right (Read, min t1 t2, if t1 < t2 then cmd1 else cmd2)
mergeFileOps r s x (Write, t1, cmd1) (Write, t2, cmd2)
  | cmd1 == cmd2 = Right (Write, max t1 t2, cmd1) -- because the first time is defunct and was redone
  | elem cmd1 r && elem cmd2 r = Left $ WriteWriteHazard x cmd1 cmd2 NonRecoverable
  | otherwise = Left $ WriteWriteHazard x cmd1 cmd2 Restartable -- one write may be an error
mergeFileOps r s x (Read, t1, cmd1) (Write, t2, cmd2)
  | elem cmd1 r && elem cmd2 r && listedBefore cmd1 cmd2
  = Left $ ReadWriteHazard x cmd2 cmd1 NonRecoverable
  | notElem cmd2 r && listedBefore cmd1 cmd2 = Left $ ReadWriteHazard x cmd2 cmd1 Restartable 
  | t1 <= t2 = Left $ ReadWriteHazard x cmd2 cmd1 Recoverable
  | otherwise = Right (Write, t2, cmd2)
  where -- FIXME: listedBefore is O(n) so want to make that partly cached
        listedBefore c1 c2 = let i1 = elemIndex c1 r
                                 i2 = elemIndex c2 r in
                               f i1 i2 c1 c2
        f Nothing Nothing c1 c2 = let Just i1 = elemIndex c1 s -- both should be in speculate list
                                      Just i2 = elemIndex c2 s
                                  in i1 < i2 -- speculate list is reverse of required
        f (Just i1) (Just i2) _ _ = i1 > i2
        f (Just i1) Nothing _ _ = True -- 2nd one isn't in required list so it must be listed after i1
        f Nothing (Just i2) _ _ = False -- first one isn't in required list so it must be listed after i2
mergeFileOps r s x v1 v2 = mergeFileOps r s x v2 v1 -- must be Write/Read, so match the other way around


allMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe [b])
allMaybeM f [] = return $ Just []
allMaybeM f (x:xs) = do
    y <- f x
    case y of
        Nothing -> return Nothing
        Just y -> fmap (y:) <$> allMaybeM f xs


unionWithKeyEithers :: (Eq k, Hashable k) => (k -> v -> v -> Either e v) -> Map.HashMap k v -> Map.HashMap k v -> ([e], Map.HashMap k v)
unionWithKeyEithers op lhs rhs = foldl' f ([], lhs) $ Map.toList rhs
    where
        f (es, mp) (k, v2) = case Map.lookup k mp of
            Nothing -> (es, Map.insert k v2 mp)
            Just v1 -> case op k v1 v2 of
                Left e -> (e:es, mp)
                Right v -> (es, Map.insert k v mp)
