
module Test.Example.CmdHelpers(seqCmds, shcCmd) where

import Development.Shake.Command
import Data.List
import Development.Rattle

shcCmd :: String -> Run ()
shcCmd c = cmd ["sh", "-c", c]

-- sequence commands with semicolons
seqCmds :: [String] -> Run ()
seqCmds cmds = shcCmd $ intercalate "; " cmds
