{-# LANGUAGE ScopedTypeVariables #-}

module Builder.VimVars(cfgVal, setupVars) where

import qualified Data.HashMap.Strict as Map
import Data.Strings
import Data.Maybe
import Data.List

setupVars :: FilePath -> IO (Map.HashMap String String)
setupVars fp = do
  cfgVars <- parseConfig fp
  return $ resolveVals $ insertVars cfgVars

parseConfig :: FilePath -> IO (Map.HashMap String String)
parseConfig fp = do
  contents <- readFile fp 
  return $ foldl (\hm l -> case words l of
                             [k,"="] -> Map.insert k "" hm
                             (k:"=":vs) -> Map.insert k (unwords vs) hm
                             _ -> hm) Map.empty $ lines contents

isResolved :: Map.HashMap String String -> String -> Bool
isResolved hm k = not $ any (\k -> isInfixOf (f k) str || isInfixOf (f2 k) str) $ Map.keys hm
  where f k = "${" ++ k ++ "}"
        f2 k = "$(" ++ k ++ ")"
        str = hm Map.! k

resolveVals :: Map.HashMap String String -> Map.HashMap String String
resolveVals hm = foldl' resolveVal hm $ Map.keys hm
                   
resolveVal :: Map.HashMap String String -> String -> Map.HashMap String String
resolveVal hm k = if isResolved hm k
                  then hm
                  else g (Map.keys hm) v hm
  where f k = "${" ++ k  ++ "}"
        f2 k = "$(" ++ k ++ ")"
        g [] s hm = Map.insert k s hm
        g (x:xs) s hm = if isInfixOf (f x) s
                        then if isResolved hm x
                             then let s2 = replaceAll (f x) (hm Map.! x) s in
                                    g xs s2 hm
                             else let nh = resolveVal hm x
                                      s2 = replaceAll (f x) (nh Map.! x) s in
                                    g xs s2 nh
                        else if isInfixOf (f2 x) s
                             then if isResolved hm x
                                  then let s2 = replaceAll (f2 x) (hm Map.! x) s in
                                         g xs s2 hm
                                  else let nh = resolveVal hm x
                                           s2 = replaceAll (f2 x) (nh Map.! x) s in
                                         g xs s2 nh
                             else g xs s hm
        v = hm Map.! k

replaceAll :: String -> String -> String -> String
replaceAll n r h = if isInfixOf n h
                   then replaceAll n r $ strReplace n r h
                   else h

insertVars :: Map.HashMap String String -> Map.HashMap String String
insertVars hm = foldl' (\m (k,v) -> Map.insert k v m) hm vars

cfgVal :: String -> Map.HashMap String String -> String
cfgVal s hm = fromMaybe "" $ Map.lookup s hm 

-- variables defined in the makefiles
vars = [("CClink", "${CC}")
       ,("ALL_CFLAGS", "${PRE_DEFS} ${CFLAGS} ${PROFILE_CFLAGS} ${SANITIZER_CFLAGS} \
                       \ ${LEAK_CFLAGS} ${ABORT_CFLAGS} ${POST_DEFS}")
       ,("CCC", "${CCC_NF} ${ALL_CFLAGS}")
       ,("CCC_NF", "${CC} -c -I${srcdir}")
       ,("ALL_LIB_DIRS", "${GUI_LIBS_DIR} ${X_LIBS_DIR}")
       ,("VIMTARGET", "${VIMNAME}${EXEEXT}")
       ,("SANITIZER_LIBS", "${SANITIZER_CFLAGS}")
       ,("ALL_LIBS", "${GUI_LIBS1} ${GUI_X_LIBS} ${GUI_LIBS2} ${X_PRE_LIBS} \ 
	             \ ${X_LIBS} ${X_EXTRA_LIBS} ${MZSCHEME_LIBS} ${LIBS} \
	             \ ${EXTRA_LIBS} ${LUA_LIBS} ${PERL_LIBS} ${PYTHON_LIBS} \
	             \ ${PYTHON3_LIBS} ${TCL_LIBS} ${RUBY_LIBS} ${PROFILE_LIBS} \
	             \ ${SANITIZER_LIBS} ${LEAK_LIBS}")
       ,("PROFILE_CFLAGS", "")
       ,("LEAK_CFLAGS", "")
       ,("ABORT_CFLAGS", "")
       ,("POST_DEFS", "")
       ,("EXTRA_IPATHS", "")
       ,("NONE_DEFS", "")
       ,("NONE_IPATH", "")
       ,("PRE_DEFS", "-Iproto $(DEFS) $(GUI_DEFS) $(GUI_IPATH) $(CPPFLAGS) $(EXTRA_IPATHS)")
       ,("NONE_LIBS1", "")
       ,("NONE_LIBS2", "")
       ,("EXTRA_LIBS", "")
       ,("PROFILE_LIBS", "")
       ,("SANITIZER_CFLAGS", "")
       ,("LEAK_LIBS", "")
       ,("NONE_LIBS_DIR", "")
       ,("SHELL", "/bin/sh")
       ,("MKDIR_P", "$(SHELL) install-sh -c -d")
       ,("VIMRCLOC", "${VIMLOC}")
       ,("VIMLOC", "${DATADIR}${VIMDIR}")
       ,("VIMDIR", "/vim")
       ,("VTERM_CFLAGS", "-Ilibvterm/include")
       ,("CCCTERM", "${CCC_NF} ${VTERM_CFLAGS} ${ALL_CFLAGS} -DINLINE=\"\" \
                    \ -DVSNPRINTF=vim_vsnprintf -DIS_COMBINGING_FUNCTION=utf_iscomposing_uint \
                    \ -DWCWIDTH_FUNCTION=utf_uint2cells")
       ,("OSDEF_CFLAGS", "$(PRE_DEFS) $(POST_DEFS)")]
         
                    
