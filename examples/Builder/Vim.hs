{-# LANGUAGE ScopedTypeVariables #-}

module Builder.Vim(build, install) where

import System.Info.Extra
import Development.Rattle
import System.FilePattern.Directory
import System.FilePath
import Control.Monad
import Data.List
import qualified Data.HashMap.Strict as Map
import Data.Strings

shcCmd :: String -> Run ()
shcCmd c = cmd ["sh", "-c", c]

cmdSeq :: [String] -> Run ()
cmdSeq xs = shcCmd $ intercalate "; " xs

parseConfig :: FilePath -> IO (Map.HashMap String String)
parseConfig fp = do
  contents <- readFile fp 
  return $ foldl (\hm l -> case words l of
                             [k,"="] -> hm
                             (k:"=":vs) -> Map.insert k (unwords vs) hm
                             _ -> hm) Map.empty $ lines contents

isResolved :: Map.HashMap String String -> String -> Bool
isResolved hm k = not $ any (\k -> isInfixOf (f k) str) $ Map.keys hm
  where f k = "${" ++ k ++ "}"
        str = hm Map.! k

resolveVals :: Map.HashMap String String -> Map.HashMap String String
resolveVals hm = foldl' resolveVal hm $ Map.keys hm
                   
resolveVal :: Map.HashMap String String -> String -> Map.HashMap String String
resolveVal hm k = if isResolved hm k
                  then hm
                  else g (Map.keys hm) v hm
  where f k = "${" ++ k  ++ "}"
        g [] s hm = Map.insert k s hm
        g (x:xs) s hm = if isInfixOf (f x) s
                        then if isResolved hm x
                             then let s2 = replaceAll (f x) (hm Map.! x) s in
                                    g xs s2 hm
                             else let nh = resolveVal hm x
                                      s2 = replaceAll (f x) (nh Map.! x) s in
                                    g xs s2 nh
                        else g xs s hm
        v = hm Map.! k

replaceAll :: String -> String -> String -> String
replaceAll n r h = if isInfixOf n h
                   then replaceAll n r $ strReplace n r h
                   else h

build :: Run ()
build = if isWindows || isMac
        then do cmd "echo Windows and Mac are not currently supported."
        else do 
  let to0 x = "objects" </> takeBaseName x <.> "o"
      tom0 x = takeBaseName x <.> "mo"
      base x = takeBaseName x
      baseAll = map base
      notCompiled = ["dlldata.c", "dosinst.c", "gui.c", "gui_at_sb.c", "gui_athena.c",
                     "gui_gtk.c", "gui_gtk_f.c", "gui_gtk_x11.c", "gui_mac.c", "gui_motif.c",
                     "gui_photon.c", "gui_w32.c", "gui_x11.c", "gui_xmdlg.c", "hangulin.c",
                     "gui_beval.c", "gui_at_fs.c", "gui_xmebw.c", "if_lua.c", "if_mzsch.c",
                     "if_python.c", "if_python3.c", "if_tcl.c", "if_ruby.c",
                     "os_amiga.c", "os_win32.c", "os_mswin.c", "winclip.c",
                     "os_beos.c", "os_mac_conv.c", "os_vms.c", "os_vms_mms.c", "os_w32dll.c",
                     "os_w32exe.c", "regexp_nfa.c", "uninstal.c", "vimrun.c", "xpm_w32.c",
                     "json_test.c", "message_test.c", "kword_test.c", "if_perlsfio.c",
                     "iid_ole.c", "iscygpty.c", "memfile_test.c", "nbdebug.c", "osqnx.c"] -- not compiled on linux
  c1 <- liftIO $ do
          xs <- getDirectoryFiles "src" ["*.c"]
          ys <- getDirectoryFiles "src" ["xdiff/*.c"]
          return $ (xs ++ ys ++ ["auto/pathdef.c"]) \\ notCompiled
  po <- liftIO $ getDirectoryFiles "src/po" ["*.po"]
  cmd ["sh", "-c", "if test ! -f src/auto/config.mk; then cp src/config.mk.dist src/auto/config.mk; fi"]
  cfgVars <- liftIO $ parseConfig ("src" </> "auto" </> "config.mk")
  cfgVars <- return $ resolveVals cfgVars
  let cfgVal k = case Map.lookup k cfgVars of
                   Nothing -> error $ "Did not find config variable: " ++ k
                   Just v -> v
      vimname = cfgVal "VIMNAME"
      cc = cfgVal "CC"
      defs = cfgVal "DEFS"
      cflags = cfgVal "CFLAGS"
      ldflags = cfgVal "LDFLAGS"
      libs = cfgVal "LIBS"
      x_pre_libs = cfgVal "X_PRE_LIBS"
      x_extra_libs = cfgVal "X_EXTRA_LIBS"
      x_libs = cfgVal "X_LIBS"
      prefix = cfgVal "prefix"
      term_src = words $ cfgVal "TERM_SRC"
      quotesed = cfgVal "QUOTESED"
      nl = cfgVal "NL"
      makemo = cfgVal "MAKEMO"
      datadir = cfgVal "DATADIR"
      
  withCmdOptions [Cwd "src"] $ do
    shcCmd "/bin/sh install-sh -c -d objects"
    shcCmd "touch objects/.dirstamp"
    shcCmd $ "CC=\"" ++ cc ++ " -Iproto " ++ defs ++ " \" srcdir=. sh ./osdef.sh"
    cmdSeq ["echo creating auto/pathdef.c"
           ,"echo '/* pathdef.c */' > auto/pathdef.c"
           ,"echo '/* This file is automatically created by Makefile' >> auto/pathdef.c"
           ,"echo ' * DO NOT EDIT! Change Makefile only. */' >> auto/pathdef.c"
           ,"echo '#include \"vim.h\"' >> auto/pathdef.c"
           ,"echo 'char_u *default_vim_dir = (char_u *)\"" ++ datadir ++ "/vim\";' | sed -e 's/[\\\\\"]/\\\\&/g' -e 's/\\\\\"/\"/' -e 's/\\\\\";$/\";/' >> auto/pathdef.c"
           ,"echo 'char_u *default_vimruntime_dir = (char_u *)\"\";' | sed -e 's/[\\\\\"]/\\\\&/g' -e 's/\\\\\"/\"/' -e 's/\\\\\";$/\";/' >> auto/pathdef.c"
           ,"echo 'char_u *all_cflags = (char_u *)\"gcc -c -I. -Iproto " ++ defs ++ " " ++ cflags ++ " \";' | sed -e 's/[\\\\\"]/\\\\&/g' -e 's/\\\\\"/\"/' -e 's/\\\\\";$/\";/' >> auto/pathdef.c"
           ,"echo 'char_u *all_lflags = (char_u *)\"gcc " ++ ldflags ++ " -o vim " ++ x_pre_libs ++ " " ++ x_libs ++ " " ++ x_extra_libs ++ " " ++ libs ++ " \";' | sed -e 's/[\\\\\"]/\\\\&/g' -e 's/\\\\\"/\"/' -e 's/\\\\\";$/\";/' >> auto/pathdef.c"
           ,"echo 'char_u *compiled_user = (char_u *)\"' | tr -d " ++ nl ++ " >> auto/pathdef.c"
           ,"if test -n \"\"; then echo \"\" | tr -d " ++ nl ++ " >> auto/pathdef.c; else ((logname) 2>/dev/null || whoami) | tr -d " ++ nl ++ " >> auto/pathdef.c; fi"
           ,"echo '\";' >> auto/pathdef.c"
           ,"echo 'char_u *compiled_sys = (char_u *)\"' | tr -d " ++ nl ++ " >> auto/pathdef.c"
           ,"if test -z \"\"; then hostname | tr -d " ++ nl ++ " >> auto/pathdef.c; fi"
           ,"echo '\";' >> auto/pathdef.c"]
    shcCmd "sh ./pathdef.sh"
    forM_ c1 $ \c -> cmd $ cc ++ " -c -I. -Iproto " ++ defs ++ " " ++ cflags ++ " -o" ++ to0 c ++ " " ++ c
    forM_ term_src $ \c -> shcCmd $ "gcc -c -I. -Ilibvterm/include -Iproto " ++ defs ++ " " ++ cflags ++ " -DINLINE=\"\" -DVSNPRINTF=vim_vsnprintf -DIS_COMBINING_FUNCTION=utf_iscomposing_uint -DWCWIDTH_FUNCTION=utf_uint2cells -o " ++ to0 c ++ " " ++ c
    o1 <- liftIO $ getDirectoryFiles "src" ["objects/*.o"]
    shcCmd $ "LINK=\" " ++ cc ++ " " ++ ldflags ++ " -o vim " ++ unwords o1 ++ " " ++ x_pre_libs ++ " " ++ x_libs ++ " " ++ x_extra_libs ++ " " ++ libs ++ " \" MAKE=\"submake\" LINK_AS_NEEDED=yes sh ./link.sh"
    withCmdOptions [Cwd "xxd"] $ do
      shcCmd $ cc ++ " " ++ cflags ++ " " ++ ldflags ++ " -DUNIX -o xxd xxd.c"
    withCmdOptions [Cwd "po"] $ do
      cmdSeq ["rm -f pl.UTF-8.po"
             ,"iconv -f iso-8859-2 -t utf-8 pl.po | sed -e 's/charset=ISO-8859-2/charset=UTF-8/' -e 's/# Original translations/# Generated from pl.po, DO NOT EDIT/' > pl.UTF-8.po"]
      cmdSeq ["rm -f ru.cp1251.po"
             ,"iconv -f utf-8 -t cp1251 ru.po | sed -e 's/charset=[uU][tT][fF]-8/charset=cp1251/' -e 's/# Original translations/# Generated from ru.po, DO NOT EDIT/' > ru.cp1251.po"]
      forM_ po $ \p -> shcCmd $ "OLD_PO_FILE_INPUT=yes msgfmt -v -o " ++ tom0 p ++ " " ++ p
      shcCmd $ "echo " ++ unwords (baseAll po) ++ " | tr \" \" \"\\n\" |sed -e '/\\./d' | sort > LINGUAS"
      shcCmd "msgfmt --desktop -d . --template gvim.desktop.in -o gvim.desktop"
      shcCmd $ "echo " ++ unwords (baseAll po) ++ " | tr \" \" \"\\n\" |sed -e '/\\./d' | sort > LINGUAS"
      shcCmd "msgfmt --desktop -d . --template vim.desktop.in -o vim.desktop"
      cmd "echo Converted submake did nothing"




install :: Run ()
install = if isWindows || isMac
          then do cmd "echo Windows and Mac are not currently supported."
          else do
  cmd ["sh", "-c", "if test ! -f src/auto/config.mk; then cp src/config.mk.dist src/auto/config.mk; fi"]
  cfgVars <- liftIO $ parseConfig ("src" </> "auto" </> "config.mk")
  cfgVars <- return $ resolveVals cfgVars
  let cfgVal k = case Map.lookup k cfgVars of
                   Nothing -> error $ "Did not find config variable: " ++ k
                   Just v -> v
      prefix = cfgVal "prefix"
      execPrefix = cfgVal "exec_prefix"
      datadir = cfgVal "DATADIR"
      mandir = cfgVal "MANDIR"
      bindir = cfgVal "BINDIR"
      
  withCmdOptions [Cwd "src"] $ do
    let installPaths = []
        installChmod p chp = cmdSeq ["/bin/sh install-sh -c -d " ++ p
                                    ,"chmod " ++ show chp ++ " " ++ p]
        cpChmod src dest chp = cmdSeq ["cp " ++ src ++ " " ++ dest
                                      ,"chmod " ++ show chp ++ " " ++ dest]
        installml f = shcCmd $ "/bin/sh ./installml.sh install \"\" " ++ f ++
                      " vim vimdiff evim ex view rvim rview gvim gview rgvim rgview gvimdiff eview"
        installman t f1 v = shcCmd $ "/bin/sh ./installman.sh " ++ t ++ " " ++ f1 ++ " " ++ v ++ " "
                            ++ datadir ++ "/vim " ++ datadir ++ "/vim/vim81 "
                            ++ datadir ++ "/vim ../runtime/doc 644 vim vimdiff evim"
                      
    installChmod bindir 755
    
    shcCmd $ "if test -f " ++ bindir ++ "/vim; then mv -f " ++ bindir ++ "/vim " ++ bindir ++ "/vim.rm; rm -f " ++ bindir ++ "/vim.rm; fi"
    cmdSeq ["cp vim " ++ bindir
           ,"strip " ++ bindir ++ "/vim"
           ,"chmod 755 " ++ bindir ++ "/vim"]
    shcCmd "echo >/dev/null"
    installChmod (datadir ++ "/vim") 755
    installChmod (bindir ++ "/vimtutor") 755
    cmdSeq ["cp vimtutor " ++ bindir ++ "/vimtutor"
           ,"chmod 755 " ++ bindir ++ "/vimtutor"]
    installChmod (datadir ++ "/vim/vim81") 755
    let vim81dirs = ["doc", "print", "colors", "syntax", "indent", "ftplugin",
                     "autoload", "plugin", "tutor", "spell", "compiler"]
    forM_ vim81dirs $ \d -> installChmod (datadir ++ "/vim/vim81/" ++ d) 755
    installChmod (datadir ++ "/vim/vim81/autoload/dist") 755
    installChmod (datadir ++ "/vim/vim81/autoload/xml") 755

    installman "install" (mandir ++ "/man1") "\"\""

    shcCmd "cd ../runtime/doc; if test -z \"\" -a -f tags; then mv -f tags tags.dist; fi"
    shcCmd "echo generating help tags"
    withCmdOptions [Cwd "../runtime/doc"] $ do
      cmdSeq ["" ++ bindir ++ "/vim -u NONE -esX -c \"helptags ++t .\" -c quit"
             ,"files=`ls *.txt tags`"
             ,"files=\"$files `ls *.??x tags-?? 2>/dev/null || true`\""
             ,"cp $files " ++ datadir ++ "/vim/vim81/doc"
             ,"cd " ++ datadir ++ "/vim/vim81/doc"
             ,"chmod 644 $files"
             ,"if test -f tags.dist; then mv -f tags.dist tags; fi"]
        
    cmdSeq ["cp ../runtime/doc/*.pl " ++ datadir ++ "/vim/vim81/doc"
           ,"chmod 755 " ++ datadir ++ "/vim/vim81/doc/*.pl"]
    let cpToVim81 = ["menu.vim", "synmenu.vim", "delmenu.vim", "defaults.vim",
                     "evim.vim", "mswin.vim", "rgb.txt", "bugreport.vim",
                     "vimrc_example.vim", "gvimrc_example.vim", "filetype.vim",
                     "ftoff.vim", "scripts.vim", "ftplugin.vim", "ftplugof.vim",
                     "indent.vim", "indoff.vim", "optwin.vim"]
    forM_ cpToVim81 $ \f -> cpChmod ("../runtime/" ++ f) (datadir ++ "/vim/vim81/" ++ f) 644
    
    cmdSeq ["cd ../runtime/print", "cp *.ps " ++ datadir ++ "/vim/vim81/print"]
    cmdSeq ["cd " ++ datadir ++ "/vim/vim81/print", "chmod 644 *.ps"]
    cmdSeq ["cd ../runtime/colors", "cp -r *.vim tools README.txt " ++ datadir ++ "/vim/vim81/colors"]
    cmdSeq ["cd " ++ datadir ++ "/vim/vim81/colors", "chmod 755 tools"]
    cmdSeq ["cd " ++ datadir ++ "/vim/vim81/colors", "chmod 644 *.vim README.txt tools/*.vim"]
    cmdSeq ["cd ../runtime/syntax", "cp *.vim README.txt " ++ datadir ++ "/vim/vim81/syntax"]
    cmdSeq ["cd " ++ datadir ++ "/vim/vim81/syntax", "chmod 644 *.vim README.txt"]
    cmdSeq ["cd ../runtime/indent", "cp *.vim README.txt " ++ datadir ++ "/vim/vim81/indent"]
    cmdSeq ["cd " ++ datadir ++ "/vim/vim81/indent", "chmod 644 *.vim README.txt"]
    cmdSeq ["cd ../runtime/autoload", "cp *.vim README.txt " ++ datadir ++ "/vim/vim81/autoload"]
    cmdSeq ["cd " ++ datadir ++ "/vim/vim81/autoload", "chmod 644 *.vim README.txt"]
    cmdSeq ["cd ../runtime/autoload/dist", "cp *.vim " ++ datadir ++ "/vim/vim81/autoload/dist"]
    cmdSeq ["cd " ++ datadir ++ "/vim/vim81/autoload/dist", "chmod 644 *.vim"]
    cmdSeq ["cd ../runtime/autoload/xml", "cp *.vim " ++ datadir ++ "/vim/vim81/autoload/xml"]
    cmdSeq ["cd " ++ datadir ++ "/vim/vim81/autoload/xml", "chmod 644 *.vim"]
    cmdSeq ["cd ../runtime/plugin", "cp *.vim README.txt " ++ datadir ++ "/vim/vim81/plugin"]
    cmdSeq ["cd " ++ datadir ++ "/vim/vim81/plugin", "chmod 644 *.vim README.txt"]
    cmdSeq ["cd ../runtime/ftplugin", "cp *.vim README.txt logtalk.dict " ++ datadir ++ "/vim/vim81/ftplugin"]
    cmdSeq ["cd " ++ datadir ++ "/vim/vim81/ftplugin", "chmod 644 *.vim README.txt"]
    cmdSeq ["cd ../runtime/compiler", "cp *.vim README.txt " ++ datadir ++ "/vim/vim81/compiler"]
    cmdSeq ["cd " ++ datadir ++ "/vim/vim81/compiler", "chmod 644 *.vim README.txt"]
    cmdSeq ["/bin/sh install-sh -c -d " ++ datadir ++ "/vim/vim81/macros"
           ,"chmod 755 " ++ datadir ++ "/vim/vim81/macros"]
    cmdSeq ["cp -r ../runtime/macros/* " ++ datadir ++ "/vim/vim81/macros"
           ,"chmod 755 `find " ++ datadir ++ "/vim/vim81/macros -type d -print`"
           ,"chmod 644 `find " ++ datadir ++ "/vim/vim81/macros -type f -print`"
           ,"chmod 755 " ++ datadir ++ "/vim/vim81/macros/less.sh"]
    cmdSeq ["cvs=`find " ++ datadir ++ "/vim/vim81/macros \\( -name CVS -o -name AAPDIR -o -name \"*.info\" \\) -print`"
           ,"if test -n \"$cvs\"; then rm -rf $cvs; fi"]
    cmdSeq ["/bin/sh install-sh -c -d " ++ datadir ++ "/vim/vim81/pack"
           ,"chmod 755 " ++ datadir ++ "/vim/vim81/pack"]
    cmdSeq ["cp -r ../runtime/pack/* " ++ datadir ++ "/vim/vim81/pack"
           ,"chmod 755 `find " ++ datadir ++ "/vim/vim81/pack -type d -print`"
           ,"chmod 644 `find " ++ datadir ++ "/vim/vim81/pack -type f -print`"]
    cmdSeq ["cp ../runtime/tutor/README* ../runtime/tutor/tutor* " ++ datadir ++ "/vim/vim81/tutor"
           ,"rm -f " ++ datadir ++ "/vim/vim81/tutor/*.info"
           ,"chmod 644 " ++ datadir ++ "/vim/vim81/tutor/*"]
    shcCmd $ "if test -f ../runtime/spell/en.latin1.spl; then cp ../runtime/spell/*.spl ../runtime/spell/*.sug ../runtime/spell/*.vim " ++ datadir ++ "/vim/vim81/spell; chmod 644 " ++ datadir ++ "/vim/vim81/spell/*.spl " ++ datadir ++ "/vim/vim81/spell/*.sug " ++ datadir ++ "/vim/vim81/spell/*.vim; fi"
    cmdSeq ["cd " ++ bindir
           ,"ln -s vim ex"]
    cmdSeq ["cd " ++ bindir
           ,"ln -s vim view"]
    cmdSeq ["cd " ++ bindir
           ,"ln -s vim rvim"]
    cmdSeq ["cd " ++ bindir
           ,"ln -s vim rview"]
    cmdSeq ["cd " ++ bindir
           ,"ln -s vim vimdiff"]
    installml $ mandir ++ "/man1"
    installChmod (datadir ++ "/vim/vim81/tools") 755

    installman "xxd" (mandir ++ "/da/man1") "\"-da\""
    installman "xxd" (mandir ++ "/da.ISO8859-1/man1") "\"-da\""
    installman "xxd" (mandir ++ "/da.UTF-8/man1") "\"-da.UTF-8\""
    installman "xxd" (mandir ++ "/de/man1") "\"-de\""
    installman "xxd" (mandir ++ "/de.ISO8859-1/man1") "\"-de\""
    installman "xxd" (mandir ++ "/de.UTF-8/man1") "\"-de.UTF-8\""
    installman "xxd" (mandir ++ "/fr/man1") "\"-fr\""
    installman "xxd" (mandir ++ "/fr.ISO8859-1/man1") "\"-fr\""
    installman "xxd" (mandir ++ "/fr.UTF-8/man1") "\"-fr.UTF-8\""
    installman "xxd" (mandir ++ "/it/man1") "\"-it\""
    installman "xxd" (mandir ++ "/it.ISO8859-1/man1") "\"-it\""
    installman "xxd" (mandir ++ "/it.UTF-8/man1") "\"-it.UTF-8\""
    installman "xxd" (mandir ++ "/ja/man1") "\"-ja.UTF-8\""
    installman "xxd" (mandir ++ "/pl/man1") "\"-pl\""
    installman "xxd" (mandir ++ "/pl.ISO8859-2/man1") "\"-pl\""
    installman "xxd" (mandir ++ "/pl.UTF-8/man1") "\"-pl.UTF-8\""
    installman "xxd" (mandir ++ "/ru.KOI8-R/man1") "\"-ru\""
    installman "xxd" (mandir ++ "/ru.UTF-8/man1") "\"-ru.UTF-8\""
    shcCmd $ "if test -f " ++ bindir ++ "/xxd; then mv -f " ++ bindir ++ "/xxd " ++ bindir ++ "/xxd.rm; rm -f " ++ bindir ++ "/xxd.rm; fi"
    cmdSeq ["cp xxd/xxd " ++ bindir
           ,"strip " ++ bindir ++ "/xxd"
           ,"chmod 755 " ++ bindir ++ "/xxd"]
    installman "xxd" (mandir ++ "/man1") "\"\""
    
    cmdSeq ["cp -r ../runtime/tools/* " ++ datadir ++ "/vim/vim81/tools"
           ,"cvs=`find " ++ datadir ++ "/vim/vim81/tools \\( -name CVS -o -name AAPDIR \\) -print`"
           ,"if test -n \"$cvs\"; then rm -rf $cvs; fi"
           ,"chmod 644 " ++ datadir ++ "/vim/vim81/tools/*"
           ,"perlpath=`./which.sh perl` && sed -e \"s+/usr/bin/perl+$perlpath+\" ../runtime/tools/efm_perl.pl >" ++ datadir ++ "/vim/vim81/tools/efm_perl.pl"
           ,"awkpath=`./which.sh nawk` && sed -e \"s+/usr/bin/nawk+$awkpath+\" ../runtime/tools/mve.awk >" ++ datadir ++ "/vim/vim81/tools/mve.awk"
           ,"if test -z \"$awkpath\"; then awkpath=`./which.sh gawk` && sed -e \"s+/usr/bin/nawk+$awkpath+\" ../runtime/tools/mve.awk >" ++ datadir ++ "/vim/vim81/tools/mve.awk; if test -z \"$awkpath\"; then awkpath=`./which.sh awk` && sed -e \"s+/usr/bin/nawk+$awkpath+\" ../runtime/tools/mve.awk >" ++ datadir ++ "/vim/vim81/tools/mve.awk; fi; fi"]
    shcCmd $ "chmod 755 `grep -l \"^#!\" " ++ datadir ++ "/vim/vim81/tools/*`"

    installChmod (datadir ++ "/vim/vim81/lang") 755
    installChmod (datadir ++ "/vim/vim81/keymap") 755

    installman "install" (mandir ++ "/da/man1") "\"-da\""
    installman "install" (mandir ++ "/da.ISO8859-1/man1") "\"-da\""
    installman "install" (mandir ++ "/da.UTF-8/man1") "\"-da.UTF-8\""
    installman "install" (mandir ++ "/de/man1") "\"-de\""
    installman "install" (mandir ++ "/de.ISO8859-1/man1") "\"-de\""
    installman "install" (mandir ++ "/de.UTF-8/man1") "\"-de.UTF-8\""
    installman "install" (mandir ++ "/fr/man1") "\"-fr\""
    installman "install" (mandir ++ "/fr.ISO8859-1/man1") "\"-fr\""
    installman "install" (mandir ++ "/fr.UTF-8/man1") "\"-fr.UTF-8\""
    installman "install" (mandir ++ "/it/man1") "\"-it\""
    installman "install" (mandir ++ "/it.ISO8859-1/man1") "\"-it\""
    installman "install" (mandir ++ "/it.UTF-8/man1") "\"-it.UTF-8\""
    installman "install" (mandir ++ "/ja/man1") "\"-ja.UTF-8\""
    installman "install" (mandir ++ "/pl/man1") "\"-pl\""
    installman "install" (mandir ++ "/pl.ISO8859-2/man1") "\"--pl\""
    installman "install" (mandir ++ "/pl.UTF-8/man1") "\"-pl.UTF-8\""
    installman "install" (mandir ++ "/ru.KOI8-R/man1") "\"-ru\""
    installman "install" (mandir ++ "/ru.UTF-8/man1") "\"-ru.UTF-8\""

    installml $ mandir ++ "/da/man1"
    installml $ mandir ++ "/da.ISO8859-1/man1"
    installml $ mandir ++ "/da.UTF-8/man1"
    installml $ mandir ++ "/de/man1"
    installml $ mandir ++ "/de.ISO8859-1/man1"
    installml $ mandir ++ "/de.UTF-8/man1"
    installml $ mandir ++ "/fr/man1"
    installml $ mandir ++ "/fr.ISO8859-1/man1"
    installml $ mandir ++ "/fr.UTF-8/man1"
    installml $ mandir ++ "/it/man1"
    installml $ mandir ++ "/it.ISO8859-1/man1"
    installml $ mandir ++ "/it.UTF-8/man1"
    installml $ mandir ++ "/ja/man1"
    installml $ mandir ++ "/pl/man1"
    installml $ mandir ++ "/pl.ISO8859-2/man1"
    installml $ mandir ++ "/pl.UTF-8/man1"
    installml $ mandir ++ "/ru.KOI8-R/man1"
    installml $ mandir ++ "/ru.UTF-8.man1"
    withCmdOptions [Cwd "po"] $ do
      shcCmd $ "if test \"x\" = \"x" ++ prefix ++ "\"; then echo \"******************************************\"; echo \" please use make from the src directory \"; echo \"******************************************\"; exit 1; fi"
      shcCmd $ "for lang in af ca cs cs.cp1250 da de en_GB eo es fi fr ga it ja ja.euc-jp ja.sjis ko ko.UTF-8 lv nb nl no pl pl.UTF-8 pl.cp1250 pt_BR ru ru.cp1251 sk sk.cp1250 sr sv uk uk.cp1251 vi zh_CN zh_CN.UTF-8 zh_CN.cp936 zh_TW zh_TW.UTF-8 ; do dir=" ++ datadir ++ "/vim/vim81/lang/$lang/; if test ! -x \"$dir\"; then mkdir $dir; chmod 755 $dir; fi; dir=" ++ datadir ++ "/vim/vim81/lang/$lang/LC_MESSAGES; if test ! -x \"$dir\"; then mkdir $dir; chmod 755 $dir; fi; if test -r $lang.mo; then cp $lang.mo $dir/vim.mo; chmod 644 $dir/vim.mo; fi; done"
    shcCmd $ "if test -d ../runtime/lang; then cp ../runtime/lang/README.txt ../runtime/lang/*.vim " ++ datadir ++ "/vim/vim81/lang; chmod 644 " ++ datadir ++ "/vim/vim81/lang/README.txt " ++ datadir ++ "/vim/vim81/lang/*.vim; fi"
    shcCmd $ "if test -d ../runtime/keymap; then cp ../runtime/keymap/README.txt ../runtime/keymap/*.vim " ++ datadir ++ "/vim/vim81/keymap; chmod 644 " ++ datadir ++ "/vim/vim81/keymap/README.txt " ++ datadir ++ "/vim/vim81/keymap/*.vim; fi"
    shcCmd $ "if test -n \"\"; then /bin/sh install-sh -c -d " ++ datadir ++ "/icons/hicolor/48x48/apps " ++ datadir ++ "/icons/locolor/32x32/apps " ++ datadir ++ "/icons/locolor/16x16/apps " ++ datadir ++ "/applications; fi"
    shcCmd $ "if test -d " ++ datadir ++ "/icons/hicolor/48x48/apps -a -w " ++ datadir ++ "/icons/hicolor/48x48/apps -a ! -f " ++ datadir ++ "/icons/hicolor/48x48/apps/gvim.png; then cp ../runtime/vim48x48.png " ++ datadir ++ "/icons/hicolor/48x48/apps/gvim.png; if test -z \"\" -a -x \"\" -a -w " ++ datadir ++ "/icons/hicolor -a -f " ++ datadir ++ "/icons/hicolor/index.theme; then -q " ++ datadir ++ "/icons/hicolor; fi fi"
    shcCmd $ "if test -d " ++ datadir ++ "/icons/locolor/32x32/apps -a -w " ++ datadir ++ "/icons/locolor/32x32/apps -a ! -f " ++ datadir ++ "/icons/locolor/32x32/apps/gvim.png; then cp ../runtime/vim32x32.png " ++ datadir ++ "/icons/locolor/32x32/apps/gvim.png; fi"
    shcCmd $ "if test -d " ++ datadir ++ "/icons/locolor/16x16/apps -a -w " ++ datadir ++ "/icons/locolor/16x16/apps -a ! -f " ++ datadir ++ "/icons/locolor/16x16/apps/gvim.png; then cp ../runtime/vim16x16.png " ++ datadir ++ "/icons/locolor/16x16/apps/gvim.png; fi"
    shcCmd $ "if test -d " ++ datadir ++ "/applications -a -w " ++ datadir ++ "/applications; then if test -f po/vim.desktop -a -f po/gvim.desktop; then cp po/vim.desktop po/gvim.desktop " ++ datadir ++ "/applications; else cp ../runtime/vim.desktop ../runtime/gvim.desktop " ++ datadir ++ "/applications; fi; if test -z \"\" -a -x \"\"; then -q " ++ datadir ++ "/applications; fi fi"
