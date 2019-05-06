{-# LANGUAGE ScopedTypeVariables #-}

module Builder.Vim(build, install) where

import System.Info.Extra
import Development.Rattle
import System.FilePattern.Directory
import System.FilePath
import Control.Monad
import Data.List


build :: FilePath -> Run ()
build prefix = if isWindows || isMac
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
  c2 <- liftIO $ getDirectoryFiles "src" ["libvterm/src/*.c"]
  po <- liftIO $ getDirectoryFiles "src/po" ["*.po"]
  cmd ["sh", "-c", "if test ! -f src/auto/config.mk; then cp src/config.mk.dist src/auto/config.mk; fi"]
  cmd ["sh", "-c", "echo \"Starting make in the src directory.\""]
  cmd ["sh", "-c", "echo \"If there are problems, cd to the src directory and run make there\""]
  withCmdOptions [Cwd "src"] $ do
    cmd ["sh", "-c", "/bin/sh install-sh -c -d objects"]
    cmd ["sh", "-c", "touch objects/.dirstamp"]
    cmd ["sh", "-c", "CC=\"gcc -Iproto -DHAVE_CONFIG_H \" srcdir=. sh ./osdef.sh"]
    cmd ["sh", "-c", "echo creating auto/pathdef.c; \
                     \ echo '/* pathdef.c */' > auto/pathdef.c; \
                     \ echo '/* This file is automatically created by Makefile' >> auto/pathdef.c; \
                     \ echo ' * DO NOT EDIT! Change Makefile only. */' >> auto/pathdef.c; \
                     \ echo '#include \"vim.h\"' >> auto/pathdef.c; \
                     \ echo 'char_u *default_vim_dir = (char_u *)\"" ++ prefix ++ "/share/vim\";' | sed -e 's/[\\\\\"]/\\\\&/g' -e 's/\\\\\"/\"/' -e 's/\\\\\";$/\";/' >> auto/pathdef.c; \
                     \ echo 'char_u *default_vimruntime_dir = (char_u *)\"\";' | sed -e 's/[\\\\\"]/\\\\&/g' -e 's/\\\\\"/\"/' -e 's/\\\\\";$/\";/' >> auto/pathdef.c; \
                                                                         \ echo 'char_u *all_cflags = (char_u *)\"gcc -c -I. -Iproto -DHAVE_CONFIG_H -g -O2 -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=1 \";' | sed -e 's/[\\\\\"]/\\\\&/g' -e 's/\\\\\"/\"/' -e 's/\\\\\";$/\";/' >> auto/pathdef.c; \
                                                                         \ echo 'char_u *all_lflags = (char_u *)\"gcc -L/usr/local/lib -Wl,--as-needed -o vim -lSM -lICE -lXt -lX11 -lXdmcp -lSM -lICE -lm -ltinfo -lnsl -ldl \";' | sed -e 's/[\\\\\"]/\\\\&/g' -e 's/\\\\\"/\"/' -e 's/\\\\\";$/\";/' >> auto/pathdef.c; \
                                                                         \ echo 'char_u *compiled_user = (char_u *)\"' | tr -d \"\\\\012\" >> auto/pathdef.c; \
                                                                         \ if test -n \"\"; then echo \"\" | tr -d \"\\\\012\" >> auto/pathdef.c; else ((logname) 2>/dev/null || whoami) | tr -d \"\\\\012\" >> auto/pathdef.c; fi; \
                                                                         \ echo '\";' >> auto/pathdef.c; \
                                                                         \ echo 'char_u *compiled_sys = (char_u *)\"' | tr -d \"\\\\012\" >> auto/pathdef.c; \
                                                                         \ if test -z \"\"; then hostname | tr -d \"\\\\012\" >> auto/pathdef.c; fi; \
                                                                         \ echo '\";' >> auto/pathdef.c"]
    cmd ["sh", "-c", "sh ./pathdef.sh"]
    forM_ c1 $ \c -> cmd "gcc -c -I. -Iproto -DHAVE_CONFIG_H -g -O2 -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=1 -o" [to0 c, c]
    forM_ c2 $ \c -> cmd ["sh", "-c", "gcc -c -I. -Ilibvterm/include -Iproto -DHAVE_CONFIG_H -g -O2 -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=1 -DINLINE=\"\" -DVSNPRINTF=vim_vsnprintf -DIS_COMBINING_FUNCTION=utf_iscomposing_uint -DWCWIDTH_FUNCTION=utf_uint2cells -o " ++ to0 c ++ " " ++ c]
    o1 <- liftIO $ getDirectoryFiles "src" ["objects/*.o"]
    cmd ["sh", "-c", "LINK=\" gcc -L/usr/local/lib -Wl,--as-needed -o vim " ++ unwords o1 ++ " -lSM -lICE -lXt -lX11 -lXdmcp -lSM -lICE -lm -ltinfo -lnsl -ldl \" MAKE=\"submake\" LINK_AS_NEEDED=yes sh ./link.sh"]
    
    withCmdOptions [Cwd "xxd"] $ do
      cmd ["sh", "-c", "gcc -g -O2 -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=1 -L/usr/local/lib -Wl,--as-needed -DUNIX -o xxd xxd.c"]
    withCmdOptions [Cwd "po"] $ do
      cmd ["sh", "-c", "rm -f pl.UTF-8.po; \
                       \ iconv -f iso-8859-2 -t utf-8 pl.po | sed -e 's/charset=ISO-8859-2/charset=UTF-8/' -e 's/# Original translations/# Generated from pl.po, DO NOT EDIT/' > pl.UTF-8.po"]
      cmd ["sh", "-c", "rm -f ru.cp1251.po; \
                       \ iconv -f utf-8 -t cp1251 ru.po | sed -e 's/charset=[uU][tT][fF]-8/charset=cp1251/' -e 's/# Original translations/# Generated from ru.po, DO NOT EDIT/' > ru.cp1251.po"] 
      forM_ po $ \p -> cmd ["sh", "-c", "OLD_PO_FILE_INPUT=yes msgfmt -v -o " ++ tom0 p ++ " " ++ p]
      cmd ["sh", "-c", "echo " ++ unwords (baseAll po) ++ " | tr \" \" \"\\n\" |sed -e '/\\./d' | sort > LINGUAS"]
      cmd ["sh", "-c", "msgfmt --desktop -d . --template gvim.desktop.in -o gvim.desktop"]
      cmd ["sh", "-c", "echo " ++ unwords (baseAll po) ++ " | tr \" \" \"\\n\" |sed -e '/\\./d' | sort > LINGUAS"]
      cmd ["sh", "-c", "msgfmt --desktop -d . --template vim.desktop.in -o vim.desktop"]
      cmd "echo Converted submake did nothing"




install :: FilePath -> Run ()
install prefix = if isWindows || isMac
                 then do cmd "echo Windows and Mac are not currently supported."
                 else do
  cmd ["sh", "-c", "if test ! -f src/auto/config.mk; then cp src/config.mk.dist src/auto/config.mk; fi"]
  cmd ["sh", "-c", "echo \"Starting make in the src directory.\""]
  cmd ["sh", "-c", "echo \"If there are problems, cd to the src directory and run make there\""]
  withCmdOptions [Cwd "src"] $ do
    let installPaths = []
        installChmod p chp = cmd ["sh", "-c", "/bin/sh install-sh -c -d " ++ p ++
                                              "; chmod " ++ show chp ++ " " ++ p]
        cpChmod src dest chp = cmd ["sh", "-c", "cp " ++ src ++ " " ++ dest ++
                                                "; chmod " ++ show chp ++ " " ++ dest]
        installml f = cmd ["sh", "-c", "/bin/sh ./installml.sh install \"\" " ++ f ++
                                       " vim vimdiff evim ex view rvim rview gvim gview rgvim rgview gvimdiff eview"]
        installman t f1 v = cmd ["sh", "-c", "/bin/sh ./installman.sh " ++ t ++ " " ++ f1 ++ " " ++ v ++ " "
                                             ++ prefix ++ "/share/vim " ++ prefix ++ "/share/vim/vim81 "
                                             ++ prefix ++ "/share/vim ../runtime/doc 644 vim vimdiff evim"]
                      
    installChmod (prefix ++ "/bin") 755
    
    cmd ["sh", "-c", "if test -f " ++ prefix ++ "/bin/vim; then mv -f " ++ prefix ++ "/bin/vim " ++ prefix ++ "/bin/vim.rm; rm -f " ++ prefix ++ "/bin/vim.rm; fi"]
    cmd ["sh", "-c", "cp vim " ++ prefix ++ "/bin; \
                                            \ strip " ++ prefix ++ "/bin/vim; \
                                                                   \ chmod 755 " ++ prefix ++ "/bin/vim"]
    cmd ["sh", "-c", "echo >/dev/null"]
    installChmod (prefix ++ "/share/vim") 755
    installChmod (prefix ++ "/bin/vimtutor") 755
    cmd ["sh", "-c", "cp vimtutor " ++ prefix ++ "/bin/vimtutor; \
                                                 \ chmod 755 " ++ prefix ++ "/bin/vimtutor"]
    installChmod (prefix ++ "/share/vim/vim81") 755
    let vim81dirs = ["doc", "print", "colors", "syntax", "indent", "ftplugin",
                     "autoload", "plugin", "tutor", "spell", "compiler"]
    forM_ vim81dirs $ \d -> installChmod (prefix ++ "/share/vim/vim81/" ++ d) 755
    installChmod (prefix ++ "/share/vim/vim81/autoload/dist") 755
    installChmod (prefix ++ "/share/vim/vim81/autoload/xml") 755

    installman "install" (prefix ++ "/share/man/man1") "\"\""

    cmd ["sh", "-c", "cd ../runtime/doc; if test -z \"\" -a -f tags; then mv -f tags tags.dist; fi"]
    cmd ["sh", "-c", "echo generating help tags"]
    withCmdOptions [Cwd "../runtime/doc"] $ do
      cmd ["sh", "-c", "" ++ prefix ++ "/bin/vim -u NONE -esX -c \"helptags ++t .\" -c quit; \
                                       \ files=`ls *.txt tags`; files=\"$files `ls *.??x tags-?? 2>/dev/null || true`\"; cp $files " ++ prefix ++ "/share/vim/vim81/doc; cd " ++ prefix ++ "/share/vim/vim81/doc; chmod 644 $files; \
                                                                                                                                                                                           \ if test -f tags.dist; then mv -f tags.dist tags; fi"]
        
    cmd ["sh", "-c", "cp ../runtime/doc/*.pl " ++ prefix ++ "/share/vim/vim81/doc; \
                                                            \ chmod 755 " ++ prefix ++ "/share/vim/vim81/doc/*.pl"]
    let cpToVim81 = ["menu.vim", "synmenu.vim", "delmenu.vim", "defaults.vim",
                     "evim.vim", "mswin.vim", "rgb.txt", "bugreport.vim",
                     "vimrc_example.vim", "gvimrc_example.vim", "filetype.vim",
                     "ftoff.vim", "scripts.vim", "ftplugin.vim", "ftplugof.vim",
                     "indent.vim", "indoff.vim", "optwin.vim"]
    forM_ cpToVim81 $ \f -> cpChmod ("../runtime/" ++ f) (prefix ++ "/share/vim/vim81/" ++ f) 644
    
    cmd ["sh", "-c", "cd ../runtime/print; cp *.ps " ++ prefix ++ "/share/vim/vim81/print"]
    cmd ["sh", "-c", "cd " ++ prefix ++ "/share/vim/vim81/print; chmod 644 *.ps"]
    cmd ["sh", "-c", "cd ../runtime/colors; cp -r *.vim tools README.txt " ++ prefix ++ "/share/vim/vim81/colors"]
    cmd ["sh", "-c", "cd " ++ prefix ++ "/share/vim/vim81/colors; chmod 755 tools"]
    cmd ["sh", "-c", "cd " ++ prefix ++ "/share/vim/vim81/colors; chmod 644 *.vim README.txt tools/*.vim"]
    cmd ["sh", "-c", "cd ../runtime/syntax; cp *.vim README.txt " ++ prefix ++ "/share/vim/vim81/syntax"]
    cmd ["sh", "-c", "cd " ++ prefix ++ "/share/vim/vim81/syntax; chmod 644 *.vim README.txt"]
    cmd ["sh", "-c", "cd ../runtime/indent; cp *.vim README.txt " ++ prefix ++ "/share/vim/vim81/indent"]
    cmd ["sh", "-c", "cd " ++ prefix ++ "/share/vim/vim81/indent; chmod 644 *.vim README.txt"]
    cmd ["sh", "-c", "cd ../runtime/autoload; cp *.vim README.txt " ++ prefix ++ "/share/vim/vim81/autoload"]
    cmd ["sh", "-c", "cd " ++ prefix ++ "/share/vim/vim81/autoload; chmod 644 *.vim README.txt"]
    cmd ["sh", "-c", "cd ../runtime/autoload/dist; cp *.vim " ++ prefix ++ "/share/vim/vim81/autoload/dist"]
    cmd ["sh", "-c", "cd " ++ prefix ++ "/share/vim/vim81/autoload/dist; chmod 644 *.vim"]
    cmd ["sh", "-c", "cd ../runtime/autoload/xml; cp *.vim " ++ prefix ++ "/share/vim/vim81/autoload/xml"]
    cmd ["sh", "-c", "cd " ++ prefix ++ "/share/vim/vim81/autoload/xml; chmod 644 *.vim"]
    cmd ["sh", "-c", "cd ../runtime/plugin; cp *.vim README.txt " ++ prefix ++ "/share/vim/vim81/plugin"]
    cmd ["sh", "-c", "cd " ++ prefix ++ "/share/vim/vim81/plugin; chmod 644 *.vim README.txt"]
    cmd ["sh", "-c", "cd ../runtime/ftplugin; cp *.vim README.txt logtalk.dict " ++ prefix ++ "/share/vim/vim81/ftplugin"]
    cmd ["sh", "-c", "cd " ++ prefix ++ "/share/vim/vim81/ftplugin; chmod 644 *.vim README.txt"]
    cmd ["sh", "-c", "cd ../runtime/compiler; cp *.vim README.txt " ++ prefix ++ "/share/vim/vim81/compiler"]
    cmd ["sh", "-c", "cd " ++ prefix ++ "/share/vim/vim81/compiler; chmod 644 *.vim README.txt"]
    cmd ["sh", "-c", "/bin/sh install-sh -c -d " ++ prefix ++ "/share/vim/vim81/macros; \
                                                              \ chmod 755 " ++ prefix ++ "/share/vim/vim81/macros"]
    cmd ["sh", "-c", "cp -r ../runtime/macros/* " ++ prefix ++ "/share/vim/vim81/macros; \
                                                               \ chmod 755 `find " ++ prefix ++ "/share/vim/vim81/macros -type d -print`; \
                                                                                                \ chmod 644 `find " ++ prefix ++ "/share/vim/vim81/macros -type f -print`; \
                                                                                                                                 \ chmod 755 " ++ prefix ++ "/share/vim/vim81/macros/less.sh"]
    cmd ["sh", "-c", "cvs=`find " ++ prefix ++ "/share/vim/vim81/macros \\( -name CVS -o -name AAPDIR -o -name \"*.info\" \\) -print`; if test -n \"$cvs\"; then rm -rf $cvs; fi"]
    cmd ["sh", "-c", "/bin/sh install-sh -c -d " ++ prefix ++ "/share/vim/vim81/pack; \
                                                              \ chmod 755 " ++ prefix ++ "/share/vim/vim81/pack"]
    cmd ["sh", "-c", "cp -r ../runtime/pack/* " ++ prefix ++ "/share/vim/vim81/pack; \
                                                             \ chmod 755 `find " ++ prefix ++ "/share/vim/vim81/pack -type d -print`; \
                                                                                              \ chmod 644 `find " ++ prefix ++ "/share/vim/vim81/pack -type f -print`"]
    cmd ["sh", "-c", "cp ../runtime/tutor/README* ../runtime/tutor/tutor* " ++ prefix ++ "/share/vim/vim81/tutor; \
                                                                                         \ rm -f " ++ prefix ++ "/share/vim/vim81/tutor/*.info; \
                                                                                                                \ chmod 644 " ++ prefix ++ "/share/vim/vim81/tutor/*"]
    cmd ["sh", "-c", "if test -f ../runtime/spell/en.latin1.spl; then cp ../runtime/spell/*.spl ../runtime/spell/*.sug ../runtime/spell/*.vim " ++ prefix ++ "/share/vim/vim81/spell; chmod 644 " ++ prefix ++ "/share/vim/vim81/spell/*.spl " ++ prefix ++ "/share/vim/vim81/spell/*.sug " ++ prefix ++ "/share/vim/vim81/spell/*.vim; fi"]
    cmd ["sh", "-c", "cd " ++ prefix ++ "/bin; ln -s vim ex"]
    cmd ["sh", "-c", "cd " ++ prefix ++ "/bin; ln -s vim view"]
    cmd ["sh", "-c", "cd " ++ prefix ++ "/bin; ln -s vim rvim"]
    cmd ["sh", "-c", "cd " ++ prefix ++ "/bin; ln -s vim rview"]
    cmd ["sh", "-c", "cd " ++ prefix ++ "/bin; ln -s vim vimdiff"]
    installml $ prefix ++ "/share/man/man1"
    installChmod (prefix ++ "/share/vim/vim81/tools") 755

    installman "xxd" (prefix ++ "/share/man/da/man1") "\"-da\""
    installman "xxd" (prefix ++ "/share/man/da.ISO8859-1/man1") "\"-da\""
    installman "xxd" (prefix ++ "/share/man/da.UTF-8/man1") "\"-da.UTF-8\""
    installman "xxd" (prefix ++ "/share/man/de/man1") "\"-de\""
    installman "xxd" (prefix ++ "/share/man/de.ISO8859-1/man1") "\"-de\""
    installman "xxd" (prefix ++ "/share/man/de.UTF-8/man1") "\"-de.UTF-8\""
    installman "xxd" (prefix ++ "/share/man/fr/man1") "\"-fr\""
    installman "xxd" (prefix ++ "/share/man/fr.ISO8859-1/man1") "\"-fr\""
    installman "xxd" (prefix ++ "/share/man/fr.UTF-8/man1") "\"-fr.UTF-8\""
    installman "xxd" (prefix ++ "/share/man/it/man1") "\"-it\""
    installman "xxd" (prefix ++ "/share/man/it.ISO8859-1/man1") "\"-it\""
    installman "xxd" (prefix ++ "/share/man/it.UTF-8/man1") "\"-it.UTF-8\""
    installman "xxd" (prefix ++ "/share/man/ja/man1") "\"-ja.UTF-8\""
    installman "xxd" (prefix ++ "/share/man/pl/man1") "\"-pl\""
    installman "xxd" (prefix ++ "/share/man/pl.ISO8859-2/man1") "\"-pl\""
    installman "xxd" (prefix ++ "/share/man/pl.UTF-8/man1") "\"-pl.UTF-8\""
    installman "xxd" (prefix ++ "/share/man/ru.KOI8-R/man1") "\"-ru\""
    installman "xxd" (prefix ++ "/share/man/ru.UTF-8/man1") "\"-ru.UTF-8\""
    cmd ["sh", "-c", "if test -f " ++ prefix ++ "/bin/xxd; then mv -f " ++ prefix ++ "/bin/xxd " ++ prefix ++ "/bin/xxd.rm; rm -f " ++ prefix ++ "/bin/xxd.rm; fi"]
    cmd ["sh", "-c", "cp xxd/xxd " ++ prefix ++ "/bin; \
                                                \ strip " ++ prefix ++ "/bin/xxd; \
                                                                       \ chmod 755 " ++ prefix ++ "/bin/xxd"]
    installman "xxd" (prefix ++ "/share/man/man1") "\"\""
    
    cmd ["sh", "-c", "cp -r ../runtime/tools/* " ++ prefix ++ "/share/vim/vim81/tools; \
    \ cvs=`find " ++ prefix ++ "/share/vim/vim81/tools \\( -name CVS -o -name AAPDIR \\) -print`; if test -n \"$cvs\"; then rm -rf $cvs; fi; \
    \ chmod 644 " ++ prefix ++ "/share/vim/vim81/tools/*; \
    \ perlpath=`./which.sh perl` && sed -e \"s+/usr/bin/perl+$perlpath+\" ../runtime/tools/efm_perl.pl >" ++ prefix ++ "/share/vim/vim81/tools/efm_perl.pl; \
    \ awkpath=`./which.sh nawk` && sed -e \"s+/usr/bin/nawk+$awkpath+\" ../runtime/tools/mve.awk >" ++ prefix ++ "/share/vim/vim81/tools/mve.awk; if test -z \"$awkpath\"; then awkpath=`./which.sh gawk` && sed -e \"s+/usr/bin/nawk+$awkpath+\" ../runtime/tools/mve.awk >" ++ prefix ++ "/share/vim/vim81/tools/mve.awk; if test -z \"$awkpath\"; then awkpath=`./which.sh awk` && sed -e \"s+/usr/bin/nawk+$awkpath+\" ../runtime/tools/mve.awk >" ++ prefix ++ "/share/vim/vim81/tools/mve.awk; fi; fi"]
    cmd ["sh", "-c", "chmod 755 `grep -l \"^#!\" " ++ prefix ++ "/share/vim/vim81/tools/*`"]

    installChmod (prefix ++ "/share/vim/vim81/lang") 755
    installChmod (prefix ++ "/share/vim/vim81/keymap") 755

    installman "install" (prefix ++ "/share/man/da/man1") "\"-da\""
    installman "install" (prefix ++ "/share/man/da.ISO8859-1/man1") "\"-da\""
    installman "install" (prefix ++ "/share/man/da.UTF-8/man1") "\"-da.UTF-8\""
    installman "install" (prefix ++ "/share/man/de/man1") "\"-de\""
    installman "install" (prefix ++ "/share/man/de.ISO8859-1/man1") "\"-de\""
    installman "install" (prefix ++ "/share/man/de.UTF-8/man1") "\"-de.UTF-8\""
    installman "install" (prefix ++ "/share/man/fr/man1") "\"-fr\""
    installman "install" (prefix ++ "/share/man/fr.ISO8859-1/man1") "\"-fr\""
    installman "install" (prefix ++ "/share/man/fr.UTF-8/man1") "\"-fr.UTF-8\""
    installman "install" (prefix ++ "/share/man/it/man1") "\"-it\""
    installman "install" (prefix ++ "/share/man/it.ISO8859-1/man1") "\"-it\""
    installman "install" (prefix ++ "/share/man/it.UTF-8/man1") "\"-it.UTF-8\""
    installman "install" (prefix ++ "/share/man/ja/man1") "\"-ja.UTF-8\""
    installman "install" (prefix ++ "/share/man/pl/man1") "\"-pl\""
    installman "install" (prefix ++ "/share/man/pl.ISO8859-2/man1") "\"--pl\""
    installman "install" (prefix ++ "/share/man/pl.UTF-8/man1") "\"-pl.UTF-8\""
    installman "install" (prefix ++ "/share/man/ru.KOI8-R/man1") "\"-ru\""
    installman "install" (prefix ++ "/share/man/ru.UTF-8/man1") "\"-ru.UTF-8\""

    installml $ prefix ++ "/share/man/da/man1"
    installml $ prefix ++ "/share/man/da.ISO8859-1/man1"
    installml $ prefix ++ "/share/man/da.UTF-8/man1"
    installml $ prefix ++ "/share/man/de/man1"
    installml $ prefix ++ "/share/man/de.ISO8859-1/man1"
    installml $ prefix ++ "/share/man/de.UTF-8/man1"
    installml $ prefix ++ "/share/man/fr/man1"
    installml $ prefix ++ "/share/man/fr.ISO8859-1/man1"
    installml $ prefix ++ "/share/man/fr.UTF-8/man1"
    installml $ prefix ++ "/share/man/it/man1"
    installml $ prefix ++ "/share/man/it.ISO8859-1/man1"
    installml $ prefix ++ "/share/man/it.UTF-8/man1"
    installml $ prefix ++ "/share/man/ja/man1"
    installml $ prefix ++ "/share/man/pl/man1"
    installml $ prefix ++ "/share/man/pl.ISO8859-2/man1"
    installml $ prefix ++ "/share/man/pl.UTF-8/man1"
    installml $ prefix ++ "/share/man/ru.KOI8-R/man1"
    installml $ prefix ++ "/share/man/ru.UTF-8.man1"
    withCmdOptions [Cwd "po"] $ do
      cmd ["sh", "-c", "if test \"x\" = \"x" ++ prefix ++ "\"; then echo \"******************************************\"; echo \" please use make from the src directory \"; echo \"******************************************\"; exit 1; fi"]
      cmd ["sh", "-c", "for lang in af ca cs cs.cp1250 da de en_GB eo es fi fr ga it ja ja.euc-jp ja.sjis ko ko.UTF-8 lv nb nl no pl pl.UTF-8 pl.cp1250 pt_BR ru ru.cp1251 sk sk.cp1250 sr sv uk uk.cp1251 vi zh_CN zh_CN.UTF-8 zh_CN.cp936 zh_TW zh_TW.UTF-8 ; do dir=" ++ prefix ++ "/share/vim/vim81/lang/$lang/; if test ! -x \"$dir\"; then mkdir $dir; chmod 755 $dir; fi; dir=" ++ prefix ++ "/share/vim/vim81/lang/$lang/LC_MESSAGES; if test ! -x \"$dir\"; then mkdir $dir; chmod 755 $dir; fi; if test -r $lang.mo; then cp $lang.mo $dir/vim.mo; chmod 644 $dir/vim.mo; fi; done"]
    cmd ["sh", "-c", "if test -d ../runtime/lang; then cp ../runtime/lang/README.txt ../runtime/lang/*.vim " ++ prefix ++ "/share/vim/vim81/lang; chmod 644 " ++ prefix ++ "/share/vim/vim81/lang/README.txt " ++ prefix ++ "/share/vim/vim81/lang/*.vim; fi"]
    cmd ["sh", "-c", "if test -d ../runtime/keymap; then cp ../runtime/keymap/README.txt ../runtime/keymap/*.vim " ++ prefix ++ "/share/vim/vim81/keymap; chmod 644 " ++ prefix ++ "/share/vim/vim81/keymap/README.txt " ++ prefix ++ "/share/vim/vim81/keymap/*.vim; fi"]
    cmd ["sh", "-c", "if test -n \"\"; then /bin/sh install-sh -c -d " ++ prefix ++ "/share/icons/hicolor/48x48/apps " ++ prefix ++ "/share/icons/locolor/32x32/apps " ++ prefix ++ "/share/icons/locolor/16x16/apps " ++ prefix ++ "/share/applications; fi"]
    cmd ["sh", "-c", "if test -d " ++ prefix ++ "/share/icons/hicolor/48x48/apps -a -w " ++ prefix ++ "/share/icons/hicolor/48x48/apps -a ! -f " ++ prefix ++ "/share/icons/hicolor/48x48/apps/gvim.png; then cp ../runtime/vim48x48.png " ++ prefix ++ "/share/icons/hicolor/48x48/apps/gvim.png; if test -z \"\" -a -x \"\" -a -w " ++ prefix ++ "/share/icons/hicolor -a -f " ++ prefix ++ "/share/icons/hicolor/index.theme; then -q " ++ prefix ++ "/share/icons/hicolor; fi fi"]
    cmd ["sh", "-c", "if test -d " ++ prefix ++ "/share/icons/locolor/32x32/apps -a -w " ++ prefix ++ "/share/icons/locolor/32x32/apps -a ! -f " ++ prefix ++ "/share/icons/locolor/32x32/apps/gvim.png; then cp ../runtime/vim32x32.png " ++ prefix ++ "/share/icons/locolor/32x32/apps/gvim.png; fi"]
    cmd ["sh", "-c", "if test -d " ++ prefix ++ "/share/icons/locolor/16x16/apps -a -w " ++ prefix ++ "/share/icons/locolor/16x16/apps -a ! -f " ++ prefix ++ "/share/icons/locolor/16x16/apps/gvim.png; then cp ../runtime/vim16x16.png " ++ prefix ++ "/share/icons/locolor/16x16/apps/gvim.png; fi"]
    cmd ["sh", "-c", "if test -d " ++ prefix ++ "/share/applications -a -w " ++ prefix ++ "/share/applications; then if test -f po/vim.desktop -a -f po/gvim.desktop; then cp po/vim.desktop po/gvim.desktop " ++ prefix ++ "/share/applications; else cp ../runtime/vim.desktop ../runtime/gvim.desktop " ++ prefix ++ "/share/applications; fi; if test -z \"\" -a -x \"\"; then -q " ++ prefix ++ "/share/applications; fi fi"]
