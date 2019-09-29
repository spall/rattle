
module Vim(main) where

import Development.Rattle
import Test.Example.VimVariables
import Test.Example.CmdHelpers
import Control.Monad
import Development.Shake.FilePath
import Data.List

main :: IO ()
main = rattleRun rattleOptions $ do
  withCmdOptions [Cwd "src"] $ do
    -- all: $(VIMTARGET) $(TOOLS) languages $(GUI_BUNDLE)
    -- $(VIMTARGET): auto/config.mk objects $(OBJ) version.c version.h
    shcCmd $ unwords [mkdir_p, "objects"]
    cmd ["touch", "objects/.dirstamp"]
    -- build all the object files

    let quotesed = "sed -e 's/[\\\\\"]/\\\\&/g' -e 's/\\\\\"/\"/' -e 's/\\\\\";$$/\";/'"
    --QUOTESED        = sed -e 's/[\\"]/\\&/g' -e 's/\\"/"/' -e 's/\\";$$/";/'

    -- there is an individual target for each object file in the makefile?
    -- need to build auto/gui_gtk_gresources.c and .h
        --cmd [glib_compile_resources, "--target=auto/gui_gtk_gresources.c", "--sourcedir=../pixmaps", "--generate", "--c-name=gui_gtk", "--manual-register", "gui_gtk_res.xml"]
        --shcCmd $ "if test -z \"" ++ glib_compile_resources ++ "\"; then touch auto/gui_gtk_gresources.h; else " ++ glib_compile_resources
          -- ++ "--target=auto/gui_gtk_gresources.h --sourcedir=../pixmaps --generate --c-name=gui_gtk --manual-register gui_gtk_res.xml; fi"
        --cmd [ccc_nf, perl_cflags, all_cflags, "-o", "objects/gui_gtk_gresources.o", "auto/gui_gtk_gresources.c"] -- dont compile this?
    
    --cmd [ccc_nf, lua_cflags, all_cflags, "-o", "objects/if_lua.o", "if_lua.c"]
    --cmd [ccc, "-o", "objects/if_mzsch.o", mzscheme_cflags_extra, "if_mzsch.c"]
    --cmd [ccc_nf, perl_cflags, all_cflags, "-o", "objects/if_perl.o", "auto/if_perl.c"]
    --cmd [ccc_nf, perl_cflags, all_cflags, "-o", "objects/if_perlsfio.o", "if_perlsfio.c"]
        --cmd [ccc_nf, python_cflags, python_cflags_extra, all_cflags, "-o", "objects/if_python.o", "if_python.c"]
        --cmd  [ccc_nf, python3_cflags, python3_cflags_extra, all_cflags, "-o", "objects/if_python3.o", "if_python3.c"]
    --cmd [ccc_nf, ruby_cflags, all_cflags, "-o", "objects/if_ruby.o", "if_ruby.c"]
    --cmd unwords [ccc_nf, tcl_cflags, all_cflags, "-o", "objects/if_tcl.o", "if_tcl.c"]
    -- TODO: get auto/config.h
    -- auto/osdef.h
    let pre_defs = unwords ["-Iproto", defs, gui_defs, gui_ipath, cppflags, extra_ipaths]
    let post_defs = unwords [x_cflags, mzscheme_cflags, extra_defs]
    let osdef_cflags = unwords [pre_defs, post_defs]
    shcCmd $ unwords ["CC=\"" ++ cc, osdef_cflags ++ "\"", "srcdir=" ++ srcdir, "sh", srcdir </> "osdef.sh"]
    shcCmd $ unwords [ccc_nf, lua_cflags, perl_cflags, python_cflags, python3_cflags, ruby_cflags, tcl_cflags, all_cflags, "-o", "objects/option.o", "option.c"] -- fix me?
  
    let vimruntimedir = "" -- todo fix me
    -- create auto/pathdef.c
    -- can use writeFile to write this file. there is an example in Test/.hs
    seqCmds ["echo '/* pathdef.c */' > auto/pathdef.c",
             "echo '/* This file is automatically created by Makefile' >> auto/pathdef.c",
             "echo ' * DO NOT EDIT! Change Makefile only. */' >> auto/pathdef.c",
             "echo '#include \"vim.h\"' >> auto/pathdef.c",
             --"echo 'char_u *default_vim_dir = (char_u *)\"" ++ vimrcloc ++ "\";' | " ++ quotesed ++ " >> auto/pathdef.c",
             "echo 'char_u *default_vim_dir = (char_u *)\"" ++ vimrcloc ++ "\";' >> auto/pathdef.c",
             
             "echo 'char_u *default_vimruntime_dir = (char_u *)\"" ++ vimruntimedir ++ "\";' >> auto/pathdef.c",
             "echo 'char_u *all_cflags = (char_u *)\"" ++ cc ++ " -c -I" ++ srcdir ++ " " ++ all_cflags ++ "\";' >> auto/pathdef.c",
             "echo 'char_u *all_lflags = (char_u *)\"" ++ cc ++ unwords all_lib_dirs ++ ldflags ++ " -o " ++ vimtarget ++ unwords all_libs ++ " \";' >> auto/pathdef.c",
             "echo 'char_u *compiled_user = (char_u *)\"' | tr -d " ++ nl ++ " >> auto/pathdef.c",
             "if test -n \"" ++ compiledby ++ "\"; then echo \"" ++ compiledby ++ "\" | tr -d " ++ nl ++ " >> auto/pathdef.c; else ((logname) 2>/dev/null || whoami) | tr -d " ++ nl ++ " >> auto/pathdef.c; fi",
             "echo '\";' >> auto/pathdef.c",
             "echo 'char_u *compiled_sys = (char_u *)\"' | tr -d " ++ nl ++ " >> auto/pathdef.c",
             "if test -z \"" ++ compiledby ++ "\"; then hostname | tr -d " ++ nl ++ " >> auto/pathdef.c; fi",
             "echo '\";' >> auto/pathdef.c",
             "sh " ++ srcdir ++ "/pathdef.sh"]

    cmd $ unwords [ccc, "-o", "objects/pathdef.o", "auto/pathdef.c"]
    -- build term_deps
    
    forM_ term_obj (\o -> shcCmd $ unwords [cccterm, "-o", o, "libvterm/src" </> (takeBaseName o <.> ".c")])
    forM_ xdiff_objs (\o -> shcCmd $ unwords [cccdiff, "-o", o, "xdiff" </> (takeBaseName o <.> ".c")])
    
    forM_ obj_minus (\o -> if o == "" then return ()
                           else let c = if o == "objects/gui_gtk_gresources.o"
                                        then "auto/gui_gtk_gresources.c"
                                        else takeBaseName o <.> ".c" in
                             cmd $ unwords [ccc, "-o", o, c])
    -- doesn't work for them all
  
    -- TODO check next cmd. not sure its correct
    cmd $ unwords [cclink, unwords all_lib_dirs, ldflags, "-o", vimtarget, unwords obj, unwords all_libs]
    {-
    shcCmd $ unwords ["LINK=\"" ++ cclink, unwords all_lib_dirs, ldflags, "-o" -- purify shrpenv
                     , vimtarget, unwords obj, unwords all_libs ++ "\""
                     , "LINK_AS_NEEDED=" ++ link_as_needed, "sh", srcdir ++ "/link.sh"]
    -}
    -- TOOLS = xxd/xxdEXEEXT
    -- depends on xxd/xxd.c
    withCmdOptions [Cwd "xxd"] $ do
      cmd $ unwords [cc, cppflags, cflags, ldflags, "-DUNIX", "-o", "xxd", "xxd.c"]
    -- languages: build the language specific files if they were unpacked.
    -- generate the converted .mo files separately, it's no problem if this fails.
    let makemo = True
    (if makemo
     then withCmdOptions [Cwd podir] $ do
        let prefix_ = destdir ++ prefix
        -- all
        let msgfmtcmd = unwords ["OLD_PO_FILE_INPUT=yes", msgfmt, "-v"]
        let pomo po = shcCmd $ unwords [msgfmtcmd, "-o", takeBaseName po <.> "mo", po]
        let vim = "../vim"
        let pock po = cmd $ unwords [vim, "-u", "NONE", "-e", "-X", "-S", "check.vim", "-c"
                          ,"\"if error == 0 | q | endiff\"", "-c", "cq", po, "touch"
                          , takeBaseName po <.> "ck"]
        -- make sure certain .po files exist; they're in the POFILES list already
        cmd ["echo", "\\#", ">", "nl.po"]
        cmd ["cp", "no.po", "nb.po"]
        -- ja.euc-jp.po
        shcCmd $ unwords ["iconv", "-f", "utf-8", "-t", "euc-jp", "ja.po", "|", "sed", "-e", "'s/charset=[uU][tT][fF]-8/charset=euc-jp/'", "-e", "'s/# Original translations/# Generated from ja.po, DO NOT EDI/'", ">", "ja.euc-jp.po"]
      -- pl.uTF-8.po
        seqCmds ["rm -f pl.UTF-8.po",
                 "iconv -f iso-8859-2 -t utf-8 pl.po | sed -e 's/charset=ISO-8859-2/charset=UTF-8/' -e 's/# Original translations/# Generated from pl.po, DO NOT EDIT/' > pl.UTF-8.po"]
        --ko.po
        seqCmds ["rm -f ko.po",
                 "iconv -f UTF-8 -t euc-kr ko.UTF-8.po | sed -e 's/charset=UTF-8/charset=euc-kr/' -e 's/# Korean translation for Vim/# Generated from ko.UTF-8.po, DO NOT EDIT/' > ko.po"]
        --ru.cp1251.po
        seqCmds ["rm -f ru.cp1251.po",
                 "iconv -f utf-8 -t cp1251 ru.po | sed -e 's/charset=[uU][tT][fF]-8/charset=cp1251/' -e 's/# Original translations/# Generated from ru.po, DO NOT EDIT/' > ru.cp1251.po"]
        -- uk.cp1251.po
        seqCmds ["rm -f uk.cp1251.po",
                 "iconv -f utf-8 -t cp1251 uk.po | sed -e 's/charset=[uU][tT][fF]-8/charset=cp1251/' -e 's/# Original translations/# Generated from uk.po, DO NOT EDIT/' > uk.cp1251.po"]
        
        forM_ pofiles pomo
     else return ())
      -- converted
    --(if MAKEMO
    --then withCmdOptions [Cwd PODIR] $ do

      
      

      -- msgfmt_desktop
      
      
                        
    
  
