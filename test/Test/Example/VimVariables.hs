
module Test.Example.VimVariables
  (cpp, defs, gui_defs, gui_ipath, cppflags, extra_ipaths,
   x_cflags, mzscheme_cflags, extra_defs, profile_cflags,
   sanitizer_cflags, leak_cflags, abort_cflags, ruby_cflags,
   lua_cflags, perl_cflags, python_cflags, mkdir_p, ccc_nf,
   all_cflags, ccc, mzscheme_cflags_extra, python_cflags_extra,
   python3_cflags_extra, python3_cflags, tcl_cflags, vimrcloc,
   cc, srcdir, all_lib_dirs, vimtarget, ldflags, all_libs, nl,
   compiledby, term_obj, cccterm, xdiff_objs, cccdiff, obj_minus,
   purify, shrpenv, cclink, link_as_needed, cflags, pofiles, podir,
   destdir, prefix, msgfmt, obj
) where

prefix = "error"
destdir = "~/pkg/vim"
exeext = ""
lnkext = ""

ldflags = "-L/usr/local/lib -Wl,--as-needed"
nl = "\"\\012\""
compiledby = ""
purify = ""
shrpenv = ""
link_as_needed = "yes"

cc = "gcc"
ccc_nf = cc ++ " -c -I" ++ srcdir
ccc =  unwords [ccc_nf, all_cflags]
cccterm = unwords [ccc_nf, vterm_cflags, all_cflags, "-DINLINE=\"\"", "-DVSNPRINTF=vim_vsnprintf",
                   "-DIS_COMBINING_FUNCTION=utf_iscomposing_uint", "-DWCWIDTH_FUNCTION=utf_uint2cells"]
cccdiff = unwords [ccc_nf, all_cflags]
cflags = "-O2 -fno-strength-reduce -Wall -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=1"
srcdir = "."
vimname = "vim"
cclink = cc
-- names of programs and targets
vimtarget = vimname ++ exeext

-- vim version
vimmajor = "8"
vimminor = "1"

-- location of vim files
vimdir = "/vim"
podir = "po"
datadir = prefix ++ "/lib"

vimloc = datadir ++ vimdir
vimrcloc = vimloc

-- files
xdiff_objs = ["objects/xdiffi.o", "objects/xemit.o", "objects/xprepare.o"
             ,"objects/xutils.o", "objects/xhistogram.o", "objects/xpatience.o"]

-- I got this from config.mk after running configure on my machine

-- need to change "-I /<path>" to "-isystem /<path>" for GCC 3.x.
cpp = "gcc -E" -- got from config.mk 

shell = "/bin/sh"
mkdir_p = unwords [shell, "install-sh", "-c", "-d"]

defs = "-DHAVE_CONFIG_H" -- config.mk
vterm_cflags = "-Ilibvterm/include"
gui_defs = ""
gui_ipath = ""
cppflags = ""
extra_ipaths = ""
x_cflags = ""
mzscheme_cflags = ""
mzscheme_cflags_extra = "" 
extra_defs = ""
profile_cflags = ""
sanitizer_cflags = ""
leak_cflags = ""
abort_cflags = ""
pre_defs = unwords ["-Iproto", defs, gui_defs, gui_ipath, cppflags, extra_ipaths]
post_defs = unwords [x_cflags, mzscheme_cflags, extra_defs]
all_cflags = unwords [pre_defs, cflags, profile_cflags, sanitizer_cflags, leak_cflags
                     ,abort_cflags, post_defs]

ruby_cflags = ""
lua_cflags = ""
perl_cflags = ""
python_cflags = ""
python_cflags_extra = ""
python3_cflags = ""
python3_cflags_extra = ""
tcl_cflags = ""
lint_cflags = unwords ["-DLINT", "-I.", pre_defs, post_defs, ruby_cflags, lua_cflags
              ,perl_cflags, python_cflags, python3_cflags, tcl_cflags, vterm_cflags
              ,"-Dinline=", "-D__extension__=", "-Dalloca=alloca"]
depend_cflags = unwords ["-DPROTO", "-DDEPEND", "-DFEAT_GUI", lint_cflags]

-- # Note: MZSCHEME_LIBS must come before LIBS, because LIBS adds -lm which is
-- # needed by racket.
gui_libs_dir = ""
gui_libs1 = ""
gui_libs2 = ""
gui_x_libs = ""
x_libs_dir = ""
-- form config.mk ^
all_lib_dirs = [gui_libs_dir, x_libs_dir]
x_pre_libs = "-lSM -lICE" -- from config.mk
x_libs = "-lXt -lX11" -- ditto
x_extra_libs = "-lXdmcp -lSM -lICE"
mzscheme_libs = ""
libs = "-lm -ltinfo -lnsl -ldl"
extra_libs = ""
lua_libs = ""
perl_libs = ""
python_libs = ""
python3_libs = ""
tcl_libs = ""
ruby_libs = ""
profile_libs = ""
sanitizer_libs = sanitizer_cflags
leak_libs = ""
all_libs = [gui_libs1, gui_x_libs, gui_libs2, x_pre_libs, x_libs, x_extra_libs
           ,mzscheme_libs, libs, extra_libs, lua_libs, perl_libs, python_libs
           ,python3_libs, tcl_libs, ruby_libs, profile_libs, sanitizer_libs
           ,leak_libs]

hangulin_obj = ""
gui_obj = ""
term_obj = ["objects/encoding.o", "objects/keyboard.o", "objects/mouse.o", "objects/parser.o", "objects/pen.o", "objects/termscreen.o", "objects/state.o", "objects/unicode.o", "objects/vterm.o"]
lua_obj = ""
mzscheme_obj = ""
perl_obj = ""
python_obj = ""
python3_obj = ""
tcl_obj = ""
ruby_obj = ""
os_extra_obj = ""
netbeans_obj = "objects/netbeans.o"
channel_obj = "objects/channel.o"
obj_common_minus = ["objects/arabic.o", "objects/arglist.o", "objects/autocmd.o", "objects/beval.o"
            ,"objects/buffer.o", "objects/change.o", "objects/blob.o", "objects/blowfish.o"
            ,"objects/cmdexpand.o", "objects/cmdhist.o", "objects/crypt.o", "objects/crypt_zip.o"
            ,"objects/debugger.o", "objects/dict.o", "objects/diff.o", "objects/digraph.o"
            ,"objects/edit.o", "objects/eval.o", "objects/evalfunc.o", "objects/evalvars.o", "objects/ex_cmds.o"
            ,"objects/ex_cmds2.o", "objects/ex_docmd.o", "objects/ex_eval.o", "objects/ex_getln.o"
            ,"objects/fileio.o", "objects/findfile.o", "objects/fold.o", "objects/getchar.o"
            ,"objects/hardcopy.o", "objects/hashtab.o", "objects/highlight.o", hangulin_obj
            ,"objects/if_cscope.o", "objects/if_xcmdsrv.o", "objects/indent.o"
            ,"objects/insexpand.o", "objects/list.o", "objects/map.o", "objects/mark.o"
            ,"objects/memline.o", "objects/menu.o", "objects/misc1.o", "objects/misc2.o"
            ,"objects/move.o", "objects/mbyte.o", "objects/normal.o", "objects/ops.o"
            , "objects/os_unix.o", "objects/popupmnu.o", "objects/popupwin.o"
            ,"objects/profiler.o", "objects/pty.o", "objects/quickfix.o", "objects/regexp.o"
            ,"objects/scriptfile.o", "objects/screen.o", "objects/search.o", "objects/session.o"
            ,"objects/sha256.o", "objects/sign.o", "objects/sound.o", "objects/spell.o"
            ,"objects/spellfile.o", "objects/syntax.o", "objects/tag.o", "objects/term.o"
            ,"objects/terminal.o", "objects/testing.o", "objects/textprop.o", "objects/ui.o"
            ,"objects/undo.o", "objects/usercmd.o", "objects/userfunc.o", "objects/version.o"
            ,"objects/viminfo.o", "objects/window.o", gui_obj, lua_obj, mzscheme_obj, perl_obj
            ,python_obj, python3_obj, tcl_obj, ruby_obj, os_extra_obj, netbeans_obj, channel_obj]

obj_main = ["objects/charset.o", "objects/json.o", "objects/main.o", "objects/memfile.o"
           ,"objects/message.o"]
obj = obj_common ++ obj_main
obj_minus = obj_common_minus ++ obj_main
obj_common = obj_common_minus ++ ["objects/pathdef.o", "objects/option.o"] ++ term_obj ++ xdiff_objs

  -- from po/Make_all.mak
pofiles = ["af.po", "ca.po", "cs.po", "cs.cp1250.po", "da.po", "de.po", "en_GB.po", "eo.po","es.po"
          ,"fi.po", "fr.po", "ga.po", "it.po", "ja.po", "ja.euc-jp.po", "ja.sjis.po", "ko.po"
          ,"ko.UTF-8.po", "lv.po", "nb.po", "nl.po", "no.po", "pl.po", "pl.UTF-8.po"
          ,"pl.cp1250.po", "pt_BR.po", "ru.po", "ru.cp1251.po", "sk.po", "sk.cp1250.po", "sr.po"
          ,"sv.po", "uk.po", "uk.cp1251.po", "vi.po", "zh_CN.po", "zh_CN.UTF-8.po"
          ,"zh_CN.cp936.po", "zh_TW.po", "zh_TW.UTF-8.po"]

msgfmt = "msgfmt" 
