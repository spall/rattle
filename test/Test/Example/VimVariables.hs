
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
   destdir, prefix, msgfmt, obj,

   makecmdgoals, gui_inc_loc, gui_lib_loc, conf_shell, conf_opt_gui,
   conf_opt_x, conf_opt_xsmp, conf_opt_autoserve, conf_opt_darwin,
   conf_opt_fail, conf_opt_perl, conf_opt_python, conf_opt_python3,
   conf_opt_tcl, conf_opt_ruby, conf_opt_nls, conf_opt_cscope,
   conf_opt_multibyte, conf_opt_input, conf_opt_output, conf_opt_gpm,
   conf_opt_feat, conf_term_lib, conf_opt_compby, conf_opt_acl,
   conf_opt_netbeans, conf_opt_channel, conf_opt_terminal, conf_args,
   conf_args1, conf_args2, conf_args3, conf_args4, conf_args5, conf_args6,
   conf_opt_mzscheme, conf_opt_plthome, conf_opt_lua, conf_opt_lua_prefix,
   conf_opt_sysmouse, conf_opt_canberra) where


-- configure variables
makecmdgoals = ""
gui_inc_loc = ""
gui_lib_loc = ""
conf_shell = ""
conf_opt_gui = ""
conf_opt_x = ""
conf_opt_xsmp = ""
conf_opt_autoserve = ""
conf_opt_darwin = ""
conf_opt_fail = ""
conf_opt_perl = ""
conf_opt_python = ""
conf_opt_python3 = ""
conf_opt_tcl = ""
conf_opt_ruby = ""
conf_opt_nls = ""
conf_opt_cscope = ""
conf_opt_multibyte = ""
conf_opt_input = ""
conf_opt_output = ""
conf_opt_gpm = ""
conf_opt_feat = ""
conf_term_lib = ""
conf_opt_compby = ""
conf_opt_acl = ""
conf_opt_netbeans = ""
conf_opt_channel = ""
conf_opt_terminal = ""
conf_args = ""
conf_args1 = ""
conf_args2 = ""
conf_args3 = ""
conf_args4 = ""
conf_args5 = ""
conf_args6 = ""
conf_opt_mzscheme = ""
conf_opt_plthome = ""
conf_opt_lua = ""
conf_opt_lua_prefix = ""
conf_opt_sysmouse = ""
conf_opt_canberra = ""
-- end

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
exname = "ex"
viewname = "view"
cclink = cc
lint = "lint"
lint_options = "-beprxzF"
-- names of programs and targets
vimtarget = vimname ++ exeext
extarget = exname ++ lnkext
viewtarget = viewname ++ lnkext
gvimname = "g" ++ vimname
gvimtarget = gviewname ++ lnkext
gviewname = "g" ++ viewname
gviewtarget = gviewname ++ lnkext
rvimname = "r" ++ vimname
rvimtarget = rvimname ++ lnkext
rviewname = "r" ++ viewname
rviewtarget = rviewname ++ lnkext
rgvimname = "r" ++ gvimname
rgvimtarget = rgvimname ++ lnkext
rgviewname = "r" ++ gviewname
rgviewtarget = rgviewname ++ lnkext
vimdiffname = vimname ++ "diff"
gvimdiffname = "g" ++ vimdiffname
vimdifftarget = vimdiffname ++ lnkext
gvimdifftarget = gvimdiffname ++ lnkext
evimname = "e" ++ vimname
evimtarget = evimname ++ lnkext
eviewname = "e" ++ viewname
eviewtarget = eviewname ++ lnkext

-- names of the tools that are also made
tools = "xxd/xxd" ++ exeext

-- vim version
vimmajor = "8"
vimminor = "1"

man1dir = "/man1"
-- location of vim files
vimdir = "/vim"
vimrtdir = "/vim" ++ vimmajor ++ vimminor
helpsubdir = "/doc"
colsubdir = "/colors"
synsubdir = "/syntax"
indsubdir = "/indent"
autosubdir = "/autoload"
plugsubdir = "/plugin"
ftplugsubdir = "/ftplugin"
langsubdir = "/lang"
compsubdir = "/compiler"
kmapsubdir = "/keymap"
macrosubdir = "/macros"
packsubdir = "/pack"
toolssubdir = "/tools"
tutorsubdir = "/tutor"
spellsubdir = "/spell"
printsubdir = "/print"
podir = "po"
datadir = prefix ++ "/lib"

vimloc = datadir ++ vimdir
vimrtloc = datadir ++ vimdir ++ vimrtdir
vimrcloc = vimloc
helpsubloc = vimrtloc ++ helpsubdir
colsubloc = vimrtloc ++ colsubdir
synsubloc = vimrtloc ++ synsubdir
indsubloc = vimrtloc ++ indsubdir
autosubloc = vimrtloc ++ autosubdir
plugsubloc = vimrtloc ++ plugsubdir
ftplugsubloc = vimrtloc ++ ftplugsubdir
langsubloc = vimrtloc ++ langsubdir
compsubloc = vimrtloc ++ compsubdir
kmapsubloc = vimrtloc ++ kmapsubdir
macrosubloc = vimrtloc ++ macrosubdir
packsubloc = vimrtloc ++ packsubdir
toolssubloc = vimrtloc ++ toolssubdir
tutorsubloc = vimrtloc ++ tutorsubdir
spellsubloc = vimrtloc ++ spellsubdir
printsubloc = vimrtloc ++ printsubdir
scriptloc = vimrtloc

-- ### Name of the defaults/evim/mswin file target.
vim_defaults_file = destdir ++ scriptloc ++ "/defaults.vim"
evim_file = destdir ++ scriptloc ++ "/evim.vim"
mswin_file = destdir ++ scriptloc ++ "/mswin.vim"
  
-- ### Name of the menu file target.
sys_menu_file = destdir ++ scriptloc ++ "/menu.vim"
sys_synmenu_file = destdir ++ scriptloc ++ "/synmenu.vim"
sys_delmenu_file = destdir ++ scriptloc ++ "/delmenu.vim"
                   
-- ### Name of the bugreport file target.
sys_bugr_file = destdir ++ scriptloc ++ "/bugreport.vim"

-- ### Name of the rgb.txt file target.
sys_rgb_file = destdir ++ scriptloc ++ "/rgb.txt"
               
-- ### Name of the file type detection file target.
sys_filetype_file = destdir ++ scriptloc ++ "/filetype.vim"

-- ### Name of the file type detection file target.
sys_ftoff_file = destdir ++ scriptloc ++ "/ftoff.vim"
                 
-- ### Name of the file type detection script file target.
sys_scripts_file = destdir ++ scriptloc ++ "/scripts.vim"

-- ### Name of the ftplugin-on file target.
sys_ftplugin_file = destdir ++ scriptloc ++ "/ftplugin.vim"

-- ### Name of the ftplugin-off file target.
sys_ftplugof_file = destdir ++ scriptloc ++ "/ftplugof.vim"

-- ### Name of the indent-on file target.
sys_indent_file = destdir ++ scriptloc ++ "/indent.vim"

-- ### Name of the indent-off file target.
sys_indoff_file = destdir ++ scriptloc ++ "/indoff.vim"

-- ### Name of the option window script file target.
sys_optwin_file = destdir ++ scriptloc ++ "/optwin.vim"

-- files
term_deps = ["libvterm/include/vterm.h", "libvterm/include/vterm_keycodes.h", "libvterm/src/rect.h"
            ,"libvterm/src/utf8.h", "libvterm/src/vterm_internal.h"]
term_src = ["libvterm/src/*.c"] -- todo fix  me
xdiff_src = ["xdiff/xdiffi.c", "xdiff/xemit.c", "xdiff/xprepare.c", "xdiff/xutils.c"
            ,"xdiff/xhistogram.c", "xdiff/xpatience.c"]
xdiff_objs = ["objects/xdiffi.o", "objects/xemit.o", "objects/xprepare.o"
             ,"objects/xutils.o", "objects/xhistogram.o", "objects/xpatience.o"]
xdiff_incl = ["xdiff/xdiff.h", "xdiff/xdiffi.h", "xdiff/xemit.h", "xdiff/xinclude.h"
             ,"xdiff/xmacros.h", "xdiff/xprepare.h", "xdiff/xtypes.h", "xdiff/xutils.h"]

cpp_mm = "M"

-- I got this from config.mk after running configure on my machine
-- need to figure out a way to run configure.
depend_cflags_filter = "| sed 's+-I */+-isystem /+g'"

-- need to change "-I /<path>" to "-isystem /<path>" for GCC 3.x.
cpp_depend = unwords [cc, "-I" ++ srcdir, "-M" ++ cpp_mm, "`echo", "\"" ++ depend_cflags ++ "\""
             ,depend_cflags_filter ++ "`"]

no_attr = ""
cpp = "gcc -E" -- got from config.mk 
proto_flags = unwords ["-d", "-E\"" ++ cpp ++ "\"", no_attr]

shell = "/bin/sh"
mkdir_p = unwords [shell, "install-sh", "-c", "-d"]

suffixes = [".c", ".o", ".pro"]

defs = "-DHAVE_CONFIG_H" -- config.mk
vterm_cflags = "-Ilibvterm/include"
none_defs = "" -- doesn't appear to be defined anywhere
none_ipath = "" -- this seems to be their way of defining nothing
gui_defs = none_defs
gui_ipath = none_ipath
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

osdef_cflags = unwords [pre_defs, post_defs]

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
lint_extra = unwords ["-DHANGUL_INPUT", "-D\"__attribute__(x)=\""]
depend_cflags = unwords ["-DPROTO", "-DDEPEND", "-DFEAT_GUI", lint_cflags]

-- # Note: MZSCHEME_LIBS must come before LIBS, because LIBS adds -lm which is
-- # needed by racket.
none_libs_dir = ""
none_libs1 = ""
none_libs2 = ""
gui_libs_dir = none_libs_dir
gui_libs1 = none_libs1
gui_libs2 = none_libs2
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

-- include Make_all.make

-- get the list of tests; maybe make this isnt own file
-- include testdir/Make_all.mak

-- BASIC_SRC: files that are always used

basic_src = ["arabic.c", "arglist.c", "autocmd.c", "beval.c", "blob.c", "blowfish.c"
            ,"buffer.c", "change.c", "charset.c", "cmdexpand.c", "cmdhist.c", "crypt.c"
            ,"crypt_zip.c", "debugger.c", "dict.c", "diff.c", "digraph.c", "edit.c"
            ," eval.c", "evalfunc.c", "evalvars.c", "ex_cmds.c", "ex_cmds2.c", "ex_docmd.c", "ex_eval.c"
            ,"ex_getln.c", "fileio.c", "findfile.c", "fold.c", "getchar.c", "hardcopy.c"
            ,"hashtab.c", "highlight.c", "if_cscope.c", "if_xcmdsrv.c", "indent.c"
            ,"insexpand.c", "json.c", "list.c", "main.c", "map.c", "mark.c", "memfile.c"
            ,"memline.c", "menu.c", "message.c", "misc1.c", "misc2.c", "move.c", "mbyte.c"
            ,"normal.c", "ops.c", "option.c", "os_unix.c", "auto/pathdef.c", "popupmnu.c"
            ,"popupwin.c", "profiler.c", "pty.c", "quickfix.c", "regexp.c", "scriptfile.c"
            ,"screen.c", "search.c", "session.c", "sha256.c", "sign.c", "sound.c", "spell.c"
            ,"spellfile.c", "syntax.c", "tag.c", "term.c", "terminal.c", "testing.c"
            ,"textprop.c", "ui.c", "undo.c", "usercmd.c", "userfunc.c", "version.c"
            ,"viminfo.c", "window.c"] ++ os_extra_src
os_extra_src = []

none_src = []
gui_src = none_src
hangulin_src = []
lua_src = []
mzscheme_src = []
perl_src = []
python_src = []
python3_src = []
tcl_src = []
ruby_src = []
src = basic_src ++ gui_src ++ term_src ++ xdiff_src ++ hangulin_src ++ lua_src ++
  mzscheme_src ++ perl_src ++ python_src ++ python3_src ++ tcl_src ++ ruby_src

gresource_src = []
netbeans_src = "netbeans.c"
channel_src = "channel.c"
extra_src = ["hangulin.c", "if_lua.c", "if_mzsch.c", "auto/if_perl.c", "if_perlsfio.c"
            ,"if_python.c", "if_python3.c", "if_tcl.c", "if_ruby.c", "gui_beval.c"
            ,"netbeans.c", "channel.c"] ++ gresource_src
all_src = basic_src ++ gui_src ++ hangulin_src ++ python_src ++ python3_src ++ tcl_src ++
          [netbeans_src] ++ [channel_src] ++ term_src

none_obj = ""
hangulin_obj = ""
gui_obj = none_obj
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

obj_special = ["objects/pathdef.o", "objects/option.o"] ++ term_obj ++ xdiff_objs

obj_main = ["objects/charset.o", "objects/json.o", "objects/main.o", "objects/memfile.o"
           ,"objects/message.o"]
obj = obj_common ++ obj_main
obj_minus = obj_common_minus ++ obj_main
obj_common = obj_common_minus ++ obj_special

  -- from po/Make_all.mak
languages = ["af", "ca", "cs", "cs.cp1250", "da", "de", "en_GB", "eo", "es", "fi", "fr", "ga", "it"
            ,"ja", "ja.euc-jp", "ja.sjis", "ko", "ko.UTF-8", "lv", "nb", "nl", "no", "pl"
            ,"pl.UTF-8", "pl.cp1250", "pt_BR", "ru", "ru.cp1251", "sk", "sk.cp1250", "sr", "sv"
            ,"uk", "uk.cp1251", "vi", "zh_CN", "zh_CN.UTF-8", "zh_CN.cp936", "zh_TW", "zh_TW.UTF-8"]
pofiles = ["af.po", "ca.po", "cs.po", "cs.cp1250.po", "da.po", "de.po", "en_GB.po", "eo.po","es.po"
          ,"fi.po", "fr.po", "ga.po", "it.po", "ja.po", "ja.euc-jp.po", "ja.sjis.po", "ko.po"
          ,"ko.UTF-8.po", "lv.po", "nb.po", "nl.po", "no.po", "pl.po", "pl.UTF-8.po"
          ,"pl.cp1250.po", "pt_BR.po", "ru.po", "ru.cp1251.po", "sk.po", "sk.cp1250.po", "sr.po"
          ,"sv.po", "uk.po", "uk.cp1251.po", "vi.po", "zh_CN.po", "zh_CN.UTF-8.po"
          ,"zh_CN.cp936.po", "zh_TW.po", "zh_TW.UTF-8.po"]
mofiles = ["af.mo", "ca.mo", "cs.mo", "da.mo", "de.mo", "en_GB.mo", "eo.mo", "es.mo", "fi.mo"
          ,"fr.mo" ,"ga.mo", "it.mo", "ja.mo", "ko.UTF-8.mo", "ko.mo", "lv.mo", "nb.mo", "nl.mo"
          ,"no.mo", "pl.mo", "pt_BR.mo", "ru.mo", "sk.mo", "sr.mo", "sv.mo", "uk.mo", "vi.mo"
          ,"zh_CN.UTF-8.mo", "zh_CN.mo", "zh_TW.UTF-8.mo", "zh_TW.mo"]
moconverted = ["cs.cp1250.mo", "ja.euc-jp.mo", "ja.sjis.mo", "pl.UTF-8.mo", "pl.cp1250.mo"
              ,"ru.cp1251.mo", "sk.cp1250.mo", "uk.cp1251.mo", "zh_CN.cp936.mo"]
checkfiles = ["af.ck", "ca.ck", "cs.ck", "cs.cp1250.ck", "da.ck", "de.ck", "en_GB.ck"
             ,"eo.ck", "es.ck", "fi.ck", "fr.ck", "ga.ck", "it.ck", "ja.ck", "ja.euc-jp.ck"
             ,"ja.sjis.ck", "ko.UTF-8.ck", "ko.ck", "lv.ck", "nb.ck", "nl.ck", "no.ck"
             ,"pl.UTF-8.ck", "pl.ck", "pl.cp1250.ck", "pt_BR.ck", "ru.ck", "ru.cp1251.ck"
             ,"sk.ck", "sk.cp1250.ck", "sr.ck", "sv.ck", "uk.ck", "uk.cp1251.ck", "vi.ck"
             ,"zh_CN.UTF-8.ck", "zh_CN.ck", "zh_CN.cp936.ck", "zh_TW.UTF-8.ck", "zh_TW.ck"]

msgfmt = "msgfmt"
msgfmt_desktop = "gvim.desktop vim.desktop"
  
