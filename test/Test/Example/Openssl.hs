
module Test.Example.Openssl(main) where

import Development.Rattle
import Test.Example.OpensslVariables
import Test.Example.LibCryptoObjs
import Test.Example.LibCryptoso3Objs
import Test.Type
import Test.Example.CmdHelpers
import Development.Shake.FilePath
import Development.Shake.Command
import Control.Monad

main :: IO ()
main = testGitConfig "https://github.com/openssl/openssl" (cmd_ "./config") $ do
  let generated_mandatory = ["crypto/include/internal/bn_conf.h" --
                            ,"crypto/include/internal/dso_conf.h"
                            ,"doc/man7/openssl_user_macros.pod", "include/openssl/opensslconf.h"
                            ,"test/provider_internal_test.conf"]
  -- build_generated: generated_mandatory
  --configdata.pm
  cmd $ unwords [perl, "configdata.pm", "-r"]
  -- crypto/include/internal/bn_conf.h.in  configdata.pm
  forM_ generated_mandatory (\f -> cmd $ unwords [perl, "\"I" ++ blddir ++ "\"", "-Mconfigdata"
                                                 ,"\"util/dofile.pl\"", "\"-oMakefile\""
                                                 , f <.> ".in", ">", f])
  --make depend
  cmd $ unwords [perl, srcdir </> "util/add-depends.pl", "gcc"]
  -- make _all
  -- all_: build_libs_nodep build_modules_nodep build_programs_nodep link-utils
  -- build_libs_nodep: libcrypto.pc libssl.pc openssl.pc
  -- they all depend on configdata.pm libs libcrypto.so libssl.so

  let libs = ["apps/libapps.a", "libcrypto.a", "libssl.a", "test/libtestutil.a"]
  -- apps/libapps.a
  let build_libapp (la, c) = do
        let dtmp = la -<.> "d.tmp"
        cmd $ unwords [cc, "-I.", "-Iinclude", "-Iapps/include", lib_cflags
                      ,lib_cppflags, "-MMD", "-MF", dtmp, "-MT"
                      ,la, "-c", "-o", la, c]
        let d = la -<.> "d"
        seqCmds ["touch " ++ dtmp
                ,"if cmp " ++ dtmp ++ " " ++ d ++ " > /dev/null 2> /dev/null; " ++
                 "then rm -f " ++ dtmp ++
                 "; else mv " ++ dtmp ++ " " ++ d ++ "; fi"]

  let libapps = [("apps/lib/lib/libapps-lib-app_params.o", "apps/lib/app_params.c")
                ,("apps/libapps-lib-app_rand.o", "apps/app_rand.c")
                ,("apps/libapps-lib-apps.o", "apps/apps.c")
                ,("apps/libapps-lib-apps_ui.o", "apps/apps_ui.c")
                ,("apps/libapps-lib-bf_prefix.o", "apps/bf_prefix.c")
                ,("apps/libapps-lib-columns.o", "apps/columns.c")
                ,("apps/libapps-lib-fmt.o", "apps/fmt.c")
                ,("apps/libapps-lib-opt.o", "apps/opt.c")
                ,("apps/libapps-lib-s_cb.o", "apps/s_cb.c")
                ,("apps/libapps-lib-s_socket.o", "apps/s_socket.c")]

  forM_ libapps build_libapp
  -- done with prereqs of apps/libapps.a
  cmd $ unwords [ar, arflags, "apps/libapps.a", unwords $ map fst libapps] -- makefile says only libapps that have changed, but not sure how to specify that here; or if its worth it
  cmd $ unwords [ranlib, "apps/libapps.a", "|| echo Never mind."]

  -- libcrypto.a": depends on 1 million object files
  cmd $ unwords [ar, arflags, "libcrypto.a", unwords libcrypto_objs] -- ditto about changed ons
  cmd $ unwords [ranlib, "libcrypto.a", "|| echo Never mind."]

  -- libssl.a"
  cmd $ unwords [ar, arflags, "libssl.a", unwords libssl_objs] -- ditto
  cmd $ unwords [ranlib, "libssl.a", "|| echo Never mind."]

  -- test/libtestutil.a
  cmd $ unwords [ar, arflags, "test/libtestutil.a", unwords libtestutil_objs] -- ditto
  cmd $ unwords [ranlib, "test/libtestutil.a", "|| echo Never mind."]

  --libcrypto.so.3: depends on 1 million object files
  cmd $ unwords [cc, lib_cflags, "-L.", lib_ldflags, "-Wl,-soname=libcrypto.so.3", "-o"
                ,"libcrypto.so.3", "-Wl,--version-script-libcrypto.ld", unwords libcryptoso3_objs
                ,lib_ex_libs]
  
  -- libcrypto.so: libcrypto.so.3
  shcCmd $ unwords ["rm", "-f", "libcrypto.so", "&&", "ln", "-s", "libcrypto.so.3", "libcrypto.so"]

  -- libssl.so.3: depends on a more reasonable list of object files
  cmd $ unwords [cc, lib_cflags, "-L.", lib_ldflags, "-Wl,-soname=libssl.so.3", "-o"
                ,"libssl.so.3", "-Wl,--version-script-libssl.ld", unwords libsslso_objs
                ,"-lcrypto", lib_ex_libs]

  --libssl.so: libssl.so.3
  shcCmd $ unwords ["rm", "-f", "libssl.so", "&&", "ln", "-s", "libssl.so.3", "libssl.so"]
  
  -- libcrypto.pc
  shcCmd $ unwords ["( echo 'prefix=" ++ installtop ++ "';"
                   ,"echo 'exec_prefix=${prefix}';"
                   ,"if [ -n \"" ++ libdir ++ "\" ];"
                   ,"then echo 'libdir=${exec_prefix}/" ++ libdir ++ "';"
                   ,"else echo 'libdir=" ++ libdir ++ "'; fi;"
                   ,"echo 'includedir=${prefix}/include';"
                   ,"echo 'enginesdir=${libdir}/engines-3';"
                   ,"echo '';"
                   ,"echo 'Name: OpenSSL-libcrypto';"
                   ,"echo 'Description: OpenSSL cryptography library';"
                   ,"echo 'Version: '" ++ version ++ ";"
                   ,"echo 'Libs: -L${libdir} -lcrypto';"
                   ,"echo 'Libs.private: " ++ lib_ex_libs ++ "';"
                   ,"echo 'Cflags: -I${includedir}' ) > libcrypto.pc"]

  -- libssl.pc
  shcCmd $ unwords ["( echo 'prefix=" ++ installtop ++ "';"
                   ,"echo 'exec_prefix=${prefix}';"
                   ,"if [ -n \"" ++ libdir ++ "\" ];"
                   ,"then echo 'libdir=${exec_prefix}/" ++ libdir ++ "';"
                   ,"else echo 'libdir=" ++ libdir ++ "'; fi;"
                   ,"echo 'includedir=${prefix}/include';"
                   ,"echo '';"
                   ,"echo 'Name: OpenSSL-libssl';"
                   ,"echo 'Description: Secure Sockets Layer and cryptography libraries';"
                   ,"echo 'Version: '" ++ version ++ ";"
                   ,"echo 'Requires.private: libcrypto';"
                   ,"echo 'Libs: -L${libdir} -lssl';"
                   ,"echo 'Cflags: -I${includedir}' ) > libssl.pc"]

  -- openssl.pc
  shcCmd $ unwords ["( echo 'prefix=" ++ installtop ++ "';"
                   ,"echo 'exec_prefix=${prefix}';"
                   ,"if [ -n \"" ++ libdir ++ "\" ];"
                   ,"then echo 'libdir=${exec_prefix}/" ++ libdir ++ "';"
                   ,"else echo 'libdir=" ++ libdir ++ "'; fi;"
                   ,"echo 'includedir=${prefix}/include';"
                   ,"echo '';"
                   ,"echo 'Name: OpenSSL';"
                   ,"echo 'Description: Secure Sockets Layer and cryptography libraries and tools';"
                   ,"echo 'Version: '" ++ version ++ ";"
                   ,"echo 'Requires: libssl libcrypto' ) > openssl.pc"]
  
  -- build_modules_nodep: modules
  let modules = ["engines/afalg.so", "engines/capi.so", "engines/dasync.so", "engines/ossltest.so"
                ,"engines/padlock.so", "providers/fips.so", "providers/legacy.so", "test/p_test.so"]
  let build_engines f c = do
        let dtmp = f -<.> "d.tmp"
        let d = f -<.> "d"
        cmd $ unwords [cc, "-Iinclude", dso_cflags, dso_cppflags, "-MMD", "-MF"
                      ,dtmp, "-MT", f, "-c", "-o", f, c]
        seqCmds ["touch " ++ dtmp
                ,"if cmp " ++ dtmp ++ d ++ " >/dev/null 2> /dev/null; " ++
                "then rm -f " ++ dtmp ++ "; " ++
                "else mv " ++ dtmp ++ " " ++ d ++ "; fi"]
  let g f = let (Just x) = stripExtension "ld" f in
              cmd $ unwords [perl, srcdir </> "util/mkdef.pl", "--ordinals", "util/engines.num"
                            ,"--name", x, "--OS", "linux", ">", f]

  let h f o = let ldf = f -<.> "ld" in
                cmd $ unwords [cc, dso_cflags, "-L.", dso_ldflags, "-o", f, "-Wl"
                              ,"--version-script=" ++ ldf, o, "-lcrypto", dso_ex_libs]

  -- engines/afalg.so: engines/afalg-dso-e_afalg.o engines/afalg.ld (libcrypto.so done)
  -- engines/afalg-dso-e_afalg.o
  build_engines "engines/afalg-dso-e_afalg.o" "engines/e_afalg.c"
  -- engines/afalg.ld: util/engines.num srcdir/util/mkdef.pl
  g "engines/afalg.ld"
  h "engines/afalg.so" "engines/afalg-dso-e_afalg.o"

  -- engines/capi.so: engines/capi-dso-e_capi.o engines/capi.ld libcrypto.so
  -- engines/capi-dso-e_capi.o
  build_engines "engines/capi-dso-e_capi.o" "engines/e_capi.c"
  -- engines/capi.ld
  g "engines/capi.ld"
  h "engines/capi.so" "engines/capi-dso-e_capi.o"
  
  -- engines/dasync.so
  build_engines "engines/dasync-dso-e_dasync.o" "engines/e_dasync.c"
  g "engines/dasync.ld"
  h "engines/dasync.so" "engines/dasync-dso-e_dasync.o"

  -- engines/ossltest.so
  build_engines "engines/ossltest-dso-e_ossltest.o" "engines/e_ossltest.c"
  g "engines/ossltest.ld"
  h "engines/ossltest.so" "engines/ossltest-dso-e_ossltest.o"

  -- engines/padlock.so
  shcCmd $ unwords ["CC=\"" ++ cc ++ "\"", perl, "engines/asm/e_padlock-x86_64.pl", perlasm_scheme
                   ,"engines/e_padlock-x86_64.s"]
  cmd $ unwords [cc, dso_cflags, dso_cppflags, "-c", "-o", "engines/padlock-dso-e_padlock-x86_64.o"
                ,"engines/e_padlock-x86_64.s"]

  cmd $ unwords [cc, "-Iinclude", "-DPADLOCK)_ASM", dso_cflags, dso_cppflags, "-MMD", "-MF"
                ,"engines/padlock-dso-e_padlock.d.tmp", "-MT", "engines/padlock-dso-e_padlock.o"
                ,"-c", "-o", "engines/padlock-dso-e_padlock.o", "engines/e_padlock.c"]
  seqCmds ["touch engines/padlock-dso-e_padlock.d.tmp"
          ,"if cmp engines/padlock-dso-e_padlock.d.tmp engines/padlock-dso-e_padlock.d > " ++
          "/dev/null 2> /dev/null; then rm -f engines/padlock-dso-e_padlock.d.tmp; " ++
          "else mv engines/padlock-dso-e_padlock.d.tp engines/padlock-dso-e_padlock.d; fi"]
  g "engines/padlock.ls"
  h "engines/padlock.so" "engines/padlock-dso-e_padlock-x86_64.o engines/padlock-dso-e_padlock.o"
 
  -- providers/fips.so: depends on 1 million object files
  cmd $ unwords [perl, srcdir </> "util/mkdef.pl", "--ordinals", "util/providers.num", "--name"
                ,"providers/fips", "-OS", "linux", ">", "providers/fips.ld"]
  cmd $ unwords [cc, dso_cflags, "-L.", dso_ldflags, "-o", "providers/fips.so", "-Wl"
                ,"--version-script=providers/fips.ld", unwords fips_objs, dso_ex_libs]

  -- providers/legacy.so: depends on some object files
  cmd $ unwords [perl, srcdir </> "util/mkdef.pl", "--ordinals", "util/providers.num"
                ,"--name", "providers/legacy", "--OS", "linux", ">", "providers/legacy.ld"]
  cmd $ unwords [cc, dso_cflags, "-L.", dso_ldflags, "-o", "providers/legacy.so", "-Wl"
                ,"--version-script=providers/legacy.ld", unwords legacy_objs, "-lcrypto"
                , dso_ex_libs]

  -- test/p_test.so
  build_engines "test/p_test-dso-p_test.o" "test/p_test.c"
  cmd $ unwords [perl, srcdir </> "util/mkdef.pl", "--ordinals", "util/providers.num",
                 "--name", "test/p_test", "--OS", "linux", ">", "test/p_test.ld"]
  cmd $ unwords [cc, dso_cflags, "-L.", dso_ldflags, "-o", "test/p_test.so", "-Wl"
                ,"--version-script=test/p_test.ld", "test/p_test-dso-p_test.o"
                ,dso_ex_libs]

  -- build_programs_nodep: programs scripts

  --programs: a truly excessive number of deps.  there better be a pattern here
  -- there appears to be a pattern. small miracles

  -- need to make sure all of their object files are built.


  -- build object files. too many hopefully they're all the same
  let build_obj o = let dtmp = o -<.> "d.tmp"
                        d = o -<.> "d"
                        cname = reverse $ takeWhile (\x -> x /= '-') $ reverse $ takeBaseName o
                        c = replaceFileName o (cname <.> "c") in
                      seqCmds [unwords [cc, "-I.", "-Iinclude", "-Iapps/include", bin_cflags
                                       , bin_cppflags, "-MMD", "-MF", dtmp, "-MT",  o, "-c", "-o"
                                       , o, c]
                              ,unwords ["touch", dtmp]
                              ,unwords ["if", "cmp", dtmp, d, ">", "/dev/null", "2>", "/dev/null;"
                                       ,"then", "rm", "-f", dtmp ++ ";", "else", "mv", dtmp
                                       , d ++ ";", "fi"]]

  -- everything in fuzz/? not that many files actually.
  let build_obj2 o = let dtmp = o -<.> "d.tmp"
                         d = o -<.> "d"
                         cname = reverse $ takeWhile (\x -> x /= '-') $ reverse $ takeBaseName o
                         c = replaceFileName o (cname <.> "c") in
                       seqCmds [unwords [cc, "-Iinclude", bin_cflags, bin_cppflags, "-MMD", "-MF"
                                        , dtmp, "-MT",  o, "-c", "-o", o, c]
                               ,unwords ["touch", dtmp]
                               ,unwords ["if", "cmp", dtmp, d, ">", "/dev/null", "2>", "/dev/null;"
                                        ,"then", "rm", "-f", dtmp ++ ";", "else", "mv", dtmp
                                        , d ++ ";", "fi"]]
  forM_ apps_openssl_objs build_obj


  forM_ fuzz_objs build_obj2
  
  --forM_ fuzz_asn1_test_objs build_obj2
  --forM_ fuzz_asn1parse_test_objs build_obj2
  
  
  let build_program (p,os) =
        seqCmds ["rm -f " ++ p
                ,unwords ["${" ++ "LDCMD" ++ ":-" ++ cc ++ "}", bin_cflags, "-L." ,bin_ldflags
                         ,"-o", unwords os, "-lssl", "-lcrypto", bin_ex_libs]]
  let build_program2 (p,os) =
        seqCmds ["rm -f " ++ p
                ,unwords ["${" ++ "LDCMD" ++ ":-" ++ cc ++ "}", bin_cflags, "-L." ,bin_ldflags
                         ,"-o", unwords os, "-lcrypto", bin_ex_libs]]
  let build_program3 (p,os) =
        seqCmds ["rm -f " ++ p
                ,unwords ["${" ++ "LDCMD" ++ ":-" ++ cc ++ "}", bin_cflags, "-L.", bin_ldflags
                         ,"-o", unwords os, "-lssl", "test/libtestutil.a", "-lcrypto", bin_ex_libs]]
-- libssl.a!!!!!!!!!
  forM_ programs build_program
  forM_ programs2 build_program2
  forM_ programs3 build_program3

  --scripts: apps/CA.pl apps/tsget.pl tools/c_rehash util/shlib_wrap.sh
  let scripts = ["apps/CA.pl", "apps/tsget.pl", "tools/c_rehash", "util/shlib_wrap.sh"]
  forM_ scripts (\f -> seqCmds [unwords [perl, "\"-I" ++ blddir ++ ")\"", "-Mconfigdata"
                                        ,"\"util/dofile.pl\"", "\"-oMakefile\"", f<.> "in", ">", f]
                               ,"chmod a+x " ++ f])

  -- link-utils: blddir/util/opensslwrap.sh
  shcCmd $ "if [ \"" ++ srcdir ++ "\" != \"" ++ blddir ++ "\" ]; " ++
    "then mkdir -p \"" ++ blddir </> "util" ++ "\"; ln -sf \"../" ++
    srcdir </> "util/opensslwrap.sh" ++ "\" \"" ++ blddir </> "util" ++ "\"; fi"
  
  
  

  

  
  
  
  
  
