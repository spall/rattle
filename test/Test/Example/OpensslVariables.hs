
module Test.Example.OpensslVariables
  (apps_openssl_objs, programs, programs2, programs3, fuzz_objs, test_objs,
   blddir, srcdir, perl, bin_ex_libs, bin_ldflags, bin_cflags, cc, cflags,
   cnf_cflags, ldflags, cnf_ldflags, ex_libs, cnf_ex_libs,  bin_cppflags,
   lib_cflags, lib_cppflags, ar, arflags, ranlib, lib_ldflags, lib_ex_libs,
   installtop, libdir, version, dso_cflags, dso_cppflags, dso_ldflags,
   dso_ex_libs, perlasm_scheme, fips_objs, legacy_objs, libssl_objs,
   libtestutil_objs, libsslso_objs, platform, cppflags_q, fips_dso_objs,
   test_objs_1, test_objs_2, test_objs_3, test_c, test_objs_4
  ) where

import Development.Shake.FilePath

cppflags_q = "-DOPENSSL_USE_NODELETE -DL_ENDIAN -DOPENSSL_PIC -DNDEBUG -DOPENSSL_IA32_SSE2"
platform = "linux-x86_64"
version = "3.0.0-dev"
srcdir = "."
blddir = "."
perl = "/usr/bin/perl"
bin_ex_libs = unwords [cnf_ex_libs, ex_libs]
bin_ldflags = unwords [cnf_ldflags, ldflags]
bin_cflags = unwords [cnf_cflags, cflags]
cross_compile = ""
cc = cross_compile ++ "gcc"
cflags = "-Wall -O3"
cnf_cflags = "-pthread -m64 -Wa,--noexecstack"
ldflags = ""
cnf_ldflags = ""
ex_libs = ""
cnf_ex_libs = "-ldl -pthread"
bin_cppflags = unwords [cnf_cppflags, cppflags]
cnf_cppflags = "-DNDEBUG"
cppflags = ""
lib_cflags = unwords ["-fPIC", cnf_cflags, cflags]
lib_cppflags = unwords ["-DOPENSSL_USE_NODELETE", "-DL_ENDIAN", "-DOPENSSL_PIC", "-DOPENSSLDIR=\"\\\"" ++ openssldir ++ "\\\"\"", "-DENGINESDIR=\"\\\"" ++ enginesdir ++ "\\\"\"", "-DMODULESDIR=\"\\\"" ++ modulesdir ++ "\\\"\"", cnf_cppflags, cppflags]
ar = cross_compile ++ "ar"
arflags = "r"
ranlib = cross_compile ++ "ranlib"
enginesdir = libdir </> "engines-3"
openssldir = "/usr/local/ssl"
modulesdir = libdir </> "ossl-modules"
libdir_ = "lib64"
libdir = installtop </> libdir_
lib_ldflags = unwords ["-Wl,-znodelete", "-shared", "-Wl,-Bsymbolic", cnf_ldflags, ldflags]
lib_ex_libs = unwords [cnf_ex_libs, ex_libs]
installtop = "/usr/local"
dso_cflags = unwords ["-fPIC", cnf_cflags, cflags]
dso_cppflags = unwords [cnf_cppflags, cppflags]
dso_ldflags = unwords ["-z", "defs", "-Wl,-znodelete", "-shared", "-Wl,-Bsymbolic", cnf_ldflags, ldflags]
dso_ex_libs = unwords [cnf_ex_libs, ex_libs]
perlasm_scheme = "elf"
legacy_objs = ["providers/common/digests/legacy-dso-digest_common.o", "providers/legacy/digests/legacy-dso-md4_prov.o", "providers/legacy/digests/legacy-dso-mdc2_prov.o", "providers/legacy/digests/legacy-dso-ripemd_prov.o", "providers/legacy/digests/legacy-dso-wp_prov.o", "providers/legacy/legacy-dso-legacyprov.o"]


fips_objs = ["crypto/aes/fips-dso-aes-x86_64.o", "crypto/aes/fips-dso-aes_ecb.o", "crypto/aes/fips-dso-aes_misc.o", "crypto/aes/fips-dso-aesni-mb-x86_64.o", "crypto/aes/fips-dso-aesni-sha1-x86_64.o", "crypto/aes/fips-dso-aesni-sha256-x86_64.o", "crypto/aes/fips-dso-aesni-x86_64.o", "crypto/aes/fips-dso-bsaes-x86_64.o", "crypto/aes/fips-dso-vpaes-x86_64.o", "crypto/bn/asm/fips-dso-x86_64-gcc.o", "crypto/bn/fips-dso-bn_add.o", "crypto/bn/fips-dso-bn_blind.o", "crypto/bn/fips-dso-bn_const.o", "crypto/bn/fips-dso-bn_conv.o", "crypto/bn/fips-dso-bn_ctx.o", "crypto/bn/fips-dso-bn_dh.o", "crypto/bn/fips-dso-bn_div.o", "crypto/bn/fips-dso-bn_exp.o", "crypto/bn/fips-dso-bn_exp2.o", "crypto/bn/fips-dso-bn_gcd.o", "crypto/bn/fips-dso-bn_gf2m.o", "crypto/bn/fips-dso-bn_intern.o", "crypto/bn/fips-dso-bn_kron.o", "crypto/bn/fips-dso-bn_lib.o", "crypto/bn/fips-dso-bn_mod.o", "crypto/bn/fips-dso-bn_mont.o", "crypto/bn/fips-dso-bn_mpi.o", "crypto/bn/fips-dso-bn_mul.o", "crypto/bn/fips-dso-bn_nist.o", "crypto/bn/fips-dso-bn_prime.o", "crypto/bn/fips-dso-bn_rand.o", "crypto/bn/fips-dso-bn_recp.o", "crypto/bn/fips-dso-bn_rsa_fips186_4.o", "crypto/bn/fips-dso-bn_shift.o", "crypto/bn/fips-dso-bn_sqr.o", "crypto/bn/fips-dso-bn_sqrt.o", "crypto/bn/fips-dso-bn_word.o", "crypto/bn/fips-dso-bn_x931p.o", "crypto/bn/fips-dso-rsaz-avx2.o", "crypto/bn/fips-dso-rsaz-x86_64.o", "crypto/bn/fips-dso-rsaz_exp.o", "crypto/bn/fips-dso-x86_64-gf2m.o", "crypto/bn/fips-dso-x86_64-mont.o", "crypto/bn/fips-dso-x86_64-mont5.o", "crypto/buffer/fips-dso-buffer.o", "crypto/cmac/fips-dso-cmac.o", "crypto/des/fips-dso-des_enc.o", "crypto/des/fips-dso-ecb3_enc.o", "crypto/des/fips-dso-fcrypt_b.o", "crypto/des/fips-dso-set_key.o", "crypto/ec/curve448/arch_32/fips-dso-f_impl.o", "crypto/ec/curve448/fips-dso-curve448.o", "crypto/ec/curve448/fips-dso-curve448_tables.o", "crypto/ec/curve448/fips-dso-eddsa.o", "crypto/ec/curve448/fips-dso-f_generic.o", "crypto/ec/curve448/fips-dso-scalar.o", "crypto/ec/fips-dso-curve25519.o", "crypto/ec/fips-dso-ec2_oct.o", "crypto/ec/fips-dso-ec2_smpl.o", "crypto/ec/fips-dso-ec_asn1.o", "crypto/ec/fips-dso-ec_check.o", "crypto/ec/fips-dso-ec_curve.o", "crypto/ec/fips-dso-ec_cvt.o", "crypto/ec/fips-dso-ec_key.o", "crypto/ec/fips-dso-ec_kmeth.o", "crypto/ec/fips-dso-ec_lib.o", "crypto/ec/fips-dso-ec_mult.o", "crypto/ec/fips-dso-ec_oct.o", "crypto/ec/fips-dso-ec_print.o", "crypto/ec/fips-dso-ecdh_ossl.o", "crypto/ec/fips-dso-ecdsa_ossl.o", "crypto/ec/fips-dso-ecdsa_sign.o", "crypto/ec/fips-dso-ecdsa_vrf.o", "crypto/ec/fips-dso-ecp_mont.o", "crypto/ec/fips-dso-ecp_nist.o", "crypto/ec/fips-dso-ecp_nistp224.o", "crypto/ec/fips-dso-ecp_nistp256.o", "crypto/ec/fips-dso-ecp_nistp521.o", "crypto/ec/fips-dso-ecp_nistputil.o", "crypto/ec/fips-dso-ecp_nistz256-x86_64.o", "crypto/ec/fips-dso-ecp_nistz256.o", "crypto/ec/fips-dso-ecp_oct.o", "crypto/ec/fips-dso-ecp_smpl.o", "crypto/ec/fips-dso-x25519-x86_64.o", "crypto/evp/fips-dso-cmeth_lib.o", "crypto/evp/fips-dso-digest.o", "crypto/evp/fips-dso-evp_enc.o", "crypto/evp/fips-dso-evp_fetch.o", "crypto/evp/fips-dso-evp_lib.o", "crypto/evp/fips-dso-evp_utils.o", "crypto/evp/fips-dso-kdf_lib.o", "crypto/evp/fips-dso-kdf_meth.o", "crypto/evp/fips-dso-keymgmt_lib.o", "crypto/evp/fips-dso-keymgmt_meth.o", "crypto/evp/fips-dso-mac_lib.o", "crypto/evp/fips-dso-mac_meth.o", "crypto/fips-dso-asn1_dsa.o", "crypto/fips-dso-bsearch.o", "crypto/fips-dso-context.o", "crypto/fips-dso-core_algorithm.o", "crypto/fips-dso-core_fetch.o", "crypto/fips-dso-core_namemap.o", "crypto/fips-dso-cryptlib.o", "crypto/fips-dso-ctype.o", "crypto/fips-dso-ex_data.o", "crypto/fips-dso-initthread.o", "crypto/fips-dso-o_str.o", "crypto/fips-dso-packet.o", "crypto/fips-dso-param_build.o", "crypto/fips-dso-params.o", "crypto/fips-dso-params_from_text.o", "crypto/fips-dso-provider_core.o", "crypto/fips-dso-provider_predefined.o", "crypto/fips-dso-sparse_array.o", "crypto/fips-dso-threads_none.o", "crypto/fips-dso-threads_pthread.o", "crypto/fips-dso-threads_win.o", "crypto/fips-dso-x86_64cpuid.o", "crypto/hmac/fips-dso-hmac.o", "crypto/lhash/fips-dso-lhash.o", "crypto/modes/fips-dso-aesni-gcm-x86_64.o", "crypto/modes/fips-dso-cbc128.o", "crypto/modes/fips-dso-ccm128.o", "crypto/modes/fips-dso-cfb128.o", "crypto/modes/fips-dso-ctr128.o", "crypto/modes/fips-dso-gcm128.o", "crypto/modes/fips-dso-ghash-x86_64.o", "crypto/modes/fips-dso-ofb128.o", "crypto/modes/fips-dso-wrap128.o", "crypto/modes/fips-dso-xts128.o", "crypto/property/fips-dso-defn_cache.o", "crypto/property/fips-dso-property.o", "crypto/property/fips-dso-property_parse.o", "crypto/property/fips-dso-property_string.o", "crypto/rand/fips-dso-drbg_ctr.o", "crypto/rand/fips-dso-drbg_hash.o", "crypto/rand/fips-dso-drbg_hmac.o", "crypto/rand/fips-dso-drbg_lib.o", "crypto/rand/fips-dso-rand_crng_test.o", "crypto/rand/fips-dso-rand_lib.o", "crypto/rand/fips-dso-rand_unix.o", "crypto/rand/fips-dso-rand_vms.o", "crypto/rand/fips-dso-rand_vxworks.o", "crypto/rand/fips-dso-rand_win.o", "crypto/sha/fips-dso-keccak1600-x86_64.o", "crypto/sha/fips-dso-sha1-mb-x86_64.o", "crypto/sha/fips-dso-sha1-x86_64.o", "crypto/sha/fips-dso-sha1dgst.o", "crypto/sha/fips-dso-sha256-mb-x86_64.o", "crypto/sha/fips-dso-sha256-x86_64.o", "crypto/sha/fips-dso-sha256.o", "crypto/sha/fips-dso-sha3.o", "crypto/sha/fips-dso-sha512-x86_64.o", "crypto/sha/fips-dso-sha512.o", "crypto/stack/fips-dso-stack.o", "providers/common/ciphers/fips-dso-block.o", "providers/common/ciphers/fips-dso-cipher_aes.o", "providers/common/ciphers/fips-dso-cipher_aes_ccm.o", "providers/common/ciphers/fips-dso-cipher_aes_ccm_hw.o", "providers/common/ciphers/fips-dso-cipher_aes_gcm.o", "providers/common/ciphers/fips-dso-cipher_aes_gcm_hw.o", "providers/common/ciphers/fips-dso-cipher_aes_hw.o", "providers/common/ciphers/fips-dso-cipher_aes_wrp.o", "providers/common/ciphers/fips-dso-cipher_aes_xts.o", "providers/common/ciphers/fips-dso-cipher_aes_xts_hw.o", "providers/common/ciphers/fips-dso-cipher_ccm.o", "providers/common/ciphers/fips-dso-cipher_ccm_hw.o", "providers/common/ciphers/fips-dso-cipher_common.o", "providers/common/ciphers/fips-dso-cipher_common_hw.o", "providers/common/ciphers/fips-dso-cipher_gcm.o", "providers/common/ciphers/fips-dso-cipher_gcm_hw.o", "providers/common/ciphers/fips-dso-cipher_tdes.o", "providers/common/ciphers/fips-dso-cipher_tdes_hw.o", "providers/common/digests/fips-dso-digest_common.o", "providers/common/digests/fips-dso-sha2_prov.o", "providers/common/digests/fips-dso-sha3_prov.o", "providers/common/fips-dso-provider_util.o", "providers/common/kdfs/fips-dso-hkdf.o", "providers/common/kdfs/fips-dso-pbkdf2.o", "providers/common/kdfs/fips-dso-sskdf.o", "providers/common/kdfs/fips-dso-tls1_prf.o", "providers/common/macs/fips-dso-cmac_prov.o", "providers/common/macs/fips-dso-gmac_prov.o", "providers/common/macs/fips-dso-hmac_prov.o", "providers/common/macs/fips-dso-kmac_prov.o", "providers/fips/fips-dso-fipsprov.o", "providers/fips/fips-dso-selftest.o"]



apps_openssl_objs = ["apps/openssl-bin-asn1pars.o", "apps/openssl-bin-ca.o", "apps/openssl-bin-ciphers.o", "apps/openssl-bin-cms.o", "apps/openssl-bin-crl.o", "apps/openssl-bin-crl2p7.o", "apps/openssl-bin-dgst.o", "apps/openssl-bin-dhparam.o", "apps/openssl-bin-dsa.o", "apps/openssl-bin-dsaparam.o", "apps/openssl-bin-ec.o", "apps/openssl-bin-ecparam.o", "apps/openssl-bin-enc.o", "apps/openssl-bin-engine.o", "apps/openssl-bin-errstr.o", "apps/openssl-bin-fipsinstall.o", "apps/openssl-bin-gendsa.o", "apps/openssl-bin-genpkey.o", "apps/openssl-bin-genrsa.o", "apps/openssl-bin-info.o", "apps/openssl-bin-kdf.o", "apps/openssl-bin-list.o", "apps/openssl-bin-mac.o", "apps/openssl-bin-nseq.o", "apps/openssl-bin-ocsp.o", "apps/openssl-bin-openssl.o", "apps/openssl-bin-passwd.o", "apps/openssl-bin-pkcs12.o", "apps/openssl-bin-pkcs7.o", "apps/openssl-bin-pkcs8.o", "apps/openssl-bin-pkey.o", "apps/openssl-bin-pkeyparam.o", "apps/openssl-bin-pkeyutl.o", "apps/openssl-bin-prime.o", "apps/openssl-bin-progs.o", "apps/openssl-bin-provider.o", "apps/openssl-bin-rand.o", "apps/openssl-bin-rehash.o", "apps/openssl-bin-req.o", "apps/openssl-bin-rsa.o", "apps/openssl-bin-rsautl.o", "apps/openssl-bin-s_client.o", "apps/openssl-bin-s_server.o", "apps/openssl-bin-s_time.o", "apps/openssl-bin-sess_id.o", "apps/openssl-bin-smime.o", "apps/openssl-bin-speed.o", "apps/openssl-bin-spkac.o", "apps/openssl-bin-srp.o", "apps/openssl-bin-storeutl.o", "apps/openssl-bin-ts.o", "apps/openssl-bin-verify.o", "apps/openssl-bin-version.o", "apps/openssl-bin-x509.o"]

-- lists of program's object files. must be better way.

fuzz_objs = fuzz_asn1test_objs ++ fuzz_asn1parse_test_objs ++ fuzz_bignum_test_objs ++
            fuzz_bndiv_test_objs ++ fuzz_cms_test_objs ++ fuzz_conf_test_objs ++
            fuzz_crl_test_objs ++ fuzz_ct_test_objs ++ fuzz_server_test_objs ++
            fuzz_x509_test_objs ++ fuzz_client_test_objs
-- still need to individually list the object files.......
fuzz_asn1test_objs = ["fuzz/asn1-test-bin-asn1.o", "fuzz/asn1-test-bin-test-corpus.o"]
fuzz_asn1parse_test_objs = ["fuzz/asn1parse-test-bin-asn1parse.o", "fuzz/asn1parse-test-bin-test-corpus.o"]
fuzz_bignum_test_objs = ["fuzz/bignum-test-bin-bignum.o", "fuzz/bignum-test-bin-test-corpus.o"]
fuzz_bndiv_test_objs = ["fuzz/bndiv-test-bin-bndiv.o", "fuzz/bndiv-test-bin-test-corpus.o"]
fuzz_client_test_objs = ["fuzz/client-test-bin-client.o", "fuzz/client-test-bin-test-corpus.o"]
fuzz_cms_test_objs = ["fuzz/cms-test-bin-cms.o", "fuzz/cms-test-bin-test-corpus.o"]
fuzz_conf_test_objs = ["fuzz/conf-test-bin-conf.o", "fuzz/conf-test-bin-test-corpus.o"]
fuzz_crl_test_objs = ["fuzz/crl-test-bin-crl.o", "fuzz/crl-test-bin-test-corpus.o"]
fuzz_ct_test_objs = ["fuzz/ct-test-bin-ct.o", "fuzz/ct-test-bin-test-corpus.o"]
fuzz_server_test_objs = ["fuzz/server-test-bin-server.o", "fuzz/server-test-bin-test-corpus.o"]
fuzz_x509_test_objs = ["fuzz/x509-test-bin-test-corpus.o", "fuzz/x509-test-bin-x509.o"]

-- almost ALL of them
test_objs = ["test/aborttest-bin-aborttest.o","test/aesgcmtest-bin-aesgcmtest.o","test/afalgtest-bin-afalgtest.o", "test/asn1_decode_test-bin-asn1_decode_test.o"
            ,"test/asn1_dsa_internal_test-bin-asn1_dsa_internal_test.o", "test/asn1_encode_test-bin-asn1_encode_test.o", "test/asn1_internal_test-bin-asn1_internal_test.o",   
             "test/asn1_string_table_test-bin-asn1_string_table_test.o", "test/asn1_time_test-bin-asn1_time_test.o", "test/asynciotest-bin-asynciotest.o",  "test/asynciotest-bin-ssltestlib.o",    
              "test/asynctest-bin-asynctest.o", "test/bad_dtls_test-bin-bad_dtls_test.o", "test/bftest-bin-bftest.o", "test/bio_callback_test-bin-bio_callback_test.o",   
                 "test/bio_enc_test-bin-bio_enc_test.o", "test/bio_memleak_test-bin-bio_memleak_test.o", "test/bioprinttest-bin-bioprinttest.o", "test/bn_internal_test-bin-bn_internal_test.o",   
                 "test/bntest-bin-bntest.o", "test/buildtest_c_aes-bin-buildtest_aes.o", "test/buildtest_c_asn1-bin-buildtest_asn1.o", "test/buildtest_c_asn1t-bin-buildtest_asn1t.o",   
                 "test/buildtest_c_blowfish-bin-buildtest_blowfish.o", "test/buildtest_c_bn-bin-buildtest_bn.o", "test/buildtest_c_buffer-bin-buildtest_buffer.o",
                 "test/buildtest_c_camellia-bin-buildtest_camellia.o", "test/buildtest_c_cast-bin-buildtest_cast.o", "test/buildtest_c_cmp-bin-buildtest_cmp.o",
                 "test/buildtest_c_cms-bin-buildtest_cms.o","test/buildtest_c_comp-bin-buildtest_comp.o", "test/buildtest_c_conf-bin-buildtest_conf.o",
                 "test/buildtest_c_conf_api-bin-buildtest_conf_api.o","test/buildtest_c_core-bin-buildtest_core.o", "test/buildtest_c_core_names-bin-buildtest_core_names.o",   
                 "test/buildtest_c_core_numbers-bin-buildtest_core_numbers.o", "test/buildtest_c_crmf-bin-buildtest_crmf.o", "test/buildtest_c_ct-bin-buildtest_ct.o",
                 "test/buildtest_c_des-bin-buildtest_des.o", "test/buildtest_c_dh-bin-buildtest_dh.o", "test/buildtest_c_dsa-bin-buildtest_dsa.o", "test/buildtest_c_dtls1-bin-buildtest_dtls1.o",   
                 "test/buildtest_c_e_os2-bin-buildtest_e_os2.o", "test/buildtest_c_ecdh-bin-buildtest_ecdh.o", "test/buildtest_c_ecdsa-bin-buildtest_ecdsa.o",
                 "test/buildtest_c_engine-bin-buildtest_engine.o", "test/buildtest_c_ess-bin-buildtest_ess.o", "test/buildtest_c_evp-bin-buildtest_evp.o",
                 "test/buildtest_c_fips_names-bin-buildtest_fips_names.o", "test/buildtest_c_idea-bin-buildtest_idea.o", "test/buildtest_c_kdf-bin-buildtest_kdf.o",
                 "test/buildtest_c_lhash-bin-buildtest_lhash.o", "test/buildtest_c_macros-bin-buildtest_macros.o", "test/buildtest_c_md4-bin-buildtest_md4.o",
                 "test/buildtest_c_md5-bin-buildtest_md5.o", "test/buildtest_c_mdc2-bin-buildtest_mdc2.o", "test/buildtest_c_modes-bin-buildtest_modes.o",
                 "test/buildtest_c_objects-bin-buildtest_objects.o", "test/buildtest_c_ocsp-bin-buildtest_ocsp.o", "test/buildtest_c_opensslv-bin-buildtest_opensslv.o",
                 "test/buildtest_c_ossl_typ-bin-buildtest_ossl_typ.o", "test/buildtest_c_params-bin-buildtest_params.o", "test/buildtest_c_pem-bin-buildtest_pem.o",
                 "test/buildtest_c_pem2-bin-buildtest_pem2.o", "test/buildtest_c_pkcs12-bin-buildtest_pkcs12.o", "test/buildtest_c_pkcs7-bin-buildtest_pkcs7.o",
                 "test/buildtest_c_provider-bin-buildtest_provider.o", "test/buildtest_c_rand-bin-buildtest_rand.o",   
                 "test/buildtest_c_rand_drbg-bin-buildtest_rand_drbg.o","test/buildtest_c_rc2-bin-buildtest_rc2.o","test/buildtest_c_rc4-bin-buildtest_rc4.o",   
                 "test/buildtest_c_ripemd-bin-buildtest_ripemd.o","test/buildtest_c_rsa-bin-buildtest_rsa.o","test/buildtest_c_safestack-bin-buildtest_safestack.o",   
                 "test/buildtest_c_seed-bin-buildtest_seed.o",   
                 "test/buildtest_c_sha-bin-buildtest_sha.o",   
                 "test/buildtest_c_srp-bin-buildtest_srp.o",   
                 "test/buildtest_c_srtp-bin-buildtest_srtp.o",   
                 "test/buildtest_c_ssl-bin-buildtest_ssl.o",   
                 "test/buildtest_c_ssl2-bin-buildtest_ssl2.o",   
                 "test/buildtest_c_stack-bin-buildtest_stack.o",   
                 "test/buildtest_c_store-bin-buildtest_store.o",   
                 "test/buildtest_c_symhacks-bin-buildtest_symhacks.o",   
                 "test/buildtest_c_tls1-bin-buildtest_tls1.o",   
                 "test/buildtest_c_ts-bin-buildtest_ts.o",   
                 "test/buildtest_c_txt_db-bin-buildtest_txt_db.o",   
                 "test/buildtest_c_ui-bin-buildtest_ui.o",   
                 "test/buildtest_c_whrlpool-bin-buildtest_whrlpool.o",   
                 "test/buildtest_c_x509-bin-buildtest_x509.o",   
                 "test/buildtest_c_x509_vfy-bin-buildtest_x509_vfy.o",   
                 "test/buildtest_c_x509v3-bin-buildtest_x509v3.o",   
                 "test/casttest-bin-casttest.o",   
                 "test/chacha_internal_test-bin-chacha_internal_test.o",   
                 "test/cipherbytes_test-bin-cipherbytes_test.o",    
                 "test/cipherlist_test-bin-cipherlist_test.o",    
                 "test/ciphername_test-bin-ciphername_test.o",    
                 "test/clienthellotest-bin-clienthellotest.o",    
                 "test/cmsapitest-bin-cmsapitest.o",   
                 "test/conf_include_test-bin-conf_include_test.o",   
                 "test/constant_time_test-bin-constant_time_test.o",   
                 "test/context_internal_test-bin-context_internal_test.o",   
                 "test/crltest-bin-crltest.o",   
                 "test/ct_test-bin-ct_test.o",   
                 "test/ctype_internal_test-bin-ctype_internal_test.o",   
                 "test/curve448_internal_test-bin-curve448_internal_test.o",   
                 "test/d2i_test-bin-d2i_test.o",   
                 "test/danetest-bin-danetest.o",    
                 "test/destest-bin-destest.o",   
                 "test/dhtest-bin-dhtest.o",   
                 "test/drbg_cavs_test-bin-drbg_cavs_data_ctr.o",  "test/drbg_cavs_test-bin-drbg_cavs_data_hash.o",  "test/drbg_cavs_test-bin-drbg_cavs_data_hmac.o",  "test/drbg_cavs_test-bin-drbg_cavs_test.o",   
                 "test/drbgtest-bin-drbgtest.o",   
                 "test/dsa_no_digest_size_test-bin-dsa_no_digest_size_test.o",   
                 "test/dsatest-bin-dsatest.o",   
                 "test/dtls_mtu_test-bin-dtls_mtu_test.o",  "test/dtls_mtu_test-bin-ssltestlib.o",    
                 "test/dtlstest-bin-dtlstest.o",  "test/dtlstest-bin-ssltestlib.o",    
                 "test/dtlsv1listentest-bin-dtlsv1listentest.o",    
                 "test/ec_internal_test-bin-ec_internal_test.o",   
                 "test/ecdsatest-bin-ecdsatest.o",   
                 "test/ecstresstest-bin-ecstresstest.o",   
                 "test/ectest-bin-ectest.o",   
                 "test/enginetest-bin-enginetest.o",   
                 "test/errtest-bin-errtest.o",   
                 "test/evp_extra_test-bin-evp_extra_test.o",   
                 "test/evp_kdf_test-bin-evp_kdf_test.o",   
                 "test/evp_pkey_dparams_test-bin-evp_pkey_dparams_test.o",   
                 "test/evp_test-bin-evp_test.o",   
                 "test/exdatatest-bin-exdatatest.o",   
                 "test/exptest-bin-exptest.o",   
                 "test/fatalerrtest-bin-fatalerrtest.o",  "test/fatalerrtest-bin-ssltestlib.o",    
                 "test/gmdifftest-bin-gmdifftest.o",   
                 "test/gosttest-bin-gosttest.o",  "test/gosttest-bin-ssltestlib.o",    
                 "test/hmactest-bin-hmactest.o",   
                 "test/ideatest-bin-ideatest.o",   
                 "test/igetest-bin-igetest.o",   
                 "test/lhash_test-bin-lhash_test.o",   
                 "test/md2test-bin-md2test.o",   
                 "test/mdc2_internal_test-bin-mdc2_internal_test.o",   
                 "test/mdc2test-bin-mdc2test.o",   
                 "test/memleaktest-bin-memleaktest.o",   
                 "test/modes_internal_test-bin-modes_internal_test.o",   
                 "test/namemap_internal_test-bin-namemap_internal_test.o",   
                 "test/ocspapitest-bin-ocspapitest.o",   
                 "test/packettest-bin-packettest.o",   
                 "test/param_build_test-bin-param_build_test.o",   
                 "test/params_api_test-bin-params_api_test.o",   
                 "test/params_conversion_test-bin-params_conversion_test.o",   
                 "test/params_test-bin-params_test.o",   
                 "test/pbelutest-bin-pbelutest.o",   
                 "test/pemtest-bin-pemtest.o",   
                 "test/pkey_meth_kdf_test-bin-pkey_meth_kdf_test.o",   
                 "test/pkey_meth_test-bin-pkey_meth_test.o",   
                 "test/poly1305_internal_test-bin-poly1305_internal_test.o",   
                 "test/property_test-bin-property_test.o",   
                 "test/provider_internal_test-bin-p_test.o",  "test/provider_internal_test-bin-provider_internal_test.o",   
                 "test/provider_test-bin-p_test.o",  "test/provider_test-bin-provider_test.o",   
                 "test/rc2test-bin-rc2test.o",   
                 "test/rc4test-bin-rc4test.o",   
                 "test/rc5test-bin-rc5test.o",   
                 "test/rdrand_sanitytest-bin-rdrand_sanitytest.o",   
                 "test/recordlentest-bin-recordlentest.o",  "test/recordlentest-bin-ssltestlib.o",    
                 "test/rsa_complex-bin-rsa_complex.o",  
                 "test/rsa_mp_test-bin-rsa_mp_test.o",   
                 "test/rsa_sp800_56b_test-bin-rsa_sp800_56b_test.o",   
                 "test/rsa_test-bin-rsa_test.o",   
                 "test/sanitytest-bin-sanitytest.o",   
                 "test/secmemtest-bin-secmemtest.o",   
                 "test/servername_test-bin-servername_test.o",  "test/servername_test-bin-ssltestlib.o",    
                 "test/shlibloadtest-bin-shlibloadtest.o",  
                 "test/siphash_internal_test-bin-siphash_internal_test.o",   
                 "test/sm2_internal_test-bin-sm2_internal_test.o",   
                 "test/sm4_internal_test-bin-sm4_internal_test.o",   
                 "test/sparse_array_test-bin-sparse_array_test.o",   
                 "test/srptest-bin-srptest.o",   
                 "test/ssl_cert_table_internal_test-bin-ssl_cert_table_internal_test.o",   
                 "test/ssl_test-bin-handshake_helper.o",  "test/ssl_test-bin-ssl_test.o",  "test/ssl_test-bin-ssl_test_ctx.o",    
                 "test/ssl_test_ctx_test-bin-ssl_test_ctx.o",  "test/ssl_test_ctx_test-bin-ssl_test_ctx_test.o",    
                 "test/sslapitest-bin-sslapitest.o",  "test/sslapitest-bin-ssltestlib.o",    
                 "test/sslbuffertest-bin-sslbuffertest.o",  "test/sslbuffertest-bin-ssltestlib.o",    
                 "test/sslcorrupttest-bin-sslcorrupttest.o",  "test/sslcorrupttest-bin-ssltestlib.o",    
                 "test/ssltest_old-bin-ssltest_old.o",   
                 "test/stack_test-bin-stack_test.o",   
                 "test/sysdefaulttest-bin-sysdefaulttest.o",    
                 "test/test_test-bin-test_test.o",   
                 "test/threadstest-bin-threadstest.o",   
                 "test/time_offset_test-bin-time_offset_test.o",   
                 "test/tls13ccstest-bin-ssltestlib.o",  "test/tls13ccstest-bin-tls13ccstest.o",    
                 "test/tls13encryptiontest-bin-tls13encryptiontest.o", 
                 "crypto/tls13secretstest-bin-packet.o",  "ssl/tls13secretstest-bin-tls13_enc.o",  "test/tls13secretstest-bin-tls13secretstest.o",    
                 "apps/uitest-bin-apps_ui.o",  "test/uitest-bin-uitest.o",   
                 "test/v3ext-bin-v3ext.o",   
                 "test/v3nametest-bin-v3nametest.o",   
                 "test/verify_extra_test-bin-verify_extra_test.o",   
                 "test/versions-bin-versions.o",  
                 "test/wpackettest-bin-wpackettest.o",   
                 "test/x509_check_cert_pkey_test-bin-x509_check_cert_pkey_test.o",   
                 "test/x509_dup_cert_test-bin-x509_dup_cert_test.o",   
                 "test/x509_internal_test-bin-x509_internal_test.o", 
                 "test/x509_time_test-bin-x509_time_test.o",   
                 "test/x509aux-bin-x509aux.o", 
                 "test/testutil/libtestutil-lib-apps_mem.o",  "test/testutil/libtestutil-lib-basic_output.o",  "test/testutil/libtestutil-lib-cb.o",  "test/testutil/libtestutil-lib-driver.o",  "test/testutil/libtestutil-lib-format_output.o",  "test/testutil/libtestutil-lib-init.o",  "test/testutil/libtestutil-lib-main.o",  "test/testutil/libtestutil-lib-options.o",  "test/testutil/libtestutil-lib-output_helpers.o",  "test/testutil/libtestutil-lib-random.o",  "test/testutil/libtestutil-lib-stanza.o",  "test/testutil/libtestutil-lib-tap_bio.o",  "test/testutil/libtestutil-lib-test_cleanup.o",  "test/testutil/libtestutil-lib-test_options.o",  "test/testutil/libtestutil-lib-tests.o"]

-- add all of these to test_objs = test_aborttest_objs ++ .... etc

-- todo figure out which ones have which flags.
test_c = ["test/buildtest_aes.c", "test/buildtest_asn1.c", "test/buildtest_asn1t.c", "test/buildtest_async.c", "test/buildtest_bio.c", "test/buildtest_blowfish.c", "test/buildtest_bn.c", "test/buildtest_buffer.c", "test/buildtest_camellia.c", "test/buildtest_cast.c", "test/buildtest_cmac.c", "test/buildtest_cmp.c", "test/buildtest_cms.c", "test/buildtest_comp.c", "test/buildtest_conf.c", "test/buildtest_conf_api.c", "test/buildtest_core.c", "test/buildtest_core_names.c", "test/buildtest_core_numbers.c", "test/buildtest_crmf.c", "test/buildtest_crypto.c", "test/buildtest_ct.c", "test/buildtest_des.c", "test/buildtest_dh.c", "test/buildtest_dsa.c", "test/buildtest_dtls1.c", "test/buildtest_e_os2.c", "test/buildtest_ebcdic.c", "test/buildtest_ec.c", "test/buildtest_ecdh.c", "test/buildtest_ecdsa.c", "test/buildtest_engine.c", "test/buildtest_ess.c", "test/buildtest_evp.c", "test/buildtest_fips_names.c", "test/buildtest_hmac.c", "test/buildtest_idea.c", "test/buildtest_kdf.c", "test/buildtest_lhash.c", "test/buildtest_macros.c", "test/buildtest_md4.c", "test/buildtest_md5.c", "test/buildtest_mdc2.c", "test/buildtest_modes.c", "test/buildtest_obj_mac.c", "test/buildtest_objects.c", "test/buildtest_ocsp.c", "test/buildtest_opensslv.c", "test/buildtest_ossl_typ.c", "test/buildtest_params.c", "test/buildtest_pem.c", "test/buildtest_pem2.c", "test/buildtest_pkcs12.c", "test/buildtest_pkcs7.c", "test/buildtest_provider.c", "test/buildtest_rand.c", "test/buildtest_rand_drbg.c", "test/buildtest_rc2.c", "test/buildtest_rc4.c", "test/buildtest_ripemd.c", "test/buildtest_rsa.c", "test/buildtest_safestack.c", "test/buildtest_seed.c", "test/buildtest_sha.c", "test/buildtest_srp.c", "test/buildtest_srtp.c", "test/buildtest_ssl.c", "test/buildtest_ssl2.c", "test/buildtest_stack.c", "test/buildtest_store.c", "test/buildtest_symhacks.c", "test/buildtest_tls1.c", "test/buildtest_ts.c", "test/buildtest_txt_db.c", "test/buildtest_ui.c", "test/buildtest_whrlpool.c", "test/buildtest_x509.c", "test/buildtest_x509_vfy.c", "test/buildtest_x509v3.c"]

test_objs_1 = ["test/aesgcmtest-bin-aesgcmtest.o", "test/asynciotest-bin-ssltestlib.o", "test/context_internal_test-bin-context_internal_test.o", "test/dtls_mtu_test-bin-dtls_mtu_test.o", "test/dtls_mtu_test-bin-ssltestlib.o", "test/dtlstest-bin-ssltestlib.o", "test/fatalerrtest-bin-ssltestlib.o", "test/gosttest-bin-gosttest.o", "test/gosttest-bin-ssltestlib.o", "test/mdc2_internal_test-bin-mdc2_internal_test.o", "test/namemap_internal_test-bin-namemap_internal_test.o", "test/params_test-bin-params_test.o", "test/property_test-bin-property_test.o", "test/recordlentest-bin-ssltestlib.o", "test/servername_test-bin-ssltestlib.o", "test/ssl_cert_table_internal_test-bin-ssl_cert_table_internal_test.o", "test/ssl_test-bin-handshake_helper.o", "test/sslapitest-bin-sslapitest.o", "test/sslapitest-bin-ssltestlib.o", "test/sslbuffertest-bin-ssltestlib.o", "test/sslcorrupttest-bin-ssltestlib.o", "test/ssltest_old-bin-ssltest_old.o", "test/tls13ccstest-bin-ssltestlib.o", "test/tls13encryptiontest-bin-tls13encryptiontest.o", "test/tls13secretstest-bin-tls13secretstest.o", "test/uitest-bin-uitest.o", "test/x509_internal_test-bin-x509_internal_test.o"]

test_objs_2 = ["test/aborttest-bin-aborttest.o", "test/afalgtest-bin-afalgtest.o", "test/asn1_decode_test-bin-asn1_decode_test.o", "test/asn1_encode_test-bin-asn1_encode_test.o", "test/asn1_string_table_test-bin-asn1_string_table_test.o", "test/asn1_time_test-bin-asn1_time_test.o", "test/asynciotest-bin-asynciotest.o", "test/asynctest-bin-asynctest.o", "test/bad_dtls_test-bin-bad_dtls_test.o", "test/bftest-bin-bftest.o", "test/bio_callback_test-bin-bio_callback_test.o", "test/bio_enc_test-bin-bio_enc_test.o", "test/bio_memleak_test-bin-bio_memleak_test.o", "test/bioprinttest-bin-bioprinttest.o", "test/bntest-bin-bntest.o", "test/casttest-bin-casttest.o", "test/cipherbytes_test-bin-cipherbytes_test.o", "test/cipherlist_test-bin-cipherlist_test.o", "test/ciphername_test-bin-ciphername_test.o", "test/clienthellotest-bin-clienthellotest.o", "test/cmsapitest-bin-cmsapitest.o", "test/conf_include_test-bin-conf_include_test.o", "test/constant_time_test-bin-constant_time_test.o", "test/crltest-bin-crltest.o", "test/ct_test-bin-ct_test.o", "test/d2i_test-bin-d2i_test.o", "test/danetest-bin-danetest.o", "test/destest-bin-destest.o", "test/dhtest-bin-dhtest.o", "test/dsa_no_digest_size_test-bin-dsa_no_digest_size_test.o", "test/dsatest-bin-dsatest.o", "test/dtlstest-bin-dtlstest.o", "test/dtlsv1listentest-bin-dtlsv1listentest.o", "test/ecdsatest-bin-ecdsatest.o", "test/ecstresstest-bin-ecstresstest.o", "test/ectest-bin-ectest.o", "test/enginetest-bin-enginetest.o", "test/errtest-bin-errtest.o", "test/evp_kdf_test-bin-evp_kdf_test.o", "test/evp_pkey_dparams_test-bin-evp_pkey_dparams_test.o", "test/evp_test-bin-evp_test.o", "test/exdatatest-bin-exdatatest.o", "test/exptest-bin-exptest.o", "test/fatalerrtest-bin-fatalerrtest.o", "test/gmdifftest-bin-gmdifftest.o", "test/hmactest-bin-hmactest.o", "test/ideatest-bin-ideatest.o", "test/igetest-bin-igetest.o", "test/lhash_test-bin-lhash_test.o", "test/md2test-bin-md2test.o", "test/mdc2test-bin-mdc2test.o", "test/memleaktest-bin-memleaktest.o", "test/ocspapitest-bin-ocspapitest.o", "test/packettest-bin-packettest.o", "test/param_build_test-bin-param_build_test.o", "test/params_api_test-bin-params_api_test.o", "test/params_conversion_test-bin-params_conversion_test.o", "test/pbelutest-bin-pbelutest.o", "test/pemtest-bin-pemtest.o", "test/pkey_meth_kdf_test-bin-pkey_meth_kdf_test.o", "test/pkey_meth_test-bin-pkey_meth_test.o", "test/rc2test-bin-rc2test.o", "test/rc4test-bin-rc4test.o", "test/rc5test-bin-rc5test.o", "test/rdrand_sanitytest-bin-rdrand_sanitytest.o", "test/recordlentest-bin-recordlentest.o", "test/rsa_complex-bin-rsa_complex.o", "test/rsa_mp_test-bin-rsa_mp_test.o", "test/rsa_test-bin-rsa_test.o", "test/sanitytest-bin-sanitytest.o", "test/secmemtest-bin-secmemtest.o", "test/servername_test-bin-servername_test.o", "test/srptest-bin-srptest.o", "test/ssl_test-bin-ssl_test.o", "test/ssl_test-bin-ssl_test_ctx.o", "test/ssl_test_ctx_test-bin-ssl_test_ctx.o", "test/ssl_test_ctx_test-bin-ssl_test_ctx_test.o", "test/sslbuffertest-bin-sslbuffertest.o", "test/sslcorrupttest-bin-sslcorrupttest.o", "test/stack_test-bin-stack_test.o", "test/sysdefaulttest-bin-sysdefaulttest.o", "test/test_test-bin-test_test.o", "test/threadstest-bin-threadstest.o", "test/time_offset_test-bin-time_offset_test.o", "test/tls13ccstest-bin-tls13ccstest.o", "test/v3ext-bin-v3ext.o", "test/v3nametest-bin-v3nametest.o", "test/verify_extra_test-bin-verify_extra_test.o", "test/versions-bin-versions.o", "test/wpackettest-bin-wpackettest.o", "test/x509_check_cert_pkey_test-bin-x509_check_cert_pkey_test.o", "test/x509_dup_cert_test-bin-x509_dup_cert_test.o", "test/x509_time_test-bin-x509_time_test.o", "test/x509aux-bin-x509aux.o"]

test_objs_3 = ["test/buildtest_c_aes-bin-buildtest_aes.o", "test/buildtest_c_asn1-bin-buildtest_asn1.o", "test/buildtest_c_asn1t-bin-buildtest_asn1t.o", "test/buildtest_c_async-bin-buildtest_async.o", "test/buildtest_c_bio-bin-buildtest_bio.o", "test/buildtest_c_blowfish-bin-buildtest_blowfish.o", "test/buildtest_c_bn-bin-buildtest_bn.o", "test/buildtest_c_buffer-bin-buildtest_buffer.o", "test/buildtest_c_camellia-bin-buildtest_camellia.o", "test/buildtest_c_cast-bin-buildtest_cast.o", "test/buildtest_c_cmac-bin-buildtest_cmac.o", "test/buildtest_c_cmp-bin-buildtest_cmp.o", "test/buildtest_c_cms-bin-buildtest_cms.o", "test/buildtest_c_comp-bin-buildtest_comp.o", "test/buildtest_c_conf-bin-buildtest_conf.o", "test/buildtest_c_conf_api-bin-buildtest_conf_api.o", "test/buildtest_c_core-bin-buildtest_core.o", "test/buildtest_c_core_names-bin-buildtest_core_names.o", "test/buildtest_c_core_numbers-bin-buildtest_core_numbers.o", "test/buildtest_c_crmf-bin-buildtest_crmf.o", "test/buildtest_c_crypto-bin-buildtest_crypto.o", "test/buildtest_c_ct-bin-buildtest_ct.o", "test/buildtest_c_des-bin-buildtest_des.o", "test/buildtest_c_dh-bin-buildtest_dh.o", "test/buildtest_c_dsa-bin-buildtest_dsa.o", "test/buildtest_c_dtls1-bin-buildtest_dtls1.o", "test/buildtest_c_e_os2-bin-buildtest_e_os2.o", "test/buildtest_c_ebcdic-bin-buildtest_ebcdic.o", "test/buildtest_c_ec-bin-buildtest_ec.o", "test/buildtest_c_ecdh-bin-buildtest_ecdh.o", "test/buildtest_c_ecdsa-bin-buildtest_ecdsa.o", "test/buildtest_c_engine-bin-buildtest_engine.o", "test/buildtest_c_ess-bin-buildtest_ess.o", "test/buildtest_c_evp-bin-buildtest_evp.o", "test/buildtest_c_fips_names-bin-buildtest_fips_names.o", "test/buildtest_c_hmac-bin-buildtest_hmac.o", "test/buildtest_c_idea-bin-buildtest_idea.o", "test/buildtest_c_kdf-bin-buildtest_kdf.o", "test/buildtest_c_lhash-bin-buildtest_lhash.o", "test/buildtest_c_macros-bin-buildtest_macros.o", "test/buildtest_c_md4-bin-buildtest_md4.o", "test/buildtest_c_md5-bin-buildtest_md5.o", "test/buildtest_c_mdc2-bin-buildtest_mdc2.o", "test/buildtest_c_modes-bin-buildtest_modes.o", "test/buildtest_c_obj_mac-bin-buildtest_obj_mac.o", "test/buildtest_c_objects-bin-buildtest_objects.o", "test/buildtest_c_ocsp-bin-buildtest_ocsp.o", "test/buildtest_c_opensslv-bin-buildtest_opensslv.o", "test/buildtest_c_ossl_typ-bin-buildtest_ossl_typ.o", "test/buildtest_c_params-bin-buildtest_params.o", "test/buildtest_c_pem-bin-buildtest_pem.o", "test/buildtest_c_pem2-bin-buildtest_pem2.o", "test/buildtest_c_pkcs12-bin-buildtest_pkcs12.o", "test/buildtest_c_pkcs7-bin-buildtest_pkcs7.o", "test/buildtest_c_provider-bin-buildtest_provider.o", "test/buildtest_c_rand-bin-buildtest_rand.o", "test/buildtest_c_rand_drbg-bin-buildtest_rand_drbg.o", "test/buildtest_c_rc2-bin-buildtest_rc2.o", "test/buildtest_c_rc4-bin-buildtest_rc4.o", "test/buildtest_c_ripemd-bin-buildtest_ripemd.o", "test/buildtest_c_rsa-bin-buildtest_rsa.o", "test/buildtest_c_safestack-bin-buildtest_safestack.o", "test/buildtest_c_seed-bin-buildtest_seed.o", "test/buildtest_c_sha-bin-buildtest_sha.o", "test/buildtest_c_srp-bin-buildtest_srp.o", "test/buildtest_c_srtp-bin-buildtest_srtp.o", "test/buildtest_c_ssl-bin-buildtest_ssl.o", "test/buildtest_c_ssl2-bin-buildtest_ssl2.o", "test/buildtest_c_stack-bin-buildtest_stack.o", "test/buildtest_c_store-bin-buildtest_store.o", "test/buildtest_c_symhacks-bin-buildtest_symhacks.o", "test/buildtest_c_tls1-bin-buildtest_tls1.o", "test/buildtest_c_ts-bin-buildtest_ts.o", "test/buildtest_c_txt_db-bin-buildtest_txt_db.o", "test/buildtest_c_ui-bin-buildtest_ui.o", "test/buildtest_c_whrlpool-bin-buildtest_whrlpool.o", "test/buildtest_c_x509-bin-buildtest_x509.o", "test/buildtest_c_x509_vfy-bin-buildtest_x509_vfy.o", "test/buildtest_c_x509v3-bin-buildtest_x509v3.o"]

test_objs_4 = ["test/asn1_dsa_internal_test-bin-asn1_dsa_internal_test.o", "test/asn1_internal_test-bin-asn1_internal_test.o", "test/chacha_internal_test-bin-chacha_internal_test.o", "test/ctype_internal_test-bin-ctype_internal_test.o", "test/modes_internal_test-bin-modes_internal_test.o", "test/poly1305_internal_test-bin-poly1305_internal_test.o", "test/siphash_internal_test-bin-siphash_internal_test.o", "test/sm4_internal_test-bin-sm4_internal_test.o"] 

test_aborttest_objs = ["test/aborttest-bin-aborttest.o"] 
test_aesgcmtest_objs = ["test/aesgcmtest-bin-aesgcmtest.o", "test/libtestutil.a"] -- -I. 
test_afalgtest_objs = ["test/afalgtest-bin-afalgtest.o", "test/libtestutil.a"] 
test_asn1_decode_test_objs = ["test/asn1_decode_test-bin-asn1_decode_test.o", "test/libtestutil.a"] 
test_asn1_dsa_internal_test_objs = ["test/asn1_dsa_internal_test-bin-asn1_dsa_internal_test.o", "test/libtestutil.a", "libcrypto.a"] -- -I. -Icrypto/include 
test_asn1_encode_test_objs = ["test/asn1_encode_test-bin-asn1_encode_test.o", "test/libtestutil.a"] 
test_asn1_internal_test_objs = ["test/asn1_internal_test-bin-asn1_internal_test.o", "test/libtestutil.a", "libcrypto.a"]
test_asn1_string_table_test_objs = ["test/asn1_string_table_test-bin-asn1_string_table_test.o", "test/libtestutil.a"]
test_asn1_time_test_objs = ["test/asn1_time_test-bin-asn1_time_test.o", "test/libtestutil.a"] 
test_asynciotest_objs = ["test/asynciotest-bin-asynciotest.o", "test/asynciotest-bin-ssltestlib.o", "test/libtestutil.a"]
test_asynctest_objs = ["test/asynctest-bin-asynctest.o"]
test_bad_dtls_test_objs = ["test/bad_dtls_test-bin-bad_dtls_test.o", "test/libtestutil.a"]
test_bftest_objs = ["test/bftest-bin-bftest.o", "test/libtestutil.a"] 
test_bio_callback_test_objs = ["test/bio_callback_test-bin-bio_callback_test.o", "test/libtestutil.a"] 
test_bio_enc_test_objs = ["test/bio_enc_test-bin-bio_enc_test.o", "test/libtestutil.a"] 
test_bio_memleak_test_objs = ["test/bio_memleak_test-bin-bio_memleak_test.o", "test/libtestutil.a"] 
test_bioprinttest_objs = ["test/bioprinttest-bin-bioprinttest.o", "test/libtestutil.a"] 
test_bn_internal_test_objs = ["test/bn_internal_test-bin-bn_internal_test.o", "test/libtestutil.a", "libcrypto.a"]
test_bntest_objs = ["test/bntest-bin-bntest.o", "test/libtestutil.a"] 
test_buildtest_c_aes_objs = ["test/buildtest_c_aes-bin-buildtest_aes.o"] 
test_buildtest_c_asn1_objs = ["test/buildtest_c_asn1-bin-buildtest_asn1.o"]
test_buildtest_c_asn1t_objs = ["test/buildtest_c_asn1t-bin-buildtest_asn1t.o"]
test_buildtest_c_blowfish_objs = ["test/buildtest_c_blowfish-bin-buildtest_blowfish.o"]
test_buildtest_c_bn_objs = ["test/buildtest_c_bn-bin-buildtest_bn.o"]
test_buildtest_c_buffer_objs = ["test/buildtest_c_buffer-bin-buildtest_buffer.o"]
test_buildtest_c_camellia_objs = ["test/buildtest_c_camellia-bin-buildtest_camellia.o"]
test_buildtest_c_cast_objs = ["test/buildtest_c_cast-bin-buildtest_cast.o"]
test_buildtest_c_cmp_objs = ["test/buildtest_c_cmp-bin-buildtest_cmp.o"]
test_buildtest_c_cms_objs = ["test/buildtest_c_cms-bin-buildtest_cms.o"]
test_buildtest_c_comp_objs = ["test/buildtest_c_comp-bin-buildtest_comp.o"]
test_buildtest_c_conf_objs = ["test/buildtest_c_conf-bin-buildtest_conf.o"]
test_buildtest_c_conf_api_objs = ["test/buildtest_c_conf_api-bin-buildtest_conf_api.o"]
test_buildtest_c_core_objs = ["test/buildtest_c_core-bin-buildtest_core.o"]
test_buildtest_c_core_names_objs = ["test/buildtest_c_core_names-bin-buildtest_core_names.o"]
test_buildtest_c_core_numbers_objs = ["test/buildtest_c_core_numbers-bin-buildtest_core_numbers.o"]
test_buildtest_c_crmf_objs = ["test/buildtest_c_crmf-bin-buildtest_crmf.o"]
test_buildtest_c_ct_objs = ["test/buildtest_c_ct-bin-buildtest_ct.o"]
test_buildtest_c_des_objs = ["test/buildtest_c_des-bin-buildtest_des.o"]
test_buildtest_c_dh_objs = ["test/buildtest_c_dh-bin-buildtest_dh.o"]
test_buildtest_c_dsa_objs = ["test/buildtest_c_dsa-bin-buildtest_dsa.o"]
test_buildtest_c_dtls1_objs = ["test/buildtest_c_dtls1-bin-buildtest_dtls1.o"]
test_buildtest_c_e_os2_objs = ["test/buildtest_c_e_os2-bin-buildtest_e_os2.o"]
test_buildtest_c_ecdh_objs = ["test/buildtest_c_ecdh-bin-buildtest_ecdh.o"]
test_buildtest_c_ecdsa_objs = ["test/buildtest_c_ecdsa-bin-buildtest_ecdsa.o"]
test_buildtest_c_engine_objs = ["test/buildtest_c_engine-bin-buildtest_engine.o"]
test_buildtest_c_ess_objs = ["test/buildtest_c_ess-bin-buildtest_ess.o"]
test_buildtest_c_evp_objs = ["test/buildtest_c_evp-bin-buildtest_evp.o"]
test_buildtest_c_fips_names_objs = ["test/buildtest_c_fips_names-bin-buildtest_fips_names.o"]
test_buildtest_c_hmac_objs = ["test/buildtest_c_hmac-bin-buildtest_hmac.o"]
test_buildtest_c_idea_objs = ["test/buildtest_c_idea-bin-buildtest_idea.o"]
test_buildtest_c_kdf_objs = ["test/buildtest_c_kdf-bin-buildtest_kdf.o"]
test_buildtest_c_lhash_objs = ["test/buildtest_c_lhash-bin-buildtest_lhash.o"]
test_buildtest_c_macros_objs = ["test/buildtest_c_macros-bin-buildtest_macros.o"]
test_buildtest_c_md4_objs = ["test/buildtest_c_md4-bin-buildtest_md4.o"]
test_buildtest_c_md5_objs = ["test/buildtest_c_md5-bin-buildtest_md5.o"]
test_buildtest_c_mdc2_objs = ["test/buildtest_c_mdc2-bin-buildtest_mdc2.o"]
test_buildtest_c_modes_objs = ["test/buildtest_c_modes-bin-buildtest_modes.o"]
test_buildtest_c_obj_mac_objs = ["test/buildtest_c_obj_mac-bin-buildtest_obj_mac.o"]
test_buildtest_c_objects_objs = ["test/buildtest_c_objects-bin-buildtest_objects.o"]
test_buildtest_c_ocsp_objs = ["test/buildtest_c_ocsp-bin-buildtest_ocsp.o"]
test_buildtest_c_opensslv_objs = ["test/buildtest_c_opensslv-bin-buildtest_opensslv.o"]
test_buildtest_c_ossl_typ_objs = ["test/buildtest_c_ossl_typ-bin-buildtest_ossl_typ.o"]
test_buildtest_c_params_objs = ["test/buildtest_c_params-bin-buildtest_params.o"]
test_buildtest_c_pem_objs = ["test/buildtest_c_pem-bin-buildtest_pem.o"]
test_buildtest_c_pem2_objs = ["test/buildtest_c_pem2-bin-buildtest_pem2.o"]
test_buildtest_c_pkcs12_objs = ["test/buildtest_c_pkcs12-bin-buildtest_pkcs12.o"]
test_buildtest_c_pkcs7_objs = ["test/buildtest_c_pkcs7-bin-buildtest_pkcs7.o"]
test_buildtest_c_provider_objs = ["test/buildtest_c_provider-bin-buildtest_provider.o"]
test_buildtest_c_rand_objs = ["test/buildtest_c_rand-bin-buildtest_rand.o"]
test_buildtest_c_rand_drbg_objs = ["test/buildtest_c_rand_drbg-bin-buildtest_rand_drbg.o"]
test_buildtest_c_rc2_objs = ["test/buildtest_c_rc2-bin-buildtest_rc2.o"]
test_buildtest_c_rc4_objs = ["test/buildtest_c_rc4-bin-buildtest_rc4.o"]
test_buildtest_c_ripemd_objs = ["test/buildtest_c_ripemd-bin-buildtest_ripemd.o"]
test_buildtest_c_rsa_objs = ["test/buildtest_c_rsa-bin-buildtest_rsa.o"]
test_buildtest_c_safestack_objs = ["test/buildtest_c_safestack-bin-buildtest_safestack.o"]
test_buildtest_c_seed_objs = ["test/buildtest_c_seed-bin-buildtest_seed.o"]
test_buildtest_c_sha_objs = ["test/buildtest_c_sha-bin-buildtest_sha.o"]
test_buildtest_c_srp_objs = ["test/buildtest_c_srp-bin-buildtest_srp.o"]
test_buildtest_c_srtp_objs = ["test/buildtest_c_srtp-bin-buildtest_srtp.o"]
test_buildtest_c_ssl_objs = ["test/buildtest_c_ssl-bin-buildtest_ssl.o"]
test_buildtest_c_ssl2_objs = ["test/buildtest_c_ssl2-bin-buildtest_ssl2.o"]
test_buildtest_c_stack_objs = ["test/buildtest_c_stack-bin-buildtest_stack.o"]
test_buildtest_c_store_objs = ["test/buildtest_c_store-bin-buildtest_store.o"]
test_buildtest_c_symhacks_objs = ["test/buildtest_c_symhacks-bin-buildtest_symhacks.o"]
test_buildtest_c_tls1_objs = ["test/buildtest_c_tls1-bin-buildtest_tls1.o"]
test_buildtest_c_ts_objs = ["test/buildtest_c_ts-bin-buildtest_ts.o"]
test_buildtest_c_txt_db_objs = ["test/buildtest_c_txt_db-bin-buildtest_txt_db.o"]
test_buildtest_c_ui_objs = ["test/buildtest_c_ui-bin-buildtest_ui.o"]
test_buildtest_c_whrlpool_objs = ["test/buildtest_c_whrlpool-bin-buildtest_whrlpool.o"]
test_buildtest_c_x509_objs = ["test/buildtest_c_x509-bin-buildtest_x509.o"]
test_buildtest_c_x509_vfy_objs = ["test/buildtest_c_x509_vfy-bin-buildtest_x509_vfy.o"]
test_buildtest_c_x509v3_objs = ["test/buildtest_c_x509v3-bin-buildtest_x509v3.o"]
test_casttest_objs = ["test/casttest-bin-casttest.o", "test/libtestutil.a"] 
test_chacha_internal_test_objs = ["test/chacha_internal_test-bin-chacha_internal_test.o", "test/libtestutil.a", "libcrypto.a"]
test_cipherbytes_test_objs = ["test/cipherbytes_test-bin-cipherbytes_test.o", "test/libtestutil.a"]
test_cipherlist_test_objs = ["test/cipherlist_test-bin-cipherlist_test.o", "test/libtestutil.a"]
test_ciphername_test_objs = ["test/ciphername_test-bin-ciphername_test.o", "test/libtestutil.a"]
test_clienthellotest_objs = ["test/clienthellotest-bin-clienthellotest.o", "test/libtestutil.a"]
test_cmsapitest_objs = ["test/cmsapitest-bin-cmsapitest.o", "test/libtestutil.a"]
test_conf_include_test_objs = ["test/conf_include_test-bin-conf_include_test.o", "test/libtestutil.a"]
test_constant_time_test_objs = ["test/constant_time_test-bin-constant_time_test.o", "test/libtestutil.a"]
test_context_internal_test_objs = ["test/context_internal_test-bin-context_internal_test.o", "test/libtestutil.a", "libcrypto.a"]
test_crltest_objs = ["test/crltest-bin-crltest.o", "test/libtestutil.a"]
test_ct_test_objs = ["test/ct_test-bin-ct_test.o", "test/libtestutil.a"]
test_ctype_internal_test_objs = ["test/ctype_internal_test-bin-ctype_internal_test.o", "test/libtestutil.a", "libcrypto.a"]
test_curve448_internal_test_objs = ["test/curve448_internal_test-bin-curve448_internal_test.o", "test/libtestutil.a", "libcrypto.a"]
test_d2i_test_objs = ["test/d2i_test-bin-d2i_test.o", "test/libtestutil.a"]
test_danetest_objs = ["test/danetest-bin-danetest.o", "test/libtestutil.a"]
test_destest_objs = ["test/destest-bin-destest.o", "test/libtestutil.a"]
test_dhtest_objs = ["test/dhtest-bin-dhtest.o", "test/libtestutil.a"]
test_drbg_cavs_test_objs = ["test/drbg_cavs_test-bin-drbg_cavs_data_ctr.o", "test/drbg_cavs_test-bin-drbg_cavs_data_hash.o", "test/drbg_cavs_test-bin-drbg_cavs_data_hmac.o", "test/drbg_cavs_test-bin-drbg_cavs_test.o", "test/libtestutil.a"]
test_drbgtest_objs = ["test/drbgtest-bin-drbgtest.o", "test/libtestutil.a", "libcrypto.a"]
test_dsa_no_digest_size_test_objs = ["test/dsa_no_digest_size_test-bin-dsa_no_digest_size_test.o", "test/libtestutil.a"]
test_dsatest_objs = ["test/dsatest-bin-dsatest.o", "test/libtestutil.a"]
test_dtls_mtu_test_objs = ["test/dtls_mtu_test-bin-dtls_mtu_test.o", "test/dtls_mtu_test-bin-ssltestlib.o", "test/libtestutil.a"]
test_dtlstest_objs = ["test/dtlstest-bin-dtlstest.o", "test/dtlstest-bin-ssltestlib.o", "test/libtestutil.a"]
test_dtlsv1listentest_objs = ["test/dtlsv1listentest-bin-dtlsv1listentest.o", "test/libtestutil.a"]
test_ec_internal_test_objs = ["test/ec_internal_test-bin-ec_internal_test.o", "test/libtestutil.a", "libcrypto.a"]
test_ecdsatest_objs = ["test/ecdsatest-bin-ecdsatest.o", "test/libtestutil.a"]
test_ecstresstest_objs = ["test/ecstresstest-bin-ecstresstest.o", "test/libtestutil.a"]
test_ectest_objs = ["test/ectest-bin-ectest.o", "test/libtestutil.a"]
test_enginetest_objs = ["test/enginetest-bin-enginetest.o", "test/libtestutil.a"]
test_errtest_objs = ["test/errtest-bin-errtest.o", "test/libtestutil.a"]
test_evp_extra_test_objs = ["test/evp_extra_test-bin-evp_extra_test.o", "test/libtestutil.a"]
test_evp_kdf_test_objs = ["test/evp_kdf_test-bin-evp_kdf_test.o", "test/libtestutil.a"]
test_evp_pkey_dparams_test_objs = ["test/evp_pkey_dparams_test-bin-evp_pkey_dparams_test.o", "test/libtestutil.a"]
test_evp_test_objs = ["test/evp_test-bin-evp_test.o", "test/libtestutil.a"]
test_exdatatest_objs = ["test/exdatatest-bin-exdatatest.o", "test/libtestutil.a"]
test_exptest_objs = ["test/exptest-bin-exptest.o", "test/libtestutil.a"]
test_fatalerrtest_objs = ["test/fatalerrtest-bin-fatalerrtest.o", "test/fatalerrtest-bin-ssltestlib.o", "test/libtestutil.a"]
test_gmdifftest_objs = ["test/gmdifftest-bin-gmdifftest.o", "test/libtestutil.a"]
test_gosttest_objs = ["test/gosttest-bin-gosttest.o", "test/gosttest-bin-ssltestlib.o", "test/libtestutil.a"] -- -I.
test_hmactest_objs = ["test/hmactest-bin-hmactest.o", "test/libtestutil.a"]
test_ideatest_objs = ["test/ideatest-bin-ideatest.o", "test/libtestutil.a"]
test_igetest_objs = ["test/igetest-bin-igetest.o", "test/libtestutil.a"]
test_lhash_test_objs = ["test/lhash_test-bin-lhash_test.o", "test/libtestutil.a"]
test_md2test_objs = ["test/md2test-bin-md2test.o", "test/libtestutil.a"]
test_mdc2_internal_test_objs = ["test/mdc2_internal_test-bin-mdc2_internal_test.o", "test/libtestutil.a"]
test_mdc2test_objs = ["test/mdc2test-bin-mdc2test.o", "test/libtestutil.a"]
test_memleaktest_objs = ["test/memleaktest-bin-memleaktest.o", "test/libtestutil.a"]
test_modes_internal_test_objs = ["test/modes_internal_test-bin-modes_internal_test.o", "test/libtestutil.a", "libcrypto.a"]
test_namemap_internal_test_objs = ["test/namemap_internal_test-bin-namemap_internal_test.o", "test/libtestutil.a", "libcrypto.a"]
test_ocspapitest_objs = ["test/ocspapitest-bin-ocspapitest.o", "test/libtestutil.a"]
test_packettest_objs = ["test/packettest-bin-packettest.o", "test/libtestutil.a"]
test_param_build_test_objs = ["test/param_build_test-bin-param_build_test.o", "test/libtestutil.a", "libcrypto.a"]
test_params_api_test_objs = ["test/params_api_test-bin-params_api_test.o", "test/libtestutil.a"]
test_params_conversion_test_objs = ["test/params_conversion_test-bin-params_conversion_test.o", "test/libtestutil.a"]
test_params_test_objs = ["test/params_test-bin-params_test.o", "test/libtestutil.a", "libcrypto.a"]
test_pbelutest_objs = ["test/pbelutest-bin-pbelutest.o", "test/libtestutil.a"]
test_pemtest_objs = ["test/pemtest-bin-pemtest.o", "test/libtestutil.a"]
test_pkey_meth_kdf_test_objs = ["test/pkey_meth_kdf_test-bin-pkey_meth_kdf_test.o", "test/libtestutil.a"]
test_pkey_meth_test_objs = ["test/pkey_meth_test-bin-pkey_meth_test.o", "test/libtestutil.a"]
test_poly1305_internal_test_objs = ["test/poly1305_internal_test-bin-poly1305_internal_test.o", "test/libtestutil.a", "libcrypto.a"]
test_property_test_objs = ["test/property_test-bin-property_test.o", "test/libtestutil.a", "libcrypto.a"]
test_provider_internal_test_objs = ["test/provider_internal_test-bin-p_test.o", "test/provider_internal_test-bin-provider_internal_test.o", "test/libtestutil.a","libcrypto.a"]
test_provider_test_objs = ["test/provider_test-bin-p_test.o", "test/provider_test-bin-provider_test.o", "test/libtestutil.a", "libcrypto.a"]
test_rc2test_objs = ["test/rc2test-bin-rc2test.o", "test/libtestutil.a"]
test_rc4test_objs = ["test/rc4test-bin-rc4test.o", "test/libtestutil.a"]
test_rc5test_objs = ["test/rc5test-bin-rc5test.o", "test/libtestutil.a"]
test_rdrand_sanitytest_objs = ["test/rdrand_sanitytest-bin-rdrand_sanitytest.o", "test/libtestutil.a", "libcrypto.a"]
test_recordlentest_objs = ["test/recordlentest-bin-recordlentest.o", "test/recordlentest-bin-ssltestlib.o", "test/libtestutil.a"]
test_rsa_complex_objs = ["test/rsa_complex-bin-rsa_complex.o"]
test_rsa_mp_test_objs = ["test/rsa_mp_test-bin-rsa_mp_test.o", "test/libtestutil.a"]
test_rsa_sp800_56b_test_objs = ["test/rsa_sp800_56b_test-bin-rsa_sp800_56b_test.o", "test/libtestutil.a", "libcrypto.a"]
test_rsa_test_objs = ["test/rsa_test-bin-rsa_test.o", "test/libtestutil.a"]
test_sanitytest_objs = ["test/sanitytest-bin-sanitytest.o", "test/libtestutil.a"]
test_secmemtest_objs = ["test/secmemtest-bin-secmemtest.o", "test/libtestutil.a"]
test_servername_test_objs = ["test/servername_test-bin-servername_test.o", "test/servername_test-bin-ssltestlib.o", "test/libtestutil.a"]
test_shlibloadtest_objs = ["test/shlibloadtest-bin-shlibloadtest.o"]
test_siphash_internal_test_objs = ["test/siphash_internal_test-bin-siphash_internal_test.o", "test/libtestutil.a", "libcrypto.a"]
test_sm2_internal_test_objs = ["test/sm2_internal_test-bin-sm2_internal_test.o", "test/libtestutil.a", "libcrypto.a"]
test_sm4_internal_test_objs = ["test/sm4_internal_test-bin-sm4_internal_test.o", "test/libtestutil.a", "libcrypto.a"]
test_sparse_array_test_objs = ["test/sparse_array_test-bin-sparse_array_test.o", "test/libtestutil.a", "libcrypto.a"]
test_srptest_objs = ["test/srptest-bin-srptest.o", "test/libtestutil.a"]
test_ssl_cert_table_internal_test_objs = ["test/ssl_cert_table_internal_test-bin-ssl_cert_table_internal_test.o", "test/libtestutil.a"]
test_ssl_test_objs = ["test/ssl_test-bin-handshake_helper.o", "test/ssl_test-bin-ssl_test.o", "test/ssl_test-bin-ssl_test_ctx.o", "test/libtestutil.a"]
test_ssl_test_ctx_test_objs = ["test/ssl_test_ctx_test-bin-ssl_test_ctx.o", "test/ssl_test_ctx_test-bin-ssl_test_ctx_test.o", "test/libtestutil.a"]
test_sslapitest_objs = ["test/sslapitest-bin-sslapitest.o", "test/sslapitest-bin-ssltestlib.o", "test/libtestutil.a"]
test_sslbuffertest_objs = ["test/sslbuffertest-bin-sslbuffertest.o", "test/sslbuffertest-bin-ssltestlib.o", "test/libtestutil.a"]
test_sslcorrupttest_objs = ["test/sslcorrupttest-bin-sslcorrupttest.o", "test/sslcorrupttest-bin-ssltestlib.o", "test/libtestutil.a"]
test_ssltest_old_objs = ["test/ssltest_old-bin-ssltest_old.o"]
test_stack_test_objs = ["test/stack_test-bin-stack_test.o", "test/libtestutil.a"]
test_sysdefaulttest_objs = ["test/sysdefaulttest-bin-sysdefaulttest.o", "test/libtestutil.a"]
test_test_test_objs = ["test/test_test-bin-test_test.o", "test/libtestutil.a"]
test_threadstest_objs = ["test/threadstest-bin-threadstest.o", "test/libtestutil.a"]
test_time_offset_test_objs = ["test/time_offset_test-bin-time_offset_test.o", "test/libtestutil.a"]
test_tls13ccstest_objs = ["test/tls13ccstest-bin-ssltestlib.o", "test/tls13ccstest-bin-tls13ccstest.o", "test/libtestutil.a"]
test_tls13encryptiontest_objs = ["test/tls13encryptiontest-bin-tls13encryptiontest.o", "libssl.a", "test/libtestutil.a"]
test_tls13secretstest_objs = ["crypto_tls13secretstest-bin-packet.o", "ssl_tls13secretstest-bin-tls13_enc.o", "test/tls13secretstest-bin-tls13secretstest.o", "test/libtestutil.a"]
test_uitest_objs = ["apps_uitest-bin-apps_ui.o", "test/uitest-bin-uitest.o", "test/libtestutil.a"]
test_v3ext_objs = ["test/v3ext-bin-v3ext.o", "test/libtestutil.a"]
test_v3nametest_objs = ["test/v3nametest-bin-v3nametest.o", "test/libtestutil.a"]
test_verify_extra_test_objs = ["test/verify_extra_test-bin-verify_extra_test.o", "test/libtestutil.a"]
test_versions_objs = ["test/versions-bin-versions.o"]
test_wpackettest_objs = ["test/wpackettest-bin-wpackettest.o", "libssl.a", "test/libtestutil.a"]
test_x509_check_cert_pkey_test_objs = ["test/x509_check_cert_pkey_test-bin-x509_check_cert_pkey_test.o", "test/libtestutil.a"]
test_x509_dup_cert_test_objs = ["test/x509_dup_cert_test-bin-x509_dup_cert_test.o", "test/libtestutil.a"]
test_x509_internal_test_objs = ["test/x509_internal_test-bin-x509_internal_test.o", "test/libtestutil.a", "libcrypto.a"]
test_x509_time_test_objs = ["test/x509_time_test-bin-x509_time_test.o", "test/libtestutil.a"]
test_x509aux_objs = ["test/x509aux-bin-x509aux.o", "test/libtestutil.a"]
test_testutil_objs = ["test/testutil_libtestutil-lib-apps_mem.o", "test/testutil_libtestutil-lib-basic_output.o", "test/testutil_libtestutil-lib-cb.o", "test/testutil_libtestutil-lib-driver.o", "test/testutil_libtestutil-lib-format_output.o", "test/testutil_libtestutil-lib-init.o", "test/testutil_libtestutil-lib-main.o", "test/testutil_libtestutil-lib-options.o", "test/testutil_libtestutil-lib-output_helpers.o", "test/testutil_libtestutil-lib-random.o", "test/testutil_libtestutil-lib-stanza.o", "test/testutil_libtestutil-lib-tap_bio.o", "test/testutil_libtestutil-lib-test_cleanup.o", "test/testutil_libtestutil-lib-test_options.o", "test/testutil_libtestutil-lib-tests.o"]

test_buildtest_c_ebcdic_objs = ["test/buildtest_c_ec-bin-buildtest_ec.o"]
test_buildtest_c_ec_objs = ["test/buildtest_c_ec-bin-buildtest_ec.o"]
test_buildtest_c_crypto_objs = ["test/buildtest_c_crypto-bin-buildtest_crypto.o"]
test_buildtest_c_cmac_objs = ["test/buildtest_c_cmac-bin-buildtest_cmac.o"]
test_buildtest_c_bio_objs = ["test/buildtest_c_bio-bin-buildtest_bio.o"]
test_buildtest_c_async_objs = ["test/buildtest_c_async-bin-buildtest_async.o"]

-- end of test objs
-- use -lssl
programs = [("apps/openssl", apps_openssl_objs ++ ["apps/libapps.a"]), ("fuzz/asn1-test", fuzz_asn1test_objs), ("fuzz/client-test", fuzz_client_test_objs), ("fuzz/server-test", fuzz_server_test_objs), ("test/buildtest_c_aes", test_buildtest_c_aes_objs), ("test/buildtest_c_asn1", test_buildtest_c_asn1_objs), ("test/buildtest_c_asn1t", test_buildtest_c_asn1t_objs), ("test/buildtest_c_async", test_buildtest_c_async_objs), ("test/buildtest_c_bio", test_buildtest_c_bio_objs), ("test/buildtest_c_blowfish", test_buildtest_c_blowfish_objs), ("test/buildtest_c_bn", test_buildtest_c_bn_objs), ("test/buildtest_c_buffer", test_buildtest_c_buffer_objs), ("test/buildtest_c_camellia", test_buildtest_c_camellia_objs), ("test/buildtest_c_cast", test_buildtest_c_cast_objs), ("test/buildtest_c_cmac", test_buildtest_c_cmac_objs), ("test/buildtest_c_cmp", test_buildtest_c_cmp_objs), ("test/buildtest_c_cms", test_buildtest_c_cms_objs), ("test/buildtest_c_comp", test_buildtest_c_comp_objs), ("test/buildtest_c_conf", test_buildtest_c_conf_objs), ("test/buildtest_c_conf_api", test_buildtest_c_conf_api_objs),("test/buildtest_c_core", test_buildtest_c_core_objs), ("test/buildtest_c_core_names", test_buildtest_c_core_names_objs), ("test/buildtest_c_core_numbers", test_buildtest_c_core_numbers_objs), ("test/buildtest_c_crmf", test_buildtest_c_crmf_objs), ("test/buildtest_c_crypto", test_buildtest_c_crypto_objs), ("test/buildtest_c_ct", test_buildtest_c_ct_objs), ("test/buildtest_c_des", test_buildtest_c_des_objs), ("test/buildtest_c_dh", test_buildtest_c_dh_objs), ("test/buildtest_c_dsa", test_buildtest_c_dsa_objs), ("test/buildtest_c_dtls1",test_buildtest_c_dtls1_objs), ("test/buildtest_c_e_os2", test_buildtest_c_e_os2_objs), ("test/buildtest_c_ebcdic",test_buildtest_c_ebcdic_objs), ("test/buildtest_c_ec", test_buildtest_c_ec_objs),("test/buildtest_c_ecdh", test_buildtest_c_ecdh_objs), ("test/buildtest_c_ecdsa",test_buildtest_c_ecdsa_objs),("test/buildtest_c_engine",test_buildtest_c_engine_objs) , ("test/buildtest_c_ess",test_buildtest_c_ess_objs) ,("test/buildtest_c_evp",test_buildtest_c_evp_objs), ("test/buildtest_c_fips_names",test_buildtest_c_fips_names_objs) ,("test/buildtest_c_hmac",test_buildtest_c_hmac_objs) ,("test/buildtest_c_idea",test_buildtest_c_idea_objs)
  ,("test/buildtest_c_kdf",test_buildtest_c_kdf_objs) ,("test/buildtest_c_lhash",test_buildtest_c_lhash_objs) ,("test/buildtest_c_macros",test_buildtest_c_macros_objs)
  ,("test/buildtest_c_md4",test_buildtest_c_md4_objs) ,("test/buildtest_c_md5",test_buildtest_c_md5_objs) ,("test/buildtest_c_mdc2",test_buildtest_c_mdc2_objs)
  ,("test/buildtest_c_modes", test_buildtest_c_modes_objs) ,("test/buildtest_c_obj_mac",test_buildtest_c_obj_mac_objs) ,("test/buildtest_c_objects",test_buildtest_c_objects_objs)
  ,("test/buildtest_c_ocsp",test_buildtest_c_ocsp_objs) ,("test/buildtest_c_opensslv",test_buildtest_c_opensslv_objs) ,("test/buildtest_c_ossl_typ",test_buildtest_c_ossl_typ_objs)
  ,("test/buildtest_c_params",test_buildtest_c_params_objs) ,("test/buildtest_c_pem",test_buildtest_c_pem_objs) ,("test/buildtest_c_pem2",test_buildtest_c_pem2_objs)
  ,("test/buildtest_c_pkcs12",test_buildtest_c_pkcs12_objs) ,("test/buildtest_c_pkcs7",test_buildtest_c_pkcs7_objs) ,("test/buildtest_c_provider",test_buildtest_c_provider_objs)
  ,("test/buildtest_c_rand",test_buildtest_c_rand_objs) ,("test/buildtest_c_rand_drbg",test_buildtest_c_rand_drbg_objs) ,("test/buildtest_c_rc2",test_buildtest_c_rc2_objs)
  ,("test/buildtest_c_rc4",test_buildtest_c_rc4_objs) ,("test/buildtest_c_ripemd",test_buildtest_c_ripemd_objs) ,("test/buildtest_c_rsa",test_buildtest_c_rsa_objs), ("test/buildtest_c_safestack",test_buildtest_c_safestack_objs) ,("test/buildtest_c_seed",test_buildtest_c_seed_objs) ,("test/buildtest_c_sha",test_buildtest_c_sha_objs)
  ,("test/buildtest_c_srp",test_buildtest_c_srp_objs) ,("test/buildtest_c_srtp",test_buildtest_c_srtp_objs) ,("test/buildtest_c_ssl",test_buildtest_c_ssl_objs)
  ,("test/buildtest_c_ssl2",test_buildtest_c_ssl2_objs) ,("test/buildtest_c_stack",test_buildtest_c_stack_objs) ,("test/buildtest_c_store",test_buildtest_c_store_objs)
  ,("test/buildtest_c_symhacks",test_buildtest_c_symhacks_objs) ,("test/buildtest_c_tls1",test_buildtest_c_tls1_objs) ,("test/buildtest_c_ts",test_buildtest_c_ts_objs)
  ,("test/buildtest_c_txt_db",test_buildtest_c_txt_db_objs) ,("test/buildtest_c_ui",test_buildtest_c_ui_objs) ,("test/buildtest_c_whrlpool",test_buildtest_c_whrlpool_objs)
  ,("test/buildtest_c_x509",test_buildtest_c_x509_objs) ,("test/buildtest_c_x509_vfy",test_buildtest_c_x509_vfy_objs) ,("test/buildtest_c_x509v3",test_buildtest_c_x509v3_objs), ("test/ssltest_old",test_ssltest_old_objs)]

-- do not use -lssl
programs2 =
  [ ("fuzz/asn1parse-test", fuzz_asn1parse_test_objs), ("fuzz/bignum-test", fuzz_bignum_test_objs)
  ,("fuzz/bndiv-test", fuzz_bndiv_test_objs),  ("fuzz/cms-test", fuzz_cms_test_objs), ("fuzz/conf-test", fuzz_conf_test_objs)
  ,("fuzz/crl-test", fuzz_crl_test_objs), ("fuzz/ct-test", fuzz_ct_test_objs),  ("fuzz/x509-test", fuzz_x509_test_objs), ("test/aborttest", test_aborttest_objs)
  ,("test/aesgcmtest", test_aesgcmtest_objs), ("test/afalgtest", test_afalgtest_objs), ("test/asn1_decode_test", test_asn1_decode_test_objs)
  ,("test/asn1_dsa_internal_test", test_asn1_dsa_internal_test_objs), ("test/asn1_encode_test", test_asn1_encode_test_objs), ("test/asn1_internal_test", test_asn1_internal_test_objs)
  ,("test/asn1_string_table_test", test_asn1_string_table_test_objs), ("test/asn1_time_test", test_asn1_time_test_objs) 
  ,("test/asynctest", test_asynctest_objs),  ("test/bftest", test_bftest_objs), ("test/bio_callback_test", test_bio_callback_test_objs)
  ,("test/bio_enc_test", test_bio_enc_test_objs), ("test/bio_memleak_test", test_bio_memleak_test_objs), ("test/bioprinttest", test_bioprinttest_objs)
  ,("test/bn_internal_test", test_bn_internal_test_objs), ("test/bntest", test_bntest_objs)
  ,("test/casttest",test_casttest_objs) ,("test/chacha_internal_test",test_chacha_internal_test_objs),("test/cmsapitest",test_cmsapitest_objs)
  ,("test/conf_include_test",test_conf_include_test_objs) ,("test/constant_time_test",test_constant_time_test_objs) ,("test/context_internal_test",test_context_internal_test_objs)
  ,("test/crltest",test_crltest_objs) ,("test/ct_test",test_ct_test_objs) ,("test/ctype_internal_test",test_ctype_internal_test_objs) ,("test/curve448_internal_test",test_curve448_internal_test_objs),("test/d2i_test",test_d2i_test_objs), ("test/destest",test_destest_objs) ,("test/dhtest",test_dhtest_objs) ,("test/drbg_cavs_test",test_drbg_cavs_test_objs)
  ,("test/drbgtest",test_drbgtest_objs) ,("test/dsa_no_digest_size_test",test_dsa_no_digest_size_test_objs) ,("test/dsatest",test_dsatest_objs), ("test/ec_internal_test",test_ec_internal_test_objs) ,("test/ecdsatest",test_ecdsatest_objs)
  ,("test/ecstresstest",test_ecstresstest_objs) ,("test/ectest",test_ectest_objs) ,("test/enginetest",test_enginetest_objs) ,("test/errtest",test_errtest_objs)
  ,("test/evp_extra_test",test_evp_extra_test_objs) ,("test/evp_kdf_test",test_evp_kdf_test_objs) ,("test/evp_pkey_dparams_test",test_evp_pkey_dparams_test_objs) ,("test/evp_test",test_evp_test_objs),("test/exdatatest",test_exdatatest_objs) ,("test/exptest",test_exptest_objs),("test/gmdifftest",test_gmdifftest_objs)
 
  ,("test/hmactest",test_hmactest_objs) ,("test/ideatest",test_ideatest_objs) ,("test/igetest",test_igetest_objs) ,("test/lhash_test",test_lhash_test_objs)
  ,("test/md2test",test_md2test_objs) ,("test/mdc2_internal_test",test_mdc2_internal_test_objs) ,("test/mdc2test",test_mdc2test_objs) ,("test/memleaktest",test_memleaktest_objs)
  ,("test/modes_internal_test",test_modes_internal_test_objs) ,("test/namemap_internal_test",test_namemap_internal_test_objs) ,("test/ocspapitest",test_ocspapitest_objs)
  ,("test/packettest",test_packettest_objs) ,("test/param_build_test",test_param_build_test_objs) ,("test/params_api_test",test_params_api_test_objs)
  ,("test/params_conversion_test",test_params_conversion_test_objs) ,("test/params_test",test_params_test_objs) ,("test/pbelutest",test_pbelutest_objs) ,("test/pemtest",test_pemtest_objs)
  ,("test/pkey_meth_kdf_test",test_pkey_meth_kdf_test_objs) ,("test/pkey_meth_test",test_pkey_meth_test_objs) ,("test/poly1305_internal_test",test_poly1305_internal_test_objs)
  ,("test/property_test",test_property_test_objs) ,("test/provider_internal_test",test_provider_internal_test_objs) ,("test/provider_test",test_provider_test_objs) ,("test/rc2test",test_rc2test_objs)
  ,("test/rc4test",test_rc4test_objs) ,("test/rc5test",test_rc5test_objs) ,("test/rdrand_sanitytest",test_rdrand_sanitytest_objs)
  ,("test/rsa_complex",test_rsa_complex_objs) ,("test/rsa_mp_test",test_rsa_mp_test_objs) ,("test/rsa_sp800_56b_test",test_rsa_sp800_56b_test_objs) ,("test/rsa_test",test_rsa_test_objs)
  ,("test/sanitytest",test_sanitytest_objs) ,("test/secmemtest",test_secmemtest_objs),("test/shlibloadtest",test_shlibloadtest_objs)
  ,("test/siphash_internal_test",test_siphash_internal_test_objs) ,("test/sm2_internal_test",test_sm2_internal_test_objs) ,("test/sm4_internal_test",test_sm4_internal_test_objs)
  ,("test/sparse_array_test",test_sparse_array_test_objs) ,("test/srptest",test_srptest_objs) ,("test/ssl_cert_table_internal_test",test_ssl_cert_table_internal_test_objs)
  ,("test/stack_test",test_stack_test_objs)
  ,("test/test_test",test_test_test_objs) ,("test/threadstest",test_threadstest_objs) ,("test/time_offset_test",test_time_offset_test_objs)
  ,("test/tls13encryptiontest",test_tls13encryptiontest_objs),("test/v3ext",test_v3ext_objs)
  ,("test/v3nametest",test_v3nametest_objs) ,("test/verify_extra_test",test_verify_extra_test_objs) ,("test/versions",test_versions_objs) ,("test/wpackettest",test_wpackettest_objs)
  ,("test/x509_check_cert_pkey_test",test_x509_check_cert_pkey_test_objs) ,("test/x509_dup_cert_test",test_x509_dup_cert_test_objs) ,("test/x509_internal_test",test_x509_internal_test_objs)
  ,("test/x509_time_test",test_x509_time_test_objs) ,("test/x509aux",test_x509aux_objs)]

-- for ones that have -lssl test/libtestutil.a REALALY
programs3 = [("test/asynciotest", test_asynciotest_objs ), ("test/bad_dtls_test", test_bad_dtls_test_objs), ("test/cipherbytes_test",test_cipherbytes_test_objs)
  ,("test/cipherlist_test",test_cipherlist_test_objs) ,("test/ciphername_test",test_ciphername_test_objs) ,("test/clienthellotest",test_clienthellotest_objs) , ("test/danetest",test_danetest_objs),("test/dtls_mtu_test",test_dtls_mtu_test_objs)
  ,("test/dtlstest",test_dtlstest_objs) ,("test/dtlsv1listentest",test_dtlsv1listentest_objs) , ("test/fatalerrtest",test_fatalerrtest_objs) , ("test/gosttest",test_gosttest_objs) , ("test/recordlentest",test_recordlentest_objs), ("test/servername_test",test_servername_test_objs), ("test/ssl_test",test_ssl_test_objs), ("test/ssl_test_ctx_test",test_ssl_test_ctx_test_objs), ("test/sslapitest",test_sslapitest_objs) , ("test/sslbuffertest",test_sslbuffertest_objs), ("test/sslcorrupttest",test_sslcorrupttest_objs), ("test/sysdefaulttest",test_sysdefaulttest_objs), ("test/tls13ccstest",test_tls13ccstest_objs), ("test/tls13secretstest",test_tls13secretstest_objs), ("test/uitest",test_uitest_objs)]

libssl_objs = ["crypto/libssl-lib-packet.o", "ssl/libssl-lib-bio_ssl.o", "ssl/libssl-lib-d1_lib.o", "ssl/libssl-lib-d1_msg.o", "ssl/libssl-lib-d1_srtp.o", "ssl/libssl-lib-methods.o", "ssl/libssl-lib-pqueue.o", "ssl/libssl-lib-s3_cbc.o", "ssl/libssl-lib-s3_enc.o", "ssl/libssl-lib-s3_lib.o", "ssl/libssl-lib-s3_msg.o", "ssl/libssl-lib-ssl_asn1.o", "ssl/libssl-lib-ssl_cert.o", "ssl/libssl-lib-ssl_ciph.o", "ssl/libssl-lib-ssl_conf.o", "ssl/libssl-lib-ssl_err.o", "ssl/libssl-lib-ssl_init.o", "ssl/libssl-lib-ssl_lib.o", "ssl/libssl-lib-ssl_mcnf.o", "ssl/libssl-lib-ssl_rsa.o", "ssl/libssl-lib-ssl_sess.o", "ssl/libssl-lib-ssl_stat.o", "ssl/libssl-lib-ssl_txt.o", "ssl/libssl-lib-ssl_utst.o", "ssl/libssl-lib-t1_enc.o", "ssl/libssl-lib-t1_lib.o", "ssl/libssl-lib-t1_trce.o", "ssl/libssl-lib-tls13_enc.o", "ssl/libssl-lib-tls_srp.o", "ssl/record/libssl-lib-dtls1_bitmap.o", "ssl/record/libssl-lib-rec_layer_d1.o", "ssl/record/libssl-lib-rec_layer_s3.o", "ssl/record/libssl-lib-ssl3_buffer.o", "ssl/record/libssl-lib-ssl3_record.o", "ssl/record/libssl-lib-ssl3_record_tls13.o", "ssl/statem/libssl-lib-extensions.o", "ssl/statem/libssl-lib-extensions_clnt.o", "ssl/statem/libssl-lib-extensions_cust.o", "ssl/statem/libssl-lib-extensions_srvr.o", "ssl/statem/libssl-lib-statem.o", "ssl/statem/libssl-lib-statem_clnt.o", "ssl/statem/libssl-lib-statem_dtls.o", "ssl/statem/libssl-lib-statem_lib.o", "ssl/statem/libssl-lib-statem_srvr.o"]

libtestutil_objs = ["apps/lib/libtestutil-lib-bf_prefix.o", "apps/lib/libtestutil-lib-opt.o", "test/testutil/libtestutil-lib-apps_mem.o", "test/testutil/libtestutil-lib-basic_output.o", "test/testutil/libtestutil-lib-cb.o", "test/testutil/libtestutil-lib-driver.o", "test/testutil/libtestutil-lib-format_output.o", "test/testutil/libtestutil-lib-init.o", "test/testutil/libtestutil-lib-main.o", "test/testutil/libtestutil-lib-options.o", "test/testutil/libtestutil-lib-output_helpers.o", "test/testutil/libtestutil-lib-random.o", "test/testutil/libtestutil-lib-stanza.o", "test/testutil/libtestutil-lib-tap_bio.o", "test/testutil/libtestutil-lib-test_cleanup.o", "test/testutil/libtestutil-lib-test_options.o", "test/testutil/libtestutil-lib-tests.o"]

libsslso_objs = ["crypto/libssl-shlib-packet.o", "ssl/libssl-shlib-bio_ssl.o", "ssl/libssl-shlib-d1_lib.o", "ssl/libssl-shlib-d1_msg.o", "ssl/libssl-shlib-d1_srtp.o", "ssl/libssl-shlib-methods.o", "ssl/libssl-shlib-pqueue.o", "ssl/libssl-shlib-s3_cbc.o", "ssl/libssl-shlib-s3_enc.o", "ssl/libssl-shlib-s3_lib.o", "ssl/libssl-shlib-s3_msg.o", "ssl/libssl-shlib-ssl_asn1.o", "ssl/libssl-shlib-ssl_cert.o", "ssl/libssl-shlib-ssl_ciph.o", "ssl/libssl-shlib-ssl_conf.o", "ssl/libssl-shlib-ssl_err.o", "ssl/libssl-shlib-ssl_init.o", "ssl/libssl-shlib-ssl_lib.o", "ssl/libssl-shlib-ssl_mcnf.o", "ssl/libssl-shlib-ssl_rsa.o", "ssl/libssl-shlib-ssl_sess.o", "ssl/libssl-shlib-ssl_stat.o", "ssl/libssl-shlib-ssl_txt.o", "ssl/libssl-shlib-ssl_utst.o", "ssl/libssl-shlib-t1_enc.o", "ssl/libssl-shlib-t1_lib.o", "ssl/libssl-shlib-t1_trce.o", "ssl/libssl-shlib-tls13_enc.o", "ssl/libssl-shlib-tls_srp.o", "ssl/record/libssl-shlib-dtls1_bitmap.o", "ssl/record/libssl-shlib-rec_layer_d1.o", "ssl/record/libssl-shlib-rec_layer_s3.o", "ssl/record/libssl-shlib-ssl3_buffer.o", "ssl/record/libssl-shlib-ssl3_record.o", "ssl/record/libssl-shlib-ssl3_record_tls13.o", "ssl/statem/libssl-shlib-extensions.o", "ssl/statem/libssl-shlib-extensions_clnt.o", "ssl/statem/libssl-shlib-extensions_cust.o", "ssl/statem/libssl-shlib-extensions_srvr.o", "ssl/statem/libssl-shlib-statem.o", "ssl/statem/libssl-shlib-statem_clnt.o", "ssl/statem/libssl-shlib-statem_dtls.o", "ssl/statem/libssl-shlib-statem_lib.o", "ssl/statem/libssl-shlib-statem_srvr.o"]

fips_dso_objs = ["crypto/modes/fips-dso-wrap128.o", "crypto/fips-dso-provider_core.o", "crypto/aes/fips-dso-aes_ecb.o", "crypto/aes/fips-dso-aes_misc.o", "crypto/bn/asm/fips-dso-x86_64-gcc.o", "crypto/bn/fips-dso-bn_add.o", "crypto/bn/fips-dso-bn_blind.o", "crypto/bn/fips-dso-bn_const.o", "crypto/bn/fips-dso-bn_conv.o", "crypto/bn/fips-dso-bn_ctx.o", "crypto/bn/fips-dso-bn_dh.o", "crypto/bn/fips-dso-bn_div.o", "crypto/bn/fips-dso-bn_exp.o", "crypto/bn/fips-dso-bn_exp2.o", "crypto/bn/fips-dso-bn_gcd.o", "crypto/bn/fips-dso-bn_gf2m.o", "crypto/bn/fips-dso-bn_intern.o", "crypto/bn/fips-dso-bn_kron.o", "crypto/bn/fips-dso-bn_lib.o", "crypto/bn/fips-dso-bn_mod.o", "crypto/bn/fips-dso-bn_mont.o", "crypto/bn/fips-dso-bn_mpi.o", "crypto/bn/fips-dso-bn_mul.o", "crypto/bn/fips-dso-bn_nist.o", "crypto/bn/fips-dso-bn_prime.o", "crypto/bn/fips-dso-bn_rand.o", "crypto/bn/fips-dso-bn_recp.o", "crypto/bn/fips-dso-bn_rsa_fips186_4.o", "crypto/bn/fips-dso-bn_shift.o", "crypto/bn/fips-dso-bn_sqr.o", "crypto/bn/fips-dso-bn_sqrt.o", "crypto/bn/fips-dso-bn_word.o", "crypto/bn/fips-dso-bn_x931p.o", "crypto/bn/fips-dso-rsaz_exp.o", "crypto/buffer/fips-dso-buffer.o", "crypto/cmac/fips-dso-cmac.o", "crypto/des/fips-dso-des_enc.o", "crypto/des/fips-dso-ecb3_enc.o", "crypto/des/fips-dso-fcrypt_b.o", "crypto/des/fips-dso-set_key.o", "crypto/ec/fips-dso-curve25519.o", "crypto/ec/fips-dso-ec2_oct.o", "crypto/ec/fips-dso-ec2_smpl.o", "crypto/ec/fips-dso-ec_asn1.o", "crypto/ec/fips-dso-ec_check.o", "crypto/ec/fips-dso-ec_curve.o", "crypto/ec/fips-dso-ec_cvt.o", "crypto/ec/fips-dso-ec_key.o", "crypto/ec/fips-dso-ec_kmeth.o", "crypto/ec/fips-dso-ec_lib.o", "crypto/ec/fips-dso-ec_mult.o", "crypto/ec/fips-dso-ec_oct.o", "crypto/ec/fips-dso-ec_print.o", "crypto/ec/fips-dso-ecdh_ossl.o", "crypto/ec/fips-dso-ecdsa_ossl.o", "crypto/ec/fips-dso-ecdsa_sign.o", "crypto/ec/fips-dso-ecdsa_vrf.o", "crypto/ec/fips-dso-ecp_mont.o", "crypto/ec/fips-dso-ecp_nist.o", "crypto/ec/fips-dso-ecp_nistp224.o", "crypto/ec/fips-dso-ecp_nistp256.o", "crypto/ec/fips-dso-ecp_nistp521.o", "crypto/ec/fips-dso-ecp_nistputil.o", "crypto/ec/fips-dso-ecp_nistz256.o", "crypto/ec/fips-dso-ecp_oct.o", "crypto/ec/fips-dso-ecp_smpl.o", "crypto/evp/fips-dso-cmeth_lib.o", "crypto/evp/fips-dso-digest.o", "crypto/evp/fips-dso-evp_enc.o", "crypto/evp/fips-dso-evp_fetch.o", "crypto/evp/fips-dso-evp_lib.o", "crypto/evp/fips-dso-evp_utils.o", "crypto/evp/fips-dso-kdf_lib.o", "crypto/evp/fips-dso-kdf_meth.o", "crypto/evp/fips-dso-keymgmt_lib.o", "crypto/evp/fips-dso-keymgmt_meth.o", "crypto/evp/fips-dso-mac_lib.o", "crypto/evp/fips-dso-mac_meth.o", "crypto/fips-dso-asn1_dsa.o", "crypto/fips-dso-bsearch.o", "crypto/fips-dso-context.o", "crypto/fips-dso-core_algorithm.o", "crypto/fips-dso-core_fetch.o", "crypto/fips-dso-core_namemap.o", "crypto/fips-dso-cryptlib.o", "crypto/fips-dso-ctype.o", "crypto/fips-dso-ex_data.o", "crypto/fips-dso-initthread.o", "crypto/fips-dso-o_str.o", "crypto/fips-dso-packet.o", "crypto/fips-dso-param_build.o", "crypto/fips-dso-params.o", "crypto/fips-dso-params_from_text.o", "crypto/fips-dso-provider_predefined.o", "crypto/fips-dso-sparse_array.o", "crypto/fips-dso-threads_none.o", "crypto/fips-dso-threads_pthread.o", "crypto/fips-dso-threads_win.o", "crypto/hmac/fips-dso-hmac.o", "crypto/lhash/fips-dso-lhash.o", "crypto/modes/fips-dso-cbc128.o", "crypto/modes/fips-dso-ccm128.o", "crypto/modes/fips-dso-cfb128.o", "crypto/modes/fips-dso-ctr128.o", "crypto/modes/fips-dso-gcm128.o", "crypto/modes/fips-dso-ofb128.o", "crypto/modes/fips-dso-xts128.o", "crypto/property/fips-dso-defn_cache.o", "crypto/property/fips-dso-property.o", "crypto/property/fips-dso-property_parse.o", "crypto/property/fips-dso-property_string.o", "crypto/rand/fips-dso-drbg_ctr.o", "crypto/rand/fips-dso-drbg_hash.o", "crypto/rand/fips-dso-drbg_hmac.o", "crypto/rand/fips-dso-drbg_lib.o", "crypto/rand/fips-dso-rand_crng_test.o", "crypto/rand/fips-dso-rand_lib.o", "crypto/rand/fips-dso-rand_unix.o", "crypto/rand/fips-dso-rand_vms.o", "crypto/rand/fips-dso-rand_vxworks.o", "crypto/rand/fips-dso-rand_win.o", "crypto/sha/fips-dso-sha1dgst.o", "crypto/sha/fips-dso-sha256.o", "crypto/sha/fips-dso-sha3.o", "crypto/sha/fips-dso-sha512.o", "crypto/stack/fips-dso-stack.o", "providers/common/ciphers/fips-dso-block.o", "providers/common/ciphers/fips-dso-cipher_aes.o", "providers/common/ciphers/fips-dso-cipher_aes_ccm.o", "providers/common/ciphers/fips-dso-cipher_aes_ccm_hw.o", "providers/common/ciphers/fips-dso-cipher_aes_gcm.o", "providers/common/ciphers/fips-dso-cipher_aes_gcm_hw.o", "providers/common/ciphers/fips-dso-cipher_aes_hw.o", "providers/common/ciphers/fips-dso-cipher_aes_xts.o", "providers/common/ciphers/fips-dso-cipher_aes_xts_hw.o", "providers/common/ciphers/fips-dso-cipher_aes_wrp.o", "providers/common/ciphers/fips-dso-cipher_ccm.o", "providers/common/ciphers/fips-dso-cipher_ccm_hw.o", "providers/common/ciphers/fips-dso-cipher_common.o", "providers/common/ciphers/fips-dso-cipher_common_hw.o", "providers/common/ciphers/fips-dso-cipher_gcm.o", "providers/common/ciphers/fips-dso-cipher_gcm_hw.o", "providers/common/ciphers/fips-dso-cipher_tdes.o", "providers/common/ciphers/fips-dso-cipher_tdes_hw.o", "providers/common/digests/fips-dso-digest_common.o", "providers/common/digests/fips-dso-sha2_prov.o", "providers/common/digests/fips-dso-sha3_prov.o", "providers/common/fips-dso-provider_util.o", "providers/common/kdfs/fips-dso-hkdf.o", "providers/common/kdfs/fips-dso-pbkdf2.o", "providers/common/kdfs/fips-dso-sskdf.o", "providers/common/kdfs/fips-dso-tls1_prf.o", "providers/common/macs/fips-dso-cmac_prov.o", "providers/common/macs/fips-dso-gmac_prov.o", "providers/common/macs/fips-dso-hmac_prov.o", "providers/common/macs/fips-dso-kmac_prov.o", "providers/fips/fips-dso-fipsprov.o", "providers/fips/fips-dso-selftest.o"]
