;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2020-2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020, 2021 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2020 Tim Howes <timhowes@lavabit.com>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021, 2022 Jean-Baptiste Volatier <jbv@pm.me>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Akira Kyle <akira@akirakyle.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu packages julia)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix search-paths) #:select ($SSL_CERT_FILE))
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision) ; mpfr
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (ice-9 match))

(define libunwind-julia
  ;; The Julia projects requires their patched version.
  ;; Get from https://github.com/JuliaLang/julia/tree/master/deps/patches
  (package
    (inherit libunwind)
    (name "libunwind-julia")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/libunwind/libunwind-"
                           version ".tar.gz"))
       (sha256
        (base32
         "05qhzcg1xag3l5m3c805np6k342gc0f3g087b7g16jidv59pccwh"))
       (patches
         (list
           (julia-patch "libunwind-prefer-extbl"
                        "0pf3lsq6zxlmqn86lk4fcj1xwdan9gbxyabrwgxcb59p8jjwsl8r")
           (julia-patch "libunwind-static-arm"
                        "1jk3bmiw61ypcchqkk1fyg5wh8wpggk574wxyfyaic870zh3lhgq")
           (julia-patch "libunwind-cfa-rsp"
                        "0qs5b1h5lsr5qakkv6sddgy5ghlxpjrn2jiqcvg7bkczy24klr6j")))))
    (arguments
     (substitute-keyword-arguments (package-arguments libunwind)
       ;; Skip tests on this older and patched version of libunwind.
       ((#:tests? _ #t) #f)))
    (home-page "https://github.com/JuliaLang/julia/tree/master/deps/")))

(define (julia-patch-url version name)
  (string-append "https://raw.githubusercontent.com/JuliaLang/julia/v" version
                 "/deps/patches/" name ".patch"))

(define-public (julia-patch name sha)
  (let ((version "1.8.2"))
    (origin (method url-fetch)
            (uri (julia-patch-url version name))
            (sha256 (base32 sha))
            (file-name name))))

(define-public libwhich
  (package
    (name "libwhich")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vtjnash/libwhich")
             ;; fixes linux-vdso.so related tests
             (commit "87cffe10080c98e7b5786c5166e420bf1ada1d41")))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "1bpa0fcqpa3ai3hm8mz0p13bf76fsq53wsfcx5qw302zh22108xr"))))
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'check 'set-ld-library-path
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             (setenv "LD_LIBRARY_PATH"
                     (string-append (assoc-ref (or native-inputs inputs) "zlib")
                                    "/lib"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "libwhich" (string-append out "/bin")))
             #t)))))
    (native-inputs
     ;; used for tests
     (list zlib))
    (build-system gnu-build-system)
    (home-page "https://github.com/vtjnash/libwhich")
    (synopsis "Like @code{which}, for dynamic libraries")
    (description "@code{libwhich} is like @code{which}, but for dynamic
libraries.  It is also a bit like @code{ldd} and @code{otool -L}.")
    (license license:expat)))

(define-public julia
  (package
    (name "julia")
    (version "1.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/JuliaLang/julia/releases/download/v" version
             "/julia-" version ".tar.gz"))
       (sha256
        (base32 "0788hbcr1v89crv2j3insy89lxs0zwjbxk9q73jpj4g7j26bszcd"))
       (patches (search-patches "julia-SOURCE_DATE_EPOCH-mtime.patch"
                                "julia-1.9-build-tweaks.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list #:test-target "test"
           #:modules `((ice-9 match)
                       (guix build gnu-build-system)
                       (guix build utils))

           ;; The test suite takes many times longer than building and
           ;; can easily fail on smaller machines when they run out of memory.
           #:tests? (not (or (%current-target-system)
                             (target-aarch64?)))

           ;; Do not strip binaries to keep support for full backtraces.
           ;; See https://github.com/JuliaLang/julia/issues/17831
           #:strip-binaries? #f

           ;; The DSOs use $ORIGIN to refer to each other, but (guix build
           ;; gremlin) doesn't support it yet, so skip this phase.
           #:validate-runpath? #f

           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-after 'unpack 'prepare-deps
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; needed by libwhich
                   (match inputs
                     (((labels . directories) ...)
                      (set-path-environment-variable
                       "LD_LIBRARY_PATH"
                       '("lib") directories)))))
               (add-before 'check 'set-home
                 ;; Some tests require a home directory to be set.
                 (lambda _
                   (setenv "HOME" "/tmp")))
               (add-before 'build 'fix-include-and-link-paths
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; The REPL must be linked with libuv.
                   (substitute* "cli/Makefile"
                     (("JLDFLAGS \\+= ")
                      (string-append "JLDFLAGS += "
                                     (search-input-file inputs
                                                        "/lib/libuv.so") " ")))))
               (add-before 'build 'fix-nested-dlopen
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "Make.inc"
                     ;; See the patch comment for the explanation, *must* be updated
                     ;; when updating julia.
                     (("GUIX_LIBUTF8PROC")
                      (search-input-file inputs "/lib/libutf8proc.a")))
                   (substitute* "Makefile"
                     ;; See the patch comment for the explanation, *must* be updated
                     ;; when updating julia.
                     (("GUIX_LIBCHOLMOD")
                      (search-input-file inputs "/lib/libcholmod.so"))
                     (("GUIX_LIBSPQR")
                      (search-input-file inputs "/lib/libspqr.so"))
                     (("GUIX_LIBSC")
                      (search-input-file inputs
                                         "/lib/libsuitesparseconfig.so"))
                     (("GUIX_LIBUMFPACK")
                      (search-input-file inputs "/lib/libumfpack.so")))))
               (add-before 'build 'replace-default-shell
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "base/client.jl"
                     (("/bin/sh")
                      (search-input-file inputs "/bin/sh")))))
               (add-before 'build 'shared-objects-paths
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((jlpath (lambda (pkgname)
                                   (string-append "stdlib/" pkgname
                                                  "_jll/src/" pkgname
                                                  "_jll.jl")))
                         (jlbasepath (lambda (pkgname)
                                       (string-append "base/" pkgname
                                                      ".jl")))
                         (tolib (lambda (libname)
                                  (search-input-file inputs
                                                     (string-append
                                                      "/lib/" libname
                                                      ".so"))))
                         (toquotedlib (lambda (libname)
                                        (define file
                                          (string-append "/lib/"
                                                         libname
                                                         ".so"))
                                        (string-append "\""
                                                       (search-input-file
                                                        inputs file)
                                                       "\"")))
                         (from (lambda (libname)
                                 (string-append "const " libname
                                                " = .*\\.so")))
                         (to (lambda* (libname #:optional libname_jl)
                               (define file
                                 (string-append "/lib/" libname ".so"))

                               (string-append "const "
                                              (or libname_jl libname)
                                              " = \""
                                              (search-input-file
                                               inputs file)))))
                     (substitute* "src/jitlayers.cpp"
                       (("libatomic.so")
                        (search-input-file inputs "/lib/libatomic.so")))
                     (substitute* (list (jlbasepath "linking")
                                        (jlpath "LLD"))
                       (("\"lld\"")
                        (string-append "\""
                                       (search-input-file inputs
                                                          "/bin/lld")
                                       "\"")))
                     (substitute* (jlbasepath "pcre")
                       (("libpcre2-8")
                        (tolib "libpcre2-8")))
                     (substitute* (jlbasepath "gmp")
                       (("libgmp.so.10")
                        (tolib "libgmp")))
                     (substitute* (jlbasepath "mpfr")
                       (("libmpfr.so.6")
                        (tolib "libmpfr")))
                     (substitute* (jlbasepath "irrationals")
                       ((":libmpfr")
                        (toquotedlib "libmpfr")))
                     (substitute* "stdlib/Random/src/generation.jl"
                       ((":libmpfr")
                        (toquotedlib "libmpfr")))
                     (substitute* "stdlib/Printf/src/Printf.jl"
                       (("\"libmpfr\"")
                        (toquotedlib "libmpfr")))
                     (substitute* "stdlib/Random/src/DSFMT.jl"
                       ((":libdSFMT")
                        (toquotedlib "libdSFMT")))
                     (for-each (lambda (file)
                                 (substitute* file
                                   ((":libgit2")
                                    (toquotedlib "libgit2"))))
                               (find-files "stdlib/LibGit2/" "\\.jl$"))
                     (substitute* (jlpath "CompilerSupportLibraries")
                       (((from "libgomp"))
                        (to "libgomp"))
                       (((from "libgfortran"))
                        (string-append
                         "const libgfortran = string(\""
                         (search-input-file inputs
                                            "/lib/libgfortran.so"))))
                     (substitute* (jlpath "dSFMT")
                       (((from "libdSFMT"))
                        (to "libdSFMT")))
                     (substitute* (jlpath "GMP")
                       (((from "libgmp"))
                        (to "libgmp"))
                       (((from "libgmpxx"))
                        (to "libgmpxx")))
                     (substitute* (jlpath "libLLVM")
                       (((from "libLLVM"))
                        (to "libLLVM")))
                     (substitute* (jlbasepath "binaryplatforms")
                       (("libLLVM-14jl")
                        (tolib "libLLVM-14jl")))
                     (substitute* (jlpath "LibCURL")
                       (((from "libcurl"))
                        (to "libcurl")))
                     (substitute* (jlpath "LibGit2")
                       (((from "libgit2"))
                        (to "libgit2")))
                     (substitute* (jlpath "LibSSH2")
                       (((from "libssh2"))
                        (to "libssh2")))
                     (substitute* (jlpath "LibUV")
                       (((from "libuv"))
                        (to "libuv")))
                     (substitute* (jlpath "LibUnwind")
                       (((from "libunwind"))
                        (to "libunwind")))
                     (substitute* (jlpath "MPFR")
                       (((from "libmpfr"))
                        (to "libmpfr")))
                     (substitute* (jlpath "MbedTLS")
                       (((from "libmbedcrypto"))
                        (to "libmbedcrypto"))
                       (((from "libmbedtls"))
                        (to "libmbedtls"))
                       (((from "libmbedx509"))
                        (to "libmbedx509")))
                     (substitute* (jlpath "nghttp2")
                       (((from "libnghttp2"))
                        (to "libnghttp2")))
                     (substitute* (jlpath "OpenBLAS")
                       (("libgfortran.so")
                        (tolib "libgfortran"))
                       (((from "libopenblas"))
                        #$@(if (target-x86-64?)
                               `((to "libopenblas64_" "libopenblas"))
                               `((to "libopenblas")))))
                     (substitute* (jlpath "OpenLibm")
                       (((from "libopenlibm"))
                        (to "libopenlibm")))
                     (substitute* (jlpath "PCRE2")
                       (((from "libpcre2_8"))
                        (to "libpcre2-8" "libpcre2_8")))
                     (substitute* (jlpath "SuiteSparse")
                       (((from "libamd"))
                        (to "libamd"))
                       (((from "libbtf"))
                        (to "libbtf"))
                       (((from "libcamd"))
                        (to "libcamd"))
                       (((from "libccolamd"))
                        (to "libccolamd"))
                       (((from "libcholmod"))
                        (to "libcholmod"))
                       (((from "libcolamd"))
                        (to "libcolamd"))
                       (((from "libklu"))
                        (to "libklu"))
                       (((from "libldl"))
                        (to "libldl"))
                       (((from "librbio"))
                        (to "librbio"))
                       (((from "libspqr"))
                        (to "libspqr"))
                       (((from "libsuitesparse"))
                        (to "libsuitesparse"))
                       (((from "libsuitesparseconfig"))
                        (to "libsuitesparseconfig"))
                       (((from "libumfpack"))
                        (to "libumfpack")))
                     (substitute* (jlpath "Zlib")
                       (((from "libz"))
                        (to "libz")))
                     (substitute* (jlpath "libblastrampoline")
                       (("libblastrampoline\\.so")
                        (search-input-file inputs
                                           "/lib/libblastrampoline.so"))))))
               (add-before 'build 'use-ssl-cert-file
                 (lambda _
                   ;; We must adapt MozillaCACerts to use SSL_CERT_FILE.
                   (substitute* "stdlib/MozillaCACerts_jll/src/MozillaCACerts_jll.jl"
                     (("global cacert = .*")
                      (string-append
                       "global cacert = get(ENV, \"SSL_CERT_FILE\","
                       ;; our fallback location.
                       "\"/etc/ssl/certs/ca-certificates.crt\")\n")))))
               (add-after 'unpack 'enable-parallel-tests
                 (lambda* (#:key parallel-tests? #:allow-other-keys)
                   (when parallel-tests?
                     (setenv "JULIA_TEST_USE_MULTIPLE_WORKERS" "true"))))
               (add-after 'unpack 'adjust-test-suite
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "test/spawn.jl"
                     (("shcmd = `sh`")
                      (string-append "shcmd = `"
                                     (which "sh") "`")))

                   ;; Some tests only check to see if the input is the correct version.
                   (substitute* "stdlib/PCRE2_jll/test/runtests.jl"
                     (("10\\.42\\.0")
                      #$(package-version (this-package-input "pcre2"))))
                   (substitute* "stdlib/MbedTLS_jll/test/runtests.jl"
                     (("2\\.28\\.2")
                      #$(package-version (this-package-input "mbedtls-apache"))))
                   (substitute* "stdlib/MPFR_jll/test/runtests.jl"
                     (("4\\.1\\.1")
                      #$(package-version (this-package-input "mpfr"))))
                   (substitute* "stdlib/GMP_jll/test/runtests.jl"
                     (("6\\.2\\.1")
                      #$(package-version (this-package-input "gmp"))))
                   (substitute* "stdlib/LibGit2_jll/test/runtests.jl"
                     (("1\\.5\\.0")
                      #$(package-version (this-package-input "libgit2"))))
                   (substitute* "stdlib/nghttp2_jll/test/runtests.jl"
                     (("1\\.48\\.0")
                      #$(package-version (this-package-input "nghttp2"))))
                   (substitute* "stdlib/Zlib_jll/test/runtests.jl"
                     (("1\\.2\\.13")
                      #$(package-version (this-package-input "zlib"))))
                   (substitute* "stdlib/SuiteSparse_jll/test/runtests.jl"
                     (("5010")
                      #$(string-replace-substring
                         (version-major+minor
                          (package-version
                           (this-package-input "suitesparse"))) "." "0")))))
               (add-before 'check 'disable-broken-tests
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; disabling REPL tests because they require a stdin
                   ;; There are some read-only precompile issues in the 1.6 series.
                   ;; https://github.com/JuliaLang/julia/pull/41614
                   ;; https://github.com/JuliaLang/julia/issues/41156
                   (substitute* "test/choosetests.jl"
                     (("\"cmdlineargs\",")
                      "")
                     (("\"precompile\",")
                      ""))
                   ;; Distributed test with remote execution
                   (substitute* "usr/share/julia/stdlib/v1.9/Distributed/test/distributed_exec.jl"
                     (("using Test,")
                      "exit()\n"))
                   ;; Dates/io tests fail on master when networking is unavailable
                   ;; https://github.com/JuliaLang/julia/issues/34655
                   (substitute* "stdlib/Dates/test/io.jl"
                     (("using Dates")
                      "import Dates
using Dates: @dateformat_str, Date, DateTime, DateFormat, Time"))
                   ;; julia embeds a certificate, we are not doing that
                   (substitute* "stdlib/MozillaCACerts_jll/test/runtests.jl"
                     (("@test isfile\\(MozillaCACerts_jll.cacert\\)")
                      "@test_broken isfile(MozillaCACerts_jll.cacert)"))
                   ;; since certificate is not present some tests are failing in network option
                   (substitute* "usr/share/julia/stdlib/v1.9/NetworkOptions/test/runtests.jl"
                     (("@test isfile\\(bundled_ca_roots\\(\\)\\)")
                      "@test_broken isfile(bundled_ca_roots())")
                     (("@test ispath\\(ca_roots_path\\(\\)\\)")
                      "@test_broken ispath(ca_roots_path())")
                     (("@test ca_roots_path\\(\\) \\!= bundled_ca_roots\\(\\)")
                      "@test_broken ca_roots_path() != bundled_ca_roots()"))
                   ;; WARNING: failed to select UTF-8 encoding, using ASCII
                   ;; Using 'setlocale' doesn't affect the test failures.
                   ;; (setlocale LC_ALL "en_US.utf8")
                   ;; (setenv "LC_ALL" "en_US.utf8")
                   (substitute* "test/cmdlineargs.jl"
                     (("test v\\[3")
                      "test_broken v[3")
                     (("test isempty\\(v\\[3")
                      "test_broken isempty(v[3"))
                   ;; These test(s) randomly fails because they depend on CPU.
                   (substitute* "test/math.jl"
                     ;; @test_broken cannot be used because if the test randomly
                     ;; passes, then it also raises an error.
                     (("@test isinf\\(log1p\\(-one\\(T\\)\\)\\)")
                      " "))

                   ;; These are new test failures for 1.8:
                   ;; This test passes on some architectures and fails on others.
                   (substitute* "stdlib/LinearAlgebra/test/lu.jl"
                     (("@test String")
                      "@test_skip String"))

                   (substitute* "usr/share/julia/stdlib/v1.9/Statistics/test/runtests.jl"
                     (("@test cov\\(A")
                      "@test_skip cov(A")
                     (("@test isfinite")
                      "@test_skip isfinite"))
                   ;; LoadError: SuiteSparse threads test failed with nthreads == 4
                   (substitute* "usr/share/julia/stdlib/v1.9/SuiteSparse/test/runtests.jl"
                     (("Base\\.USE_GPL_LIBS")
                      "false"))
                   ;; Got exception outside of a @test
                   ;; LinearAlgebra.LAPACKException(16)
                   ;; eliminate all the test bits.
                   (substitute* "stdlib/LinearAlgebra/test/schur.jl"
                     (("f = schur\\(A, B\\)")
                      "f = schur(A, A)")
                     (("@test f\\.Q\\*f\\.S\\*f\\.Z'.*")
                      "\n")
                     (("@test f\\.Q\\*f\\.T\\*f\\.Z'.*")
                      "\n"))))
               ;; Doesn't this just mean they weren't linked correctly?
               (add-after 'install 'symlink-missing-libraries
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (link (lambda (pkgname pred)
                                  (map (lambda (file)
                                         (unless (file-exists? (string-append
                                                                out
                                                                "/lib/julia/"
                                                                (basename
                                                                 file)))
                                           (symlink file
                                                    (string-append
                                                     out
                                                     "/lib/julia/"
                                                     (basename file)))))
                                       (find-files (string-append (assoc-ref
                                                                   inputs
                                                                   pkgname)
                                                                  "/lib") pred)))))
                     (link "libunwind-julia" "libunwind\\.so")
                     (link "llvm" "libLLVM-14jl\\.so")
                     (link "gmp" "libgmp\\.so.10")
                     (link "mpfr" "libmpfr\\.so.6")
                     (link "zlib" "libz\\.so"))))
               (add-after 'install 'make-wrapper
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (bin (string-append out "/bin"))
                          (program "julia"))
                     (with-directory-excursion bin
                       (wrap-program program
                         `("JULIA_LOAD_PATH" ":" prefix
                           ("" "$JULIA_LOAD_PATH"))
                         `("JULIA_DEPOT_PATH" ":" prefix
                           ("" "$JULIA_DEPOT_PATH"))))))))

           #:make-flags
           #~(list (string-append "prefix="
                                  (assoc-ref %outputs "out"))

                   ;; Passing the MARCH or JULIA_CPU_TARGET flag is necessary to build
                   ;; binary substitutes for the supported architectures.  See also
                   ;; https://docs.julialang.org/en/v1/devdocs/sysimg/#Specifying-multiple-system-image-targets
                   #$(match (or (%current-target-system)
                                (%current-system))
                       ("x86_64-linux"
                        ;; These are the flags that upstream uses for their binaries.
                        "JULIA_CPU_TARGET=generic;generic,-cx16,clone_all;sandybridge,-xsaveopt,clone_all;haswell,-rdrnd,base(1)")
                       ("i686-linux" "MARCH=pentium4")
                       ("armhf-linux"
                        "JULIA_CPU_TARGET=armv7-a,neon")
                       ("powerpc64le-linux"
                        "JULIA_CPU_TARGET=pwr8")
                       ;; Prevent errors when querying this package on unsupported
                       ;; platforms, e.g. when running "guix package --search="
                       ;; and also of targeting the builder's architecture.
                       (_ "JULIA_CPU_TARGET=generic"))

                   "CONFIG_SHELL=bash -x" ;needed to build bundled libraries
                   (string-append "CC="
                                  #$(cc-for-target))

                   #$@(if (target-x86-64?)
                          `("USE_BLAS64=1"
                            "LIBBLAS=-lopenblas64_"
                            "LIBBLASNAME=libopenblas64_")
                          `("USE_BLAS64=0" "LIBBLAS=-lopenblas"
                            "LIBBLASNAME=libopenblas"))

                   (string-append "UTF8PROC_INC="
                                  (assoc-ref %build-inputs "utf8proc")
                                  "/include")
                   ;; Make.inc expects a static library for libuv.
                   (string-append "LIBUV="
                                  (search-input-file %build-inputs
                                                     "/lib/libuv.a"))
                   (string-append "LIBUV_INC="
                                  (dirname
                                   (search-input-file
                                    %build-inputs "/include/uv.h"))))))
    (inputs (list coreutils             ;for bindings to "mkdir" and the like
                  curl-ssh
                  gfortran
                  `(,gfortran "lib")              ;required for libgcc_s.so
                  gmp
                  lapack
                  libblastrampoline
                  libgit2-1.5
                  `(,nghttp2 "lib")
                  libssh2
                  libunwind-julia
                  libuv-julia
                  llvm-14-julia
                  lld-14
                  mbedtls-apache
                  mpfr
                  (if (target-x86-64?) openblas-ilp64 openblas)
                  openlibm
                  p7zip
                  pcre2
                  suitesparse
                  utf8proc-2.7.0
                  wget
                  which
                  zlib
                  ;; Find dependencies versions here:
                  ;; https://raw.githubusercontent.com/JuliaLang/julia/v1.6.0/deps/Versions.make
                  dsfmt
                  libwhich))
    (native-inputs (list openssl perl patchelf pkg-config python))
    (native-search-paths
     (list (search-path-specification
            (variable "JULIA_LOAD_PATH")
            (files (list "share/julia/loadpath/")))
           (search-path-specification
            (variable "JULIA_DEPOT_PATH")
            (files (list "share/julia/")))
           $SSL_CERT_FILE))
    ;; Julia only officially supports some of our platforms:
    ;; https://julialang.org/downloads/#supported_platforms
    (supported-systems '("i686-linux" "x86_64-linux" "aarch64-linux"))
    (home-page "https://julialang.org/")
    (synopsis "High-performance dynamic language for technical computing")
    (description
     "Julia is a high-level, high-performance dynamic programming language for
technical computing, with syntax that is familiar to users of other technical
computing environments.  It provides a sophisticated compiler, distributed
parallel execution, numerical accuracy, and an extensive mathematical function
library.")
    (license license:expat)))
