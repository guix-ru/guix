;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2018-2020, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016-2024 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Peter Kreye <kreyepr@gmail.com>
;;; Copyright © 2018, 2019 Gabriel Hondet <gabrielhondet@gmail.com>
;;; Copyright © 2018 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020, 2021, 2025 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2020 divoplade <d@divoplade.fr>
;;; Copyright © 2020, 2021, 2022 pukkamustard <pukkamustard@posteo.net>
;;; Copyright © 2021 aecepoglu <aecepoglu@fastmail.fm>
;;; Copyright © 2021 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Ivan Gankevich <i.gankevich@spbu.ru>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2022 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2022 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Garek Dyszel <garekdyszel@disroot.org>
;;; Copyright © 2023 Csepp <raingloom@riseup.net>
;;; Copyright © 2023, 2024 Foundation Devices, Inc. <hello@foundation.xyz>
;;; Copyright © 2023 Arnaud DABY-SEESARAM <ds-ac@nanein.fr>
;;; Copyright © 2024 Sören Tempel <soeren@soeren-tempel.net>
;;; Copyright © 2025 Jussi Timperi <jussi.timperi@iki.fi>
;;; Copyright © 2025 Jason Conroy <jconroy@tscripta.net>
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

(define-module (gnu packages ocaml5)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages node)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages unicode)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xorg)
  #:use-module ((guix build-system dune)
                #:select ((ocaml5-dune-build-system . dune-build-system)))
  #:use-module ((guix build-system ocaml)
                #:select ((ocaml5-build-system . ocaml-build-system)))
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

;; An origin for a Jane Street project hosted on Github.
(define (janestreet-git-origin name version hash-string)
  (origin
    (method git-fetch)
    (uri (git-reference
           (url (string-append
                 "https://github.com/janestreet/" name))
           (commit (string-append "v" version))))
    (file-name (git-file-name name version))
    (sha256
     (base32 hash-string))))

(define-public ocaml-5.0
  (package
    (name "ocaml")
    (version "5.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/ocaml")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p0p8wldrnbr61wfy3x4122017g4k5gjvfwlg3mvlqn8r2fxn2m5"))))
    (build-system gnu-build-system)
    (native-search-paths
     (list (search-path-specification
            (variable "OCAMLPATH")
            (files (list "lib/ocaml" "lib/ocaml/site-lib")))
           (search-path-specification
            (variable "CAML_LD_LIBRARY_PATH")
            (files (list "lib/ocaml/site-lib/stubslibs"
                         "lib/ocaml/site-lib/stublibs")))))
    (native-inputs (list parallel perl pkg-config))
    (inputs (list libx11 libiberty ;needed for objdump support
                  zlib)) ;also needed for objdump support
    (arguments
     `(#:configure-flags '("--enable-ocamltest")
       #:test-target "tests"
       ;; This doesn't have the desired effect and makes test runs less
       ;; stable. See https://codeberg.org/guix/guix/pulls/2933.
       #:parallel-tests? #f
       #:make-flags '("defaultentry")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'enable-parallel-tests
                    (lambda _
                      ;; Patch the `tests` build target to enable a special parallel
                      ;; execution mode based on GNU Parallel.
                      (substitute* "Makefile"
                        (("-C testsuite all")
                         "-C testsuite parallel"))))
                  (add-after 'unpack 'patch-/bin/sh-references
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let* ((sh (search-input-file inputs "/bin/sh"))
                             (quoted-sh (string-append "\"" sh "\"")))
                        (with-fluids ((%default-port-encoding #f))
                                     (for-each (lambda (file)
                                                 (substitute* file
                                                   (("\"/bin/sh\"")
                                                    (begin
                                                      (format (current-error-port)
                                                       "patch-/bin/sh-references: ~a: changing `\"/bin/sh\"' to `~a'~%"
                                                       file quoted-sh)
                                                      quoted-sh))))
                                               (find-files "." "\\.ml$")))))))))
    (home-page "https://ocaml.org/")
    (synopsis "The OCaml programming language")
    (description
     "OCaml is a general purpose industrial-strength programming language with
an emphasis on expressiveness and safety.  Developed for more than 20 years at
Inria it benefits from one of the most advanced type systems and supports
functional, imperative and object-oriented styles of programming.")
    ;; The compiler is distributed under qpl1.0 with a change to choice of
    ;; law: the license is governed by the laws of France.  The library is
    ;; distributed under lgpl2.0.
    (license (list license:qpl license:lgpl2.0))))

(define-public ocaml-5.3
  (package
    (inherit ocaml-5.0)
    (version "5.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ocaml/ocaml")
                    (commit version)))
              (file-name (git-file-name "ocaml" version))
              (sha256
               (base32
                "05jhy9zn53v12rn3sg3vllqf5blv1gp7f06803npimc58crxy6rv"))))))

;; The default ocaml 5.x compiler, for those packages that depend on a
;; compiler explicitly.
(define-public ocaml ocaml-5.3)

(define-public ocamlbuild
  (package
    (name "ocaml5-ocamlbuild")
    (version "0.16.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/ocamlbuild")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "148r0imzsalr7c3zqncrl4ji29wpb5ls5zkqxy6xnh9q99gxb4a6"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:make-flags ,#~(list (string-append "OCAMLBUILD_PREFIX="
                                            #$output)
                             (string-append "OCAMLBUILD_BINDIR="
                                            #$output "/bin")
                             (string-append "OCAMLBUILD_LIBDIR="
                                            #$output "/lib/ocaml/site-lib")
                             (string-append "OCAMLBUILD_MANDIR="
                                            #$output "/share/man"))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))
       ;; some failures because of changes in OCaml's error message formatting
       #:tests? #f))
    (home-page "https://github.com/ocaml/ocamlbuild")
    (synopsis "OCaml build tool")
    (description "OCamlbuild is a generic build tool, that has built-in rules
for building OCaml library and programs.")
    (license license:lgpl2.1+)))

(define-public ocaml-opam-file-format
  (package
    (name "ocaml5-opam-file-format")
    (version "2.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/opam-file-format")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dmnb1mqdy4913f9ma446hi5m99q7hfibj6j0m8x2wsfnfy2fw62"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f ;No tests
       #:make-flags ,#~(list (string-append "LIBDIR="
                                            #$output "/lib/ocaml/site-lib"))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (home-page "https://opam.ocaml.org")
    (synopsis "Parser and printer for the opam file syntax")
    (description "This package contains a parser and a pretty-printer for
the opam file format.")
    ;; With static-linking exception
    (license license:lgpl2.1+)))

(define-public opaline
  (package
    (name "opaline")
    (version "0.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jaapb/opaline")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gilxynfxfbahzfg64h371lq6b3wrlsfy53xb8ccai0waa45l6za"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f ;No tests
       #:make-flags ,#~(list (string-append "PREFIX="
                                            #$output))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (propagated-inputs (list ocaml-opam-file-format))
    (native-inputs (list ocamlbuild))
    (home-page "https://opam.ocaml.org")
    (synopsis "Tool for installing OCaml packages")
    (description
     "@var{opaline} is a tool for installing OCaml packages
based on @code{.install} files defined by the OPAM package manager.  It serves
the same purpose as @var{opam-installer} but has fewer library dependencies.")
    (license license:expat)))

(define-public ocaml5.3-result
  (package
    (name "ocaml5.3-result")
    (version "1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/janestreet/result")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "166laj8qk7466sdl037c6cjs4ac571hglw4l5qpyll6df07h6a7q"))))
    (build-system dune-build-system)
    (arguments
     `(#:dune ,ocaml5.3-dune-bootstrap))
    (home-page "https://github.com/janestreet/result")
    (synopsis "Compatibility Result module")
    (description
     "Uses the new result type defined in OCaml >= 4.03 while
staying compatible with older version of OCaml should use the Result module
defined in this library.")
    (license license:bsd-3)))

;; Use this version of `ocaml-result` for packages built with the default
;; ocaml 5.x compiler.
(define-public ocaml-result ocaml5.3-result)

(define-public ocaml-topkg
  (package
    (name "ocaml5-topkg")
    (version "1.0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/topkg/releases/"
                                  "topkg-" version ".tbz"))
              (sha256
               (base32
                "11ycfk0prqvifm9jca2308gw8a6cjb1hqlgfslbji2cqpan09kpq"))))
    (build-system ocaml-build-system)
    (native-inputs
     (list ocamlbuild opaline))
    (propagated-inputs (list ocaml-result))
    (arguments
     `(#:tests? #f
       #:build-flags '("build")
       #:phases
       ,#~(modify-phases %standard-phases
            (delete 'configure))))
    (home-page "https://erratique.ch/software/topkg")
    (synopsis "Transitory OCaml software packager")
    (description "Topkg is a packager for distributing OCaml software.  It
provides an API to describe the files a package installs in a given build
configuration and to specify information about the package's distribution,
creation and publication procedures.")
    (license license:isc)))

(define-public ocaml-seq
  (package
    (name "ocaml5-seq")
    (version "0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/c-cube/seq")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cjpsc7q76yfgq9iyvswxgic4kfq2vcqdlmxjdjgd4lx87zvcwrv"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((install-dir (string-append (assoc-ref outputs "out")
                                               "/lib/ocaml/site-lib/seq")))
               (mkdir-p install-dir)
               (with-output-to-file (string-append install-dir "/META")
                 (lambda _
                   (display "name=\"seq\"
version=\"[distributed with ocaml]\"
description=\"dummy package for compatibility\"
requires=\"\"")))
               #t))))))
    (home-page "https://github.com/c-cube/seq")
    (synopsis "OCaml's standard iterator type")
    (description "This package is a compatibility package for OCaml's
standard iterator type starting from 4.07.")
    (license license:lgpl2.1+)))

(define-public ocaml-stdlib-shims
  (package
    (name "ocaml5-stdlib-shims")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml/stdlib-shims")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gmg8w67j3ww17llk7hl4dx0vq7p50rn5s4ib9sy984k543rz59h"))))
    (build-system dune-build-system)
    (home-page "https://github.com/ocaml/stdlib-shims")
    (synopsis "OCaml stdlib features backport to older OCaml compilers")
    (description "This package backports some of the new stdlib features to
older compilers, such as the Stdlib module.  This allows projects that require
compatibility with older compiler to use these new features in their code.")
    ;; with ocaml-linking exception
    (license license:lgpl2.1+)))

(define-public ocaml-cmdliner
  (package
    (name "ocaml5-cmdliner")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://erratique.ch/software/cmdliner/releases/"
                           "cmdliner-" version ".tbz"))
       (sha256
        (base32 "1fwc2rj6xfyihhkx4cn7zs227a74rardl262m2kzch5lfgsq10cf"))))
    (build-system dune-build-system)
    (inputs (list ocaml-result))
    (arguments
     `(#:tests? #f))
    (home-page "https://erratique.ch/software/cmdliner")
    (synopsis "Declarative definition of command line interfaces for OCaml")
    (description
     "Cmdliner is a module for the declarative definition of command
line interfaces.  It provides a simple and compositional mechanism to convert
command line arguments to OCaml values and pass them to your functions.  The
module automatically handles syntax errors, help messages and UNIX man page
generation.  It supports programs with single or multiple commands and respects
most of the POSIX and GNU conventions.")
    (license license:bsd-3)))

(define-public ocaml-cppo
  (package
    (name "ocaml5-cppo")
    (version "1.6.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mjambon/cppo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c8jlr2s0allw1h6czz5q24vn5jsnrrh44j7hjyilzaifm17dlrm"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs (list ocamlbuild))
    (home-page "https://github.com/mjambon/cppo")
    (synopsis "Equivalent of the C preprocessor for OCaml programs")
    (description
     "Cppo is an equivalent of the C preprocessor for OCaml
programs.  It allows the definition of simple macros and file inclusion.  Cppo is:
@enumerate
@item more OCaml-friendly than @command{cpp}
@item easy to learn without consulting a manual
@item reasonably fast
@item simple to install and to maintain.
@end enumerate")
    (license license:bsd-3)))

(define-public ocaml-bigarray-compat
  (package
    (name "ocaml5-bigarray-compat")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mirage/bigarray-compat")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hif5baiwswdblymyfbxh9066pfqynlz5vj3b2brpn0a12k6i5fq"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f)) ;no tests
    (home-page "https://github.com/mirage/bigarray-compat")
    (synopsis "OCaml compatibility library")
    (description "This package contains a compatibility library for
@code{Stdlib.Bigarray} in OCaml.")
    (license license:isc)))

(define-public ocaml-mmap
  (package
    (name "ocaml5-mmap")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mirage/mmap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a7w7l682cbksn2zlmz24gb519x7wb65ivr5vndm9x5pi9fw5pfb"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-bigarray-compat))
    (home-page "https://github.com/mirage/mmap")
    (synopsis "File mapping for OCaml")
    (description
     "This project provides a @command{Mmap.map_file} function
for mapping files in memory.  This function is the same as the
@command{Unix.map_file} function added in OCaml >= 4.06.")
    (license (list license:qpl license:lgpl2.0))))

(define-public ocaml-astring
  (package
    (name "ocaml5-astring")
    (version "0.8.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://erratique.ch/software/astring/releases/astring-" version
             ".tbz"))
       (sha256
        (base32 "1ykhg9gd3iy7zsgyiy2p9b1wkpqg9irw5pvcqs3sphq71iir4ml6"))))
    (build-system ocaml-build-system)
    (native-inputs (list ocamlbuild ocaml-topkg opaline))
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases ,#~(modify-phases %standard-phases
                     (delete 'configure))))
    (home-page "https://erratique.ch/software/astring")
    (synopsis "Alternative String module for OCaml")
    (description
     "Astring exposes an alternative String module for OCaml.  This
module balances minimality and expressiveness for basic, index-free, string
processing and provides types and functions for substrings, string sets and
string maps.  The String module exposed by Astring has exception safe functions,
removes deprecated and rarely used functions, alters some signatures and names,
adds a few missing functions and fully exploits OCaml's newfound string
immutability.")
    (license license:isc)))

(define-public ocaml-uuidm
  (package
    (name "ocaml5-uuidm")
    (version "0.9.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://erratique.ch/software/uuidm/"
                           "releases/uuidm-" version ".tbz"))
       (sha256
        (base32 "1cr6xlzla9fmd587lfhzac0icifspjnqi9f4cdafshj3jn85nrpw"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:build-flags (list "build" "--tests" "true" "--with-cmdliner" "true")
       #:phases ,#~(modify-phases %standard-phases
                     (delete 'configure))))
    (native-inputs (list ocamlbuild opaline))
    (propagated-inputs (list ocaml-cmdliner ocaml-topkg))
    (home-page "https://erratique.ch/software/uuidm")
    (synopsis "Universally unique identifiers for OCaml")
    (description
     "Uuidm is an OCaml module implementing 128 bits universally
unique identifiers (UUIDs) version 3, 5 (named based with MD5, SHA-1 hashing)
and 4 (random based) according to RFC 4122.")
    (license license:isc)))

(define-public ocaml-uutf
  (package
    (name "ocaml5-uutf")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://erratique.ch/software/uutf/releases/"
                           "uutf-" version ".tbz"))
       (sha256
        (base32 "0s05r8ggp1g97zq4rnvbxzj22pv8ld0k5wsdw662jw0y7mhsawl7"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases ,#~(modify-phases %standard-phases
                     (delete 'configure))))
    (native-inputs (list ocamlbuild ocaml-topkg opaline))
    (propagated-inputs (list ocaml-cmdliner))
    (home-page "https://erratique.ch/software/uutf")
    (synopsis "Non-blocking streaming Unicode codec for OCaml")
    (description
     "Uutf is a non-blocking streaming codec to decode and encode
the UTF-8, UTF-16, UTF-16LE and UTF-16BE encoding schemes.  It can efficiently
work character by character without blocking on IO.  Decoders perform character
position tracking and support newline normalization.

Functions are also provided to fold over the characters of UTF encoded OCaml
string values and to directly encode characters in OCaml Buffer.t values.")
    (license license:isc)))

(define-public ocaml-fmt
  (package
    (name "ocaml5-fmt")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://erratique.ch/software/fmt/releases/fmt-"
                           version ".tbz"))
       (sha256
        (base32 "0q8j2in2473xh7k4hfgnppv9qy77f2ih89yp6yhpbp92ba021yzi"))))
    (build-system ocaml-build-system)
    (native-inputs (list ocamlbuild ocaml-topkg opaline))
    (propagated-inputs (list ocaml-cmdliner ocaml-stdlib-shims))
    (arguments
     `(#:tests? #f
       #:build-flags (list "build" "--with-base-unix" "true" "--with-cmdliner"
                           "true")
       #:phases ,#~(modify-phases %standard-phases
                     (delete 'configure))))
    (home-page "https://erratique.ch/software/fmt")
    (synopsis "OCaml Format pretty-printer combinators")
    (description "Fmt exposes combinators to devise Format pretty-printing
functions.")
    (license license:isc)))

(define-public ocaml-uchar
  (package
    (name "ocaml5-uchar")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/ocaml/uchar/releases/download/v" version
             "/uchar-" version ".tbz"))
       (sha256
        (base32 "1w2saw7zanf9m9ffvz2lvcxvlm118pws2x1wym526xmydhqpyfa7"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "native=true" "native-dynlink=true")
       #:phases ,#~(modify-phases %standard-phases
                     (delete 'configure))))
    (native-inputs (list ocamlbuild opaline))
    (home-page "https://github.com/ocaml/uchar")
    (synopsis "Compatibility library for OCaml's Uchar module")
    (description "The uchar package provides a compatibility library for the
`Uchar` module introduced in OCaml 4.03.")
    (license license:lgpl2.1)))

(define-public ocaml-mtime
  (package
    (name "ocaml5-mtime")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://erratique.ch/software/mtime/releases/"
                           "mtime-" version ".tbz"))
       (sha256
        (base32 "1ss4w3qxsfp51d88r0j7dzqs05dbb1xdx11hn1jl9cvd03ma0g9z"))))
    (build-system ocaml-build-system)
    (native-inputs (list ocamlbuild opaline))
    (propagated-inputs (list ocaml-topkg))
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases ,#~(modify-phases %standard-phases
                     (delete 'configure))))
    (home-page "https://erratique.ch/software/mtime")
    (synopsis "Monotonic wall-clock time for OCaml")
    (description
     "Access monotonic wall-clock time.  It measures time
spans without being subject to operating system calendar time adjustments.")
    (license license:isc)))

(define-public ocaml-ptime
  (package
    (name "ocaml5-ptime")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://erratique.ch/software/ptime/releases/ptime-1.2.0.tbz")
       (sha256
        (base32 "1c1swx6h794gcck358nqfzshlfhyw1zb5ji4h1pc63j9vxzp85ln"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:build-flags (list "build" "--tests" "true")
       #:phases ,#~(modify-phases %standard-phases
                     (delete 'configure))))
    (propagated-inputs (list ocaml-result))
    (native-inputs (list ocamlbuild ocaml-topkg opaline))
    (home-page "https://erratique.ch/software/ptime")
    (synopsis "POSIX time for OCaml")
    (description
     "Ptime offers platform independent POSIX time support in pure OCaml.  It
provides a type to represent a well-defined range of POSIX timestamps with
picosecond precision, conversion with date-time values, conversion with RFC
3339 timestamps and pretty printing to a human-readable, locale-independent
representation.")
    (license license:isc)))

(define-public ocaml-rresult
  (package
    (name "ocaml5-rresult")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://erratique.ch/software/rresult/releases/"
                           "rresult-" version ".tbz"))
       (sha256
        (base32 "0h2mjyzhay1p4k7n0mzaa7hlc7875kiy6m1i3r1n03j6hddpzahi"))))
    (build-system ocaml-build-system)
    (native-inputs (list ocamlbuild opaline))
    (propagated-inputs (list ocaml-topkg))
    (arguments
     `(#:tests? #f
       #:build-flags '("build")
       #:phases ,#~(modify-phases %standard-phases
                     (delete 'configure))))
    (home-page "https://erratique.ch/software/rresult")
    (synopsis "Result value combinators for OCaml")
    (description
     "Handle computation results and errors in an explicit and
declarative manner, without resorting to exceptions.  It defines combinators
to operate on the result type available from OCaml 4.03 in the standard
library.")
    (license license:isc)))

(define-public ocaml-fpath
  (package
    (name "ocaml5-fpath")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://erratique.ch/software/fpath/releases/"
                           "fpath-" version ".tbz"))
       (sha256
        (base32 "03z7mj0sqdz465rc4drj1gr88l9q3nfs374yssvdjdyhjbqqzc0j"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases ,#~(modify-phases %standard-phases
                     (delete 'configure))))
    (native-inputs (list ocamlbuild opaline))
    (propagated-inputs (list ocaml-topkg ocaml-astring))
    (home-page "https://erratique.ch/software/fpath")
    (synopsis "File system paths for OCaml")
    (description
     "Fpath is an OCaml module for handling file system paths with
POSIX or Windows conventions.  Fpath processes paths without accessing the
file system and is independent from any system library.")
    (license license:isc)))

(define-public ocaml-logs
  (package
    (name "ocaml5-logs")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://erratique.ch/software/logs/releases/"
                           "logs-" version ".tbz"))
       (sha256
        (base32 "1jnmd675wmsmdwyb5mx5b0ac66g4c6gpv5s4mrx2j6pb0wla1x46"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "build" "--with-js_of_ocaml" "false")
       #:phases ,#~(modify-phases %standard-phases
                     (delete 'configure))))
    (native-inputs (list ocamlbuild opaline))
    (propagated-inputs (list ocaml-fmt
                             ocaml-lwt
                             ocaml-mtime
                             ocaml-result
                             ocaml-cmdliner
                             ocaml-topkg))
    (home-page "https://erratique.ch/software/logs")
    (synopsis "Logging infrastructure for OCaml")
    (description
     "Logs provides a logging infrastructure for OCaml.  Logging is
performed on sources whose reporting level can be set independently.  Log
message report is decoupled from logging and is handled by a reporter.")
    (license license:isc)))

(define-public ocaml-bos
  (package
    (name "ocaml5-bos")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://erratique.ch/software/bos/releases/" "bos-"
                           version ".tbz"))
       (sha256
        (base32 "0dwg7lpaq30rvwc5z1gij36fn9xavvpah1bj8ph9gmhhddw2xmnq"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases ,#~(modify-phases %standard-phases
                     (delete 'configure))))
    (native-inputs (list ocamlbuild opaline))
    (propagated-inputs (list ocaml-topkg
                             ocaml-astring
                             ocaml-fmt
                             ocaml-fpath
                             ocaml-logs
                             ocaml-rresult))
    (home-page "https://erratique.ch/software/bos")
    (synopsis "Basic OS interaction for OCaml")
    (description
     "Bos provides support for basic and robust interaction with
the operating system in OCaml.  It has functions to access the process
environment, parse command line arguments, interact with the file system and
run command line programs.")
    (license license:isc)))

(define-public ocaml-jsonm
  (package
    (name "ocaml5-jsonm")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://erratique.ch/software/jsonm/releases/"
                           "jsonm-" version ".tbz"))
       (sha256
        (base32 "1mkvi88vwn4rxa5ir3k5qhm8h8z15iyd5rp1ycfj0ls0xfgj6aga"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases ,#~(modify-phases %standard-phases
                     (delete 'configure))))
    (native-inputs (list ocamlbuild ocaml-topkg opaline))
    (propagated-inputs (list ocaml-uutf ocaml-cmdliner))
    (home-page "https://erratique.ch/software/jsonm")
    (synopsis "Non-blocking streaming JSON codec for OCaml")
    (description
     "Jsonm is a non-blocking streaming codec to decode and encode
the JSON data format.  It can process JSON text without blocking on IO and
without a complete in-memory representation of the data.")
    (license license:isc)))

(define-public ocaml-xmlm
  (package
    (name "ocaml5-xmlm")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://erratique.ch/software/xmlm/releases/"
                           "xmlm-" version ".tbz"))
       (sha256
        (base32 "1ynrjba3wm3axscvggrfijfgsznmphhxnkffqch67l9xiqjm44h9"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (native-inputs (list ocamlbuild ocaml-topkg opam-installer))
    (home-page "https://erratique.ch/software/xmlm")
    (synopsis "Streaming XML codec for OCaml")
    (description
     "Xmlm is a streaming codec to decode and encode the XML data
format.  It can process XML documents without a complete in-memory
representation of the data.")
    (license license:isc)))

(define-public ocaml-react
  (package
    (name "ocaml5-react")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://erratique.ch/software/react/releases/react-" version
             ".tbz"))
       (sha256
        (base32 "16cg4byj8lfbbw96dhh8sks5y9n1c3fshz7f2p8m7wgisqax7bf4"))))
    (build-system ocaml-build-system)
    (native-inputs (list ocamlbuild opam-installer ocaml-topkg))
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (home-page "https://erratique.ch/software/react")
    (synopsis "Declarative events and signals for OCaml")
    (description
     "React is an OCaml module for functional reactive programming
(FRP).  It provides support to program with time varying values: declarative
events and signals.  React doesn't define any primitive event or signal, it
lets the client choose the concrete timeline.")
    (license license:bsd-3)))

(define-public ocaml-uunf
  (package
    (name "ocaml5-uunf")
    (version "17.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://erratique.ch/software/uunf/releases/uunf-"
             version ".tbz"))
       (sha256
        (base32 "0q6l33146kymq7nqvx8ck6zpw4y2w8rcff1d1fi6dcv8qi9ijxp5"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags (list "build")
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (native-inputs (list ocamlbuild opam-installer ocaml-topkg))
    (propagated-inputs (list ocaml-uutf))
    (home-page "https://erratique.ch/software/uunf")
    (synopsis "Unicode text normalization for OCaml")
    (description
     "Uunf is an OCaml library for normalizing Unicode text.  It supports all
Unicode normalization forms.  The library is independent from any
IO mechanism or Unicode text data structure and it can process text
without a complete in-memory representation.")
    (license license:isc)))

(define-public ocaml-uucd
  (package
    (name "ocaml5-uucd")
    (version "17.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://erratique.ch/software/uucd/releases/"
                           "uucd-" version ".tbz"))
       (sha256
        (base32 "0b98phs4i4p6fyj9pmsbqb2yf62wz1js0pnibcwaqbky8c2w9y49"))))
    (build-system ocaml-build-system)
    (arguments
     '(#:build-flags '("build" "--tests" "true")
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (propagated-inputs (list ocaml-xmlm))
    (native-inputs (list opam-installer ocamlbuild ocaml-topkg))
    (home-page "https://erratique.ch/software/uucd")
    (synopsis "Unicode character database decoder for OCaml")
    (description
     "Uucd is an OCaml module to decode the data of the Unicode
character database from its XML representation.  It provides high-level (but
not necessarily efficient) access to the data so that efficient
representations can be extracted.")
    (license license:isc)))

(define-public ocaml-uucp
  (package
    (name "ocaml5-uucp")
    (version "17.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://erratique.ch/software/uucp/releases/"
                           "uucp-" version ".tbz"))
       (sha256
        (base32 "06xs0ig337vq5a4apyws2qjjj4gwgr1bg1c5lmwssq83gr72s94r"))))
    (build-system ocaml-build-system)
    (arguments
     '(#:build-flags '("build" "--tests" "true")
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (native-inputs (list opam-installer
                         ocamlbuild
                         ocaml-topkg
                         ocaml-uucd
                         ocaml-uunf
                         ocaml-uutf))
    (home-page "https://erratique.ch/software/uucp")
    (synopsis "Unicode character properties for OCaml")
    (description
     "Uucp is an OCaml library providing efficient access to a
selection of character properties of the Unicode character database.")
    (license license:isc)))

(define-public ocaml-ocplib-endian
  (package
    (name "ocaml5-ocplib-endian")
    (version "1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OCamlPro/ocplib-endian/")
             (commit version)))
       (sha256
        (base32 "1klj4g451s7m5r8bxmwc1rpvngpqdm40csnx9smgc06pwy2fax2c"))
       (file-name (git-file-name name version))))
    (build-system dune-build-system)
    (native-inputs (list ocaml-cppo))
    (home-page "https://github.com/OCamlPro/ocplib-endian")
    (synopsis "Optimised functions to read and write int16/32/64 from strings
and bigarrays")
    (description
     "Optimised functions to read and write int16/32/64 from strings
and bigarrays, based on new primitives added in version 4.01.  It works on
strings, bytes and bigstring (Bigarrys of chars), and provides submodules for
big- and little-endian, with their unsafe counter-parts.")
    (license license:lgpl2.1)))

(define-public ocaml-lwt
  (package
    (name "ocaml5-lwt")
    (version "5.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocsigen/lwt")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cfmhw4nsnwba49p06l9fbnbcq75w9fd3kvrr615ihjc9frlmjsy"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "lwt"))
    (native-inputs (list ocaml-cppo pkg-config))
    (inputs (list glib))
    (propagated-inputs (list ocaml-mmap ocaml-ocplib-endian ocaml-result
                             ocaml-seq libev))
    (home-page "https://github.com/ocsigen/lwt")
    (synopsis "Cooperative threads and I/O in monadic style")
    (description
     "Lwt provides typed, composable cooperative threads.  These
make it easy to run normally-blocking I/O operations concurrently in a single
process.  Also, in many cases, Lwt threads can interact without the need for
locks or other synchronization primitives.")
    (license license:lgpl2.1)))

(define-public ocaml-ounit2
  (package
    (name "ocaml5-ounit2")
    (version "2.2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gildor478/ounit.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04c841hpk2yij370w30w3pis8nibnr28v74mpq2qz7z5gb8l07p1"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-lwt ocaml-stdlib-shims))
    (home-page "https://github.com/gildor478/ounit")
    (synopsis "Unit testing framework for OCaml")
    (description "OUnit2 is a unit testing framework for OCaml.  It is similar
to JUnit and other XUnit testing frameworks.")
    (license license:expat)))

(define-public ocaml-ounit
  (package
    (inherit ocaml-ounit2)
    (name "ocaml5-ounit")
    (arguments
     `(#:tests? #f ;tests have been observed to hang.
       #:phases (modify-phases %standard-phases
                  (replace 'install
                    (lambda _
                      (invoke "make" "install-ounit"
                              ,(string-append "version="
                                              (package-version ocaml-ounit2))))))))
    (propagated-inputs (list ocaml-ounit2))
    (home-page "http://ounit.forge.ocamlcore.org")
    (synopsis "Unit testing framework for OCaml")
    (description
     "Unit testing framework for OCaml.  It is similar to JUnit and
other XUnit testing frameworks.")
    (license license:expat)))

(define-public ocaml-camlp-streams
  (package
    (name "ocaml5-camlp-streams")
    (version "5.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/camlp-streams")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r3wvffkzyyk4als78akirxanzbib5hvc3kvwxpk36mlmc38aywh"))))
    (build-system dune-build-system)
    (home-page "https://github.com/ocaml/camlp-streams")
    (synopsis "Stream and Genlex libraries for use with Camlp4 and Camlp5")
    (description
     "This package provides two library modules:

@itemize
@item Stream: imperative streams, with in-place update and memoization of
the latest element produced.
@item Genlex: a small parameterized lexical analyzer producing streams of
tokens from streams of characters.
@end itemize

The two modules are designed for use with Camlp4 and Camlp5: The stream
patterns and stream expressions of Camlp4/Camlp5 consume and produce data of
type 'a Stream.t.  The Genlex tokenizer can be used as a simple lexical
analyzer for Camlp4/Camlp5-generated parsers.

The Stream module can also be used by hand-written recursive-descent parsers,
but is not very convenient for this purpose.

The Stream and Genlex modules have been part of the OCaml standard library for a
long time, and have been distributed as part of the core OCaml system.  They
will be removed from the OCaml standard library at some future point, but will
be maintained and distributed separately in the camlpstreams package.")
    (license license:lgpl2.1)))

(define-public ocaml-re
  (package
    (name "ocaml5-re")
    (version "1.10.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/ocaml-re")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g0vmpx6ylv8m0w77zarn215pgb4czc6gcpb2fi5da1s307zwr0w"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-seq))
    (native-inputs (list ocaml-ounit))
    (home-page "https://github.com/ocaml/ocaml-re/")
    (synopsis "Regular expression library for OCaml")
    (description
     "Pure OCaml regular expressions with:
@enumerate
@item Perl-style regular expressions (module Re_perl)
@item Posix extended regular expressions (module Re_posix)
@item Emacs-style regular expressions (module Re_emacs)
@item Shell-style file globbing (module Re_glob)
@item Compatibility layer for OCaml's built-in Str module (module Re_str)
@end enumerate")
    (license license:expat)))

(define-public ocaml-alcotest
  (package
    (name "ocaml5-alcotest")
    (version "1.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mirage/alcotest")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a8ljwmbm7yp9kvfpfg1153amg7f54gh8jnmv485bhs8am1m0w7c"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "alcotest"
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-test-format
                    (lambda _
                      ;; cmdliner changed the format and the tests fail
                      (substitute* "test/e2e/alcotest/failing/unknown_option.expected"
                        (("`")
                         "'")
                        (("\\.\\.\\.")
                         "…")))))))
    (native-inputs (list ocamlbuild))
    (propagated-inputs (list ocaml-astring
                             ocaml-cmdliner
                             ocaml-fmt
                             ocaml-re
                             ocaml-stdlib-shims
                             ocaml-uuidm
                             ocaml-uutf))
    (home-page "https://github.com/mirage/alcotest")
    (synopsis "Lightweight OCaml test framework")
    (description
     "Alcotest exposes simple interface to perform unit tests.  It
exposes a simple TESTABLE module type, a check function to assert test
predicates and a run function to perform a list of unit -> unit test callbacks.
Alcotest provides a quiet and colorful output where only faulty runs are fully
displayed at the end of the run (with the full logs ready to inspect), with a
simple (yet expressive) query language to select the tests to run.")
    (license license:isc)))

(define-public ocaml5.3-dune-bootstrap
  (package
    (name "ocaml5.3-dune")
    (version "3.19.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml/dune")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01ys792jnld5yihhyirwkk4jlqm59bk0vrqjvvk5xjn8pp26vryq"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f; require odoc
       #:make-flags ,#~(list "release"
                             (string-append "PREFIX=" #$output)
                             (string-append "LIBDIR=" #$output
                                            "/lib/ocaml/site-lib"))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p "src/dune")
             (invoke "./configure")
             #t)))))
    (home-page "https://github.com/ocaml/dune")
    (synopsis "OCaml build system")
    (description "Dune is a build system for OCaml.  It provides a consistent
experience and takes care of the low-level details of OCaml compilation.
Descriptions of projects, libraries and executables are provided in
@file{dune} files following an s-expression syntax.")
    (properties '((hidden? . #t)))
    (license license:expat)))

;; Use this alias for packages built with the default ocaml 5.x compiler.
(define-public dune ocaml5.3-dune-bootstrap)

(define-public ocaml5.0-dune-bootstrap
  (package
    (inherit ocaml5.3-dune-bootstrap)
    (name "ocaml5.0-dune-bootstrap")
    (arguments
     `(,@(package-arguments ocaml5.3-dune-bootstrap)
       #:ocaml ,ocaml-5.0
       #:findlib ,ocaml5.0-findlib))))

(define-public ocaml5.3-csexp
  (package
    (name "ocaml5.3-csexp")
    (version "1.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml-dune/csexp")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p3ajswxwc43cvpbmi7c897jhp9z7nlys1qic960cwgpgvfa95d4"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f ;FIXME: needs ppx_expect, but which version?
       #:dune ,ocaml5.3-dune-bootstrap
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'chmod
                    (lambda _
                      (for-each (lambda (file)
                                  (chmod file #o644))
                                (find-files "." ".*")) #t)))))
    (propagated-inputs (list ocaml5.3-result))
    (home-page "https://github.com/ocaml-dune/csexp")
    (synopsis "Parsing and printing of S-expressions in Canonical form")
    (description
     "This library provides minimal support for Canonical
S-expressions.  Canonical S-expressions are a binary encoding of
S-expressions that is super simple and well suited for communication
between programs.

This library only provides a few helpers for simple applications.  If
you need more advanced support, such as parsing from more fancy input
sources, you should consider copying the code of this library given
how simple parsing S-expressions in canonical form is.

To avoid a dependency on a particular S-expression library, the only
module of this library is parameterised by the type of S-expressions.")
    (license license:expat)))

;; Use this alias for packages built with the default ocaml 5.x compiler.
(define-public ocaml-csexp ocaml5.3-csexp)

(define-public ocaml5.3-dune-configurator
  (package
    (inherit ocaml5.3-dune-bootstrap)
    (name "ocaml5.3-dune-configurator")
    (build-system dune-build-system)
    (arguments
     `(#:package "dune-configurator"
       #:dune ,ocaml5.3-dune-bootstrap
       ;; require ppx_expect
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  ;; When building dune, these directories are normally removed after
                  ;; the bootstrap.
                  (add-before 'build 'remove-vendor
                    (lambda _
                      (delete-file-recursively "vendor/csexp")
                      (delete-file-recursively "vendor/pp"))))))
    (propagated-inputs (list ocaml5.3-csexp))
    (synopsis "Dune helper library for gathering system configuration")
    (description
     "Dune-configurator is a small library that helps writing
OCaml scripts that test features available on the system, in order to generate
config.h files for instance.  Among other things, dune-configurator allows one to:

@itemize
@item test if a C program compiles
@item query pkg-config
@item import #define from OCaml header files
@item generate config.h file
@end itemize")))

;; Use this version of `dune-configurator` for packages built with the default
;; ocaml 5.x compiler.
(define-public dune-configurator ocaml5.3-dune-bootstrap)

(define-public ocaml5.3-dune
  (package
    (inherit ocaml5.3-dune-bootstrap)
    (propagated-inputs (list ocaml5.3-dune-configurator))
    (properties '())))

(define-public ocaml5.3-findlib
  (package
    (name "ocaml5.3-findlib")
    (version "1.9.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/ocamlfind")
             (commit (string-append "findlib-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a0mxwf3mwl2n6ryqqsvcg73ly8ji0xi0xa2v1p02695495grkki"))))
    (build-system gnu-build-system)
    (native-inputs (list m4 ocaml-5.3))
    (arguments
     `(#:tests? #f ;no test suite
       #:parallel-build? #f
       #:make-flags (list "all" "opt")
       #:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (invoke "./configure"
                                "-bindir"
                                (string-append out "/bin")
                                "-config"
                                (string-append out "/etc/ocamfind.conf")
                                "-mandir"
                                (string-append out "/share/man")
                                "-sitelib"
                                (string-append out "/lib/ocaml/site-lib")
                                "-with-toolbox"))))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (invoke "make" "install"
                                (string-append "OCAML_CORE_STDLIB=" out
                                               "/lib/ocaml/site-lib"))))))))
    (home-page "http://projects.camlcity.org/projects/findlib.html")
    (synopsis "Management tool for OCaml libraries")
    (description
     "The \"findlib\" library provides a scheme to manage reusable software
components (packages), and includes tools that support this scheme.  Packages
are collections of OCaml modules for which metainformation can be stored.  The
packages are kept in the file system hierarchy, but with strict directory
structure.  The library contains functions to look the directory up that
stores a package, to query metainformation about a package, and to retrieve
dependency information about multiple packages.  There is also a tool that
allows the user to enter queries on the command-line.  In order to simplify
compilation and linkage, there are new frontends of the various OCaml
compilers that can directly deal with packages.")
    (license license:x11)))

(define-public ocaml5.0-findlib
  (package
    (inherit ocaml5.3-findlib)
    (name "ocaml5.0-findlib")
    (native-inputs
     (list m4 ocaml-5.0))))

(define-public ocaml-compiler-libs
  (package
    (name "ocaml5-compiler-libs")
    (version "0.17.0")
    (home-page "https://github.com/janestreet/ocaml-compiler-libs")
    (source
     (janestreet-git-origin "ocaml-compiler-libs" version
      "0cs3waqdnf5xv5cv5g2bkjypgqibwlxgkxd5ddmvj5g9d82vm821"))
    (build-system dune-build-system)
    (properties `((upstream-name . "ocaml-compiler-libs")))
    (synopsis "Compiler libraries repackaged")
    (description
     "This package simply repackages the OCaml compiler libraries
so they don't expose everything at toplevel.  For instance, @code{Ast_helper}
is now @code{Ocaml_common.Ast_helper}.")
    (license license:expat)))

(define-public ocaml-intrinsics-kernel
  (package
    (name "ocaml5-intrinsics-kernel")
    (version "0.17.1")
    (source
     (janestreet-git-origin "ocaml_intrinsics_kernel" version
      "1a85l2cns5g8vnxri1pxrx1zhs2r04bjl2sj2vfpcv9vs8k6pw6r"))
    (home-page "https://github.com/janestreet/ocaml_intrinsics_kernel")
    (build-system dune-build-system)
    (properties `((upstream-name . "ocaml_intrinsics_kernel")))
    (synopsis "Architecture-specific intrinsic operations")
    (description
     "Provides functions to invoke amd64 instructions (such as cmov, min/maxsd, popcnt)
when available, or compatible software implementation on other targets.
See also ocaml_intrinsics library.")
    (license license:expat)))

(define-public ocaml-jane-street-headers
  (package
    (name "ocaml5-jane-street-headers")
    (version "0.17.0")
    (home-page "https://github.com/janestreet/jane-street-headers")
    (source
     (janestreet-git-origin "jane-street-headers" version
      "0hq29ip8k7vyjrjm5hq9bq6b5cmssqlzcsaqi350sp39xg9bhilw"))
    (build-system dune-build-system)
    (synopsis "Jane Street C header files")
    (description "C header files shared between the various Jane Street
packages.")
    (license license:expat)))

(define-public ocaml-num
  (package
    (name "ocaml5-num")
    (version "1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/num")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vzdnvpj5dbj3ifx03v25pj2jj1ccav072v4d29pk1czdba2lzfc"))))
    (build-system dune-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-race
                    (lambda _
                      ;; There's a race between bng.o and bng_generic.c.  Both depend on
                      ;; the architecture specific bng.c, but only the latter declares
                      ;; the dependency.
                      (mkdir-p "_build/default/src")
                      (for-each (lambda (f)
                                  (copy-file f
                                             (string-append "_build/default/"
                                                            f)))
                                (find-files "src" "bng_.*\\.c")))))))
    (home-page "https://github.com/ocaml/num")
    (synopsis "Arbitrary-precision integer and rational arithmetic")
    (description
     "OCaml-Num contains the legacy Num library for
arbitrary-precision integer and rational arithmetic that used to be part of
the OCaml core distribution.")
    (license license:lgpl2.1+)))

(define-public ocaml-octavius
  (package
    (name "ocaml5-octavius")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml-doc/octavius")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c5m51xcn2jv42kjjpklr6g63sgx1k885wfdp1yr4wrmiaj9cbpx"))))
    (build-system dune-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'build 'make-writable
                    (lambda _
                      (for-each (lambda (file)
                                  (chmod file #o644))
                                (find-files "." ".")) #t)))))
    (properties `((upstream-name . "octavius")))
    (home-page "https://github.com/ocaml-doc/octavius")
    (synopsis "Ocamldoc comment syntax parser")
    (description
     "Octavius is a library to parse the `ocamldoc` comment syntax.")
    (license license:isc)))

(define-public ocaml-ppx-derivers
  (package
    (name "ocaml5-ppx-derivers")
    (version "1.2.1")
    (home-page "https://github.com/ocaml-ppx/ppx_derivers")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yqvqw58hbx1a61wcpbnl9j30n495k23qmyy2xwczqs63mn2nkpn"))))
    (build-system dune-build-system)
    (properties `((upstream-name . "ppx_derivers")))
    (synopsis "Shared @code{@@deriving} plugin registry")
    (description
     "Ppx_derivers is a tiny package whose sole purpose is to allow
ppx_deriving and ppx_type_conv to inter-operate gracefully when linked
as part of the same ocaml-migrate-parsetree driver.")
    (license license:bsd-3)))

(define-public ocaml-sexplib0
  (package
    (name "ocaml5-sexplib0")
    (version "0.17.0")
    (home-page "https://github.com/janestreet/sexplib0")
    (source
     (janestreet-git-origin "sexplib0" version
      "1s6bc7hj7zwrrz7m5c994h0zjx69af9lvx5ayjpg7dsy2h9g17a3"))
    (build-system dune-build-system)
    (synopsis "Base definition of S-expressions and some converters")
    (description
     "Part of Jane Street's Core library.  The Core suite of
libraries is an alternative to OCaml's standard library that was developed by
Jane Street.")
    (license license:expat)))

(define-public ocaml-sexplib
  (package
    (name "ocaml5-sexplib")
    (version "0.17.0")
    (home-page "https://github.com/janestreet/sexplib")
    (source
     (janestreet-git-origin "sexplib" version
      "0q2cgpvayczvcn1a2l765hc4d2ny3hb9yl65lhxrz4gc0q0wq50g"))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-base ocaml-num ocaml-parsexp ocaml-sexplib0))
    (synopsis "Library for serializing OCaml values to and from S-expressions")
    (description
     "This package is part of Jane Street's Core library.  Sexplib contains
functionality for parsing and pretty-printing s-expressions.")
    (license license:expat)))

(define-public ocaml-version
  (package
    (name "ocaml5-version")
    (version "3.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocurrent/ocaml-version")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pnw2ym021j48zknhbi1kdiyfv9si8p2l04rdzbv4g51fclsqs92"))))
    (build-system dune-build-system)
    (native-inputs (list ocaml-alcotest))
    (properties '((upstream-name . "ocaml-version")))
    (home-page "https://github.com/ocurrent/ocaml-version")
    (synopsis "Manipulate, parse and generate OCaml compiler version strings")
    (description
     "This library provides facilities to parse version numbers of the OCaml
compiler, and enumerates the various official OCaml releases and configuration
variants.")
    (license license:isc)))

(define-public ocaml-menhir
  (package
    (name "ocaml5-menhir")
    (version "20240715")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.inria.fr/fpottier/menhir.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0clg9w9a49wixdrk6y5r9kgwd8kfpz5qz6jzq3j4as5li40b297l"))))
    (build-system dune-build-system)
    (inputs (list ocaml))
    (home-page "https://gallium.inria.fr/~fpottier/menhir/")
    (synopsis "Parser generator")
    (description
     "Menhir is a parser generator.  It turns high-level grammar
specifications, decorated with semantic actions expressed in the OCaml
programming language into parsers, again expressed in OCaml.  It is based on
Knuth’s LR(1) parser construction technique.")
    ;; The file src/standard.mly and all files listed in src/mnehirLib.mlpack
    ;; that have an *.ml or *.mli extension are GPL licensed. All other files
    ;; are QPL licensed.
    (license (list license:gpl2+ license:qpl))))

(define-public ocaml-base
  (package
    (name "ocaml5-base")
    (version "0.17.3")
    (home-page "https://github.com/janestreet/base")
    (source
     (janestreet-git-origin "base" version
      "0yyd9cs6qf8bzk4cpga6hh0iiarhyl2kn15ar3jgqgfmg3p6bcyb"))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-intrinsics-kernel ocaml-sexplib0))
    (synopsis "Full standard library replacement for OCaml")
    (description
     "Base is a complete and portable alternative to the OCaml standard
library.  It provides all standard functionalities one would expect
from a language standard library.  It uses consistent conventions
across all of its module.

Base aims to be usable in any context.  As a result system dependent
features such as I/O are not offered by Base.  They are instead
provided by companion libraries such as
@url{https://github.com/janestreet/stdio, ocaml-stdio}.")
    (license license:expat)))

(define-public ocaml-parsexp
  (package
    (name "ocaml5-parsexp")
    (version "0.17.0")
    (home-page "https://github.com/janestreet/parsexp")
    (source
     (janestreet-git-origin "parsexp" version
      "1mfw44f41kiy3jxbrina3vqrgbi8hdw6l6xsjy8sfcybf3lxkal8"))
    (build-system dune-build-system)
    (inputs (list ocaml-sexplib0 ocaml-base))
    (synopsis "S-expression parsing library")
    (description
     "This library provides generic parsers for parsing S-expressions from
strings or other medium.

The library is focused on performances but still provide full generic
parsers that can be used with strings, bigstrings, lexing buffers,
character streams or any other sources effortlessly.

It provides three different class of parsers:
@itemize
@item
the normal parsers, producing [Sexp.t] or [Sexp.t list] values
@item
the parsers with positions, building compact position sequences so
that one can recover original positions in order to report properly
located errors at little cost
@item
the Concrete Syntax Tree parsers, produce values of type
@code{Parsexp.Cst.t} which record the concrete layout of the s-expression
syntax, including comments
@end itemize

This library is portable and doesn't provide IO functions.  To read
s-expressions from files or other external sources, you should use
parsexp_io.")
    (license license:expat)))

(define-public ocaml-stdio
  (package
    (name "ocaml5-stdio")
    (version "0.17.0")
    (home-page "https://github.com/janestreet/stdio")
    (source
     (janestreet-git-origin "stdio" version
      "1l3da9qri8d04440ps51j9ffh6bpk8j11mda4lidcndkmr94r19p"))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-base ocaml-sexplib0))
    (synopsis "Standard IO library for OCaml")
    (description
     "Stdio implements simple input/output functionalities for OCaml.  It
re-exports the input/output functions of the OCaml standard libraries using
a more consistent API.")
    (license license:expat)))

(define-public ocaml-crunch
  (package
    (name "ocaml5-crunch")
    (version "4.0.0")
    (home-page "https://github.com/mirage/ocaml-crunch")
    (source
     (origin
       (method url-fetch)
       (uri (string-append home-page
                           "/releases/download/v4.0.0/crunch-4.0.0.tbz"))
       (sha256
        (base32 "16xq8hlid8725qghkrjws0y718aaskvcdk7rn1666v7d548qv6wk"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs (list ocaml-cmdliner ocaml-ptime))
    (synopsis "Convert a filesystem into a static OCaml module")
    (description
     "`ocaml-crunch` takes a directory of files and compiles them into a standalone
OCaml module which serves the contents directly from memory.  This can be
convenient for libraries that need a few embedded files (such as a web server)
and do not want to deal with all the trouble of file configuration.")
    (license license:isc)))

(define-public ocaml-cinaps
  ;; This commit removes an unused dependency on ocaml-ppx-jane. It avoids a
  ;; dependency cycle between ocaml-ppx-jane and ocaml-ppxlib.
  (let ((commit "d974bb2db3ab1ab14e81f989b5bdb609462bff47")
        (revision "0"))
    (package
      (name "ocaml5-cinaps")
      (version (git-version "0.15.1" revision commit))
      (home-page "https://github.com/ocaml-ppx/cinaps")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "00kb04vqlnk1pynqjhna5qhn8790ab17baxf4na5py1l1h1js8qx"))))
      (build-system dune-build-system)
      (propagated-inputs (list ocaml-re))
      (synopsis "Trivial metaprogramming tool for OCaml")
      (description
       "Cinaps is a trivial Metaprogramming tool using the OCaml toplevel.  It is based
on the same idea as expectation tests.  The user writes some OCaml code inside
special comments and cinaps makes sure that what follows is what is printed by
the OCaml code.")
      (license license:expat))))

(define-public ocaml-ppxlib
  (package
    (name "ocaml5-ppxlib")
    (version "0.35.0")
    (home-page "https://github.com/ocaml-ppx/ppxlib")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q0vsik0mqk5f6gin7a8c1m1zk5172jy2sfz8mkl6hf13nlcc3s3"))))
    (build-system dune-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-test-format
                    (lambda _
                      ;; Since sexplib >= 0.15, error formatting has changed
                      (substitute* "test/driver/exception_handling/run.t"
                        (("\\(Failure ")
                         "Failure("))
                      (substitute* "test/base/test.ml"
                        (("Invalid_argument \\((.*)\\)." _ m)
                         (string-append "Invalid_argument " m "."))
                        (("\\(Invalid_argument (.*)\\)" _ m)
                         (string-append "Invalid_argument " m ".")))
                      (substitute* "test/ppx_import_support/test.ml"
                        (("\\(Failure")
                         "Failure")
                        (("  \"(Some ppx-es.*)\")" _ m)
                         (string-append " \"" m "\".")))))
                  (add-after 'fix-test-format 'fix-egrep
                    (lambda _
                      ;; egrep is obsolescent; using grep -E
                      (substitute* "test/expansion_context/run.t"
                        (("egrep")
                         "grep -E")))))))
    (propagated-inputs (list ocaml-compiler-libs ocaml-cmdliner
                             ocaml-ppx-derivers ocaml-sexplib0
                             ocaml-stdlib-shims))
    (native-inputs (list ocaml-stdio ocaml-cinaps ocaml-base))
    (synopsis "Base library and tools for ppx rewriters")
    (description
     "A comprehensive toolbox for ppx development.  It features:
@itemize
@item an OCaml AST / parser / pretty-printer snapshot, to create a full frontend
independent of the version of OCaml;
@item a library for library for ppx rewriters in general, and type-driven code
generators in particular;
@item
a feature-full driver for OCaml AST transformers;
@item a quotation mechanism allowing to write values representing the
OCaml AST in the OCaml syntax;
@item a generator of open recursion classes from type definitions.
@end itemize")
    (license license:expat)))

(define-public ocaml-bisect-ppx
  (package
    (name "ocaml5-bisect-ppx")
    (version "2.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aantron/bisect_ppx")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1albx01qvr4fax4wkdmy0dd21q0fd9ixsgsvnr1z32ngj9nyi9fy"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-ppxlib ocaml-cmdliner))
    (arguments
     ;; Tests require ocamlformat which would lead to circular dependencies
     '(#:tests? #f))
    (properties `((upstream-name . "bisect_ppx")))
    (home-page "https://github.com/aantron/bisect_ppx")
    (synopsis "Code coverage for OCaml")
    (description
     "Bisect_ppx helps you test thoroughly.  It is a small
preprocessor that inserts instrumentation at places in your code, such as
if-then-else and match expressions.  After you run tests, Bisect_ppx gives a
nice HTML report showing which places were visited and which were missed.

Usage is simple - add package bisect_ppx when building tests, run your tests,
then run the Bisect_ppx report tool on the generated visitation files.")
    (license license:mpl2.0)))

(define-public ocaml-ppx-cold
  (package
    (name "ocaml5-ppx-cold")
    (version "0.17.0")
    (home-page "https://github.com/janestreet/ppx_cold")
    (source
     (janestreet-git-origin "ppx_cold" version
      "1l0gg8dyjawb71nz6w4r3svi0jbjk0qlmw9r3bzb0jylqsanlmkw"))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-base ocaml-ppxlib))
    (properties `((upstream-name . "ppx_cold")))
    (synopsis "Syntax extension for indicating cold path")
    (description
     "This package contains an syntax extension to indicate that the code is
on the cold path and should be kept out of the way to avoid polluting the
instruction cache on the hot path.  See also
https://github.com/ocaml/ocaml/issues/8563.")
    (license license:expat)))

(define-public ocaml-ppx-here
  (package
    (name "ocaml5-ppx-here")
    (version "0.17.0")
    (source
     (janestreet-git-origin "ppx_here" version
      "1hr6ymfkz5xhsciia8bi23mnlx94h4345njp9r7k9f1nzxr0xg69"))
    (build-system dune-build-system)
    (arguments
     ;; broken tests
     `(#:tests? #f))
    (propagated-inputs (list ocaml-base ocaml-ppxlib))
    (properties `((upstream-name . "ppx_here")))
    (home-page "https://github.com/janestreet/ppx_here")
    (synopsis "Expands [%here] into its location")
    (description "Part of the Jane Street's PPX rewriters collection.")
    (license license:expat)))

(define-public ocaml-ppx-optcomp
  (package
    (name "ocaml5-ppx-optcomp")
    (version "0.17.0")
    (home-page "https://github.com/janestreet/ppx_optcomp")
    (source
     (janestreet-git-origin "ppx_optcomp" version
      "0287r4sqv752wsyx6k04kxw61wvj5y0xj66cj68q3x3i2b717nhz"))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-base ocaml-stdio ocaml-ppxlib))
    (properties `((upstream-name . "ppx_optcomp")))
    (synopsis "Optional compilation for OCaml")
    (description
     "Ppx_optcomp stands for Optional Compilation.  It is a tool
used to handle optional compilations of pieces of code depending of the word
size, the version of the compiler, ...")
    (license license:expat)))

(define-public ocaml-ppxlib-jane
  (package
    (name "ocaml5-ppxlib-jane")
    (version "0.17.2")
    (source
     (janestreet-git-origin "ppxlib_jane" version
      "0abnhxpvb0sykaifi9qygyq25sqhsm8z3wcj3gd9zsj5mds540h1"))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-ppxlib))
    (properties `((upstream-name . "ppx_jane")))
    (home-page "https://github.com/janestreet/ppxlib_jane")
    (synopsis "Utilities for working with Jane Street AST constructs")
    (description "Part of the Jane Street's PPX rewriters collection.")
    (license license:expat)))

(define-public ocaml-tyxml
  (package
    (name "ocaml5-tyxml")
    (version "4.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocsigen/tyxml")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bh66wknc7sx2r63kscp0hg6h73dkv6qpkx0cdz2qp7p28pg2ixz"))))
    (build-system dune-build-system)
    (inputs (list ocaml-re ocaml-seq ocaml-uutf))
    (native-inputs (list ocaml-alcotest))
    (arguments
     `(#:package "tyxml"))
    (home-page "https://github.com/ocsigen/tyxml/")
    (synopsis "TyXML is a library for building correct HTML and SVG documents")
    (description
     "TyXML provides a set of convenient combinators that uses the
OCaml type system to ensure the validity of the generated documents.  TyXML can
be used with any representation of HTML and SVG: the textual one, provided
directly by this package, or DOM trees (@code{js_of_ocaml-tyxml}) virtual DOM
(@code{virtual-dom}) and reactive or replicated trees (@code{eliom}).  You can
also create your own representation and use it to instantiate a new set of
combinators.")
    (license license:lgpl2.1)))

(define-public ocaml-yojson
  (package
    (name "ocaml5-yojson")
    (version "2.0.2")
    (home-page "https://github.com/ocaml-community/yojson")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1habsh00ihjhk1g1csxqg3hj8izk5zvgc7wm579wyjw35vzcmwr1"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "yojson"))
    (propagated-inputs (list ocaml-seq))
    (native-inputs (list ocaml-alcotest ocaml-cppo))
    (synopsis "Low-level JSON library for OCaml")
    (description
     "Yojson is an optimized parsing and printing library for the
JSON format.  It addresses a few shortcomings of json-wheel including 2x
speedup, polymorphic variants and optional syntax for tuples and variants.
@code{ydump} is a pretty printing command-line program provided with the
yojson package.  The program @code{atdgen} can be used to derive OCaml-JSON
serializers and deserializers from type definitions.")
    (license license:bsd-3)))

(define-public ocaml-ppx-compare
  (package
    (name "ocaml5-ppx-compare")
    (version "0.17.0")
    (source
     (janestreet-git-origin "ppx_compare" version
      "13g1g0f8z40yjiipwp07rsi6wp2mhq5hhdn0z5jq1l6sqvsw21dq"))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-base ocaml-ppxlib ocaml-ppxlib-jane))
    (properties `((upstream-name . "ppx_compare")))
    (home-page "https://github.com/janestreet/ppx_compare")
    (synopsis "Generation of comparison functions from types")
    (description
     "Generation of fast comparison functions from type expressions
and definitions.  Ppx_compare is a ppx rewriter that derives comparison functions
from type representations.  The scaffolded functions are usually much faster
than ocaml's Pervasives.compare.  Scaffolding functions also gives you more
flexibility by allowing you to override them for a specific type and more safety
by making sure that you only compare comparable values.")
    (license license:expat)))


(define-public ocaml-ppx-enumerate
  (package
    (name "ocaml5-ppx-enumerate")
    (version "0.17.0")
    (source
     (janestreet-git-origin
      "ppx_enumerate" version
      "1vkn3ii16974p68n97187wz062ksp9al3nmxy1jdsywzkp36p832"))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml-base ocaml-ppxlib ocaml-ppxlib-jane))
    (properties `((upstream-name . "ppx_enumerate")))
    (home-page "https://github.com/janestreet/ppx_enumerate")
    (synopsis "Generate a list containing all values of a finite type")
    (description "Ppx_enumerate is a ppx rewriter which generates a definition
for the list of all values of a type (for a type which only has finitely
many values).")
    (license license:expat)))

(define-public ocaml-ppx-globalize
  (package
   (name "ocaml5-ppx-globalize")
   (version "0.17.0")
   (source
    (janestreet-git-origin
     "ppx_globalize" version
     "0bv2bc70fmc2k8gqnk2gif66vhvblkfq9knwvkkhbs4zyv6pk99c"))
   (build-system dune-build-system)
   (propagated-inputs (list ocaml-base ocaml-ppxlib-jane ocaml-ppxlib))
   (properties `((upstream-name . "ppx_globalize")))
   (home-page "https://github.com/janestreet/ppx_globalize")
   (synopsis "Generates functions to copy local values to the global heap")
   (description "Part of the Jane Street's PPX rewriters collection.")
   (license license:expat)))

(define-public ocaml-ppx-js-style
  (package
    (name "ocaml5-ppx-js-style")
    (version "0.17.0")
    (source
     (janestreet-git-origin "ppx_js_style" version
      "0iciy8d1dh97q0fls0jlacx2y9clidlakpkiprv2cbqvxv2p6d7f"))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-base ocaml-octavius ocaml-ppxlib))
    (properties `((upstream-name . "ppx_js_style")))
    (home-page "https://github.com/janestreet/ppx_js_style")
    (synopsis "Code style checker for Jane Street Packages")
    (description
     "This package is a no-op ppx rewriter.  It is used as a
@code{lint} tool to enforce some coding conventions across all Jane Street
packages.")
    (license license:expat)))

(define-public ocaml-ppx-sexp-conv
  (package
    (name "ocaml5-ppx-sexp-conv")
    (version "0.17.0")
    (home-page "https://github.com/janestreet/ppx_sexp_conv")
    (source
     (janestreet-git-origin "ppx_sexp_conv" version
      "1gdgwxcwrzdkw3pm3azqqygk5kvqphffpz4j6ask4f0jkliv8j45"))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-base ocaml-ppxlib ocaml-ppxlib-jane))
    (properties `((upstream-name . "ppx_sexp_conv")))
    (synopsis
     "@code{[@@deriving]} plugin to generate S-expression conversion functions")
    (description "Part of the Jane Street's PPX rewriters collection.")
    (license license:expat)))

(define-public ocaml-ppx-hash
  (package
    (name "ocaml5-ppx-hash")
    (version "0.17.0")
    (source
     (janestreet-git-origin "ppx_hash" version
      "0zxb1n9zx4k44hilibdgasrq45y965ywx7h8pij3c6knh4pc400q"))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-base ocaml-ppx-compare ocaml-ppx-sexp-conv
                             ocaml-ppxlib))
    (properties `((upstream-name . "ppx_hash")))
    (home-page "https://github.com/janestreet/ppx_hash")
    (synopsis
     "Generation of hash functions from type expressions and definitions")
    (description "This package is a collection of ppx rewriters that generate
hash functions from type exrpessions and definitions.")
    (license license:expat)))

(define-public ocaml-ppx-assert
  (package
    (name "ocaml5-ppx-assert")
    (version "0.17.0")
    (source
     (janestreet-git-origin "ppx_assert" version
      "1h0gynscd3d9vdx1rf6cf281cn8sw3gxp6z5vl4smypsa5sb1p53"))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-base
                             ocaml-ppx-cold
                             ocaml-ppx-compare
                             ocaml-ppx-here
                             ocaml-ppx-sexp-conv
                             ocaml-ppxlib))
    (properties `((upstream-name . "ppx_assert")))
    (home-page "https://github.com/janestreet/ppx_assert")
    (synopsis
     "Assert-like extension nodes that raise useful errors on failure")
    (description "This package contains assert-like extension nodes that raise
useful errors on failure.")
    (license license:expat)))

(define-public ocaml-markup
  (package
    (name "ocaml5-markup")
    (version "1.0.3")
    (home-page "https://github.com/aantron/markup.ml")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append home-page ".git"))
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1acgcbhx4rxx92rf65lsns588d6zzfrin2pnpkx24jw5vbgz7idn"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "markup"))
    (propagated-inputs (list ocaml-bisect-ppx ocaml-uchar ocaml-uutf ocaml-lwt))
    (native-inputs (list ocaml-ounit2 pkg-config))
    (synopsis "Error-recovering functional HTML5 and XML parsers and writers")
    (description
     "Markup.ml provides an HTML parser and an XML parser.  The
parsers are wrapped in a simple interface: they are functions that transform
byte streams to parsing signal streams.  Streams can be manipulated in various
ways, such as processing by fold, filter, and map, assembly into DOM tree
structures, or serialization back to HTML or XML.

Both parsers are based on their respective standards.  The HTML parser, in
particular, is based on the state machines defined in HTML5.

The parsers are error-recovering by default, and accept fragments.  This makes
it very easy to get a best-effort parse of some input.  The parsers can,
however, be easily configured to be strict, and to accept only full documents.

Apart from this, the parsers are streaming (do not build up a document in
memory), non-blocking (can be used with threading libraries), lazy (do not
consume input unless the signal stream is being read), and process the input in
a single pass.  They automatically detect the character encoding of the input
stream, and convert everything to UTF-8.")
    (license license:bsd-3)))

(define-public ocaml-jst-config
  (package
    (name "ocaml5-jst-config")
    (version "0.17.0")
    (source
     (janestreet-git-origin "jst-config" version
      "1dy345p6825wyhpv6drlrl9gqwcgx341a5k3pnvfnxpcc6mkw167"))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-base ocaml-ppx-assert ocaml-stdio
                             dune-configurator))
    (home-page "https://github.com/janestreet/jst-config")
    (synopsis "Compile-time configuration for Jane Street libraries")
    (description
     "Defines compile-time constants used in Jane Street libraries
such as Base, Core, and Async.  This package has an unstable interface; it is
intended only to share configuration between different packages from Jane
Street.  Future updates may not be backward-compatible, and we do not
recommend using this package directly.")
    (license license:expat)))

(define-public ocaml-ppx-base
  (package
    (name "ocaml5-ppx-base")
    (version "0.17.0")
    (source
     (janestreet-git-origin "ppx_base" version
      "14lvhy842fjjm2qwqhxkqig4mc5s439rbkd87mlys86byzrdrkpy"))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-ppx-compare
                             ocaml-ppx-cold
                             ocaml-ppx-enumerate
                             ocaml-ppx-globalize
                             ocaml-ppx-hash
                             ocaml-ppx-js-style
                             ocaml-ppx-sexp-conv
                             ocaml-ppxlib))
    (properties `((upstream-name . "ppx_base")))
    (home-page "https://github.com/janestreet/ppx_base")
    (synopsis "Base set of ppx rewriters")
    (description
     "Ppx_base is the set of ppx rewriters used for Base.

Note that Base doesn't need ppx to build, it is only used as a
verification tool.")
    (license license:expat)))

(define-public ocaml-time-now
  (package
    (name "ocaml5-time-now")
    (version "0.17.0")
    (source
     (janestreet-git-origin "time_now" version
      "1abn5fqqixlj1jbqb6vwysn48m0fv9cp7jyw5nfkkyxivw9xccvd"))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-base ocaml-jane-street-headers
                             ocaml-jst-config ocaml-ppx-base ocaml-ppx-optcomp))
    (properties `((upstream-name . "time_now")))
    (home-page "https://github.com/janestreet/time_now")
    (synopsis "Reports the current time")
    (description
     "Provides a single function to report the current time in nanoseconds
since the start of the Unix epoch.")
    (license license:expat)))

(define-public ocaml-ppx-inline-test
  (package
    (name "ocaml5-ppx-inline-test")
    (version "0.17.0")
    (home-page "https://github.com/janestreet/ppx_inline_test")
    (source
     (janestreet-git-origin "ppx_inline_test" version
      "0azhkx19srpjl748zznrhyzifhkij5h3477mp0dwbp2k16c6pmx4"))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f)) ;see home page README for further information
    (propagated-inputs (list ocaml-base
                             ocaml-compiler-libs
                             ocaml-sexplib0
                             ocaml-stdio
                             ocaml-ppxlib
                             ocaml-time-now))
    (properties `((upstream-name . "ppx_inline_test")))
    (synopsis "Syntax extension for writing in-line tests in ocaml code")
    (description
     "This package contains a syntax extension for writing
in-line tests in ocaml code.  It is part of Jane Street's PPX rewriters
collection.")
    (license license:expat)))

(define-public ocaml-ppx-expect
  (package
    (name "ocaml5-ppx-expect")
    (version "0.17.2")
    (source
     (janestreet-git-origin "ppx_expect" version
      "0nimi4zqlw816j2hbxljqhv8s3kdf8mncdj4474234i1xgzngbwx"))
    (build-system dune-build-system)
    (arguments
     ;; Cyclic dependency with ocaml-ppx-jane
     `(#:tests? #f))
    (propagated-inputs (list ocaml-base
                             ocaml-ppx-here
                             ocaml-ppx-inline-test
                             ocaml-stdio
                             ocaml-ppxlib
                             ocaml-re))
    (properties `((upstream-name . "ppx_expect")))
    (home-page "https://github.com/janestreet/ppx_expect")
    (synopsis "Cram like framework for OCaml")
    (description
     "Expect-test is a framework for writing tests in OCaml, similar
to Cram.  Expect-tests mimics the existing inline tests framework with the
@code{let%expect_test} construct.  The body of an expect-test can contain
output-generating code, interleaved with @code{%expect} extension expressions
to denote the expected output.")
    (license license:expat)))

(define %ocaml-odoc-base
  (package
    (name "ocaml5-odoc-base")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/odoc")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0azxv64jfgncq11ys9li9mn2vc3s5a5k48pbnrj3qmaj2xzkslbw"))))
    (build-system dune-build-system)
  (home-page "https://github.com/ocaml/odoc")
    (synopsis "OCaml documentation generator")
    (description "Odoc is a documentation generator for OCaml.  It reads
@emph{doc comments}, delimited with @code{(** ... *)}, and outputs
@acronym{HTML}.

Text inside doc comments is marked up in ocamldoc syntax.  Odoc's main
advantage over ocamldoc is an accurate cross-referencer, which handles the
complexity of the OCaml module system.")
  (license license:isc)))

(define-public ocaml-odoc-parser
  (package
    (inherit %ocaml-odoc-base)
    (name "ocaml5-odoc-parser")
    (arguments
     `(#:package "odoc-parser"))
    (propagated-inputs (list ocaml-astring ocaml-camlp-streams ocaml-fpath
                             ocaml-result ocaml-uutf))
    (native-inputs (list ocaml-ppx-expect ocaml-cppo ocaml-tyxml))
    (home-page "https://github.com/ocaml/odoc")
    (synopsis "Parser for ocaml documentation comments")
    (description
     "This package provides a library for parsing the contents of OCaml
documentation comments, formatted using Odoc syntax, an extension of the
language understood by ocamldoc.")))

(define-public ocaml-odoc
  (package
    (inherit %ocaml-odoc-base)
    (name "ocaml5-odoc")
    (arguments
     `(#:package "odoc"
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-test
                    (lambda _
                      ;; test results expects #!/bin/sh but gets a store path instead
                      (substitute* "test/xref2/with.t/run.t"
                        (("#!/bin/sh")
                         (string-append "#!"
                                        (which "sh")))))))))
    (inputs (list ocaml-astring
                  ocaml-bisect-ppx
                  ocaml-cmdliner
                  ocaml-crunch
                  ocaml-fmt
                  ocaml-fpath
                  ocaml-logs
                  ocaml-odoc-parser
                  ocaml-re
                  ocaml-result
                  ocaml-sexplib
                  ocaml-tyxml))
    (native-inputs (list ocaml-alcotest
                         ocaml-bos
                         ocaml-cppo
                         ocaml-lwt
                         ocaml-markup
                         ocaml-menhir
                         ocaml-ppx-expect
                         ocaml-version
                         ocaml-yojson
                         jq))))

(define-public ocaml-graphics
  (package
    (name "ocaml5-graphics")
    (version "5.1.2")
    (home-page "https://github.com/ocaml/graphics")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q20f8y6ijxbvzik2ns4yl3w54q5z8kd0pby8i8c64a04hvly08m"))))
    (build-system dune-build-system)
    (propagated-inputs (list libx11))
    (synopsis "Portable drawing primatives")
    (description
     "The graphics library provides a set of portable drawing primitives.
Drawing takes place in a separate window that is created when
Graphics.open_graph is called.  This library used to be distributed with OCaml
up to OCaml 4.08.")
    (license license:lgpl2.1+)))

(define-public ocaml-graph
  (package
    (name "ocaml5-graph")
    (version "2.2.0")
    (home-page "https://github.com/backtracking/ocamlgraph/")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url home-page)
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02f4iyrnnhi7kam7qrnny76vbdh1q68748bcrq02cy8wa79chp3r"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "ocamlgraph"))
    (native-inputs (list ocaml-graphics))
    (properties `((upstream-name . "ocamlgraph")))
    (synopsis "Generic graph data structures for OCaml")
    (description
     "This library defines a set of graph data structures and algorithms
to operate on them.")
    (license license:lgpl2.1)))

(define-public ocaml-extlib
  (package
    (name "ocaml5-extlib")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://ygrek.org/p/release/ocaml-extlib/"
                           "extlib-" version ".tar.gz"))
       (sha256
        (base32 "0w2xskv8hl0fwjri68q5bpf6n36ab4fp1q08zkfqw2i807q7fhln"))))
    (build-system dune-build-system)
    (arguments
     (list
      #:package "extlib"))
    (native-inputs (list ocaml-cppo))
    (home-page "https://github.com/ygrek/ocaml-extlib")
    (synopsis "Complete and small extension for OCaml standard library")
    (description
     "This library adds new functions to OCaml standard library
modules, modifies some functions in order to get better performances or
safety (tail-recursive) and also provides new modules which should be useful
for day to day programming.")
    ;; With static-linking exception
    (license license:lgpl2.1+)))

(define-public ocaml-0install-solver
  (package
    (name "ocaml5-0install-solver")
    (version "2.18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/0install/0install")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hm72k355qwgh16hngmnd77bgawf20ipnqxfncdzl10rqrc0640b"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "0install-solver"))
    (native-inputs (list ocaml-ounit2))
    (home-page "https://docs.0install.net/developers/solver/")
    (synopsis "Package dependency solver")
    (description
     "This package provides a package dependency resolver based on a SAT solver.  This
was originally written for the 0install package manager, but is now generic and
is also used as a solver backend for opam.  The SAT solver is based on
@code{MiniSat} (http://minisat.se/Papers.html) and the application to package
management is based on OPIUM (Optimal Package Install/Uninstall Manager).
0install-solver uses a (novel?) strategy to find the optimal solution extremely
quickly (even for a SAT-based solver).")
    (license license:lgpl2.1+)))

(define-public ocaml-cudf
  (package
    (name "ocaml5-cudf")
    (version "0.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/irill/cudf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lvrmpscbk1kjv5ag5bzlzv520xk5zw2haf6q7chvz98gcm9g0hk"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-extlib))
    (native-inputs (list ocaml-ounit2))
    (home-page "https://www.mancoosi.org/cudf/")
    (synopsis "CUDF library (part of the Mancoosi tools)")
    (description
     "@acronym{CUDF, Common Upgradeability Description Format} is a format for
describing upgrade scenarios in package-based software distributions.")
    ;; With static-linking exception
    (license license:lgpl2.1+)))

(define-public ocaml-mccs
  (package
    (name "ocaml5-mccs")
    (version "1.1+19")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AltGr/ocaml-mccs")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x1y5rhj4f0xakbgfn9f90a9xy09v99p8mc42pbnam5kghyjmxy6"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-cudf))
    (home-page "https://www.i3s.unice.fr/~cpjm/misc/")
    (synopsis "Upgrade path problem solver")
    (description
     "Mccs (Multi Criteria CUDF Solver) is a CUDF problem solver.
Mccs take as input a CUDF problem and computes the best solution according to
a set of criteria.  It relies on a Integer Programming solver or a
Pseudo Boolean solver to achieve its task.  Mccs can use a wide set of
underlying solvers like Cplex, Gurobi, Lpsolver, Glpk, CbC, SCIP or WBO.")
    (license (list license:bsd-3 license:gpl3+
                   ;; With static-linking exception
                   license:lgpl2.1+))))

(define-public ocaml-opam-0install-cudf
  (package
    (name "ocaml5-opam-0install-cudf")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml-opam/opam-0install-cudf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12v1bgnxcxdylgxbsjlcr90rzwcp39rjlv191cy8g2s33nyxyi2c"))))

    (build-system dune-build-system)
    (propagated-inputs (list ocaml-cudf ocaml-0install-solver))
    (native-inputs (list ocaml-alcotest))
    (home-page "https://github.com/ocaml-opam/opam-0install-cudf")
    (synopsis "Opam solver using 0install backend using the CUDF interface")
    (description
     "Opam's default solver is designed to maintain a set of packages over time,
minimising disruption when installing new programs and finding a compromise
solution across all packages.  In many situations (e.g. CI, local roots or
duniverse builds) this is not necessary, and we can get a solution much faster
by using a different algorithm.  This package provides a generic solver library
which uses 0install's solver library.  The library uses the CUDF library in
order to interface with opam as it is the format common used to talk to all the
supported solvers.")
    (license license:isc)))

(define-public ocaml-base64
  (package
    (name "ocaml5-base64")
    (version "3.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mirage/ocaml-base64")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jq349jp663hq51a941afr2y4yyh34r19zsxla73ks9bywj4mm2q"))))
    (build-system dune-build-system)
    (native-inputs (list ocaml-alcotest ocaml-bos ocaml-rresult))
    (home-page "https://github.com/mirage/ocaml-base64")
    (synopsis "Base64 encoding for OCaml")
    (description
     "Base64 is a group of similar binary-to-text encoding schemes
that represent binary data in an ASCII string format by translating it into a
radix-64 representation.  It is specified in RFC 4648.")
    (license license:isc)))

(define-public ocaml-dose3
  (package
    (name "ocaml5-dose3")
    (version "7.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/irill/dose3")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hcjh68svicap7j9bghgkp49xa12qhxa1pygmrgc9qwm0m4dhirb"))))
    (build-system dune-build-system)
    (arguments `(#:package "dose3"))
    (propagated-inputs (list ocaml-extlib
                             ocaml-base64
                             ocaml-cudf
                             ocaml-graph
                             ocaml-re
                             ocaml-stdlib-shims))
    (native-inputs (list ocaml-ounit))
    (home-page "https://www.mancoosi.org/software/")
    (synopsis "Package distribution management framework")
    (description "Dose3 is a framework made of several OCaml libraries for
managing distribution packages and their dependencies.  Though not tied to
any particular distribution, dose3 constitutes a pool of libraries which
enable analyzing packages coming from various distributions.  Besides basic
functionalities for querying and setting package properties, dose3 also
implements algorithms for solving more complex problems such as monitoring
package evolutions, correct and complete dependency resolution and
repository-wide uninstallability checks.")
    ;; with static-linking exception
    (license license:lgpl2.1+)))

(define-public ocaml-sha
  (package
    (name "ocaml5-sha")
    (version "1.15.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/djs55/ocaml-sha")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fpvfa522zx0rb5n7y3agklp4acpwbr430zg4qmfx1vwsxyx44mc"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-stdlib-shims ocaml-odoc))
    (native-inputs (list ocaml-ounit2))
    (home-page "https://github.com/djs55/ocaml-sha")
    (synopsis "OCaml binding to the SHA cryptographic functions")
    (description
     "This is the binding for SHA interface code in OCaml, offering the same
interface as the MD5 digest included in the OCaml standard library.  It
currently provides SHA1, SHA256 and SHA512 hash functions.")
    (license license:isc)))

(define-public ocaml-patch
  (package
    (name "ocaml5-patch")
    (version "v3.0.0")
    (build-system dune-build-system)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hannesm/patch")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fpxkc84g98ar321dl1fzr4cqbz990acj03n80pwg9y62x9mx2aq"))))
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/hannesm/patch")
    (synopsis "Applies unified diffs in pure OCaml")
    (description
     "Applies unified diffs in pure OCaml.

The loosely specified diff file format is widely used for transmitting
differences of line-based information.  The motivating example is opam, which
is able to validate updates being cryptographically signed (e.g. conex) by
providing a unified diff.")
    (license license:isc)))

(define-public ocaml-swhid-core
  (package
    (name "ocaml5-swhid-core")
    (version "0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OCamlPro/swhid_core")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h3zndyk94lf2lakc3cb8b7a00jqh0y1m8xk6mg61gj2kdpdbfdq"))))
    (build-system dune-build-system)
    (properties `((upstream-name . "swhid_core")))
    (home-page "https://github.com/ocamlpro/swhid_core")
    (synopsis "OCaml library to work with swhids")
    (description
     "swhid_core is an OCaml library to work with Software Heritage persistent
identifiers (swhids).  This is the core library, for most use cases you should
use the swhid library instead.")
    (license license:isc)))

(define-public ocaml-spdx-licenses
  (package
    (name "ocaml5-spdx-licenses")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kit-ty-kate/spdx_licenses")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08vh9mfkq34f0l5ka94hhklc43kvz09wd04x1gkxkawn4786rf9z"))))
    (build-system dune-build-system)
    (native-inputs (list ocaml-alcotest))
    (properties `((upstream-name . "spdx_licenses")))
    (home-page "https://github.com/kit-ty-kate/spdx_licenses")
    (synopsis "Strict SPDX License Expression parser")
    (description
     "This package provides an up-to-date and strict SPDX License Expression
parser.  It implements the format described in:
https://spdx.github.io/spdx-spec/v2.3/SPDX-license-expressions/
See https://spdx.org/licenses/ for more details.")
    (license license:expat)))

(define ocaml-opam-core
  (package
    (name "ocaml5-opam-core")
    (version "2.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/opam")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0php0b31cwyabhds477abk8qyz4whl3kncpbka4dynzpaf9xnqsm"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "opam-core"
       ;; tests are run with the opam package
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'pre-build
                    (lambda* (#:key inputs make-flags #:allow-other-keys)
                      (let ((bash (assoc-ref inputs "bash"))
                            (bwrap (search-input-file inputs "/bin/bwrap")))
                        (substitute* "src/core/opamSystem.ml"
                          (("\"/bin/sh\"")
                           (string-append "\"" bash "/bin/sh\""))
                          (("getconf")
                           (which "getconf")))))))))
    (propagated-inputs (list ocaml-graph
                             ocaml-re
                             ocaml-patch
                             ocaml-uutf
                             ocaml-cppo
                             ocaml-swhid-core
                             ocaml-jsonm
                             ocaml-cmdliner
                             ocaml-sha))
    (inputs (list bubblewrap ocaml-patch ocaml-uutf))
    (home-page "https://opam.ocamlpro.com/")
    (synopsis "Package manager for OCaml")
    (description
     "OPAM is a tool to manage OCaml packages.  It supports multiple
simultaneous compiler installations, flexible package constraints, and a
Git-friendly development workflow.")
    ;; The 'LICENSE' file waives some requirements compared to LGPLv3.
    (license license:lgpl3)))

(define ocaml-opam-format
  (package
    (inherit ocaml-opam-core)
    (name "ocaml5-opam-format")
    (inputs '())
    (propagated-inputs (list ocaml-opam-core ocaml-opam-file-format ocaml-re))
    (arguments
     `(#:package "opam-format"
       ;; tests are run with the opam package
       #:tests? #f
       #:phases %standard-phases))))

(define-public opam-installer
  (package
    (inherit ocaml-opam-core)
    (name "ocaml5-opam-installer")
    (native-inputs (list ocaml-opam-format ocaml-cmdliner))
    (inputs '())
    (propagated-inputs '())
    (arguments
     `(#:package "opam-installer"
       ;; requires all of opam
       #:tests? #f))
    (synopsis "Tool for installing OCaml packages")
    (description
     "@var{opam-installer} is a tool for installing OCaml packages
based on @code{.install} files defined by the OPAM package manager.  It is
useful for installing OCaml packages without requiring the entirety of
OPAM.")))

(define ocaml-opam-repository
  (package
    (inherit ocaml-opam-core)
    (name "ocaml5-opam-repository")
    (inputs '())
    (propagated-inputs (list ocaml-opam-format))
    (arguments
     `(#:package "opam-repository"
       ;; tests are run with the opam package
       #:tests? #f
       #:phases %standard-phases))))

(define ocaml-opam-state
  (package
    (inherit ocaml-opam-core)
    (name "ocaml5-opam-state")
    (arguments
     `(#:package "opam-state"
       ;; tests are run with the opam package
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'pre-build
                    (lambda* (#:key inputs make-flags #:allow-other-keys)
                      (let ((bwrap (search-input-file inputs "/bin/bwrap")))
                        ;; Use bwrap from the store directly.
                        (substitute* "src/state/shellscripts/bwrap.sh"
                          (("-v bwrap")
                           (string-append "-v " bwrap))
                          (("exec bwrap")
                           (string-append "exec " bwrap))
                          ;; Mount /gnu and /run/current-system in the
                          ;; isolated environment when building with opam.
                          ;; This is necessary for packages to find external
                          ;; dependencies, such as a C compiler, make, etc...
                          (("^add_sys_mounts /usr")
                           (string-append "add_sys_mounts "
                                          (%store-directory)
                                          " /run/current-system /usr")))))))))
    (inputs (list bubblewrap ocaml-spdx-licenses))
    (propagated-inputs (list ocaml-opam-repository))))

(define ocaml-opam-solver
  (package
    (inherit ocaml-opam-core)
    (name "ocaml5-opam-solver")
    (inputs '())
    (propagated-inputs (list ocaml-opam-format ocaml-mccs
                             ocaml-opam-0install-cudf ocaml-dose3))
    (arguments
     `(#:package "opam-solver"
       ;; tests are run with the opam package
       #:tests? #f
       #:phases %standard-phases))))

(define ocaml-opam-client
  (package
    (inherit ocaml-opam-core)
    (name "ocaml5-opam-client")
    (arguments
     `(#:package "opam-client"
       ;; tests are run with the opam package
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'pre-build
                    (lambda* (#:key inputs make-flags #:allow-other-keys)
                      (let ((bwrap (search-input-file inputs "/bin/bwrap")))
                        (substitute* "src/client/opamInitDefaults.ml"
                          (("\"bwrap\"")
                           (string-append "\"" bwrap "\"")))))))))
    (inputs (list bubblewrap))
    (propagated-inputs (list ocaml-base64
                             ocaml-cmdliner
                             ocaml-opam-repository
                             ocaml-opam-solver
                             ocaml-opam-state
                             ocaml-re
                             ocaml-spdx-licenses))))

(define-public opam
  (package
    (inherit ocaml-opam-core)
    (name "ocaml5-opam")
    (build-system dune-build-system)
    (arguments
     `(#:package "opam"
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'prepare-checks
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; Opam tests need to run an isolated environment from a writable
                      ;; home directory.
                      (mkdir-p "test-home")
                      (setenv "HOME"
                              (string-append (getcwd) "/test-home"))
                      (with-output-to-file (string-append (getcwd)
                                            "/test-home/.gitconfig")
                        (lambda _
                          (display "[user]
email = guix@localhost.none
name = Guix Builder

[protocol \"file\"]
allow = always")
                          (newline)))

                      ;; Opam tests require data from opam-repository. Instead
                      ;; of downloading them with wget from the guix
                      ;; environment, copy the content to the expected
                      ;; directory.
                      (substitute* "tests/reftests/dune.inc"
                        (("tar -C.*opam-archive-([0-9a-f]*)[^)]*" _ commit)
                         (string-append "rmdir %{targets}) (run cp -r "
                          (begin
                            ;; When updating opam, uncomment the following
                            ;; call.  It will list "opam-repo" commit-ish
                            ;; required to run tests (see native-inputs
                            ;; below).  Remove versions that are no longer
                            ;; required, and add new ones if necessary.
                            ;;
                            ;; (pk commit)
                            (assoc-ref inputs
                                       (string-append "opam-repo-" commit)))
                          "/ %{targets}) (run chmod +w -R %{targets}"))
                        (("wget[^)]*")
                         "touch %{targets}")

                        ;; Selectively disable a fraction of opam's "reftests"
                        ;; (CRAM-style regression tests). Most of these tests
                        ;; make assumptions about the build environment that
                        ;; don't hold for Guix.

                        ;; Test tries to clone a git repository from inside
                        ;; bwrap
                        (("diff upgrade-format.test upgrade-format.out")
                         "run true")
                        ;; Test tries to figure out which distro this is, and
                        ;; it doesn't know Guix
                        (("diff pin.unix.test pin.unix.out")
                         "run true")
                        ;; A variable expansion fails.
                        (("diff opamroot-versions.test opamroot-versions.out")
                         "run true")
                        ;; The repository we replaced is probably not as
                        ;; expected
                        (("diff opamrt-big-upgrade.test opamrt-big-upgrade.out")
                         "run true")
                        ;; Disable a failing test because of missing sandboxing
                        ;; functionality
                        (("diff init.test init.out")
                         "run true")
                        ;; Opam commit c4d912e normalizes test output to
                        ;; exclude version strings, but this seems to break
                        ;; some output assertions if $BASEDIR happens to
                        ;; include the version.
                        (("diff cli-versioning.test cli-versioning.out")
                         "run true")
                        ;; Fails on noisy git output.
                        (("diff action-disk.test action-disk.out")
                         "run true")
                        ;; Requires network access.
                        (("diff download.test download.out")
                         "run true")
                        (("diff swhid.unix.test swhid.unix.out")
                         "run true")
                        ;; Depends on 3rd-party git repos.
                        (("diff lock.test lock.out")
                         "run true"))

                      (substitute* "tests/reftests/dune"
                        ;; Because of our changes to the previous file, we
                        ;; cannot check it can be regenerated
                        (("diff dune.inc dune.inc.gen")
                         "run true"))
                      ;; Ensure we can run the generated build.sh (no /bin/sh)
                      (substitute* '("tests/reftests/legacy-local.test"
                                     "tests/reftests/legacy-git.test")
                        (("#! ?/bin/sh")
                         (string-append "#!"
                                        (search-input-file inputs "/bin/sh"))))
                      (substitute* "tests/reftests/testing-env"
                        (("OPAMSTRICT=1")
                         (string-append "OPAMSTRICT=1\nLIBRARY_PATH="
                                        (assoc-ref inputs "libc") "/lib"))))))))

    (native-inputs (let ((opam-repo (lambda (commit hash)
                                      (origin
                                        (method git-fetch)
                                        (uri (git-reference (url
                                                             "https://github.com/ocaml/opam-repository")
                                                            (commit commit)))
                                        (file-name (git-file-name "opam-repo"
                                                                  commit))
                                        (sha256 (base32 hash))))))
                     `(("dune" ,dune)
                       ("ocaml-cppo" ,ocaml-cppo)

                       ;; For tests.
                       ("git" ,git-minimal/pinned)
                       ("openssl" ,openssl)
                       ("python" ,python-wrapper)
                       ("rsync" ,rsync)
                       ("unzip" ,unzip)
                       ("which" ,which)
                       ("xxd" ,xxd)

                       ;; Data for tests
                       ("opam-repo-0070613707" ,(opam-repo
                                                 "00706137074d536d2019d2d222fbe1bea929deda"
                                                 "1gv1vvmfscj7wirfv6qncp8pf81wygnpzjwd0lyqcxm7g8r8lb4w"))
                       ("opam-repo-009e00fa" ,(opam-repo
                                               "009e00fa86300d11c311309a2544e5c6c3eb8de2"
                                               "1wwy0rwrsjf4q10j1rh1dazk32fbzhzy6f7zl6qmndidx9b1bq7w"))
                       ("opam-repo-11ea1cb" ,(opam-repo
                                              "11ea1cb6f2418b1f8a6679e4422771a04c9c3655"
                                              "1s4p0wfn3bx97yvm8xvj3yhzv2pz0jwml68g2ybv37hj9mpbrsq0"))
                       ("opam-repo-143dd2a2f59f5befbf3cb90bb2667f911737fbf8" ,
                        (opam-repo "143dd2a2f59f5befbf3cb90bb2667f911737fbf8"
                         "1kliiy9n2j8myxhbz1brq6yingfy0si1bmv15j1hbnbpyi3bavr9"))
                       ("opam-repo-297366c" ,(opam-repo
                                              "297366cd01c3aaf29b967bf0b34ccc7989d4d5b3"
                                              "1ysg69gys37nc2cxivs2ikh6xp0gj85if4rcrr874mqb9z12dm0j"))
                       ("opam-repo-3235916" ,(opam-repo
                                              "3235916a162a59d7c82dac3fe24214975d48f1aa"
                                              "1yf73rv2n740a4s9g7a9k4j91b4k7al88nwnw9cdw0k2ncbmr486"))
                       ("opam-repo-7090735c" ,(opam-repo
                                               "7090735c9d1dd2dc481c4128c5ef4d3667238f15"
                                               "1bccsgjhlp64lmvfjfn6viywf3x73ji75myg9ssf1ij1fkmabn0z"))
                       ("opam-repo-7371c1d9" ,(opam-repo
                                               "7371c1d9c53000840fb9a6d8ec13d87ffaa98401"
                                               "0lmy3rmp5liyp2dsx4s90rjdwc012947ig2fz6y97s3pmwsbf9g8"))
                       ("opam-repo-a5d7cdc0" ,(opam-repo
                                               "a5d7cdc0c91452b0aef4fa71c331ee5237f6dddd"
                                               "0z7kawqisy07088p5xjxwpvmvzlbj1d9cgdipsj90yx7nc5qh369"))
                       ("opam-repo-ad4dd344" ,(opam-repo
                                               "ad4dd344fe5cd1cab49ced49d6758a9844549fb4"
                                               "1a1qj47kj8xjdnc4zc50ijrix1kym1n7k20n3viki80a7518baw8"))
                       ("opam-repo-c1842d168d" ,(opam-repo
                                                 "c1842d168de956caf06d7ac8588e65020d7594d8"
                                                 "142y1ac7sprygyh91shcp0zcyfxjjkshi9g44qgg4rx60rbsbhai"))
                       ("opam-repo-c1ba97dafe95c865d37ad4d88f6e57c9ffbe7f0a" ,
                        (opam-repo "c1ba97dafe95c865d37ad4d88f6e57c9ffbe7f0a"
                         "0sllm110dvs3w1k7qhias5y8v6ikkk2knw97v1fk9lnw4lq45gv6"))
                       ("opam-repo-de897adf36c4230dfea812f40c98223b31c4521a" ,
                        (opam-repo "de897adf36c4230dfea812f40c98223b31c4521a"
                         "1m18x9gcwnbar8yv9sbfz8a3qpw412fp9cf4d6fb7syn0p0h96jw"))
                       ("opam-repo-f372039d" ,(opam-repo
                                               "f372039db86a970ef3e662adbfe0d4f5cd980701"
                                               "0ld7fcry6ss6fmrpswvr6bikgx299w97h0gwrjjh7kd7rydsjdws")))))
    (inputs (list ocaml-opam-client))))

(define-public ocaml-opam-monorepo
  (package
    (name "ocaml5-opam-monorepo")
    (version "0.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tarides/opam-monorepo/")
             (commit version)))
       (file-name name)
       (sha256
        (base32 "0nani7qhp54i6mhz8gxk9nzl15bz237qbqb0mc9jhb1016xwbvj3"))))
    (build-system dune-build-system)
    (arguments
     ;; TODO: Too many tests require a fully initialized opam, disabling them
     ;; would be a huge pain.  "Mocking" opam init is difficult because it
     ;; requires networking access.
     '(#:tests? #f))
    ;; TODO: not entirely clear if these should be native, test cross-building
    (native-inputs (list ocaml-odoc pkg-config))
    (home-page "https://github.com/tarides/opam-monorepo")
    (synopsis "Assemble and manage fully vendored Dune repositories")
    (description
     "The opam monorepo plugin provides a convenient interface to bridge the
opam package manager with having a local copy of all the source code required
to build a project using the dune build tool.")
    (license license:isc)))

(define-public ocaml-pprint
  (package
    (name "ocaml5-pprint")
    (version "20220103")
    (home-page "https://github.com/fpottier/pprint")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09y6nwnjldifm47406q1r9987njlk77g4ifqg6qs54dckhr64vax"))))
    (build-system dune-build-system)
    (synopsis "OCaml pretty-printing combinator library and rendering
engine")
    (description
     "This OCaml library offers a set of combinators for building
so-called documents as well as an efficient engine for converting documents to
a textual, fixed-width format.  The engine takes care of indentation and line
breaks, while respecting the constraints imposed by the structure of the
document and by the text width.")
    (license license:lgpl2.0)))

(define-public ocaml-syntax-shims
  (package
    (name "ocaml5-syntax-shims")
    (version "1.0.0")
    (home-page "https://github.com/ocaml-ppx/ocaml-syntax-shims")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l1i8z95qgb0lxlrv3yb5nkp391hqsiyi4r91p12k3xmggqixagf"))))
    (build-system dune-build-system)
    (properties `((upstream-name . "ocaml-syntax-shims")))
    (synopsis "Backport of new OCaml syntax to older compilers")
    (description
     "This package backports new language features such as @code{let+} to older
OCaml compilers.")
    (license license:expat)))

(define-public ocaml-bigstringaf
  (package
    (name "ocaml5-bigstringaf")
    (version "0.9.0")
    (home-page "https://github.com/inhabitedtype/bigstringaf")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "188j9awxg99vrp2l3rqfmdxdazq5xrjmg1wf62vfqsks9sff6wqx"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-bigarray-compat))
    (native-inputs (list ocaml-alcotest pkg-config))
    (synopsis "Bigstring intrinsics and fast blits based on memcpy/memmove")
    (description
     "The OCaml compiler has a bunch of intrinsics for Bigstrings, but they're
not widely-known, sometimes misused, and so programs that use Bigstrings are
slower than they have to be.  And even if a library got that part right and
exposed the intrinsics properly, the compiler doesn't have any fast blits
between Bigstrings and other string-like types.  @code{bigstringaf} provides
these missing pieces.")
    (license license:bsd-3)))

(define-public ocaml-cstruct
  (package
    (name "ocaml5-cstruct")
    (version "6.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mirage/ocaml-cstruct")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dpbirs6lzp0bclr3wcw407jjspll7iy66z18zks3mjccvlxd21w"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "cstruct"))
    (propagated-inputs (list ocaml-bigarray-compat))
    (native-inputs (list ocaml-alcotest))
    (home-page "https://github.com/mirage/ocaml-cstruct")
    (synopsis "Accesses C structures via a camlp4 extension")
    (description
     "Cstruct is a library and syntax extension to make it easier
to access C-like structures directly from OCaml.  It supports both reading and
writing to these structures, and they are accessed via the Bigarray module.")
    (license license:isc)))

(define-public ocaml-domain-name
  (package
    (name "ocaml5-domain-name")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hannesm/domain-name/")
             (commit (string-append "v" version))))
       (file-name name)
       (sha256
        (base32 "1a669zz1pc7sqbi1c13jsnp8algcph2b8gr5fjrjhyh3p232770k"))))
    (build-system dune-build-system)
    (native-inputs (list ocaml-alcotest))
    (home-page "https://github.com/hannesm/domain-name")
    (synopsis "RFC 1035 Internet domain name data structure and parser")
    (description
     "Parses and constructs RFC compliant domain names.  The invariants on the
length of domain names are preserved throughout the module.")
    (license license:isc)))

(define-public ocaml-ppx-let
  (package
    (name "ocaml5-ppx-let")
    (version "0.17.0")
    (source
     (janestreet-git-origin "ppx_let" version
      "0ijdpgikzx89m6srm3xdl657zim5c8pmzf6rwzx32m67nf0m0hr6"))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-base ocaml-ppxlib ocaml-ppx-here))
    (properties `((upstream-name . "ppx_let")))
    (home-page "https://github.com/janestreet/ppx_let")
    (synopsis "Monadic let-bindings")
    (description "A ppx rewriter for monadic and applicative let bindings,
match expressions, and if expressions.")
    (license license:asl2.0)))

(define %ocaml-merlin-base
  (package
    (name "ocaml5-merlin-base")
    (version "5.5-503")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml/merlin")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qzj4aqx5x76ldwhpxzz2ynjdh43l8nk6lmj93hr53c6ykrgn9nw"))))
    (build-system dune-build-system)
    (home-page "https://ocaml.github.io/merlin/")
    (synopsis "Code completion, typing, and navigation in Vim and Emacs")
    (description
     "Merlin is an assistant for editing OCaml code.  It aims to provide the features
available in modern IDEs: error reporting, auto completion, source browsing,
etc.")
    (license license:expat)))

(define-public ocaml-merlin-lib
  (package
    (inherit %ocaml-merlin-base)
    (name "ocaml5-merlin-lib")
    (arguments
     '(#:package "merlin-lib"))
    (propagated-inputs (list ocaml-alcotest ocaml-csexp ocaml-menhir))))

(define-public ocaml-index
  (package
    (inherit %ocaml-merlin-base)
    (name "ocaml5-index")
    (arguments
     '(#:package "ocaml-index"))
    (propagated-inputs (list ocaml-merlin-lib))
    (synopsis "Produces indicies for value occurrences in cmt files")
    (description
     "This package integrates with the build system to index a codebase and allow
tools such as Merlin to perform project-wide occurrences queries.")))

(define-public ocaml-dot-merlin-reader
  (package
    (inherit %ocaml-merlin-base)
    (name "ocaml5-dot-merlin-reader")
    (arguments
     '(#:package "dot-merlin-reader"))
    (propagated-inputs (list ocaml-merlin-lib))
    (synopsis "Reads config files for @code{ocaml-merlin}")
    (description "@code{ocaml-dot-merlin-reader} is an external reader for
@code{ocaml-merlin} configurations.")))

(define-public ocaml-merlin
  (package
    (inherit %ocaml-merlin-base)
    (name "ocaml5-merlin")
    (arguments
     `(#:package "merlin"
       #:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      ;; Tests require a writable cache directory
                      (setenv "HOME" "/tmp")
                      (when tests?
                        (invoke "dune" "runtest" "-p"
                                "merlin,dot-merlin-reader")))))))
    (propagated-inputs (list ocaml-merlin-lib ocaml-yojson ocaml-index))
    (native-inputs (list ocaml-dot-merlin-reader ;required for tests
                         ocaml-ppxlib ocaml-mdx jq))
    (license license:expat)))

(define-public ocaml-qcheck
  (package
    (name "ocaml5-qcheck")
    (version "0.20")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/c-cube/qcheck")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1r0m5p1dd06lbgfxqdpl1ya4vb8252z7hqkvdi9k444g4rx2ay3p"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-alcotest ocaml-ounit ocaml-ppxlib))
    (native-inputs (list ocamlbuild))
    (home-page "https://github.com/c-cube/qcheck")
    (synopsis "QuickCheck-inspired property-based testing for OCaml")
    (description
     "This module checks invariants (properties of some types) over randomly
generated instances of the type.  It provides combinators for generating
instances and printing them.")
    (license license:lgpl3+)))

(define-public ocaml-qtest
  (package
    (name "ocaml5-qtest")
    (version "2.11.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vincent-hugot/qtest/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04ghjshh6104xyglm0db9kv90m62qla5f4bfrlndv6dsvgw3rdjl"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-ounit ocaml-qcheck))
    (home-page "https://github.com/vincent-hugot/qtest")
    (synopsis "Inline (Unit) Tests for OCaml")
    (description
     "Qtest extracts inline unit tests written using a special
syntax in comments.  Those tests are then run using the oUnit framework and the
qcheck library.  The possibilities range from trivial tests -- extremely simple
to use -- to sophisticated random generation of test cases.")
    (license license:lgpl3+)))

(define-public ocaml-afl-persistent
  (package
    (name "ocaml5-afl-persistent")
    (version "1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stedolan/ocaml-afl-persistent")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06yyds2vcwlfr2nd3gvyrazlijjcrd1abnvkfpkaadgwdw3qam1i"))))
    (build-system ocaml-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (replace 'build
                    (lambda _
                      (invoke "./build.sh")))
                  ;; The tests are already run in the build.sh script.
                  (delete 'check))))
    (native-inputs (list opam-installer))
    (home-page "https://github.com/stedolan/ocaml-afl-persistent")
    (synopsis "Allows use of afl-fuzz in 'persistent mode' for performance")
    (description
     "The afl-fuzz tool normally works by repeatedly forking the program being
tested.  Using this package, you can run afl-fuzz in 'persistent mode', which
avoids repeated forking and is much faster.")
    (license license:expat)))

(define-public ocaml-reactivedata
  (package
    (name "ocaml5-reactivedata")
    (version "0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ocsigen/reactiveData")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gmpfnw08c7hx4bsgrgvp6w7pq2ghqxq3qd1cbdyscbg9n22jrca"))))
    (arguments
     `(#:tests? #f)) ;no tests
    (build-system dune-build-system)
    (properties `((upstream-name . "reactiveData")))
    (propagated-inputs
     (list ocaml-react))
    (home-page "https://github.com/ocsigen/reactiveData")
    ;; The upstream package cribs the exact synopsis and description of
    ;; ocaml-react but appears to be otherwise distinct.
    (synopsis "Declarative events and signals for OCaml")
    (description
     "reactiveData is an OCaml module for functional reactive programming (FRP).  It
provides support to program with time varying values: declarative events and
 signals.  React doesn't define any primitive event or signal, it lets the
client chooses the concrete timeline.")
    (license license:lgpl2.1+)))

(define-public ocaml-stringext
  (package
    (name "ocaml5-stringext")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rgrinberg/stringext")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m09cmn3vrk3gdm60fb730qsygcfyxsyv7gl9xfzck08q1x2x9qx"))))
    (build-system dune-build-system)
    (native-inputs (list ocamlbuild ocaml-qtest))
    (home-page "https://github.com/rgrinberg/stringext")
    (synopsis "Extra string functions for OCaml")
    (description
     "Provides a single module named Stringext that provides a grab
bag of often used but missing string functions from the stdlib.  E.g, split,
full_split, cut, rcut, etc..")
    (license license:expat)))

(define-public ocaml-angstrom
  (package
    (name "ocaml5-angstrom")
    (version "0.15.0")
    (home-page "https://github.com/inhabitedtype/angstrom")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hmrkdcdlkwy7rxhngf3cv3sa61cznnd9p5lmqhx20664gx2ibrh"))))
    (build-system dune-build-system)
    (arguments
     ;; Only build the base angstrom package.
     '(#:package "angstrom"))
    (propagated-inputs (list ocaml-bigstringaf))
    (native-inputs (list ocaml-alcotest ocaml-ppx-let ocaml-syntax-shims))
    (synopsis "Parser combinators built for speed and memory-efficiency")
    (description
     "Angstrom is a parser-combinator library that makes it easy to write
efficient, expressive, and reusable parsers suitable for high-performance
applications.  It exposes monadic and applicative interfaces for composition,
and supports incremental input through buffered and unbuffered interfaces.
Both interfaces give the user total control over the blocking behavior of
their application, with the unbuffered interface enabling zero-copy IO.
Parsers are backtracking by default and support unbounded lookahead.")
    (license license:bsd-3)))

(define-public ocaml-macaddr
  (package
    (name "ocaml5-macaddr")
    (version "5.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mirage/ocaml-ipaddr/")
             (commit (string-append "v" version))))
       (file-name name)
       (sha256
        (base32 "1zgwx0ms3l4k4dzwnkrwq4zzqjrddjsvqn66mbd0rm6aq1ib019d"))))
    (build-system dune-build-system)
    (arguments
     '(#:package "macaddr"))
    (propagated-inputs (list ocaml-cstruct ocaml-domain-name))
    (native-inputs (list ocaml-ounit2 ocaml-ppx-sexp-conv))
    (home-page "https://github.com/mirage/ocaml-ipaddr")
    (synopsis "OCaml library for manipulation of MAC address representations")
    (description
     "This library is for parsing and manipulating MAC addresses. Features include:
@itemize
@item MAC-48 (Ethernet) address support
@item @code{Macaddr} is a @code{Map.OrderedType}
@item All types have sexplib serializers/deserializers optionally via the
@code{Macaddr_sexp} library
@end itemize")
    (license license:isc)))

(define-public ocaml-ipaddr
  (package
    (name "ocaml5-ipaddr")
    (version "5.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mirage/ocaml-ipaddr/")
             (commit (string-append "v" version))))
       (file-name name)
       (sha256
        (base32 "1zgwx0ms3l4k4dzwnkrwq4zzqjrddjsvqn66mbd0rm6aq1ib019d"))))
    (build-system dune-build-system)
    (arguments
     '(#:package "ipaddr"))
    (propagated-inputs (list ocaml-macaddr ocaml-domain-name))
    (native-inputs (list ocaml-ounit2 ocaml-ppx-sexp-conv))
    (home-page "https://github.com/mirage/ocaml-ipaddr")
    (synopsis "Manipulates IP address representations")
    (description
     "This library provides IP address types with serialization, supporting a wide
range of RFCs.")
    (license license:isc)))

(define-public ocaml-uri
  (package
    (name "ocaml5-uri")
    (version "4.2.0")
    (home-page "https://github.com/mirage/ocaml-uri")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bgkc66cq00mgnkz3i535srwzwc4cpdsv0mly5dzvvq33451xwf0"))))
    (build-system dune-build-system)
    (arguments
     '(#:package "uri"))
    (propagated-inputs (list ocaml-stringext ocaml-angstrom))
    (native-inputs (list ocaml-ounit ocaml-ppx-sexp-conv))
    (properties `((upstream-name . "uri")))
    (synopsis "RFC3986 URI/URL parsing library")
    (description
     "OCaml-uri is a library for parsing and unparsing RFC3986 URI strings.")
    (license license:isc)))

(define-public ocaml-calendar
  (package
    (name "ocaml5-calendar")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml-community/calendar")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jw8sdz1kl53fzdyxixd8ljfr25vvn4f2z4lspasqcj4ma5k6m7r"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-re ocaml-odoc ocaml-alcotest))
    (home-page "https://github.com/ocaml-community/calendar")
    (synopsis "OCaml library for handling dates and times")
    (description "This package provides types and operations over
dates and times.")
    ;; With linking exception.
    (license license:lgpl2.1+)))

(define-public ocaml-mdx
  (package
    (name "ocaml5-mdx")
    (version "2.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/realworldocaml/mdx")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rhj00gsj1zz8yd99wkcpsgf0ym1fg940zk2jq29fysk4zd1g7m3"))))
    (build-system dune-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-test-assertions
                    (lambda _
                      (let ((path-prefix "test/bin/mdx-test/misc/"))
                        ;; cmdliner changed the format and the tests fail
                        (substitute* (list (string-append path-prefix
                                            "no-such-file/test.expected")
                                           (string-append path-prefix
                                            "no-such-prelude/test.expected"))
                          (("`")
                           "'")
                          (("COMMAND")
                           "[COMMAND]")
                          (("\\.\\.\\.")
                           "…")))))
                  (add-after 'fix-test-assertions 'fix-egrep
                    (lambda _
                      ;; egrep is obsolescent; using grep -E
                      (substitute* "test/bin/mdx-test/expect/padding/test-case.md"
                        (("egrep")
                         "grep -E")))))))
    (propagated-inputs (list ocaml-fmt
                             ocaml-astring
                             ocaml-logs
                             ocaml-cmdliner
                             ocaml-re
                             ocaml-result
                             ocaml-odoc
                             ocaml-odoc-parser
                             ocaml-version))
    (native-inputs (list ocaml-cppo ocaml-lwt ocaml-alcotest))
    (home-page "https://github.com/realworldocaml/mdx")
    (synopsis "Executable code blocks inside markdown files")
    (description
     "@code{ocaml-mdx} executes code blocks inside markdown files.
There are (currently) two sub-commands, corresponding
to two modes of operations: pre-processing (@code{ocaml-mdx pp})
and tests (@code{ocaml-mdx test}]).

The pre-processor mode allows mixing documentation and code,
and to practice @dfn{literate programming} using markdown and OCaml.

The test mode ensures that shell scripts and OCaml fragments
in the documentation always stays up-to-date.

@code{ocaml-mdx} is released as two binaries called @code{ocaml-mdx} and
@code{mdx} which are the same, mdx being the deprecated name, kept for now for
compatibility.")
    (license license:isc)))

(define-public ocaml-gen
  (package
    (name "ocaml5-gen")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/c-cube/gen")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1z5nw5wljvcqp8q07h336bbvf9paynia0jsdh4486hlkbmr1ask1"))))
    (build-system dune-build-system)
    (arguments
     `(#:package "gen"))
    (propagated-inputs (list ocaml-odoc ocaml-seq))
    (native-inputs (list ocaml-qtest ocaml-qcheck))
    (home-page "https://github.com/c-cube/gen/")
    (synopsis "Iterators for OCaml, both restartable and consumable")
    (description
     "This package implements restartable and consumable iterators for OCaml.")
    (license license:bsd-2)))

(define-public ocaml-sedlex
  (package
    (name "ocaml5-sedlex")
    (version "3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml-community/sedlex")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "104h0v5qkgy847vyy43zhdrvbq38mhgm8b97qsfywwgcpsx6l8rn"))))
    (build-system dune-build-system)
    (arguments
     (list
      #:package "sedlex"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'copy-resources
            ;; These three files are needed by src/generator/data/dune,
            ;; but would be downloaded using curl at build time.
            (lambda* (#:key inputs #:allow-other-keys)
              (with-directory-excursion "src/generator/data"
                ;; Newer versions of dune emit an error if files it wants to
                ;; build already exist. Delete the dune file so dune doesn't
                ;; complain.
                (delete-file "dune")
                (for-each (lambda (file)
                            (copy-file (search-input-file inputs file)
                                       (basename file)))
                          '("share/ucd/extracted/DerivedGeneralCategory.txt"
                            "share/ucd/DerivedCoreProperties.txt"
                            "share/ucd/PropList.txt")))))
          (add-before 'build 'chmod
            (lambda _
              (for-each (lambda (file)
                          (chmod file #o644))
                        (find-files "." ".*")))))))
    (native-inputs (list ocaml-ppx-expect))
    (propagated-inputs (list ocaml-gen ocaml-ppxlib ocaml-uchar))
    (inputs (list ucd))
    (home-page "https://www.cduce.org/download.html#side")
    (synopsis "Lexer generator for Unicode")
    (description
     "sedlex is a lexer generator for OCaml.  It is similar to ocamllex, but
supports Unicode.  Unlike ocamllex, sedlex allows lexer specifications within
regular OCaml source files.  Lexing specific constructs are provided via a ppx
syntax extension.")
    (license license:expat)))

(define %js-of-ocaml-base
  (package
    (name "ocaml5-js-of-ocaml-base")
    (version "6.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocsigen/js_of_ocaml")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04y0bn7jmk30aq1q56rpb09n89zy71fv7qfn0b8fzrv6w9z8asnh"))))
    (build-system dune-build-system)
    (home-page "https://ocsigen.org/js_of_ocaml/")
    (synopsis "Compiler from OCaml bytecode to Javascript")
    (description
     "Js_of_ocaml is a compiler from OCaml bytecode to JavaScript.
It makes it possible to run pure OCaml programs in JavaScript environment like
browsers and Node.js.")
    ;; With static-linking exception
    (license license:lgpl2.1+)))

(define-public js-of-ocaml-compiler
  (package
    (inherit %js-of-ocaml-base)
    (name "ocaml5-js-of-ocaml-compiler")
    (arguments
     `(#:package "js_of_ocaml-compiler"
       #:tests? #f))
    (propagated-inputs (list ocaml-ppxlib
                             ocaml-menhir
                             ocaml-reactivedata
                             ocaml-cmdliner
                             ocaml-lwt
                             ocaml-sedlex
                             ocaml-re
                             ocaml-yojson))
    (native-inputs
     ;; for tests
     (list node-lts ocaml-ppx-expect ocaml-num))
    (properties `((upstream-name . "js_of_ocaml")))
    (home-page "https://ocsigen.org/js_of_ocaml/")
    (synopsis "Compiler from OCaml bytecode to Javascript (compiler core)")
    (license license:lgpl2.1+)))

(define-public js-of-ocaml
  (package
    (inherit %js-of-ocaml-base)
    (name "ocaml5-js-of-ocaml")
    (arguments
     `(#:package "js_of_ocaml"
       #:tests? #f))
    (propagated-inputs (list js-of-ocaml-compiler
                             ocaml-ppxlib
                             ocaml-uchar
                             ocaml-menhir
                             ocaml-reactivedata
                             ocaml-cmdliner
                             ocaml-lwt
                             ocaml-sedlex
                             ocaml-tyxml
                             ocaml-re
                             ocaml-uutf
                             ocaml-graphics
                             ocaml-yojson))
    (native-inputs
     ;; for tests
     (list node-lts ocaml-ppx-expect ocaml-num))
    (properties `((upstream-name . "js_of_ocaml")))
    (home-page "https://ocsigen.org/js_of_ocaml/")
    (license license:lgpl2.1+)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
