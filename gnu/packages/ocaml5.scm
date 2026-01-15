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
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
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

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
