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
;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
