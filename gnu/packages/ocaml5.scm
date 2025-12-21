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
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module ((guix build-system ocaml)
                #:select ((ocaml5-build-system . ocaml-build-system)))
  #:use-module (guix build-system gnu)
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

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
