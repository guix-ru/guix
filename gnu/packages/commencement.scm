;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2017, 2018, 2019, 2021, 2022, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019, 2020, 2021, 2022, 2023, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2019-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020, 2022 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2020 Guy Fleury Iteriteka <gfleury@disroot.org>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2022, 2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2022-2024 Ekaitz Zarraga <ekaitz@elenq.tech>
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

(define-module (gnu packages commencement)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages c)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages mes)
  #:use-module (gnu packages musl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix platform)
  #:use-module ((guix store) #:select (%store-monad
                                       store-lift
                                       built-in-builders))
  #:use-module (guix monads)
  #:use-module (guix download)
  #:use-module ((guix git-download)
                #:select (git-fetch git-reference git-file-name))
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix memoization)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match))

;;; Commentary:
;;;
;;; This is the commencement, this is where things start.  Before the
;;; commencement, of course, there's the 'bootstrap' module, which provides us
;;; with the initial binaries.  This module uses those bootstrap binaries to
;;; actually build up the whole tool chain that make up the implicit inputs of
;;; 'gnu-build-system'.
;;;
;;; To avoid circular dependencies, this module should not be imported
;;; directly from anywhere.
;;;
;;; Below, we frequently use "inherit" to create modified packages.  The
;;; reason why we use "inherit" instead of "package/inherit" is because we do
;;; not want these commencement packages to inherit grafts.  By definition,
;;; these packages are not depended on at run time by any of the packages we
;;; use.  Thus it does not make sense to inherit grafts.  Furthermore, those
;;; grafts would often lead to extra overhead for users who would end up
;;; downloading those "-boot0" packages just to build package replacements
;;; that are in fact not going to be used.
;;;
;;; Code:

(define built-in-builders*
  (store-lift built-in-builders))

(define* (git-fetch-from-tarball tarball)
  "Return an <origin> method equivalent to 'git-fetch', except that it fetches
the checkout from TARBALL, a tarball containing said checkout.

  The purpose of this procedure is to work around bootstrapping issues:
'git-fetch' depends on Git, which is much higher in the dependency graph."
  (lambda* (ref hash-algo hash
                #:optional name
                #:key (system (%current-system))
                (guile %bootstrap-guile))
    (mlet %store-monad ((builtins (built-in-builders*)))
      ;; Use the 'git-download' built-in builder when it's available: it's the
      ;; preferred and most reliable method.
      (if (member "git-download" builtins)
          (git-fetch ref hash-algo hash name #:system system)

          ;; This method is kept for compatibility with daemons that lack
          ;; 'git-download' to work around bootstrapping issues.
          (mlet %store-monad ((guile (package->derivation guile system)))
            (gexp->derivation
             (or name "git-checkout")
             (with-imported-modules '((guix build utils))
               #~(begin
                   (use-modules (guix build utils)
                                (ice-9 ftw)
                                (ice-9 match))
                   (setenv "PATH"
                           #+(file-append %bootstrap-coreutils&co "/bin"))

                   ;; FIXME: This assumes that TARBALL, an origin, was
                   ;; successfully downloaded under its given hash; however
                   ;; that hash is bound to change over time since it's a
                   ;; generated tarball.
                   (invoke "tar" "xf" #$tarball)
                   (match (scandir ".")
                     (("." ".." directory)
                      (copy-recursively directory #$output)))))
             #:recursive? #t
             #:hash-algo hash-algo
             #:hash hash
             #:system system
             #:guile-for-build guile
             #:graft? #f
             #:local-build? #t))))))

(define* (commencement-build-target #:optional (system (%current-system)))
  "Return an architecture triplet for use in various build scripts.  This is
the supported 32-bit version of the architecture currently being built on."
  (cond
    ((target-x86-64?) "i686-unknown-linux-gnu")
    ((target-arm?) "arm-unknown-linux-gnu")
    (#t (nix-system->gnu-triplet system))))

(define bootar
  (package
    (name "bootar")
    (version "1b")
    (source (origin
              (method url-fetch)
              (uri (list (string-append
                          "mirror://gnu/guix/mirror/bootar-" version ".ses")
                         (string-append
                          "https://files.ngyro.com/bootar/bootar-"
                          version ".ses")))
              (sha256
               (base32
                "0cf5vj5yxfvkgzvjvh2l7b2nz5ji5l534n9g4mfp8f5jsjqdrqjc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:implicit-inputs? #f
       #:tests? #f
       #:guile ,%bootstrap-guile
       #:imported-modules ((guix build gnu-bootstrap)
                           ,@%default-gnu-imported-modules)
       #:phases
       (begin
         (use-modules (guix build gnu-bootstrap))
         (modify-phases %standard-phases
           (replace 'unpack
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((source (assoc-ref inputs "source"))
                      (guile-dir (assoc-ref inputs "guile"))
                      (guile (string-append guile-dir "/bin/guile")))
                 (invoke guile "--no-auto-compile" source)
                 (chdir "bootar"))))
           (replace 'configure (bootstrap-configure "Bootar" ,version
                                                    '(".") "scripts"))
           (replace 'build (bootstrap-build '(".")))
           (replace 'install (bootstrap-install '(".") "scripts"))))))
    (inputs `(("guile" ,%bootstrap-guile)))
    (home-page "https://git.ngyro.com/bootar")
    (synopsis "Tar decompression and extraction in Guile Scheme")
    (description "Bootar is a simple Tar extractor written in Guile
Scheme.  It supports running 'tar xvf' on uncompressed tarballs or
tarballs that are compressed with BZip2, GZip, or XZ.  It also provides
standalone scripts for 'bzip2', 'gzip', and 'xz' that each support
decompression to standard output.

What makes this special is that Bootar is distributed as a
self-extracting Scheme (SES) program.  That is, a little script that
outputs the source code of Bootar.  This makes it possible to go from
pure Scheme to Tar and decompression in one easy step.")
    (license license:gpl3+)))

(define gash-boot
  (package
    (inherit gash)
    (name "gash-boot")
    (arguments
     `(#:implicit-inputs? #f
       #:tests? #f
       #:guile ,%bootstrap-guile
       #:imported-modules ((guix build gnu-bootstrap)
                           ,@%default-gnu-imported-modules)
       #:phases
       (begin
         (use-modules (guix build gnu-bootstrap))
         (modify-phases %standard-phases
           (replace 'configure
             (bootstrap-configure "Gash" ,(package-version gash)
                                  '("gash") "scripts"))
           (replace 'build (bootstrap-build '("gash")))
           (replace 'install (bootstrap-install '("gash") "scripts"))
           (add-after 'install 'install-symlinks
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (symlink (string-append out "/bin/gash")
                          (string-append out "/bin/sh"))
                 (symlink (string-append out "/bin/gash")
                          (string-append out "/bin/bash")))))))))
    (inputs `(("guile" ,%bootstrap-guile)))
    (native-inputs `(("bootar" ,bootar)))))

(define gash-utils-boot
  (package
    (inherit gash-utils)
    (name "gash-utils-boot")
    (arguments
     `(#:implicit-inputs? #f
       #:tests? #f
       #:guile ,%bootstrap-guile
       #:imported-modules ((guix build gnu-bootstrap)
                           ,@%default-gnu-imported-modules)
       #:phases
       (begin
         (use-modules (guix build gnu-bootstrap))
         (modify-phases %standard-phases
           (add-after 'unpack 'set-load-path
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((gash (assoc-ref inputs "gash")))
                 (add-to-load-path (string-append gash "/share/guile/site/"
                                                  (effective-version))))))
           (add-before 'configure 'pre-configure
             (lambda _
               (format #t "Creating gash/commands/testb.scm~%")
               (copy-file "gash/commands/test.scm"
                          "gash/commands/testb.scm")
               (substitute* "gash/commands/testb.scm"
                 (("gash commands test") "gash commands testb")
                 (("apply test [(]cdr") "apply test/bracket (cdr"))
               (for-each (lambda (script)
                           (let ((target (string-append "scripts/"
                                                        script ".in")))
                             (format #t "Creating scripts/~a~%" target)
                             (copy-file "scripts/template.in" target)
                             (substitute* target
                               (("@UTILITY@") script))))
                         '("awk" "basename" "cat" "chmod" "cmp" "command"
                           "compress" "cp" "cut" "diff" "dirname" "env"
                           "expr" "false" "find" "grep" "head" "ln" "ls"
                           "mkdir" "mv" "printf" "pwd" "reboot" "rm" "rmdir"
                           "sed" "sleep" "sort" "tar" "test" "touch" "tr"
                           "true" "uname" "uniq" "wc" "which"))
               (format #t "Creating scripts/[.in~%")
               (copy-file "scripts/template.in" "scripts/[.in")
               (substitute* "scripts/[.in"
                 (("@UTILITY@") "testb"))
               (delete-file "scripts/template.in")))
           (replace 'configure
             (bootstrap-configure "Gash-Utils" ,(package-version gash-utils)
                                  '("gash" "gash-utils") "scripts"))
           (replace 'build (bootstrap-build '("gash" "gash-utils")))
           (replace 'install
             (bootstrap-install '("gash" "gash-utils") "scripts"))
           ;; XXX: The scripts should add Gash to their load paths and
           ;; this phase should not exist.
           (add-after 'install 'copy-gash
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (moddir (string-append out "/share/guile/site/"
                                             (effective-version)))
                      (godir (string-append out "/lib/guile/"
                                            (effective-version)
                                            "/site-ccache"))
                      (gash (assoc-ref inputs "gash"))
                      (gash-moddir (string-append gash "/share/guile/site/"
                                                  (effective-version)))
                      (gash-godir (string-append gash "/lib/guile/"
                                                 (effective-version)
                                                 "/site-ccache")))
                 (copy-file (string-append gash-moddir "/gash/compat.scm")
                            (string-append moddir "/gash/compat.scm"))
                 (copy-recursively (string-append gash-moddir "/gash/compat")
                                   (string-append moddir "/gash/compat"))
                 (copy-file (string-append gash-godir "/gash/compat.go")
                            (string-append godir "/gash/compat.go"))
                 (copy-recursively (string-append gash-godir "/gash/compat")
                                   (string-append godir "/gash/compat")))))
           ;; We need an external echo.
           (add-after 'install 'make-echo
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (gash (assoc-ref inputs "gash")))
                 (with-output-to-file (string-append out "/bin/echo")
                   (lambda ()
                     (display (string-append "#!" gash "/bin/gash\n"))
                     (newline)
                     (display "echo \"$@\"")
                     (newline)))
                 (chmod (string-append out "/bin/echo") #o755))))))))
    (inputs `(("gash" ,gash-boot)
              ("guile" ,%bootstrap-guile)))
    (native-inputs `(("bootar" ,bootar)))))

(define (%boot-gash-inputs)
  `(("bash" , gash-boot)                ; gnu-build-system wants "bash"
    ("coreutils" , gash-utils-boot)
    ("bootar" ,bootar)
    ("guile" ,%bootstrap-guile)))

(define stage0-posix
  ;; The initial bootstrap package: no binary inputs except those from
  ;; `bootstrap-seeds, for x86 a 190 byte binary seed: `x86/hex0-seed'.
    (package
      (name "stage0-posix")
      (version "1.7.0")
      (source (origin
                (method url-fetch)
                (uri (string-append "https://github.com/oriansj/stage0-posix/"
                                    "releases/download/Release_" version
                                    "/stage0-posix-" version ".tar.gz"))
                (sha256
                 (base32
                  "10c79z2c62gvrvpshvj94zz2hczdq1p4g1v0dakx1jiz2fx32vvn"))))
      (supported-systems '("i686-linux" "x86_64-linux"
                           "aarch64-linux"
                           "riscv64-linux"))
      (native-inputs (%boot-gash-inputs))
      (build-system trivial-build-system)
      (arguments
       (list
        #:guile %bootstrap-guile
        #:modules '((guix build utils))
        #:builder
        #~(begin
            (use-modules (guix build utils))
            (let* ((source #$(package-source this-package))
                   (tar #$(this-package-native-input "bootar"))
                   (bash #$(this-package-native-input "bash"))
                   (coreutils #$(this-package-native-input "coreutils"))
                   (guile #$(this-package-input "guile"))
                   (out #$output)
                   (bindir (string-append out "/bin"))
                   (target (or #$(%current-target-system)
                               #$(%current-system)))
                   (stage0-cpu
                    (cond
                     ((or #$(target-x86-64?) #$(target-x86-32?))
                      "x86")
                     (#$(target-aarch64?)
                      "AArch64")
                     (#$(target-riscv64?)
                      "riscv64")
                     (else
                      (error "stage0-posix: system not supported" target))))
                   (kaem (string-append "bootstrap-seeds/POSIX/"
                                        stage0-cpu "/kaem-optional-seed")))
              (setenv "PATH" (string-append tar "/bin:"
                                            coreutils "/bin:"
                                            bash "/bin"))
              (invoke "tar" "xvf" source)
              (chdir (string-append "stage0-posix-" #$version))
              (mkdir-p bindir)
              ;; Keep the same capitalization between the file name and the folder.
              (rename-file "kaem.aarch64" "kaem.AArch64")
              (invoke kaem (string-append "kaem." stage0-cpu))
              (with-directory-excursion (string-append stage0-cpu "/bin")
                (install-file "hex2" bindir)
                (install-file "M1" bindir)
                (install-file "blood-elf" bindir)
                (install-file "kaem" bindir)
                (install-file "get_machine" bindir)
                (install-file "M2-Planet" bindir))))))
      (home-page "https://github.com/oriansj/stage0-posix/")
      (synopsis "The initial bootstrap package, builds stage0 up to M2-Planet")
      (description "Starting from the 190-byte hex0-seed binary provided by
the bootstrap-seeds, the stage0-posix package first builds hex0 and then all
the way up: hex1, catm, hex2, M0, cc_x86, M1, M2, get_machine (that's all of
MesCC-Tools), and finally M2-Planet.")
      (license license:gpl3+)))


(define mes-boot
  (package
    (inherit mes)
    (name "mes-boot")
    (version "0.27")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "mirror://gnu/mes/"
                                        "mes-" version ".tar.gz")
                         (string-append "https://lilypond.org/janneke/mes/"
                                        "mes-" version ".tar.gz")))
              (sha256
               (base32
                "1a5ag8i303yhf76sg05rpcans9vadvnpxcpa4sl09z4cv5bfcgh3"))))
    (inputs '())
    (propagated-inputs '())
    (supported-systems '("i686-linux" "x86_64-linux"
                         "armhf-linux" "aarch64-linux"
                         "riscv64-linux"))
    (native-inputs
     `(("m2-planet" ,stage0-posix)
       ("nyacc-source" ,(bootstrap-origin
                         (origin (inherit (package-source nyacc-1.00.2))
                                 (snippet #f))))
       ,@(%boot-gash-inputs)))
    (arguments
     (list
      #:implicit-inputs? #f
      #:tests? #f
      #:guile %bootstrap-guile
      #:strip-binaries? #f              ;no strip yet
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'unpack-seeds
            (lambda _
              (let ((nyacc-source #$(this-package-native-input "nyacc-source")))
                (with-directory-excursion ".."
                  (invoke "tar" "-xvf" nyacc-source)))))
          (replace 'configure
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out #$output)
                    (gash #$(this-package-native-input "bash"))
                    (dir (with-directory-excursion ".." (getcwd))))
                (setenv "GUILE_LOAD_PATH" (string-append
                                           dir "/nyacc-1.00.2/module"))
                (invoke "gash" "configure.sh"
                        (string-append "--prefix=" out)
                        (string-append "--host="
                          #$(string-replace-substring
                              (commencement-build-target)
                              "-unknown-" "-"))))))
          (replace 'build
            (lambda _
              ;; TODO: GUILE_LOAD_PATH is leaking. We need to clean it.
              (substitute* "kaem.run"
                (("cp bin/mes-m2 bin/mes" all)
                 (string-append "GUILE_LOAD_PATH=/fubar\n" all)))
              (invoke "gash" "bootstrap.sh")))
          (delete 'check)
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* "install.sh" ; show some progress
                ((" -xf") " -xvf")
                (("^( *)((cp|mkdir|tar) [^']*[^\\])\n" all space cmd)
                 (string-append space "echo '" cmd "'\n"
                                space cmd "\n")))
              (invoke "gash" "install.sh")
              ;; Keep ASCII output, for friendlier comparison and bisection
              (let* ((out #$output)
                     (cache (string-append out "/lib/cache")))
                (define (objects-in-dir dir)
                  (find-files dir
                              (lambda (name stat)
                                (and (equal? (dirname name) dir)
                                     (or (string-suffix? ".M1" name)
                                         (string-suffix? ".hex2" name)
                                         (string-suffix? ".o" name)
                                         (string-suffix? ".s" name))))))
                (for-each (lambda (x) (install-file x cache))
                          (append (objects-in-dir "m2")
                                  (objects-in-dir ".")
                                  (objects-in-dir "mescc-lib")))))))))
    (native-search-paths
     (list (search-path-specification
            (variable "C_INCLUDE_PATH")
            (files '("include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (files '("lib")))
           (search-path-specification
            (variable "MES_PREFIX")
            (separator #f)
            (files '("")))))
    ;; This can take up to 3 days on riscv64-linux.
    (properties `((timeout . ,(* 3 24 60 60))))))

(define tcc-boot0
  ;; Pristine tcc cannot be built by MesCC, we are keeping a delta of 30
  ;; patches.  In a very early and rough form they were presented to the
  ;; TinyCC developers, who at the time showed no interest in supporting the
  ;; bootstrappable effort; we will try again later.  These patches have been
  ;; ported to 0.9.27, alas the resulting tcc is buggy.  Once MesCC is more
  ;; mature, this package should use the 0.9.27 sources (or later).
  (package
    (inherit tcc)
    (name "tcc-boot0")
    (version "0.9.26-1157-gdd46e018")
    (source (origin
              (method url-fetch)
              (uri (list
                    (string-append "mirror://gnu/guix/mirror/"
                                   "tcc-" version ".tar.gz")
                    (string-append "https://lilypond.org/janneke/tcc/"
                                   "tcc-" version ".tar.gz")))
              (sha256
               (base32
                "0bi32a1xdndz4ffway4ghcn2nfcnyd78mwh9vq2khyqyrnmc0j1p"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux" "riscv64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs
     `(("mes" ,mes-boot)
       ("mescc-tools" ,stage0-posix)
       ("nyacc-source" ,(bootstrap-origin
                         (origin (inherit (package-source nyacc-1.00.2))
                                 (snippet #f))))
       ,@(%boot-gash-inputs)))
    (arguments
     (list
      #:implicit-inputs? #f
      #:guile %bootstrap-guile
      #:validate-runpath? #f            ; no dynamic executables
      #:strip-binaries? #f              ; no strip yet
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'unpack-extra-sources
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((nyacc-source #$(this-package-native-input "nyacc-source")))
                (with-directory-excursion ".."
                  (invoke "tar" "-xvf" nyacc-source)))))
          (replace 'configure
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out #$output)
                     (dir (with-directory-excursion ".." (getcwd)))
                     (interpreter "/lib/mes-loader")
                     (mes #$(this-package-native-input "mes"))
                     (mescc (string-append mes "/bin/mescc")))
                (substitute* "conftest.c"
                  (("volatile") ""))
                (setenv "prefix" out)
                (setenv "GUILE_LOAD_PATH"
                        (string-append dir "/nyacc-1.00.2/module"))
                (setenv "ONE_SOURCE" "true")
                (invoke "sh" "configure"
                        "--cc=mescc"
                        (string-append
                          "--cpu=" (car (string-split
                                          #$(commencement-build-target) #\-)))
                        (string-append "--prefix=" out)
                        (string-append "--elfinterp=" interpreter)
                        "--crtprefix=."
                        "--tccdir=."))))
          (replace 'build
            (lambda _
              (substitute* "bootstrap.sh" ; Show some progress
                (("^( *)((cp|ls|mkdir|rm|[.]/tcc|[.]/[$][{program_prefix[}]tcc) [^\"]*[^\\])\n" all space cmd)
                 (string-append space "echo \"" cmd "\"\n"
                                space cmd "\n")))
              (invoke "sh" "bootstrap.sh")))
          (replace 'check
            (lambda _
              ;; fail fast tests
              (system* "./tcc" "--help") ; --help exits 1
              ;; (invoke "sh" "test.sh" "mes/scaffold/tests/30-strlen")
              ;; (invoke "sh" "-x" "test.sh" "mes/scaffold/tinycc/00_assignment")
              ;; TODO: add sensible check target (without depending on make)
              ;; (invoke "sh" "check.sh")
              ))
          (replace 'install
            (lambda _
              (substitute* "install.sh" ; Show some progress
                (("^( *)((cp|ls|mkdir|rm|tar|./[$][{PROGRAM_PREFIX[}]tcc) [^\"]*[^\\])\n" all space cmd)
                 (string-append space "echo \"" cmd "\"\n"
                                space cmd "\n")))

              (invoke "sh" "install.sh"))))))
    (native-search-paths
     (list (search-path-specification
            (variable "C_INCLUDE_PATH")
            (files '("include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (files '("lib")))))))

(define gzip-mesboot
  ;; The initial gzip.  We keep this scripted gzip build before building make
  ;; to soften the dependency on Gash Core Utils gzip.
  (package
    (inherit gzip)
    (version "1.2.4")
    (name "gzip-mesboot")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gzip/gzip-" version ".tar"))
              (sha256
               (base32
                "1rhgk2vvmdvnn6vygf0dja92ryyng00knl0kz5srb77k2kryjb2d"))))
    (inputs '())
    (propagated-inputs '())
    (native-inputs `(("tcc" ,tcc-boot0)
                     ,@(%boot-gash-inputs)))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:strip-binaries? #f             ; no strip yet
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'scripted-patch
           (lambda _
             (substitute* "util.c"
               (("^char [*]strlwr" all) (string-append all "_tcc_cannot_handle_dupe")))))
         (replace 'build
           (lambda _
             (let ((files '("bits" "crypt" "deflate" "getopt" "gzip"
                            "inflate" "lzw" "trees" "unlzh" "unlzw"
                            "unpack" "unzip" "util" "zip")))
               (define (compile x)
                 (invoke "tcc" "-c" "-D NO_UTIME=1" "-D HAVE_UNISTD_H=1"
                         (string-append x ".c")))
               (for-each compile files)
               (apply invoke
                      (cons* "tcc" "-o" "gzip"
                             (map (lambda (x) (string-append x ".o")) files)))
               (link "gzip" "gunzip"))))
         (replace 'install
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "gzip" bin)
               (install-file "gunzip" bin))))
         (replace 'check
           (lambda _
             (invoke "./gzip" "--version")))
         ;; no gzip yet
         (delete 'compress-documentation))))))

(define gnu-make-mesboot0
  ;; The initial make
  (package
    (inherit gnu-make)
    (name "make-mesboot0")
    (version "3.82")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/make/make-" version ".tar.gz"))
              (sha256
               (base32
                "1rs2f9hmvy3q6zkl15jnlmnpgffm0bhw5ax0h5c7q604wqrip69x"))))
    (inputs '())
    (propagated-inputs '())
    (native-inputs `(("tcc" ,tcc-boot0)
                     ,@(%boot-gash-inputs)))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:configure-flags '("CC=tcc"
                           "CPP=tcc -E"
                           "LD=tcc"
                           ,(string-append "--build=" (commencement-build-target))
                           ,(string-append "--host=" (commencement-build-target))
                           "--disable-nls")
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:strip-binaries? #f             ; no strip yet
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'scripted-patch
           (lambda _
             (substitute* "build.sh.in"
               (("@LIBOBJS@") "getloadavg.o")
               (("@REMOTE@") "stub"))))
         (add-after 'configure 'configure-fixup
           (lambda _
             (substitute* "make.h"
               (("^extern long int lseek.*" all) (string-append "// " all)))))
         (replace 'build
           (lambda _
             (invoke "sh" "./build.sh")))
         (replace 'check                ; proper check needs awk
           (lambda _
             (invoke "./make" "--version")))
         (replace 'install
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "make" bin)))))))))

(define (%boot-tcc0-inputs)
  `(("make" ,gnu-make-mesboot0)
    ("tcc" ,tcc-boot0)
    ,@(%boot-gash-inputs)))

(define tcc-boot
 (let ((commit "8cd21e91ccee3baf15ad2f8cba9cbc4b618695a0")
       (revision "1139")
       (mes-system
        (cond ((target-x86-32?) "x86")
              ((target-x86-64?) "x86_64")
              ((target-aarch64?) "aarch64")
              ((target-riscv64?) "riscv64")
              (#t ""))))
  (package
    (inherit tcc-boot0)
    (name "tcc-boot")
    (version (string-append "0.9.27" "." revision "-" (string-take commit 7)))
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo.or.cz/tinycc.git/snapshot/"
                                  commit ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1nrv5ggcsczkw2vib4551c89rwzylf669z178yzm5agnm479cy8j"))))
    (build-system gnu-build-system)
    (inputs '())
    (propagated-inputs '())
    (native-inputs (%boot-tcc0-inputs))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:validate-runpath? #f           ; no dynamic executables
       #:strip-binaries? #f             ; no strip yet
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'scripted-patch
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "libtcc.c"
               (("s->nocommon = 1;" all)
                (string-append all "\ns->static_link = 1;")))))
         ;; Gash doesn't play well with passing flags to the configure script.
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (call-with-output-file
                 "config.h"
                 (lambda (port)
                   (display "#define TCC_VERSION \"0.9.28rc\" " port)))))

         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (tcc (assoc-ref inputs "tcc"))
                   (libc (assoc-ref inputs "libc"))
                   (interpreter "/mes/loader"))
               (invoke
                "tcc"
                "-vvv"
                "-D" "ONE_SOURCE=1"
                "-D" "CONFIG_TCC_STATIC=1"
                "-D" "CONFIG_USE_LIBGCC=1"
                "-D" "CONFIG_TCC_SEMLOCK=0"
                "-D" (string-append "CONFIG_TCCDIR=\"" out "/lib/tcc\"")
                "-D" (string-append "CONFIG_TCC_CRTPREFIX=\"" out "/lib:{B}/lib:.\"")
                "-D" (string-append "CONFIG_TCC_ELFINTERP=\"" interpreter "\"")
                "-D" (string-append "CONFIG_TCC_LIBPATHS=\""
                                    tcc "/lib:"
                                    out "/lib:"
                                    "{B}/lib:.\"")
                "-D" (string-append "CONFIG_TCC_SYSINCLUDEPATHS=\""
                                    tcc "/include:"
                                    out "/include:"
                                    "{B}/include\"")
                "-D" (string-append "TCC_LIBGCC=\"" tcc "/lib/libc.a\"")
                "-o" "tcc"
                "tcc.c"))))
         (add-after 'build 'build-libtcc1.a
           (lambda _
             (invoke "./tcc"
                     "-g" "-vvv"
                     "-I" "include"
                     "-c" "-o" "libtcc1.o" "lib/libtcc1.c")
             (cond
               (,(or (target-aarch64?)
                     (target-riscv64?))
                 (invoke "./tcc"
                         "-g" "-vvv"
                         "-I" "include"
                         "-c" "-o" "lib-arm64.o" "lib/lib-arm64.c")
                 (invoke "./tcc" "-ar" "rc" "libtcc1.a" "libtcc1.o" "lib-arm64.o"))
               (else
                 (invoke "./tcc" "-ar" "rc" "libtcc1.a" "libtcc1.o")))))
         (add-after 'build-libtcc1.a 'rebuild-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((tcc (assoc-ref inputs "tcc"))
                    (flags (list "-I" (string-append tcc "/include")
                                 "-I" (string-append tcc "/include/linux/" ,mes-system)
                                 "-I" "include")))
               (apply invoke "./tcc" "-g" "-vvv"
                      "-c" "-o" "libc.o"
                      (string-append tcc "/share/libc.c")
                      flags)
               (invoke "./tcc" "-ar" "rc" "libc.a" "libc.o")
               (apply invoke "./tcc" "-g" "-vvv"
                      "-c" "-o" "libgetopt.o"
                      (string-append tcc "/share/libgetopt.c")
                      flags)
               (invoke "./tcc" "-ar" "rc" "libgetopt.a" "libgetopt.o")
               (apply invoke "./tcc" "-g" "-vvv"
                      "-c" "-o" "crt1.o"
                      (string-append tcc "/share/crt1.c")
                      flags)
               (copy-file (string-append tcc "/lib/crti.o") "crti.o")
               (copy-file (string-append tcc "/lib/crtn.o") "crtn.o"))))
         (replace 'check
           (lambda _
             ;; FIXME: add sensible check target (without depending on make)
             ;; ./check.sh ?
             (= 1 (status:exit-val (system* "./tcc" "--help")))))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (tcc (assoc-ref inputs "tcc")))
               (mkdir-p (string-append out "/share"))
               (install-file "tcc" bin)
               (copy-recursively "include"
                                 (string-append out "/include"))
               (install-file "libtcc1.a" (string-append lib "/tcc"))
               (for-each (lambda (file)
                           (install-file file lib))
                         '("libtcc1.a" "libc.a" "libgetopt.a"
                           "crt1.o" "crti.o" "crtn.o"))
               ;; Install from previous tcc.
               (copy-recursively (string-append tcc "/include")
                                 (string-append out "/include"))
               (copy-recursively (string-append tcc "/share")
                                 (string-append out "/share")))))))))))

(define patch-mesboot
  ;; The initial patch.
  (package
    (inherit patch/pinned)
    (name "patch-mesboot")
    (version "2.5.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/patch/patch-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "12nv7jx3gxfp50y11nxzlnmqqrpicjggw6pcsq0wyavkkm3cddgc"))))
    (inputs '())
    (propagated-inputs '())
    (native-inputs (%boot-tcc0-inputs))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:parallel-build? #f
       #:tests? #f            ; check is naive, also checks non-built PROGRAMS
       #:strip-binaries? #f   ; no strip yet
       #:configure-flags '("AR=tcc -ar" "CC=tcc" "LD-tcc")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'scripted-patch
           (lambda _
             ;; avoid another segfault
             (substitute* "pch.c"
               (("while [(]p_end >= 0[)]" all)
                "p_end = -1;\nwhile (0)"))))
         ;; FIXME: no compressing gzip yet
         (delete 'compress-documentation))))))

(define (%boot-tcc-inputs)
  `(("gzip" ,gzip-mesboot)
    ("patch" ,patch-mesboot)
    ("tcc" ,tcc-boot)
    ,@(alist-delete "tcc" (%boot-tcc0-inputs))))

;; The last musl in the 1.1.x series.
;; For now this starts the riscv64 specific portion of the bootstrap.
(define musl-boot0
  (package
    (inherit musl)
    (name "musl-boot0")
    (version "1.1.24")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.musl-libc.org/releases/"
                                  "musl-" version ".tar.gz"))
              (sha256
               (base64 "E3DJqBKyzyp9koAlEMygBYzDfmanvt1wBR8KNAFQIqM="))))
    (native-inputs (%boot-tcc-inputs))
    (inputs '())
    (arguments
     (list
       #:tests? #f                      ; musl has no tests
       #:guile %bootstrap-guile
       #:implicit-inputs? #f
       #:strip-binaries? #f
       #:parallel-build? #f             ; Race conditions otherwise
       #:make-flags
       #~(list
           (string-append "SHELL=" #$(this-package-native-input "bash")
                          "/bin/bash")
           "CC=tcc"
           "AR=tcc -ar"
           "RANLIB=true"
           "CFLAGS=-DSYSCALL_NO_TLS -D__riscv_float_abi_soft -U__riscv_flen")
       #:configure-flags
       #~(let ((bash #$(this-package-native-input "bash")))
           (list "CC=tcc"
                 (string-append "CONFIG_SHELL=" bash "/bin/sh")
                 "--disable-shared"
                 "--disable-gcc-wrapper"))
       #:phases
       #~(modify-phases %standard-phases
           (add-before 'build 'add-libg
             (lambda _
               (substitute* "Makefile"
                   (("(EMPTY_LIB_NAMES = )(.*)" all var content)
                    (string-append var "g " content)))))
           (add-before 'build 'patch-shebang-in-makefile
             (lambda _
               (let ((bash #$(this-package-native-input "bash")))
                 (substitute* "Makefile"
                   (("#!/bin/sh") (string-append "#!" bash "/bin/bash"))))))
           (add-after 'configure 'remove-complex
             (lambda _
               (delete-file-recursively "src/complex")))
           (add-after 'unpack 'remove-optimized-math
             ;; TCC does not support the extended asm for float registers.
             ;; All src/math/{i386,x86_64}/*.c files make use of it.  Luckily
             ;; musl has an automatic fallback to generic C implementations
             ;; in src/math.  Therefore we can simply delete these files.
             (lambda _
               (for-each (lambda (path)
                           (for-each delete-file (find-files path "\\.c$")))
                         '("src/math/i386" "src/math/x86_64"))))
           (add-after 'unpack 'adjust-i386-setjmp
             (lambda _
               ;; We can't just delete the file or we get:
               ;; tcc: error: undefined symbol 'sigsetjmp'
               (substitute* "src/signal/i386/sigsetjmp.s"
                 ;; TCC has a bug with forward referencing numeric labels.  We
                 ;; move the label and its code to the top.
                 (("^1:[\t ]*jmp ___setjmp") "")
                 (("^sigsetjmp:") "1:\tjmp ___setjmp\nsigsetjmp:")
                 ;; And we turn the forward into a backward reference.
                 (("jecxz 1f") "jecxz 1b"))))
           (add-after 'unpack 'use-fallback-ldso-on-riscv64
             ;; src/ldso/riscv64/tlsdesc.s:13: error: 'add': Expected second source
             ;; operand that is a register or immediate
             (lambda _
               (delete-file "src/ldso/riscv64/tlsdesc.s")))
           ;; We can't use the install script since it doesn't play well with gash.
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (lib (string-append out "/lib"))
                      (incl (string-append out "/include"))
                      ;; Taken from the ARCH variable in configure.
                      (arch #$(cond ((target-x86-32?) "i386")
                                    ((target-x32?) "x32")
                                    ((target-arm32?) "arm")
                                    ((target-ppc64le?) "powerpc64")
                                    (#t (string-take
                                          (%current-system)
                                          (string-index (%current-system) #\-))))))
                 (for-each (lambda (file)
                             (when (file-exists? file)
                               (install-file file bin)))
                           '("obj/ld.musl-clang"
                             "obj/musl-clang"
                             "obj/musl-gcc"))
                 (for-each (lambda (file)
                             (install-file file lib))
                           (find-files "lib" "\\.(a|o|so)$"))
                 (when (file-exists? "lib/musl-gcc.specs")
                   (install-file "lib/musl-gcc.specs" lib))
                 (copy-recursively "include" incl)
                 (for-each (lambda (file)
                             (install-file file (string-append incl "/bits")))
                           (append
                             ;; Instead of checking if the header already exists
                             ;; install them in a 'backwards' order, so the
                             ;; preferred one never ends up overridden.
                             (find-files "obj/include/bits")
                             (find-files "arch/generic/bits")
                             (find-files (string-append "arch/" arch "/bits"))))
                 (when (file-exists? (string-append lib "/libc.so"))
                   (symlink "libc.so"
                            (string-append lib "/ld-musl-" arch ".so.1")))))))))))

(define tcc-boot-musl
  (package
    (inherit tcc-boot)
    (name "tcc-boot-musl")
    (native-inputs `(("libc" ,musl-boot0)
                     ,@(modify-inputs (package-native-inputs tcc-boot)
                                      (replace "tcc" tcc-boot))))
    (arguments
      (substitute-keyword-arguments (package-arguments tcc-boot)
        ((#:phases phases)
         #~(modify-phases #$phases
             (delete 'rebuild-libraries)
             (replace 'build
               (lambda* (#:key outputs inputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "out"))
                       (libc (assoc-ref inputs "libc"))
                       (tcc (assoc-ref inputs "tcc"))
                       (interpreter "/musl/loader"))
                   (invoke
                    "tcc"
                    "-g"
                    "-vvv"
                    "-I" (string-append tcc "/include")
                    "-L" (string-append tcc "/lib")
                    "-D" "ONE_SOURCE=1"
                    "-D" "TCC_VERSION=\"0.9.28rc\""
                    "-D" "CONFIG_TCC_STATIC=1"
                    "-D" "CONFIG_USE_LIBGCC=1"
                    "-D" "CONFIG_TCC_SEMLOCK=0"
                    "-D" (string-append "CONFIG_TCCDIR=\"" out "/lib/tcc\"")
                    "-D" (string-append "CONFIG_TCC_CRTPREFIX=\"" libc "/lib\"")
                    "-D" (string-append "CONFIG_TCC_ELFINTERP=\"" interpreter "\"")
                    "-D" (string-append "CONFIG_TCC_LIBPATHS=\"" libc "/lib:"
                                                                 out "/lib:"
                                                                 "{B}/lib:.\"")
                    "-D" (string-append "CONFIG_TCC_SYSINCLUDEPATHS=\""
                                        libc "/include:"
                                        out "/include:"
                                        "{B}/include\"")
                    "-D" (string-append "TCC_LIBGCC=\"" libc "/lib/libc.a\"")
                    "-o" "tcc"
                    "tcc.c"))))
             (replace 'install
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "out")))
                   (install-file "tcc" (string-append out "/bin"))
                   (copy-recursively "include"
                                     (string-append out "/include"))
                   (install-file "libtcc1.a" (string-append out "/lib"))
                   (install-file "libtcc1.a" (string-append out "/lib/tcc")))))))))))

(define tcc-musl
  (package
    (inherit tcc-boot-musl)
    (name "tcc-musl")
    (native-inputs (modify-inputs (package-native-inputs tcc-boot-musl)
                                  (replace "tcc" tcc-boot-musl)))
    (arguments
      (substitute-keyword-arguments (package-arguments tcc-boot-musl)
        ((#:phases phases)
         #~(modify-phases #$phases
             (replace 'build
               (lambda* (#:key outputs inputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "out"))
                       (libc (assoc-ref inputs "libc"))
                       (interpreter "/musl/loader"))
                   (invoke
                    "tcc"
                    "-g"
                    "-vvv"
                    ;; Some missed bits from musl in arch/$ARCH/bits/signal.h
                    #$@(cond
                         ((target-riscv64?)
                          #~("-D" "REG_PC=0"
                             "-D" "REG_S0=8"))
                         ((target-x86-64?)
                          #~("-D" "REG_EBP=6"
                             "-D" "REG_EIP=14"))
                         (#t #~()))
                    "-D" "ONE_SOURCE=1"
                    "-D" "TCC_VERSION=\"0.9.28rc\""
                    "-D" "CONFIG_TCC_STATIC=1"
                    "-D" "CONFIG_USE_LIBGCC=1"
                    "-D" "CONFIG_TCC_SEMLOCK=0"
                    "-D" (string-append "CONFIG_TCCDIR=\"" out "/lib/tcc\"")
                    "-D" (string-append "CONFIG_TCC_CRTPREFIX=\"" libc "/lib\"")
                    "-D" (string-append "CONFIG_TCC_ELFINTERP=\"" interpreter "\"")
                    "-D" (string-append "CONFIG_TCC_LIBPATHS=\"" libc "/lib:"
                                                                 out "/lib:"
                                                                 "{B}/lib:.\"")
                    "-D" (string-append "CONFIG_TCC_SYSINCLUDEPATHS=\""
                                        libc "/include:"
                                        out "/include:"
                                        "{B}/include\"")
                    "-D" (string-append "TCC_LIBGCC=\"" libc "/lib/libc.a\"")
                    "-o" "tcc"
                    "tcc.c"))))))))))

;; Gash served us well, but there are known issues on riscv64.
;; OpenBSD's ksh will do just fine as a replacement until we get to bash.
(define oksh-muslboot0
  (package
    (inherit oksh)
    (source (bootstrap-origin (package-source oksh)))
    (arguments
     (list
       #:implicit-inputs? #f
       #:guile %bootstrap-guile
       #:tests? #f                  ; No tests.
       #:strip-binaries? #f         ; No strip yet.
       #:parallel-build? #f         ; Race conditions.
       #:configure-flags
       #~(list "--cc=tcc"
               "--enable-static")
       #:phases
       #~(modify-phases %standard-phases
           ;; make: install: Command not found
           (replace 'install
             (lambda _
               (install-file "oksh" (string-append %output "/bin"))
               (install-file "oksh.1" (string-append %output "/share/man/man1"))
               ;; For compatibility and ease of use in later builds.
               (symlink "oksh" (string-append %output "/bin/sh"))
               (symlink "oksh" (string-append %output "/bin/bash"))))
           (delete 'compress-documentation))))
    (native-inputs (modify-inputs (package-native-inputs tcc-musl)
                                  (replace "tcc" tcc-musl)))))

(define binutils-muslboot0
  ;; The initial Binutils
  (package
    (inherit binutils)
    (name "binutils-muslboot0")
    (version "2.30")
    (source (bootstrap-origin
             (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/binutils/binutils-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "1sp9g7zrrcsl25hxiqzmmcrdlbm7rbmj0vki18lks28wblcm0f4c")))))
    (inputs '())
    (propagated-inputs '())
    (native-inputs
     (modify-inputs (%boot-tcc-inputs)
                    (replace "tcc" tcc-musl)
                    (replace "bash" oksh-muslboot0)))
    (arguments
     (list #:implicit-inputs? #f
           #:guile %bootstrap-guile
           #:tests? #f          ; tests hang forever
           #:parallel-build? #f
           #:strip-binaries? #f ; no strip yet
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'configure 'fix-build
                 (lambda _
                   ;; bfd/po doesn't have a Makefile, so the recursive calls just
                   ;; fail. We add files with the same name Make targets have, to
                   ;; trick Make into thinking there's nothing to do.
                   (call-with-output-file "bfd/po/install"
                     (lambda (p) (display "" p)))
                   (call-with-output-file "bfd/po/all"
                     (lambda (p) (display "" p)))
                   (call-with-output-file "bfd/po/info"
                     (lambda (p) (display "" p))))))
           #:configure-flags
           #~(list
               (string-append "CONFIG_SHELL="
                              (search-input-file %build-inputs "/bin/bash"))
               (string-append "SHELL="
                              (search-input-file %build-inputs "/bin/bash"))
               "CFLAGS=-g"
               "CC=tcc"
               "LD=tcc"
               "AR=tcc -ar"
               "MAKEINFO=true"
               "RANLIB=true"
               "--enable-64-bit-bfd"
               "--disable-nls"
               "--disable-shared"
               "--disable-werror"
               "--disable-plugins"
               "--enable-deterministic-archives"
               "--with-sysroot=/"
               (string-append "--build=" #$(commencement-build-target))
               (string-append "--host=" #$(commencement-build-target)))))))

(define (%boot-tcc-musl-inputs)
  ;; Adjust the naming so that we can more easily swap for their counterparts
  ;; later in the bootstrap chain.
  `(("gcc" ,tcc-musl)
    ("binutils" ,binutils-muslboot0)
    ("kernel-headers" ,%bootstrap-linux-libre-headers)
    ("libc" ,musl-boot0)
    ("bash" ,oksh-muslboot0)
    ,@(fold alist-delete (%boot-tcc-inputs)
            '("bash" "tcc"))))

(define binutils-mesboot0
  ;; The initial Binutils
  (package
    (inherit binutils)
    (name "binutils-mesboot0")
    (version "2.20.1a")
    (source (bootstrap-origin
             (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/binutils/binutils-"
                                   version ".tar.bz2"))
               (patches (search-patches "binutils-boot-2.20.1a.patch"))
               (patch-guile %bootstrap-guile)
               (sha256
                (base32
                 "0r7dr0brfpchh5ic0z9r4yxqn4ybzmlh25sbp30cacqk8nb7rlvi")))))
    (inputs '())
    (propagated-inputs '())
    (native-inputs (%boot-tcc-inputs))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (arguments
     (list #:implicit-inputs? #f
           #:guile %bootstrap-guile
           #:tests? #f ; runtest: command not found
           #:parallel-build? #f
           #:strip-binaries? #f ; no strip yet
           #:configure-flags
           #~(let ((cppflags (string-append
                              " -D __GLIBC_MINOR__=6"
                              " -D MES_BOOTSTRAP=1"))
                   (bash (assoc-ref %build-inputs "bash")))
               `(,(string-append "CONFIG_SHELL=" bash "/bin/sh")
                 ,(string-append "CPPFLAGS=" cppflags)
                 "AR=tcc -ar"
                 "CXX=false"
                 "RANLIB=true"
                 ,(string-append "CC=tcc" cppflags)
                 "--disable-nls"
                 "--disable-shared"
                 "--disable-werror"
                 ,(string-append "--build=" #$(commencement-build-target))
                 ,(string-append "--host=" #$(commencement-build-target))
                 "--with-sysroot=/"))))))

(define gcc-core-mesboot0
  ;; Gcc-2.95.3 is the most recent GCC that is supported by what the Mes C
  ;; Library v0.16 offers.  Gcc-3.x (and 4.x) place higher demands on a C
  ;; library, such as dir.h/struct DIR/readdir, locales, signals...  Also,
  ;; with gcc-2.95.3, binutils (2.14.0, 2.20.1a) and glibc-2.2.5 we found a
  ;; GNU toolchain triplet "that works".
  (package
    (inherit gcc)
    (name "gcc-core-mesboot0")
    (version "2.95.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gcc/gcc-2.95.3/gcc-core-"
                                  version
                                  ".tar.gz"))
              ;; `patches' needs XZ
              ;; (patches (search-patches "gcc-boot-2.95.3.patch"))
              (sha256
               (base32
                "1xvfy4pqhrd5v2cv8lzf63iqg92k09g6z9n2ah6ndd4h17k1x0an"))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs `(("binutils" ,binutils-mesboot0)
                     ,@(%boot-tcc-inputs)))
    (outputs '("out"))
    (arguments
     (list #:implicit-inputs? #f
           #:guile %bootstrap-guile
           #:tests? #f
           #:parallel-build? #f
           #:strip-binaries? #f
           #:configure-flags
           #~(let ((out (assoc-ref %outputs "out")))
               `("--enable-static"
                 "--disable-shared"
                 "--disable-werror"
                 ,(string-append "--build=" #$(commencement-build-target))
                 ,(string-append "--host=" #$(commencement-build-target))
                 ,(string-append "--prefix=" out)))
           #:make-flags
           #~`("CC=tcc -static -D __GLIBC_MINOR__=6"
               "OLDCC=tcc -static -D __GLIBC_MINOR__=6"
               "CC_FOR_BUILD=tcc -static -D __GLIBC_MINOR__=6"
               "AR=ar"
               "RANLIB=ranlib"
               ,(string-append "LIBGCC2_INCLUDES=-I "
                               (assoc-ref %build-inputs "tcc")
                               "/include")
               "LANGUAGES=c"
               ,(string-append "BOOT_LDFLAGS="
                               " -B" (assoc-ref %build-inputs "tcc")
                               "/lib/"))
           #:modules '((guix build gnu-build-system)
                       (guix build utils)
                       (srfi srfi-1))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'apply-boot-patch
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((patch-file
                          #$(local-file
                             (search-patch "gcc-boot-2.95.3.patch"))))
                     (invoke "patch" "--force" "-p1" "-i" patch-file))))
               (add-before 'configure 'setenv
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (bash (assoc-ref %build-inputs "bash"))
                          (shell (string-append bash "/bin/bash"))
                          (tcc (assoc-ref %build-inputs "tcc"))
                          (cppflags " -D __GLIBC_MINOR__=6"))
                     (setenv "CONFIG_SHELL" shell)
                     (setenv "CPPFLAGS" cppflags)
                     (setenv "CC" (string-append "tcc" cppflags))
                     (setenv "CC_FOR_BUILD" (string-append "tcc" cppflags))
                     (setenv "CPP" (string-append "tcc -E" cppflags))
                     (with-output-to-file "config.cache"
                       (lambda _
                         (display "
ac_cv_c_float_format='IEEE (little-endian)'
"))))))
               ;; gcc-2.95.3
               (replace 'configure           ; needs classic invocation of configure
                 (lambda* (#:key configure-flags  #:allow-other-keys)
                   (format (current-error-port)
                           "running ./configure ~a\n" (string-join configure-flags))
                   (apply invoke "./configure" configure-flags)))
               (add-after 'configure 'remove-info
                 (lambda _
                   ;; no info at this stage
                   (delete-file-recursively "texinfo")
                   (invoke "touch" "gcc/cpp.info" "gcc/gcc.info")))
               (add-after 'install 'install2
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((tcc (assoc-ref %build-inputs "tcc"))
                          (out (assoc-ref outputs "out"))
                          (gcc-dir (string-append
                                    out "/lib/gcc-lib/" #$(commencement-build-target)
                                    "/2.95.3")))
                     (mkdir-p "tmp")
                     (with-directory-excursion "tmp"
                       (invoke "ar" "x" (string-append "../gcc/libgcc2.a"))
                       (invoke "ar" "x" (string-append tcc "/lib/libtcc1.a"))
                       (apply invoke "ar" "r" (string-append gcc-dir "/libgcc.a")
                              (find-files "." "\\.o")))
                     (copy-file "gcc/libgcc2.a" (string-append out "/lib/libgcc2.a"))
                     (copy-file (string-append tcc "/lib/libtcc1.a")
                                (string-append out "/lib/libtcc1.a"))
                     (invoke "ar" "x" (string-append tcc "/lib/libtcc1.a"))
                     (invoke "ar" "x" (string-append tcc "/lib/libc.a"))
                     (invoke "ar" "r" (string-append gcc-dir "/libc.a")
                             "libc.o" "libtcc1.o")))))))
    (native-search-paths
     (list (search-path-specification
            (variable "C_INCLUDE_PATH")
            (files `("include"

                     ;; Needed to get things like GCC's <stddef.h>.
                     "lib/gcc-lib/" ,(commencement-build-target) "/2.95.3/include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (files '("lib")))))))

(define (%boot-mesboot-core-inputs)
  `(("binutils" ,binutils-mesboot0)
    ("gcc" ,gcc-core-mesboot0)
    ,@(alist-delete "tcc" (%boot-tcc-inputs))))

(define mesboot-headers
  (package
    (inherit mes-boot)
    (name "mesboot-headers")
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs `(("kernel-headers" ,%bootstrap-linux-libre-headers)
                     ,@(%boot-tcc-inputs)))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:tests? #f
       #:strip-binaries? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (include (string-append out "/include"))
                    (headers (assoc-ref %build-inputs "kernel-headers")))
               (mkdir-p include)
               (copy-recursively "include" out)
               (copy-recursively headers out)))))))))

(define glibc-mesboot0
  ;; GNU C Library 2.2.5 is the most recent glibc that we managed to build
  ;; using gcc-2.95.3.  Newer versions (2.3.x, 2.6, 2.1x) seem to need a newer
  ;; gcc.
  (package
    (inherit glibc)
    (name "glibc-mesboot0")
    (version "2.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/glibc/glibc-"
                                  version
                                  ".tar.gz"))
              ;; Patch needs XZ
              ;; (patches (search-patches "glibc-boot-2.2.5.patch"))
              (sha256
               (base32
                "1vl48i16gx6h68whjyhgnn1s57vqq32f9ygfa2fls7pdkbsqvp2q"))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs `(("headers" ,mesboot-headers)
                     ,@(%boot-mesboot-core-inputs)))
    (outputs '("out"))
    (arguments
     (list #:implicit-inputs? #f
           #:guile %bootstrap-guile
           #:tests? #f
           #:strip-binaries? #f
           #:validate-runpath? #f   ; no dynamic executables
           #:parallel-build? #f     ; gcc-2.95.3 ICEs on massively parallel builds
           #:make-flags #~(list (string-append
                                 "SHELL="
                                 (assoc-ref %build-inputs "bash")
                                 "/bin/sh"))
           #:configure-flags
           #~(let ((out (assoc-ref %outputs "out"))
                   (headers (assoc-ref %build-inputs "headers")))
               `("--disable-shared"
                 "--enable-static"
                 "--disable-sanity-checks"
                 ,(string-append "--build=" #$(commencement-build-target))
                 ,(string-append "--host=" #$(commencement-build-target))
                 ,(string-append "--with-headers=" headers "/include")
                 "--enable-static-nss"
                 "--without-__thread"
                 "--without-cvs"
                 "--without-gd"
                 "--without-tls"
                 ,(string-append "--prefix=" out)))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'apply-boot-patch
                 (lambda* (#:key inputs #:allow-other-keys)
                   (invoke "patch" "--force" "-p1" "-i"
                           #$(local-file
                              (search-patch "glibc-boot-2.2.5.patch")))
                   (invoke "patch" "--force" "-p1" "-i"
                           #$(local-file
                              (search-patch "glibc-bootstrap-system-2.2.5.patch")))))
               (add-before 'configure 'setenv
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (bash (assoc-ref %build-inputs "bash"))
                          (shell (string-append bash "/bin/bash"))
                          (gcc (assoc-ref %build-inputs "gcc"))
                          (headers (assoc-ref %build-inputs "headers"))
                          (cppflags (string-append
                                     ;;" -D __STDC__=1"
                                     " -D MES_BOOTSTRAP=1"
                                     " -D BOOTSTRAP_GLIBC=1"))
                          (cflags (string-append " -L " (getcwd))))
                     (setenv "CONFIG_SHELL" shell)
                     (setenv "SHELL" shell)
                     (setenv "CPP" (string-append gcc "/bin/gcc -E " cppflags))
                     (setenv "CC" (string-append gcc "/bin/gcc " cppflags cflags)))))
               (replace 'configure           ; needs classic invocation of configure
                 (lambda* (#:key configure-flags #:allow-other-keys)
                   (format (current-error-port)
                           "running ./configure ~a\n" (string-join configure-flags))
                   (apply invoke "./configure" configure-flags)))
               (add-after 'configure 'fixup-configure
                 (lambda _
                   (let* ((out (assoc-ref %outputs "out"))
                          (bash (assoc-ref %build-inputs "bash"))
                          (shell (string-append bash "/bin/bash")))
                     (substitute* "config.make"
                       (("INSTALL = scripts/") "INSTALL = $(..)./scripts/"))
                     (substitute* "config.make"
                       (("INSTALL = scripts/") "INSTALL = $(..)./scripts/")
                       (("BASH = ") (string-append
                                     "SHELL = " shell "
         BASH = ")))))))))))

(define gcc-mesboot0
  (package
    (inherit gcc-core-mesboot0)
    (name "gcc-mesboot0")
    (native-inputs `(;; Packages are given in an order that's relevant for
                     ;; #include_next purposes.
                     ("libc" ,glibc-mesboot0)
                     ("kernel-headers" ,%bootstrap-linux-libre-headers)
                     ,@(%boot-mesboot-core-inputs)))
    (arguments
     (substitute-keyword-arguments (package-arguments gcc-core-mesboot0)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'setenv
              (lambda _
                (setenv "CONFIG_SHELL" (which "sh"))
                (with-output-to-file "config.cache"
                  (lambda _
                    (display "
ac_cv_c_float_format='IEEE (little-endian)'
")))))
            (replace 'install2
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (gcc-dir (string-append
                                 out "/lib/gcc-lib/" #$(commencement-build-target)
                                 "/2.95.3")))
                  (and
                   (mkdir-p "tmp")
                   (zero? (system (string-append "set -x; cd tmp && ar x ../gcc/libgcc2.a")))
                   (zero? (system (string-append "set -x; cd tmp && ar r " gcc-dir "/libgcc.a *.o")))
                   (copy-file "gcc/libgcc2.a" (string-append out "/lib/libgcc2.a"))))))))
       ((#:configure-flags configure-flags)
        #~(let ((out (assoc-ref %outputs "out")))
            `("--disable-shared"
              "--disable-werror"
              ,(string-append "--build=" #$(commencement-build-target))
              ,(string-append "--host=" #$(commencement-build-target))
              ,(string-append "--prefix=" out))))
       ((#:make-flags make-flags)
        #~(let ((gcc (assoc-ref %build-inputs "gcc")))
            `("RANLIB=true"
              ,(string-append "LIBGCC2_INCLUDES=-I " gcc "/include")
              "LANGUAGES=c")))))))

(define (%boot-mesboot0-inputs)
  `(("gcc" ,gcc-mesboot0)
    ("kernel-headers" ,%bootstrap-linux-libre-headers)
    ("libc" ,glibc-mesboot0)
    ,@(alist-delete "gcc" (%boot-mesboot-core-inputs))))

(define binutils-mesboot1
  (package
    (inherit binutils-mesboot0)
    (name "binutils-mesboot1")
    (native-inputs (%boot-mesboot0-inputs))
    (arguments
     (substitute-keyword-arguments (package-arguments binutils-mesboot0)
       ((#:configure-flags configure-flags)
        #~(let ((out (assoc-ref %outputs "out")))
            `("--disable-nls" "--disable-shared"
              "--disable-werror"
              ,(string-append "--build=" #$(commencement-build-target))
              ,(string-append "--host=" #$(commencement-build-target))
              "--with-sysroot=/"
              ,(string-append "--prefix=" out))))))))

(define gnu-make-mesboot
  (package
    (inherit gnu-make)
    (name "make-mesboot")
    (version "3.82")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/make/make-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1rs2f9hmvy3q6zkl15jnlmnpgffm0bhw5ax0h5c7q604wqrip69x"))))
    (native-inputs (%boot-mesboot0-inputs))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (inputs '())
    (propagated-inputs '())
    (arguments
     `(#:implicit-inputs? #f
       #:parallel-build? #f
       #:guile ,%bootstrap-guile
       #:configure-flags '("LIBS=-lc -lnss_files -lnss_dns -lresolv")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "./make" "--version")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "make" bin)))))))))

(define (%boot-mesboot1-inputs)
  `(("binutils" ,binutils-mesboot1)
    ("make" ,gnu-make-mesboot)
    ,@(fold alist-delete (%boot-mesboot0-inputs)
            '("binutils" "make"))))

(define m4-boot
  (package
    (inherit m4)
    (name "m4-boot")
    (version "1.4.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/m4/m4-" version ".tar.gz"))
              (sha256
               (base32 "1arz972zxmwhnyik9007g6ww9gars8d55xbvg548xlsw3a9369mb"))))
    (native-inputs (if (target-x86?)
                       (%boot-mesboot1-inputs)
                       (%boot-tcc-musl-inputs)))
    (inputs '())
    (propagated-inputs '())
    (arguments
     (list
       #:tests? #f
       #:guile %bootstrap-guile
       #:implicit-inputs? #f
       #:parallel-build? (target-x86?)
       #:configure-flags
       #~(list #$@(if (target-x86?)
                      #~()
                      #~("CC=tcc"))
               (string-append "--build=" #$(commencement-build-target))
               (string-append "--host=" #$(commencement-build-target)))))))

(define gmp-boot
  (package
    (inherit gmp)
    (outputs '("out"))
    (name "gmp-boot")
    (version "5.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gmp/gmp-" version ".tar.gz"))
              (sha256
               (base32 "09w5yzlvdll19fa9zhh0f4y97hv8483cbly0003zvbvyighpzwvi"))))
    (native-inputs (if (target-x86?)
                       (%boot-mesboot1-inputs)
                       (%boot-tcc-musl-inputs)))
    (inputs (list m4-boot))
    (propagated-inputs '())
    (arguments
     (list
       #:guile %bootstrap-guile
       #:tests? #f
       #:implicit-inputs? #f
       #:parallel-build? (target-x86?)
       #:configure-flags
       #~(list #$@(if (target-x86?)
                      #~()
                      #~("CC=tcc"
                         "CFLAGS=-DHAVE_ALLOCA_H"))
               ;; These break building on x86_64-linux.
               ;(string-append "--build=" #$(commencement-build-target))
               ;(string-append "--host=" #$(commencement-build-target))
               "--enable-static"
               "--disable-shared"
               "--disable-assembly")
       ;; Gash crashes on mkdir called through install creating %output/share/info
       #:make-flags #~(list "MKDIRPROG=mkdir -p")))))

(define mpfr-boot
  (package
    (inherit mpfr)
    (outputs '("out"))
    (name "mpfr-boot")
    (version "4.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/mpfr/mpfr-" version ".tar.gz"))
              (sha256
               (base32 "1mm2zxjqxxqlacd87cxlyi63pwrxwafqks7lmpqa3wqq6a0zw9ri"))))
    (native-inputs (if (target-x86?)
                       (%boot-mesboot1-inputs)
                       (%boot-tcc-musl-inputs)))
    (inputs '())
    (propagated-inputs (list gmp-boot))
    (arguments
     (list
       #:guile %bootstrap-guile
       #:tests? #f
       #:implicit-inputs? #f
       #:parallel-build? (target-x86?)
       #:configure-flags
       #~(list #$@(if (target-x86?)
                      #~()
                      #~("CC=tcc"
                         "CFLAGS=-DHAVE_ALLOCA_H"))
               (string-append "--build=" #$(commencement-build-target))
               (string-append "--host=" #$(commencement-build-target))
               "--enable-static"
               "--disable-shared")))))

(define mpc-boot
  (package
    (inherit mpc)
    (outputs '("out"))
    (name "mpc-boot")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/mpc/mpc-" version ".tar.gz"))
              (sha256
               (base32 "0n846hqfqvmsmim7qdlms0qr86f1hck19p12nq3g3z2x74n3sl0p"))))
    (native-inputs (if (target-x86?)
                       (%boot-mesboot1-inputs)
                       (%boot-tcc-musl-inputs)))
    (inputs '())
    (propagated-inputs (list gmp-boot mpfr-boot))
    (arguments
     (list
       #:guile %bootstrap-guile
       #:tests? #f
       #:implicit-inputs? #f
       #:parallel-build? (target-x86?)
       #:configure-flags
       #~(list #$@(if (target-x86?)
                      #~()
                      #~("CC=tcc"
                         "CFLAGS=-DHAVE_ALLOCA_H"))
               (string-append "--build=" #$(commencement-build-target))
               (string-append "--host=" #$(commencement-build-target))
               "--enable-static"
               "--disable-shared")))))

(define gcc-core-mesboot1
  ;; GCC 4.6.4 is the latest modular distribution.  This package is not
  ;; stricly needed, but very helpful for development because it builds
  ;; relatively fast.  If this configures and builds then gcc-mesboot1 also
  ;; builds.
  (package
    (inherit gcc-mesboot0)
    (name "gcc-core-mesboot1")
    (version "4.6.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gcc/gcc-"
                                  version "/gcc-core-" version ".tar.gz"))
              (sha256
               (base32
                "173kdb188qg79pcz073cj9967rs2vzanyjdjyxy9v0xb0p5sad75"))))
    (inputs (list gmp-boot mpfr-boot mpc-boot))
    (native-inputs (%boot-mesboot1-inputs))
    (arguments
     (list #:implicit-inputs? #f
           #:guile %bootstrap-guile
           #:tests? #f
           #:modules '((guix build gnu-build-system)
                       (guix build utils)
                       (srfi srfi-1))
           #:parallel-build? #f             ; for debugging
           #:make-flags
           #~(let* ((libc (assoc-ref %build-inputs "libc"))
                    (ldflags (string-append
                              "-B" libc "/lib "
                              "-Wl,-dynamic-linker "
                              "-Wl," libc
                              #$(glibc-dynamic-linker
                                  (gnu-triplet->nix-system
                                    (commencement-build-target))))))
               (list (string-append "LDFLAGS=" ldflags)
                     (string-append "LDFLAGS_FOR_TARGET=" ldflags)))
           #:configure-flags
           #~(let ((out (assoc-ref %outputs "out"))
                   (glibc (assoc-ref %build-inputs "libc")))
               (list (string-append "--prefix=" out)
                     (string-append "--build=" #$(commencement-build-target))
                     (string-append "--host=" #$(commencement-build-target))
                     (string-append "--with-native-system-header-dir=" glibc "/include")
                     (string-append "--with-build-sysroot=" glibc "/include")
                     "--disable-bootstrap"
                     "--disable-decimal-float"
                     "--disable-libatomic"
                     "--disable-libcilkrts"
                     "--disable-libgomp"
                     "--disable-libitm"
                     "--disable-libmudflap"
                     "--disable-libquadmath"
                     "--disable-libsanitizer"
                     "--disable-libssp"
                     "--disable-libvtv"
                     "--disable-lto"
                     "--disable-lto-plugin"
                     "--disable-multilib"
                     "--disable-plugin"
                     "--disable-threads"
                     "--enable-languages=c"
                     "--enable-static"
                     "--disable-shared"
                     "--enable-threads=single"
                     "--disable-libstdcxx-pch"
                     "--disable-build-with-cxx"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'apply-boot-patch
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((patch-file
                          #$(local-file
                             (search-patch "gcc-boot-4.6.4.patch"))))
                     (invoke "patch" "--force" "-p1" "-i" patch-file)))))))))

(define gcc-muslboot0
  (package
    (inherit gcc-4.7)
    (name "gcc-muslboot0")
    (version "4.6.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gcc/gcc-"
                                  version "/gcc-core-" version ".tar.gz"))
              (sha256
               (base32
                "173kdb188qg79pcz073cj9967rs2vzanyjdjyxy9v0xb0p5sad75"))))
    (outputs '("out"))
    (inputs (list gmp-boot mpfr-boot mpc-boot))
    (native-inputs (%boot-tcc-musl-inputs))
    (arguments
     (list #:implicit-inputs? #f
           #:guile %bootstrap-guile
           #:tests? #f
           #:modules '((guix build gnu-build-system)
                       (guix build utils)
                       (srfi srfi-1))
           #:parallel-build? #f             ; for debugging
           #:configure-flags
           #~(let ((out  (assoc-ref %outputs "out"))
                   (libc (assoc-ref %build-inputs "libc"))
                   (bash (assoc-ref %build-inputs "bash")))
               (list (string-append "--prefix=" out)
                     (string-append "--with-build-sysroot=" libc "/include")
                     (string-append "--with-native-system-header-dir=" libc "/include")
                     (string-append "--build="
                                    #$(string-replace-substring
                                        (commencement-build-target)
                                        "-gnu" "-musl"))
                     (string-append "--host="
                                    #$(string-replace-substring
                                        (commencement-build-target)
                                        "-gnu" "-musl"))
                     "--disable-bootstrap"
                     "--disable-decimal-float"
                     "--disable-libatomic"
                     "--disable-libcilkrts"
                     "--disable-libgomp"
                     "--disable-libitm"
                     "--disable-libmudflap"
                     "--disable-libquadmath"
                     "--disable-libsanitizer"
                     "--disable-libssp"
                     "--disable-libvtv"
                     "--disable-lto"
                     "--disable-lto-plugin"
                     "--disable-multilib"
                     "--disable-plugin"
                     "--disable-threads"
                     "--enable-languages=c"
                     "--enable-static"
                     "--disable-shared"
                     "--enable-threads=single"
                     "--disable-libstdcxx-pch"
                     "--disable-build-with-cxx"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'apply-riscv64-patch
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((patch-file
                          #$(local-file
                             (search-patch "gcc-boot-4.6.4-riscv64-support.patch"))))
                     (invoke "patch" "--force" "-p1" "-i" patch-file))))
               (add-after 'unpack 'fix-alloca
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* (list "libiberty/alloca.c"
                                      "include/libiberty.h")
                     (("C_alloca") "alloca"))))
               (add-before 'configure 'fix-dynamic-linker-for-musl
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((libc (assoc-ref inputs "libc")))
                     ;; Fix the dynamic linker's file name.
                     ;; This should work on gcc-13 for all architectures except loongarch.
                     (substitute* (find-files "gcc/config"
                                              "^(aarch64-)?(linux|gnu|sysv4)(64|-elf|-eabi)?\\.h$")
                       (("(#define MUSL_DYNAMIC_LINKER*).*$" _ dynamic-linker)
                        ;; TODO: Make this use architecture specific ld-musl-*.so
                        (string-append dynamic-linker " \"" libc "/lib/libc.so\"")))
                     ;; We also need to adjust the references made for glibc
                     ;; to point to the musl linker location
                     (substitute* (find-files "gcc/config"
                                              "^(linux|gnu|sysv4)(64|-elf|-eabi)?\\.h$")
                       (("#define (GLIBC|GNU_USER)_DYNAMIC_LINKER([^ \t]*).*$"
                         _ gnu-user suffix)
                        (format #f "#define ~a_DYNAMIC_LINKER~a \"~a\"~%"
                                gnu-user suffix
                                (string-append libc "/lib/libc.so")))))))
               (add-after 'apply-riscv64-patch 'patch-for-modern-libc
                 (lambda _
                   (for-each
                     (lambda (dir)
                       (substitute* (string-append "gcc/config/"
                                                   dir "/linux-unwind.h")
                                    (("struct ucontext") "ucontext_t")))
                     '("alpha" "bfin" "i386" "pa" "sh" "xtensa" "riscv"))))
               (add-before 'configure 'setenv
                 (lambda _
                   (setenv "CC" "tcc")
                   (setenv "CFLAGS" "-D HAVE_ALLOCA_H"))))))
    (native-search-paths
      (list (search-path-specification
              (variable "C_INCLUDE_PATH")
              (files '("include")))
            (search-path-specification
              (variable "LIBRARY_PATH")
              (files '("lib")))))))

(define gcc-mesboot1
  (package
    (inherit gcc-core-mesboot1)
    (name "gcc-mesboot1")
    (version "4.6.4")
    (native-inputs
     `(("gcc-g++"
        ,(origin
           (method url-fetch)
           (uri (string-append "mirror://gnu/gcc/gcc-"
                               version "/gcc-g++-" version ".tar.gz"))
           (sha256
            (base32
             "1fqqk5zkmdg4vmqzdmip9i42q6b82i3f6yc0n86n9021cr7ms2k9"))))
       ,@(package-native-inputs gcc-core-mesboot1)))
    (arguments
     (substitute-keyword-arguments (package-arguments gcc-core-mesboot1)
       ((#:configure-flags configure-flags)
        #~(let ((out (assoc-ref %outputs "out")))
            `("--enable-languages=c,c++"
              ,@(filter
                 (negate (lambda (x) (string-prefix? "--enable-languages=" x)))
                 #$configure-flags))))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'unpack 'unpack-g++
              (lambda _
                (let ((source-g++ (assoc-ref %build-inputs "gcc-g++")))
                  (invoke "tar" "xvf" source-g++))))
            (add-before 'configure 'set-cplus-include-path
              (lambda _
                ;; Set the C++ search path so that C headers can be found as
                ;; libstdc++ is being compiled.
                (setenv "CPLUS_INCLUDE_PATH" (getenv "C_INCLUDE_PATH"))))))))
    (native-search-paths
     (list (search-path-specification
            (variable "C_INCLUDE_PATH")
            (files '("include")))
           (search-path-specification
            (variable "CPLUS_INCLUDE_PATH")
            (files '("include/c++" "include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (files '("lib")))))))

(define musl-boot
  (package
    (inherit musl)
    (name "musl-boot")
    (native-inputs
     (modify-inputs (%boot-tcc-musl-inputs)
                    (replace "gcc" gcc-muslboot0)))
    (inputs '())
    (arguments
     (substitute-keyword-arguments
       (package-arguments musl-boot0)
       ((#:guile _) %bootstrap-guile)
       ((#:implicit-inputs? _ #f) #f)
       ((#:make-flags _)
        #~(list (string-append "SHELL=" #$(this-package-native-input "bash")
                               "/bin/bash")))
       ((#:configure-flags _ #~'())
        #~(list (string-append "CONFIG_SHELL="
                               #$(this-package-native-input "bash")
                               "/bin/sh")
                (string-append "--syslibdir=" #$output "/lib")
                "CC=gcc"
                "--enable-gcc-wrapper"))
       ((#:phases phases #~'%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'symlink-dynamic-linker
              ;; This is a hack to work around not being able to hardcode
              ;; the location of the dynamic linker for musl in gcc without
              ;; adding musl as an input for gcc.
              ;; Because honestly it's easier to add support to musl for
              ;; (glibc-dynamic-linker) support than to adjust other software
              ;; to use musl's dynamic-linker location.
              (lambda _
                (symlink "libc.so"
                         (string-append #$output #$(glibc-dynamic-linker)))))))))))

(define gcc-muslboot
  ;; GCC 4.6.4 is the latest modular distribution. We backported RISC-V support
  ;; here.
  (package
    (inherit gcc-muslboot0)
    (name "gcc-muslboot")
    (version "4.6.4")
    (native-inputs
     `(("gcc-g++"
        ,(origin
           (method url-fetch)
           (uri (string-append "mirror://gnu/gcc/gcc-"
                               version "/gcc-g++-" version ".tar.gz"))
           (sha256
            (base32
             "1fqqk5zkmdg4vmqzdmip9i42q6b82i3f6yc0n86n9021cr7ms2k9"))))
       ,@(modify-inputs (%boot-tcc-musl-inputs)
                        (replace "gcc" gcc-muslboot0)
                        (replace "libc" musl-boot))))
    (arguments
     (substitute-keyword-arguments (package-arguments gcc-muslboot0)
       ((#:configure-flags configure-flags)
        #~(let ((out (assoc-ref %outputs "out")))
            `("--enable-languages=c,c++"
              ,(string-append "--with-gmp=" (assoc-ref %build-inputs "gmp-boot"))
              ,(string-append "--with-mpfr=" (assoc-ref %build-inputs "mpfr-boot"))
              ,(string-append "--with-mpc=" (assoc-ref %build-inputs "mpc-boot"))
              ,@(filter
                 (negate (lambda (x) (string-prefix? "--enable-languages=" x)))
                 #$configure-flags))))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'unpack 'unpack-g++
              (lambda _
                (let ((source-g++ (assoc-ref %build-inputs "gcc-g++")))
                  (invoke "tar" "xvf" source-g++))))
            (add-after 'apply-riscv64-patch 'apply-second-riscv64-patch
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((patch-file
                        #$(local-file
                            (search-patch
                              "gcc-boot-4.6.4-riscv64-libstdc++-support.patch"))))
                  (invoke "patch" "--force" "-p1" "-i" patch-file))))
            (replace 'setenv
              (lambda _
                (setenv "CC" "musl-gcc")
                (setenv "CPLUS_INCLUDE_PATH" (getenv "C_INCLUDE_PATH"))))))))
   (native-search-paths
    (list (search-path-specification
           (variable "C_INCLUDE_PATH")
           (files '("include")))
          (search-path-specification
           (variable "CPLUS_INCLUDE_PATH")
           (files '("include/c++" "include")))
          (search-path-specification
           (variable "LIBRARY_PATH")
           (files '("lib")))))))

(define (%boot-mesboot2-inputs)
  `(("gcc" ,gcc-mesboot1)
    ,@(alist-delete "gcc" (%boot-mesboot1-inputs))))

(define (%boot-muslboot2-inputs)
  `(("gcc" ,gcc-muslboot)
    ,@(modify-inputs (%boot-tcc-musl-inputs)
                     (delete "gcc")
                     (replace "libc" musl-boot))))

(define hello-mesboot
  ;; Check for Scheme-only bootstrap.  Note that newer versions of Hello
  ;; break due to the way that newer versions of Gnulib handle
  ;; "limits.h".  Hence, we stick to 2.10.
  (package
    (inherit hello)
    (name "hello-mesboot")
    (version "2.10")
    (source
     (origin
       (inherit (package-source hello))
       (uri (string-append "mirror://gnu/hello/hello-" version
                           ".tar.gz"))
       (sha256
        (base32
         "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i"))))
    (supported-systems '("i686-linux" "x86_64-linux" "riscv64-linux"))
    (inputs '())
    (propagated-inputs '())
    (native-inputs (if (target-x86?)
                       (%boot-mesboot2-inputs)
                       (%boot-muslboot2-inputs)))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:parallel-build? #f
       ;; checking for grep that handles long lines and -e...
       ;; configure: error: no acceptable grep could be found
       #:configure-flags '("ac_cv_path_GREP=grep")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "./hello"))))))))

(define binutils-mesboot
  (package
    (inherit binutils-mesboot1)
    (name "binutils-mesboot")
    (version "2.30")
    (source (bootstrap-origin
             (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/binutils/binutils-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "1sp9g7zrrcsl25hxiqzmmcrdlbm7rbmj0vki18lks28wblcm0f4c")))))
    (arguments
     (list #:implicit-inputs? #f
           #:guile %bootstrap-guile
           #:tests? #f                  ; runtest: command not found
           #:parallel-build? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'configure 'fix-build
                 (lambda _
                   ;; bfd/po doesn't have a Makefile, so the recursive calls just
                   ;; fail. We add files with the same name Make targets have, to
                   ;; trick Make into thinking there's nothing to do.
                   (call-with-output-file "bfd/po/install"
                     (lambda (p) (display "" p)))
                   (call-with-output-file "bfd/po/all"
                     (lambda (p) (display "" p)))
                   (call-with-output-file "bfd/po/info"
                     (lambda (p) (display "" p))))))
           #:configure-flags
           #~(list
               (string-append "CONFIG_SHELL="
                              (search-input-file %build-inputs "/bin/bash"))
               (string-append "SHELL="
                              (search-input-file %build-inputs "/bin/bash"))
               "--enable-64-bit-bfd"
               "--disable-nls"
               "--disable-shared"
               "--disable-plugins"
               "--enable-deterministic-archives"
               (string-append "--build=" #$(commencement-build-target))
               (string-append "--host=" #$(commencement-build-target)))))
    (native-inputs (if (target-x86?)
                       (%boot-mesboot2-inputs)
                       (%boot-muslboot2-inputs)))
    (supported-systems %supported-systems)))

;; We need to introduce byacc in order to process some pre-generated
;; files in gawk and possibly elsewhere.
(define byacc-mesboot
  (package
    (inherit byacc)
    (name "byacc")
    (version "20240109")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "https://invisible-mirror.net/archives/byacc/byacc-"
                     version ".tgz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0il4w1vwbglayakywyghiqhcjpg1yvv5ww2p8ylz32bi05wpg2gj"))))
    (native-inputs (if (target-x86?)
                       (%boot-mesboot1-inputs)
                       (%boot-muslboot2-inputs)))
    (inputs '())
    (propagated-inputs '())
    (arguments
     `(#:implicit-inputs? #f
       #:parallel-build? #f
       #:guile ,%bootstrap-guile
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bin  (string-append out "/bin"))
                    (man1 (string-append out "/share/man/man1")))
               (install-file "yacc" bin)
               (symlink "yacc" (string-append bin "/byacc"))
               (install-file "yacc.1" man1)))))))))

;; Sadly we have to introduce Gawk here.  The "versions.awk" script of
;; glibc 2.16.0 is too complicated for Gash-Utils.  This is the version
;; of Gawk used previously during bootstrap.  It's possible that a newer
;; version would work, too, but this one was already ready to go.
(define gawk-mesboot
  (package
    (inherit gawk)
    (name "gawk-mesboot")
    (version "3.1.8")
    (source (bootstrap-origin
              (origin
                (method url-fetch)
                (uri (string-append "mirror://gnu/gawk/gawk-"
                                    version ".tar.gz"))
                (sha256
                 (base32
                  "03d5y7jabq7p2s7ys9alay9446mm7i5g2wvy8nlicardgb6b6ii1"))
                (snippet
                 #~(begin (delete-file "awkgram.c"))))))
    (native-inputs
     (modify-inputs (if (target-x86?)
                        (%boot-mesboot2-inputs)
                        (%boot-muslboot2-inputs))
                    (append byacc-mesboot)))
    (inputs '())
    (propagated-inputs '())
    (arguments
     `(#:implicit-inputs? #f
       #:parallel-build? #f
       #:guile ,%bootstrap-guile
       #:configure-flags '("ac_cv_func_connect=no")
       #:make-flags '("gawk")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "./gawk" "--version")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "gawk" bin)
               (symlink "gawk" (string-append bin "/awk"))))))))))

(define mawk-mesboot
  (package
    (inherit mawk)
    (name "mawk-mesboot")
    (version "1.3.4-20240905")
    (source (bootstrap-origin
              (origin
                (method url-fetch)
                (uri (string-append "https://invisible-mirror.net/archives/mawk"
                                    "/mawk-" version ".tgz"))
                ;; Our gzip and tar don't know how to unpack a '.tgz' file.
                (file-name (string-append name "-" version ".tar.gz"))
                (sha256
                 (base32
                  "1q4rfcv7ppfw4fra8hp8z0s4qnsv1x598ny4xwb026zsgn96g6d3"))
                (snippet
                 #~(begin (delete-file "parse.c"))))))
    (native-inputs
     (modify-inputs (if (target-x86?)
                        (%boot-mesboot2-inputs)
                        (%boot-muslboot2-inputs))
                    (append byacc-mesboot)))
    (inputs '())
    (propagated-inputs '())
    (arguments
     `(#:implicit-inputs? #f
       #:parallel-build? #f         ; Prevent race condition
       #:guile ,%bootstrap-guile
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "./mawk" "--version"))))))))

(define (%boot-mesboot3-inputs)
  `(("binutils" ,binutils-mesboot)
    ("gawk" ,gawk-mesboot)
    ,@(alist-delete "binutils" (%boot-mesboot2-inputs))))

;; Mirror the names from the x86 bootstrap path.
(define (%boot-muslboot3-inputs)
  `(("gawk" ,mawk-mesboot)
    ,@(%boot-muslboot2-inputs)))

(define glibc-headers-mesboot
  (package
    (inherit glibc-mesboot0)
    (name "glibc-headers-mesboot")
    (version "2.16.0")
    (source (bootstrap-origin
             (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/glibc/glibc-"
                                   version
                                   ".tar.gz"))
               (patches (search-patches "glibc-boot-2.16.0.patch"
                                        "glibc-bootstrap-system-2.16.0.patch"))
               (modules '((guix build utils)))
               (snippet
                ;; No one should need obstack_compat, it's an unused pre-GNU C macro.
                ;; Removing this lets us use binutils-2.30.
                '(begin (substitute* "malloc/obstack.c"
                          ((".*obstack_compat" all)
                           (string-append "// " all)))))
               (sha256
                (base32
                 "0vlz4x6cgz7h54qq4528q526qlhnsjzbsvgc4iizn76cb0bfanx7")))))
    (native-inputs `(("headers" ,mesboot-headers)
                     ,@(%boot-mesboot3-inputs)))
    (arguments
     (substitute-keyword-arguments (package-arguments glibc-mesboot0)
       ((#:configure-flags configure-flags)
        #~(let ((out (assoc-ref %outputs "out"))
                (headers (assoc-ref %build-inputs "headers")))
            (list
             (string-append "--prefix=" out)
             "--disable-obsolete-rpc"
             (string-append "--host=" #$(commencement-build-target))
             (string-append "--with-headers=" headers "/include")
             "--enable-static-nss"
             "--with-pthread"
             "--without-cvs"
             "--without-gd"
             "--enable-add-ons=nptl"
             ;; avoid: configure: error: confusing output from nm -u
             "libc_cv_predef_stack_protector=no")))
       ((#:make-flags make-flags)
        #~(list "install-bootstrap-headers=yes" "install-headers"))
       ((#:phases phases)
        #~(modify-phases #$phases
            (delete 'apply-boot-patch)
            (delete 'fixup-configure)
            (delete 'set-path)
            (replace 'unpack
              (lambda* (#:key source #:allow-other-keys)
                (invoke "tar" "xvf" source)
                (chdir (string-append "glibc-" #$version))))
            (replace 'setenv
              (lambda* (#:key inputs #:allow-other-keys)
                (let* ((headers  (assoc-ref inputs "headers"))
                       (libc     (assoc-ref inputs "libc"))
                       (gcc      (assoc-ref inputs "gcc"))
                       (cppflags (string-append
                                  " -I " (getcwd) "/nptl/sysdeps/pthread/bits"
                                  " -D BOOTSTRAP_GLIBC=1"))
                       (cflags (string-append " -L " (getcwd)
                                              " -L " libc "/lib")))
                  (setenv "libc_cv_friendly_stddef" "yes")
                  (setenv "CONFIG_SHELL" (which "sh"))
                  (setenv "SHELL" (which "sh"))

                  (setenv "CPP" (string-append gcc "/bin/gcc -E " cppflags))
                  (setenv "CC" (string-append gcc "/bin/gcc " cppflags cflags))
                  (setenv "LD" "gcc")

                  ;; avoid -fstack-protector
                  (setenv "libc_cv_ssp" "false")
                  (substitute* "configure"
                    (("/bin/pwd") "pwd")))))
            (replace 'install
              (lambda* (#:key outputs make-flags #:allow-other-keys)
                (let ((kernel-headers (assoc-ref %build-inputs "kernel-headers"))
                      (out (assoc-ref outputs "out")))
                  (apply invoke "make" make-flags)
                  (copy-recursively kernel-headers out))))
            (add-before 'configure 'remove-bashism
              (lambda _
                (substitute* "sysdeps/unix/make-syscalls.sh"
                  (("\\$[{]vdso_symver//\\./_[}]")
                   "$(echo $vdso_symver | sed -e 's/\\./_/g')"))))
            (replace 'configure
              (lambda* (#:key configure-flags #:allow-other-keys)
                (format (current-error-port) "running ../configure ~a\n"
                        (string-join configure-flags))
                (mkdir-p "build")
                (chdir "build")
                (apply invoke "../configure" configure-flags)))
            (add-after 'configure 'remove-sunrpc
              (lambda _
                (let* ((out (assoc-ref %outputs "out"))
                       (bash (assoc-ref %build-inputs "bash"))
                       (shell (string-append bash "/bin/bash")))

                  (let ((Makefile (open-file "Makefile" "a")))
                    (display (string-append "

SHELL := " shell "
")
                             Makefile)
                    (close Makefile))
                  (substitute* "../Makefile"
                    (("^SHELL := /bin/sh") (string-append "SHELL := " shell)))
                  (substitute* "../Makeconfig"
                    (("^SHELL := /bin/sh") (string-append "SHELL := " shell)))
                  (substitute* "../elf/Makefile"
                    (("^SHELL := /bin/sh") (string-append "SHELL := " shell)))
                  (invoke "make" (string-append (getcwd) "/sysd-sorted" ))
                  (substitute* "sysd-sorted"
                    ((" sunrpc") " ")
                    ((" nis") " ")))))))))))

(define glibc-mesboot
  (package
    (inherit glibc-headers-mesboot)
    (name "glibc-mesboot")
    (native-inputs `(("headers" ,glibc-headers-mesboot)
                     ,@(%boot-mesboot3-inputs)))
    (arguments
     `(#:validate-runpath? #f ; fails when using --enable-shared
       ,@(substitute-keyword-arguments (package-arguments glibc-headers-mesboot)
           ((#:make-flags make-flags)
            #~(let ((bash (assoc-ref %build-inputs "bash")))
                (list (string-append "SHELL=" bash "/bin/sh"))))
           ((#:phases phases)
            #~(modify-phases #$phases
                (add-after 'unpack 'simplify-intl-tests
                  (lambda _
                    ;; The bootstrap Guile (2.0.9) crashes trying to
                    ;; perform a regex on non-ASCII text.  This gets
                    ;; triggered by 'intl/po2test.sed' running over
                    ;; 'po/de.po'.  If we ever remove the bootstrap
                    ;; Guile or add pure-Scheme regex to Gash, this can
                    ;; be removed.
                    (substitute* '("catgets/Makefile"
                                   "intl/Makefile")
                      (("de\\.po") "en_GB.po"))))
                (replace 'install
                  (lambda* (#:key outputs make-flags #:allow-other-keys)
                    (let* ((kernel-headers (assoc-ref %build-inputs "kernel-headers"))
                           (out (assoc-ref outputs "out"))
                           (install-flags (cons "install" make-flags)))
                      (apply invoke "make" install-flags)
                      (copy-recursively kernel-headers out)))))))))))

(define (%boot-mesboot4-inputs)
  `(("libc" ,glibc-mesboot)
    ,@(alist-delete "libc" (%boot-mesboot3-inputs))))

(define gcc-mesboot1-wrapper
  ;; We need this so gcc-mesboot1 can be used to create shared binaries that
  ;; have the correct interpreter, otherwise configuring gcc-mesboot using
  ;; --enable-shared will fail.
  (package
    (inherit gcc-mesboot1)
    (name "gcc-mesboot1-wrapper")
    (source #f)
    (inputs '())
    (native-inputs `(("bash" ,gash-boot)
                     ("coreutils" ,gash-utils-boot)
                     ("libc" ,glibc-mesboot)
                     ("gcc" ,gcc-mesboot1)))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:phases
       (modify-phases %standard-phases
         (delete 'unpack)
         (delete 'configure)
         (delete 'install)
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bash (assoc-ref %build-inputs "bash"))
                    (libc (assoc-ref %build-inputs "libc"))
                    (gcc (assoc-ref %build-inputs "gcc"))
                    (bin (string-append out "/bin")))
               (mkdir-p bin)
               (for-each
                (lambda (program)
                  (let ((wrapper (string-append bin "/" program)))
                    (with-output-to-file wrapper
                      (lambda _
                        (display (string-append "#! " bash "/bin/bash
exec " gcc "/bin/" program
" -Wl,--dynamic-linker"
" -Wl," libc ,(glibc-dynamic-linker
                (gnu-triplet->nix-system
                  (commencement-build-target)))
" -Wl,--rpath"
" -Wl," libc "/lib"
" \"$@\"
"))
                        (chmod wrapper #o555)))))
                '("cpp"
                  "gcc"
                  "g++"
                  ,(string-append (commencement-build-target) "-cpp")
                  ,(string-append (commencement-build-target) "-gcc")
                  ,(string-append (commencement-build-target) "-g++"))))))
         (replace 'check
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (program (string-append bin "/gcc")))
               (invoke program "--help")))))))))

(define gcc-mesboot
  (package
    (inherit gcc-mesboot1)
    (name "gcc-mesboot")
    (version (package-version gcc-4.9))
    (source (bootstrap-origin (package-source gcc-4.9)))
    (native-inputs `(("gcc-wrapper" ,gcc-mesboot1-wrapper)
                     ("headers" ,glibc-headers-mesboot)
                     ,@(%boot-mesboot4-inputs)))
    (inputs `(("gmp-source" ,(bootstrap-origin (package-source gmp-6.0)))
              ("mpfr-source" ,(bootstrap-origin (package-source mpfr)))
              ("mpc-source" ,(bootstrap-origin (package-source mpc)))))
    (arguments
     `(#:validate-runpath? #f
       ,@(substitute-keyword-arguments (package-arguments gcc-mesboot1)
           ((#:configure-flags configure-flags)
            #~(let ((out (assoc-ref %outputs "out"))
                    (glibc (assoc-ref %build-inputs "libc")))
                (list (string-append "--prefix=" out)
                      (string-append "--build=" #$(commencement-build-target))
                      (string-append "--host=" #$(commencement-build-target))

                      "--with-host-libstdcxx=-lsupc++"

                      (string-append "--with-native-system-header-dir=" glibc "/include")
                      (string-append "--with-build-sysroot=" glibc "/include")

                      "--disable-bootstrap"
                      "--disable-decimal-float"
                      "--disable-libatomic"
                      "--disable-libcilkrts"
                      "--disable-libgomp"
                      "--disable-libitm"
                      "--disable-libmudflap"
                      "--disable-libquadmath"
                      "--disable-libsanitizer"
                      "--disable-libssp"
                      "--disable-libvtv"
                      "--disable-lto"
                      "--disable-lto-plugin"
                      "--disable-multilib"
                      "--disable-plugin"
                      "--disable-threads"
                      "--enable-languages=c,c++"

                      "--enable-static"
                      "--enable-shared"
                      "--enable-threads=single"

                      ;; No pre-compiled libstdc++ headers, to save space.
                      "--disable-libstdcxx-pch"

                      ;; for libcpp ...
                      "--disable-build-with-cxx")))
           ((#:phases phases)
            #~(modify-phases #$phases
                (delete 'apply-boot-patch)
                (delete 'unpack-g++)     ; sadly, gcc-4.9.4 does not provide
                                                  ; modular core/language downloads
                (add-after 'unpack 'unpack-gmp&co
                  (lambda* (#:key inputs #:allow-other-keys)
                    (let ((gmp  (assoc-ref %build-inputs "gmp-source"))
                          (mpfr (assoc-ref %build-inputs "mpfr-source"))
                          (mpc  (assoc-ref %build-inputs "mpc-source")))

                      ;; To reduce the set of pre-built bootstrap inputs, build
                      ;; GMP & co. from GCC.
                      (for-each (lambda (source)
                                  (invoke "tar" "xvf" source))
                                (list gmp mpfr mpc))

                      ;; Create symlinks like `gmp' -> `gmp-x.y.z'.
                      #$@(map (lambda (lib)
                                ;; Drop trailing letters, as gmp-6.0.0a unpacks
                                ;; into gmp-6.0.0.
                                #~(symlink #$(string-trim-right
                                              (package-full-name lib "-")
                                              char-set:letter)
                                           #$(package-name lib)))
                              (list gmp-6.0 mpfr mpc)))))
                (replace 'set-cplus-include-path
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((bash (assoc-ref %build-inputs "bash"))
                           (gcc (assoc-ref %build-inputs "gcc")))
                      (setenv "CONFIG_SHELL" (string-append bash "/bin/sh"))
                      (setenv "C_INCLUDE_PATH" (string-append
                                                (getenv "C_INCLUDE_PATH") ":"
                                                gcc "/lib/gcc-lib/"
                                                #$(commencement-build-target)
                                                "/4.6.4/include"
                                                ":" (getcwd) "/mpfr/src"))
                      (setenv "CPLUS_INCLUDE_PATH" (string-append
                                                    (getenv "CPLUS_INCLUDE_PATH") ":"
                                                    gcc "/lib/gcc-lib/"
                                                    #$(commencement-build-target)
                                                    "/4.6.4/include"
                                                    ":" (getcwd) "/mpfr/src"))
                      (format (current-error-port) "C_INCLUDE_PATH=~a\n" (getenv "C_INCLUDE_PATH"))
                      (format (current-error-port) "CPLUS_INCLUDE_PATH=~a\n" (getenv "CPLUS_INCLUDE_PATH"))
                      (format (current-error-port) "LIBRARY_PATH=~a\n"
                              (getenv "LIBRARY_PATH"))))))))))))

(define gcc-mesboot-wrapper
  ;; We need this so gcc-mesboot can be used to create shared binaries that
  ;; have the correct interpreter and runpath to libc.
  (package
    (inherit gcc-mesboot1-wrapper)
    (name "gcc-mesboot-wrapper")
    (version (package-version gcc-mesboot))
    (source #f)
    (inputs '())
    (native-inputs `(("bash" ,gash-boot)
                     ("coreutils" ,gash-utils-boot)
                     ("libc" ,glibc-mesboot)
                     ("gcc" ,gcc-mesboot)))))

(define (%boot-mesboot5-inputs)
  `(("gcc-wrapper" ,gcc-mesboot-wrapper)
    ("gcc" ,gcc-mesboot)
    ,@(fold alist-delete (%boot-mesboot4-inputs) '("gcc" "gcc-wrapper"))))

(define (mesboot-package name pkg)
  (package
    (inherit pkg)
    (name name)
    (source (bootstrap-origin (package-source pkg)))
    (native-inputs (if (target-x86?)
                       (%boot-mesboot5-inputs)
                       (%boot-muslboot3-inputs)))
    (supported-systems '("i686-linux" "x86_64-linux" "riscv64-linux"))
    (inputs '())
    (propagated-inputs '())
    (arguments
     (ensure-keyword-arguments (package-arguments pkg)
                               `(#:implicit-inputs? #f
                                 #:guile ,%bootstrap-guile
                                 #:parallel-build? ,(not (target-riscv64?))
                                 #:tests? #f)))))

;; These packages are needed to complete the rest of the bootstrap.
;; In the future, Gash et al. could handle it directly, but it's not
;; ready yet.
(define bash-mesboot (mesboot-package "bash-mesboot" static-bash))
(define sed-mesboot (mesboot-package "sed-mesboot" sed))

;; "sed" from Gash-Utils lacks the 'w' command as of 0.2.0.
(define coreutils-mesboot
  (let ((pkg (mesboot-package "coreutils-mesboot" coreutils)))
    (package
      (inherit pkg)
      (native-inputs
       `(("sed" ,sed-mesboot)
         ,@(package-native-inputs pkg))))))

(define grep-mesboot
  (let ((pkg (mesboot-package "grep-mesboot" grep)))
    (package
      (inherit pkg)
      (arguments
       (substitute-keyword-arguments
         (strip-keyword-arguments
           '(#:configure-flags)
           (package-arguments pkg))))
      (native-inputs
       `(("sed" ,sed-mesboot)
         ,@(package-native-inputs pkg))))))

;; The XZ implementation in Bootar cannot decompress 'tar'.
(define xz-mesboot
  (let ((pkg (mesboot-package "xz-mesboot" xz)))
    (package
      (inherit pkg)
      (arguments
       (ensure-keyword-arguments (package-arguments pkg)
                                 ;; XXX: This fails even though the
                                 ;; actual runpaths seem fine.
                                 `(#:validate-runpath? #f))))))

;; We don't strictly need Tar here, but it allows us to get rid of
;; Bootar and Gash-Utils and continue with the standard GNU tools.
(define tar-mesboot
  (let ((pkg (mesboot-package "tar-mesboot" tar)))
    (package
      (inherit pkg)
      (native-inputs
       `(("xz" ,xz-mesboot)
         ,@(package-native-inputs pkg))))))

(define (%boot-mesboot6-inputs)
  `(("bash" ,bash-mesboot)
    ("coreutils" ,coreutils-mesboot)
    ("grep" ,grep-mesboot)
    ("sed" ,sed-mesboot)
    ("tar" ,tar-mesboot)
    ("xz" ,xz-mesboot)
    ,@(fold alist-delete (if (target-x86?)
                             (%boot-mesboot5-inputs)
                             (%boot-muslboot3-inputs))
            '("bash" "coreutils" "bootar" "kernel-headers"))))

(define (%bootstrap-inputs+toolchain)
  ;; The traditional bootstrap-inputs.  For the i686-linux, x86_64-linux
  ;; Scheme-only bootstrap the actual reduced set with bootstrapped toolchain.
  (match (%current-system)
    ((or "i686-linux" "x86_64-linux" "riscv64-linux")
     (%boot-mesboot6-inputs))
    (_
     (%bootstrap-inputs))))

(define gnu-make-boot0
  (package
    (inherit gnu-make)
    (outputs (delete "debug" (package-outputs gnu-make)))
    (source (bootstrap-origin (package-source gnu-make)))
    (name "make-boot0")
    (arguments
     `(#:guile ,%bootstrap-guile
       #:implicit-inputs? #f
       #:tests? #f                                ; cannot run "make check"
       ,@(substitute-keyword-arguments (package-arguments gnu-make)
           ((#:configure-flags flags ''())
            ;; The generated config.status has some problems due to the
            ;; bootstrap environment.  Disable dependency tracking to work
            ;; around it.
            `(cons "--disable-dependency-tracking"

                   ;; 'glibc-bootstrap' on non-x86 platforms has a buggy
                   ;; 'posix_spawn'.  Thus, disable it.  See
                   ;; <https://bugs.gnu.org/49367>.
                   ,(match (%current-system)
                      ((or "i686-linux" "x86_64-linux" "riscv64-linux")
                       flags)
                      (_
                       `(cons "--disable-posix-spawn" ,flags)))))
           ((#:phases phases)
            `(modify-phases ,phases
               (replace 'build
                 (lambda _
                   (invoke "./build.sh")))
               (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (bin (string-append out "/bin")))
                     (install-file "make" bin)))))))))
    (native-inputs '())                           ; no need for 'pkg-config'
    (inputs (%bootstrap-inputs+toolchain))))

(define bzip2-boot0
  (package
    (inherit bzip2)
    (name "bzip2-boot0")
    (native-inputs `())
    (inputs
     `(("diffutils" ,diffutils-boot0)
       ("make" ,gnu-make-boot0)
       ,@(%bootstrap-inputs+toolchain)))
    (arguments
     `(#:guile ,%bootstrap-guile
       #:implicit-inputs? #f
       ,@(package-arguments bzip2)))))

(define coreutils-boot0
  (package
    (inherit coreutils)
    (outputs (delete "debug" (package-outputs coreutils)))
    (source (bootstrap-origin (package-source coreutils)))
    (name "coreutils-boot0")
    (native-inputs `())
    (inputs
     `(("make" ,gnu-make-boot0)
       ,@(%bootstrap-inputs+toolchain)))
    (arguments
     `(#:tests? #f
       #:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       ,@(package-arguments coreutils)
       ;; The %bootstrap-glibc for aarch64 and armhf doesn't have
       ;; $output/include/linux/prctl.h which causes some binaries
       ;; to fail to build with coreutils-9.0+.
       ,@(if (target-arm?)
           `(#:configure-flags '(,(string-append
                                    "--enable-no-install-program="
                                    ;; the defaults to not install.
                                    "arch,coreutils,hostname"
                                    ;; fails due to missing headers.
                                    ",timeout,sort")
                                  ,@(if (target-arm32?)
                                      `("--disable-year2038")
                                      `())))
           '())))))

(define diffutils-boot0
  (package
    (inherit diffutils)
    (name "diffutils-boot0")
    (source (origin
              (inherit (package-source diffutils))
              (patches '())))                 ;patches for tests unneeded here
    (native-inputs `())
    (inputs
     `(("make" ,gnu-make-boot0)
       ,@(%bootstrap-inputs+toolchain)))
    (arguments
     `(#:tests? #f                            ; the test suite needs diffutils
       #:guile ,%bootstrap-guile
       #:implicit-inputs? #f
       ,@(substitute-keyword-arguments (package-arguments diffutils)
           ((#:configure-flags flags ''())
            (match (%current-system)
              ((or "arm-linux" "aarch64-linux")
               ;; The generated config.status has some problems due to the
               ;; bootstrap environment.  Disable dependency tracking to work
               ;; around it.
               `(cons "--disable-dependency-tracking" ,flags))
              (_ flags))))))))

(define findutils-boot0
  (package
    (inherit findutils)
    (name "findutils-boot0")
    (source (bootstrap-origin (package-source findutils)))
    (inputs
     `(("make" ,gnu-make-boot0)
       ("diffutils" ,diffutils-boot0) ; for tests
       ,@(%bootstrap-inputs+toolchain)))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       ;; The build system assumes we have done a mistake when time_t is 32-bit
       ;; on a 64-bit system.  Ignore that for our bootstrap toolchain.
       ,@(substitute-keyword-arguments (package-arguments findutils)
           ((#:configure-flags flags ''())
            `(append
               ;; TODO: Figure out exactly with architectures need this.
              ,(if (target-64bit?)
                   ''("TIME_T_32_BIT_OK=yes")
                   ''())
              ,(match (%current-system)
                 ((or "arm-linux" "aarch64-linux")
                  ''("--disable-dependency-tracking"))
                 (_ ''()))
              ,flags))
           ((#:phases phases '%standard-phases)
            `(modify-phases ,phases
               (add-before 'check 'skip-problematic-tests
                 (lambda _
                   ,(match (%current-system)
                     ;; 'test-fnmatch' fails when using glibc-mesboot@2.16, due
                     ;; to incorrect handling of the [:alpha:] regexp character
                     ;; class.  Ignore it.
                     ((or "x86_64-linux" "i686-linux")
                      '(substitute* "gnulib-tests/Makefile"
                         (("^XFAIL_TESTS =")
                          "XFAIL_TESTS = test-fnmatch ")))
                     ("armhf-linux"
                      '(substitute* "gnulib-tests/Makefile"
                         (("^XFAIL_TESTS =")
                          "XFAIL_TESTS = test-fnmatch ")
                         (("test-pthread-thread\\$\\(EXEEXT\\)") "")))
                     ("riscv64-linux"
                      '(substitute* "gnulib-tests/Makefile"
                         (("^XFAIL_TESTS =")
                          (string-append "XFAIL_TESTS = "
                                         "test-hard-locale "
                                         "test-sigprocmask "))
                         ;; This test fails non-deterministically.
                         (("test-setlocale_null-mt-all\\$\\(EXEEXT\\)") "")
                         (("test-pthread_sigmask1\\$\\(EXEEXT\\)") "")))
                     (_
                      ;; XXX: The pthread tests are known to fail at least on
                      ;; ARM; skip them.
                      '(substitute* "gnulib-tests/Makefile"
                         (("test-pthread\\$\\(EXEEXT\\)") "")
                         (("test-pthread-thread\\$\\(EXEEXT\\)") "")
                         (("test-pthread_sigmask1\\$\\(EXEEXT\\)") "")
                         (("test-pthread_sigmask2\\$\\(EXEEXT\\)") "")))))))))))))

(define file
  (package
    (inherit (@ (gnu packages file) file))
    (arguments
     `(#:configure-flags
       `("--disable-bzlib"
         ,,@(match (%current-system)
              ((or "arm-linux" "aarch64-linux")
               '("--disable-dependency-tracking"))
              ("riscv64-linux"
               '("--disable-xzlib"))
              (_ '())))))))

(define file-boot0
  (package
    (inherit file)
    (source (bootstrap-origin (package-source file)))
    (name "file-boot0")
    (inputs
     `(("make" ,gnu-make-boot0)
       ,@(%bootstrap-inputs+toolchain)))
    (arguments
     `(#:tests? #f                      ; merge test fails
       #:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:configure-flags '("--disable-bzlib")
       ;; riscv64's gcc-4.6.4 doesn't have full C11 support.
       #:make-flags ,(if (target-riscv64?)
                          ''("CFLAGS+=-std=c1x")
                          ''("CFLAGS+=-std=c11"))
       #:strip-binaries? #f
       #:validate-runpath? #f
       ,@(package-arguments file)))))

(define byacc-boot0
  (package
    (inherit byacc)
    (name "byacc-boot0")
    (source (bootstrap-origin (package-source byacc)))
    (native-inputs '())
    (inputs
     `(("make" ,gnu-make-boot0)
       ,@(%bootstrap-inputs+toolchain)))
    (propagated-inputs '())
    (arguments
     `(#:tests? #f
       #:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bin  (string-append out "/bin"))
                    (man1 (string-append out "/share/man/man1")))
               (install-file "yacc" bin)
               (symlink "yacc" (string-append bin "/byacc"))
               (install-file "yacc.1" man1)))))))))

(define gawk-boot0
  (package
    (inherit gawk)
    (source (bootstrap-origin
             (origin (inherit (package-source gawk))
                     (snippet
                      #~(begin (delete-file "awkgram.c"))))))
    (name "gawk-boot0")
    (native-inputs (list byacc-boot0))
    (inputs
     `(("make" ,gnu-make-boot0)
       ,@(%bootstrap-inputs+toolchain)))
    (arguments
     `(#:tests? #f
       #:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:strip-binaries? #f
       #:validate-runpath? #f))))

(define patch-boot0
  (package
    (inherit patch/pinned)
    (source (bootstrap-origin (package-source patch/pinned)))
    (name "patch-boot0")
    (native-inputs '())
    (inputs
     `(("make" ,gnu-make-boot0)
       ,@(%bootstrap-inputs+toolchain)))
    (arguments
     `(#:tests? #f                      ; merge test fails
       #:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:strip-binaries? #f
       #:validate-runpath? #f))))

(define sed-boot0
  (package
    (inherit sed)
    (name "sed-boot0")
    (source (bootstrap-origin (package-source sed)))
    (native-inputs '())
    (inputs
     `(("make" ,gnu-make-boot0)
       ,@(%bootstrap-inputs+toolchain)))
    (arguments
     `(#:implicit-inputs? #f
       #:tests? #f
       #:guile ,%bootstrap-guile
       ,@(package-arguments sed)))))

(define tar-boot0
  (package
    (inherit tar)
    (name "tar-boot0")
    (source (bootstrap-origin (package-source tar)))
    (native-inputs '())
    (inputs
     `(("make" ,gnu-make-boot0)
       ,@(%bootstrap-inputs+toolchain)))
    (arguments
     `(#:implicit-inputs? #f
       #:tests? #f
       #:guile ,%bootstrap-guile
       ,@(package-arguments tar)))))

(define (%boot0-inputs)
  `(,@(match (%current-system)
        ((or "i686-linux" "x86_64-linux" "riscv64-linux")
         `(("bzip2" ,bzip2-boot0)
           ("coreutils" ,coreutils-boot0)
           ("gawk" ,gawk-boot0)
           ("patch" ,patch-boot0)
           ("sed" ,sed-boot0)
           ("tar" ,tar-boot0)
           ("make" ,gnu-make-boot0)
           ("diffutils" ,diffutils-boot0)
           ("findutils" ,findutils-boot0)
           ("file" ,file-boot0)
           ,@(fold alist-delete (%bootstrap-inputs+toolchain)
                   '("coreutils" "gawk" "patch" "sed" "tar" "make"))))
        (_
          `(("make" ,gnu-make-boot0)
            ("diffutils" ,diffutils-boot0)
            ("findutils" ,findutils-boot0)
            ("file" ,file-boot0)
            ,@(%bootstrap-inputs+toolchain))))))

(define* (boot-triplet #:optional (system (%current-system)))
  ;; Return the triplet used to create the cross toolchain needed in the
  ;; first bootstrapping stage.
  (nix-system->gnu-triplet system "guix"))

;; Following Linux From Scratch, build a cross-toolchain in stage 0.  That
;; toolchain actually targets the same OS and arch, but it has the advantage
;; of being independent of the libc and tools in
;; (%BOOTSTRAP-INPUTS+TOOLCHAIN), since GCC-BOOT0 (below) is built without any
;; reference to the target libc.

(define binutils-boot0
  (package
    (inherit binutils)
    (source (bootstrap-origin (package-source binutils)))
    (name "binutils-cross-boot0")
    (arguments
     (append (list #:guile %bootstrap-guile
                   #:implicit-inputs? #f

                   #:modules '((guix build gnu-build-system)
                               (guix build utils)
                               (ice-9 ftw)) ; for 'scandir'
                   #:phases
                   #~(modify-phases %standard-phases
                       (add-after 'install 'add-symlinks
                         (lambda* (#:key outputs #:allow-other-keys)
                           ;; The cross-gcc invokes 'as', 'ld', etc, without the
                           ;; triplet prefix, so add symlinks.
                           (let ((out (assoc-ref outputs "out"))
                                 (triplet-prefix (string-append #$(boot-triplet)
                                                                "-")))
                             (define (has-triplet-prefix? name)
                               (string-prefix? triplet-prefix name))
                             (define (remove-triplet-prefix name)
                               (substring name
                                          (string-length triplet-prefix)))

                             (with-directory-excursion (string-append out "/bin")
                               (for-each (lambda (name)
                                           (symlink name
                                                    (remove-triplet-prefix name)))
                                         (scandir "."
                                                  has-triplet-prefix?))))))))
             (substitute-keyword-arguments (package-arguments binutils)
               ((#:configure-flags cf)
                #~(append (list #$(string-append "--target="
                                                 (boot-triplet))
                                "--disable-gprofng") ;requires Bison
                          #$cf)))))
    (native-inputs '())                           ;no Bison
    (inputs (%boot0-inputs))))

(define libstdc++-boot0
  ;; GCC's libcc1 is always built as a shared library (the top-level
  ;; 'Makefile.def' forcefully adds --enable-shared) and thus needs to refer
  ;; to libstdc++.so.  We cannot build libstdc++-5.3 because it relies on
  ;; C++14 features missing in some of our bootstrap compilers.
  (let ((lib (make-libstdc++ gcc-4.9)))
    (package
      (inherit lib)
      (source (bootstrap-origin (package-source lib)))
      (name "libstdc++-boot0")
      (arguments
       `(#:guile ,%bootstrap-guile
         #:implicit-inputs? #f

         ;; XXX: libstdc++.so NEEDs ld.so for some reason.
         #:validate-runpath? #f

         ,@(match (%current-system)
             ((or "i686-linux" "x86_64-linux")
              (substitute-keyword-arguments (package-arguments lib)
                ((#:phases phases)
                 #~(modify-phases #$phases
                     (add-after 'unpack 'workaround-wrapper-bug
                       ;; XXX: The crude gcc-cross-wrapper causes "g++ -v" to
                       ;; fail, which in turn confuses the configure script.
                       (lambda _
                         (substitute* "libstdc++-v3/configure"
                           (("g\\+\\+ -v") "true"))))))))
             (_ (package-arguments lib)))

         ;; Explicitly add #:modules so MAKE-LIBSTDC++ can be changed
         ;; without a full bootstrap.
         #:modules ((guix build gnu-build-system)
                    (guix build utils))))

      (inputs (%boot0-inputs))
      (native-inputs '()))))

(define (make-libstdc++-boot0 gcc)
  ;; GCC >= 7 is needed by architectures which use C++-14 features.
  (let ((lib (make-libstdc++ gcc)))
    (package
      (inherit lib)
      (source (bootstrap-origin (package-source lib)))
      (name "libstdc++-boot0")
      (arguments
       `(#:guile ,%bootstrap-guile
         #:implicit-inputs? #f

         ;; XXX: libstdc++.so NEEDs ld.so for some reason.
         #:validate-runpath? #f

         ,@(substitute-keyword-arguments (package-arguments lib)
             ((#:configure-flags flags)
              (if (target-hurd64?)
                  #~(cons* "--disable-shared"
                           "--disable-libstdcxx-dual-abi"
                           "--disable-libstdcxx-threads"
                           "--disable-libstdcxx-pch"
                           #$flags)
                  flags))
             ((#:phases phases)
              #~(modify-phases #$phases
                  (add-after 'unpack 'unpack-gmp&co
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((gmp  (assoc-ref %build-inputs "gmp-source"))
                            (mpfr (assoc-ref %build-inputs "mpfr-source"))
                            (mpc  (assoc-ref %build-inputs "mpc-source")))

                        ;; To reduce the set of pre-built bootstrap inputs, build
                        ;; GMP & co. from GCC.
                        (for-each (lambda (source)
                                    (invoke "tar" "xvf" source))
                                  (list gmp mpfr mpc))

                        ;; Create symlinks like `gmp' -> `gmp-x.y.z'.
                        #$@(map (lambda (lib)
                                  ;; Drop trailing letters, as gmp-6.0.0a unpacks
                                  ;; into gmp-6.0.0.
                                  #~(symlink #$(string-trim-right
                                                (package-full-name lib "-")
                                                char-set:letter)
                                             #$(package-name lib)))
                                (list gmp-6.0 mpfr mpc))))))))))
      (inputs `(("gmp-source" ,(bootstrap-origin (package-source gmp-6.0)))
                ("mpfr-source" ,(bootstrap-origin (package-source mpfr)))
                ("mpc-source" ,(bootstrap-origin (package-source mpc)))
                ,@(%boot0-inputs)))
      (native-inputs '()))))

(define gcc-boot0
  (package
    (inherit gcc)
    (name "gcc-cross-boot0")
    (outputs (delete "debug" (package-outputs gcc)))
    (source
     (bootstrap-origin
      (origin
        (inherit (package-source gcc))
        (snippet
         #~(begin
             ;; XXX: The GCC test suite contains files with non-ASCII file
             ;; names, which cannot be repacked by BOOTSTRAP-ORIGIN.  Nor
             ;; can it be deleted from Guile, so resort to this evil hack.
             #$(origin-snippet (package-source gcc))
             (system* #$(file-append (let-system system
                                       ;; 'coreutils-boot0' is Linux-only.
                                       (if (target-hurd? system)
                                           %bootstrap-coreutils&co
                                           coreutils-boot0))
                                     "/bin/rm") "-rf"
                      "gcc/testsuite/go.test/test/fixedbugs/issue27836.dir"))))))
    (arguments
     (cons*
      #:guile %bootstrap-guile
      #:implicit-inputs? #f
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 regex)
                  (srfi srfi-1)
                  (srfi srfi-26))
      (substitute-keyword-arguments (package-arguments gcc)
        ((#:configure-flags flags)
         #~(append (list #$(string-append "--target=" (boot-triplet))

                         ;; No libc yet.
                         "--without-headers"

                         ;; Disable features not needed at this stage.
                         "--disable-shared"
                         "--enable-languages=c,c++"

                         ;; libstdc++ cannot be built at this stage
                         ;; ("Link tests are not allowed after
                         ;; GCC_NO_EXECUTABLES.").
                         "--disable-libstdc++-v3"

                         "--disable-threads"
                         "--disable-libmudflap"
                         "--disable-libatomic"
                         "--disable-libsanitizer"
                         "--disable-libitm"
                         "--disable-libgomp"
                         "--disable-libmpx"
                         "--disable-libcilkrts"
                         "--disable-libvtv"
                         "--disable-libssp"
                         "--disable-libquadmath"
                         "--disable-decimal-float")
                   (remove (cut string-match
                                "--(with-system-zlib|enable-languages.*)" <>)
                           #$flags)))
        ((#:make-flags flags)
         `(let* ((libc        (assoc-ref %build-inputs "libc"))
                 (libc-native (or (assoc-ref %build-inputs "libc-native")
                                  libc)))
            `(,,@(append
                  `((string-append "LDFLAGS="
                                   "-Wl,-rpath=" libc-native "/lib "
                                   "-Wl,-dynamic-linker "
                                   "-Wl," libc-native
                                   ,(glibc-dynamic-linker
                                     (match (%current-system)
                                       ("x86_64-linux" "i686-linux")
                                       (_ (%current-system))))))
                  (if (target-hurd64?)
                      ;;Convince gmp's configure that gcc works
                      (list (string-append
                             "CC_FOR_BUILD=gcc"
                             " -Wno-implicit-function-declaration"))
                      '())))))
        ((#:phases phases)
         #~(modify-phases #$phases
             (add-after 'unpack 'unpack-gmp&co
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((gmp  (assoc-ref %build-inputs "gmp-source"))
                       (mpfr (assoc-ref %build-inputs "mpfr-source"))
                       (mpc  (assoc-ref %build-inputs "mpc-source")))

                   ;; To reduce the set of pre-built bootstrap inputs, build
                   ;; GMP & co. from GCC.
                   (for-each (lambda (source)
                               (invoke "tar" "xvf" source))
                             (list gmp mpfr mpc))

                   ;; Create symlinks like `gmp' -> `gmp-x.y.z'.
                   #$@(map (lambda (lib)
                             ;; Drop trailing letters, as gmp-6.0.0a unpacks
                             ;; into gmp-6.0.0.
                             #~(symlink #$(string-trim-right
                                           (package-full-name lib "-")
                                           char-set:letter)
                                        #$(package-name lib)))
                           (list gmp-6.0 mpfr mpc)))))
             #$@(if (target-hurd64?)
                    #~((add-after 'unpack 'patch-libcc1-static
                         (lambda _
                           ;;Attempting to build libcc1 shared gives:
                           ;;  install: cannot stat '.libs/libcc1.so.0.0.0':
                           ;;  No such file or directory
                           ;;convince gcc harder to not build a shared libcc1
                           (substitute* "Makefile.def"
                             (("module= libcc1; [^;]*;") "module= libcc1;"))
                           (substitute* "Makefile.in"
                             (("(--target=[$][{]target_alias[}]) --enable-shared \\\\"
                               all target)
                              (string-append target " \\"))))))
                    #~())
             #$(match (%current-system)
                 ((or "i686-linux" "x86_64-linux")
                  #~(add-before 'configure 'fix-libcc1
                      (lambda* (#:key inputs #:allow-other-keys)
                        ;; libcc1.so NEEDs libgcc_s.so, so provide one here
                        ;; to placate the 'validate-runpath' phase.
                        (substitute* "libcc1/Makefile.in"
                          (("la_LDFLAGS =")
                           (string-append "la_LDFLAGS = -Wl,-rpath="
                                          (assoc-ref inputs "gcc") "/lib")))
                        ;; XXX: "g++ -v" is broken (see also libstdc++ above).
                        (substitute* "libcc1/configure"
                          (("g\\+\\+ -v") "true")))))
                 (_ #~(add-before 'configure 'return-true
                        (lambda _ #t))))
             (add-after 'install 'symlink-libgcc_eh
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "lib")))
                   ;; Glibc wants to link against libgcc_eh, so provide
                   ;; it.
                   (with-directory-excursion
                       (string-append out "/lib/gcc/"
                                      #$(boot-triplet)
                                      "/" #$(package-version gcc))
                     (symlink "libgcc.a" "libgcc_eh.a"))))))))))

    (inputs `(("gmp-source" ,(bootstrap-origin (package-source gmp-6.0)))
              ("mpfr-source" ,(bootstrap-origin (package-source mpfr)))
              ("mpc-source" ,(bootstrap-origin (package-source mpc)))
              ("binutils-cross" ,binutils-boot0)

              ;; The libstdc++ that libcc1 links against.
              ("libstdc++" ,(match (%current-system)
                                   ("riscv64-linux" (make-libstdc++-boot0 gcc-7))
                                   ("x86_64-gnu" (make-libstdc++-boot0 gcc-14))
                                   (_ libstdc++-boot0)))

              ;; Call it differently so that the builder can check whether
              ;; the "libc" input is #f.
              ("libc-native" ,@(assoc-ref (%boot0-inputs) "libc"))
              ,@(alist-delete "libc" (%boot0-inputs))))

    ;; No need for the native-inputs to build the documentation at this stage.
    (native-inputs '())))

(define perl-boot0
  (package
    (inherit perl)
    (name "perl-boot0")
    (source (bootstrap-origin (package-source perl)))
    (inputs (%boot0-inputs))
    (arguments
     (append (list #:implicit-inputs? #f
                   #:guile %bootstrap-guile
                   #:validate-runpath? #f

                   ;; At the very least, this must not depend on GCC & co.
                   #:disallowed-references (list %bootstrap-binutils))
             (substitute-keyword-arguments (package-arguments perl)
               ((#:phases phases)
                #~(modify-phases #$phases
                    ;; Pthread support is missing in the bootstrap compiler
                    ;; (broken spec file), so disable it.
                    (add-before 'configure 'disable-pthreads
                      (lambda _
                        (substitute* "Configure"
                          (("^libswanted=(.*)pthread" _ before)
                           (string-append "libswanted=" before)))))))
               ;; Do not configure with '-Dusethreads' since pthread
               ;; support is missing.
               ((#:configure-flags configure-flags)
                #~(delete "-Dusethreads"

                          ;; On i586-gnu, linking fails with "undefined
                          ;; reference to `__stack_chk_guard'" so avoid
                          ;; '-fstack-protector'.
                          #$(if (target-hurd?)
                                #~(cons* "-A" "ccflags=-fno-stack-protector"
                                         #$configure-flags)
                                configure-flags))))))))

(define m4-boot0
  (package
    (inherit m4)
    (name "m4-boot0")
    (source (bootstrap-origin (package-source m4)))
    (inputs (%boot0-inputs))
    (arguments
     `(#:guile ,%bootstrap-guile
       #:implicit-inputs? #f
       ,@(package-arguments m4)
       ;; Ignore test failure in gnulib for armhf/aarch64 and Hurd
       #:tests? ,(and (not (target-arm?))
                      (not (target-hurd?)))))))

(define bison-boot0
  ;; This Bison is needed to build MiG so we need it early in the process.
  ;; Recent versions of Linux-Libre headers also depend on this.
  (package
    (inherit bison)
    (name "bison-boot0")
    (propagated-inputs `(("m4" ,m4-boot0)))
    (native-inputs `(("perl" ,perl-boot0)))
    (inputs (%boot0-inputs))                      ;remove Flex...
    (arguments
     `(#:tests? #f                                ;... and thus disable tests
       #:implicit-inputs? #f
       #:guile ,%bootstrap-guile

       ;; Zero timestamps in liby.a; this must be done
       ;; explicitly here because the bootstrap Binutils don't
       ;; do that (default is "cru".)
       #:make-flags `("ARFLAGS=crD"
                      ,,(match (%current-system)
                          ;; ranlib: '-D': No such file
                          ((or "i686-linux" "x86_64-linux")
                           "RANLIB=ranlib")
                          (_
                           "RANLIB=ranlib -D"))
                      "V=1")

       ;; 'glibc-bootstrap' on non-x86 platforms has a buggy 'posix_spawn'.
       ;; Thus, use the Gnulib replacement instead.  See
       ;; <https://bugs.gnu.org/49367>.
       ,@(match (%current-system)
           ((or "i686-linux" "x86_64-linux")
            '())
           (_
            '(#:configure-flags '("gl_cv_func_posix_spawn_works=no"))))

       ,@(package-arguments bison)))))

(define flex-boot0
  ;; This Flex is needed to build MiG as well as Linux-Libre headers.
  (package
    (inherit flex)
    (native-inputs `(("bison" ,bison-boot0)))
    (propagated-inputs `(("m4" ,m4-boot0)))
    (inputs (%boot0-inputs))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:tests? #f))))

(define-syntax define/system-dependent
  (lambda (s)
    "Bind IDENTIFIER to EXP, where the value of EXP is known to depend on
'%current-system'.  The definition ensures that (1) EXP is \"thunked\" so that
it sees the right value of '%current-system', and (2) that its result is
memoized as a function of '%current-system'."
    (syntax-case s ()
      ((_ identifier exp)
       (with-syntax ((memoized (datum->syntax #'identifier
                                              (symbol-append
                                               (syntax->datum #'identifier)
                                               '/memoized))))
         #'(begin
             (define memoized
               (mlambda (system) exp))
             (define-syntax identifier
               (identifier-syntax (memoized (%current-system))))))))))

(define/system-dependent linux-libre-headers-boot0
  ;; Note: this is wrapped in a thunk to nicely handle circular dependencies
  ;; between (gnu packages linux) and this module.  Additionally, memoize
  ;; the result to play well with further memoization and code that relies
  ;; on pointer identity; see <https://bugs.gnu.org/30155>.
  (package
    (inherit linux-libre-headers)
    (arguments
     `(#:guile ,%bootstrap-guile
       #:implicit-inputs? #f
       ,@(substitute-keyword-arguments (package-arguments linux-libre-headers)
           ((#:phases phases)
            `(modify-phases ,phases
               (add-after 'unpack 'lower-version-requirements
                 (lambda _
                   ;; Pacify version checks so it works with the bootstrap
                   ;; toolchain, since we are not building the full kernel.
                   (substitute* "scripts/min-tool-version.sh"
                     (("echo 5\\.1\\.0")  ;GCC
                      "echo 4.8.4")
                     (("echo 2\\.23\\.0") ;binutils
                      "echo 2.20.1")))))))))
    (native-inputs
     `(("perl" ,perl-boot0)

       ;; Flex and Bison are required since version 4.16.
       ("flex" ,flex-boot0)
       ("bison" ,bison-boot0)
       ,@(%boot0-inputs)))))

(define with-boot0
  (package-with-explicit-inputs %boot0-inputs
                                %bootstrap-guile))

(define autoconf-boot0
  (with-boot0
   (package
     (inherit autoconf)
     (name "autoconf-boot0")
     (native-inputs (list m4-boot0 perl-boot0))
     (inputs '())
     (arguments (list #:tests? #f)))))

(define automake-boot0
  (with-boot0
   (package
     (inherit automake)
     (name "automake-boot0")
     (source (origin
               (inherit (package-source automake))
               (patches '())))        ;test are skipped anyway
     (native-inputs (list autoconf-boot0 m4-boot0 perl-boot0))
     (inputs '())
     (arguments
      (list #:tests? #f)))))

(define gnumach-headers-boot0
  (with-boot0
   (package
     (inherit gnumach-headers)
     (name "gnumach-headers-boot0")
     (version "1.8+git20240714")
     (source
      (origin
        (inherit (package-source gnumach-headers))
        (patches '())
        (method
         (git-fetch-from-tarball
          (origin
            (method url-fetch)
            (uri (string-append
                  "https://git.savannah.gnu.org/cgit/hurd/gnumach.git/snapshot/"
                  "gnumach-" version ".tar.gz"))
            (sha256
             (base32
              "1bnw5vdbq91zjxklx23qvim40fb0yw1qdxhn9n37jdfypm6q3xir")))))))
     (native-inputs (list autoconf-boot0 automake-boot0 texinfo-boot0))
     (arguments
      (substitute-keyword-arguments (package-arguments gnumach-headers)
        ((#:phases phases)
         #~(modify-phases #$phases
             (add-after 'unpack 'patch-compat
               (lambda _
                 (substitute* '("include/device/device_types.h"
                                "include/mach_debug/slab_info.h"
                                "include/mach_debug/vm_info.h")
                   (("rpc_vm_size_t") "unsigned int")
                   (("rpc_vm_offset_t") "unsigned int")
                   (("rpc_long_natural_t") "unsigned long")
                   (("long_natural_t") "unsigned long")))))))))))

(define mig-boot0
  (with-boot0
   (package
     (inherit mig)
     (name "mig-boot0")
     (version "1.8+git20231217")
     (source
      (origin
        (inherit (package-source mig))
        (method
         (git-fetch-from-tarball
          (origin
            (method url-fetch)
            (uri (string-append
                  "https://git.savannah.gnu.org/cgit/hurd/mig.git/snapshot/"
                  "mig-" version ".tar.gz"))
            (sha256
             (base32
              "18vz3ifrhhlvrdmlv70h63wl0kh5w8jcpsjx9yscsw9yazm1lzs7")))))))
     (native-inputs (list autoconf-boot0 automake-boot0 bison-boot0 flex-boot0
                          gnumach-headers-boot0))
     (inputs (list flex-boot0 gnumach-headers-boot0))
     (arguments
      (substitute-keyword-arguments (package-arguments mig)
        ((#:configure-flags flags '())
         #~(list (string-append "LDFLAGS=-Wl,-rpath="
                                #$(this-package-native-input "flex")
                                "/lib/"))))))))

(define hurd-headers-boot0
  (with-boot0
   (package
     (inherit hurd-headers)
     (name "hurd-headers-boot0")
     (version "0.9.git20240714")
     (source
      (origin
        (inherit (package-source hurd-headers))
        (method
         (git-fetch-from-tarball
          (origin
            (method url-fetch)
            (uri (string-append
                  "https://git.savannah.gnu.org/cgit/hurd/hurd.git/snapshot/"
                  "hurd-v" version ".tar.gz"))
            (sha256
             (base32
              "0wcihffclwijjamx4cjbr8i92yg780538ipg2z208ahg96jjrmgq")))))))
     (native-inputs
      (list autoconf-boot0 automake-boot0 mig-boot0))
     (inputs '()))))

(define hurd-minimal-boot0
  (with-boot0
   (package
     (inherit hurd-minimal)
     (name "hurd-minimal-boot0")
     (source (package-source hurd-headers-boot0))
     (native-inputs
      (list autoconf-boot0 automake-boot0 gnumach-headers-boot0 mig-boot0))
     (inputs (list gnumach-headers-boot0)))))

(define/system-dependent hurd-core-headers-boot0
  ;; Return the Hurd and Mach headers as well as initial Hurd libraries for
  ;; the bootstrap environment.
  (package (inherit (package-with-bootstrap-guile hurd-core-headers))
           (arguments `(#:guile ,%bootstrap-guile
                        ,@(package-arguments hurd-core-headers)))
           (inputs
            `(("gnumach-headers" ,gnumach-headers-boot0)
              ("hurd-headers" ,hurd-headers-boot0)
              ("hurd-minimal" ,hurd-minimal-boot0)
              ,@(%boot0-inputs)))))

(define* (kernel-headers-boot0 #:optional (system (%current-system)))
  (match system
    ((? target-hurd?) hurd-core-headers-boot0)
    (_ linux-libre-headers-boot0)))

(define texinfo-boot0
  ;; Texinfo used to build libc's manual.
  ;; We build without ncurses because it fails to build at this stage, and
  ;; because we don't need the stand-alone Info reader.
  ;; Also, use (%BOOT0-INPUTS) to avoid building Perl once more.
  (package
    (inherit texinfo)
    (source (bootstrap-origin (package-source texinfo)))
    (native-inputs '())
    (inputs `(,@(%boot0-inputs)
              ("perl" ,perl-boot0)))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile

       ;; Some of Texinfo 6.1's tests would fail with "Couldn't set UTF-8
       ;; character type in locale" but we don't have a UTF-8 locale at this
       ;; stage, so skip them.
       #:tests? #f))))

(define expat-sans-tests
  (package
    (inherit expat)
    (inputs (%boot0-inputs))
    (outputs '("out"))
    (arguments
     ;; XXX: Linking 'runtestscpp' fails with things like:
     ;;
     ;;   ld: Dwarf Error: found dwarf version '3789', this reader only handles version 2 and 3 information.
     ;;
     ;; Skip tests altogether.
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile

       ,@(substitute-keyword-arguments (package-arguments expat)
           ((#:configure-flags flags ''())
            ;; Since we're not passing the right -Wl,-rpath flags, build the
            ;; static library to avoid RUNPATH validation failure.
            `(cons "--disable-shared" ,flags))
           ((#:phases phases) '%standard-phases)
           ((#:tests? _ #f) #f))))))

(define python-boot0
  (package
    (inherit python-minimal)
    ;; We cannot use Python 3.7 and later here, because they require
    ;; pthreads, which is missing on non-x86 platforms at this stage.
    ;; Python 3.6 technically supports being built without threading
    ;; support, but requires additional patches.
    (name "python-boot0")
    (version "3.5.9")
    (source (bootstrap-origin
             (origin
               (method url-fetch)
               (uri (string-append "https://www.python.org/ftp/python/"
                                   version "/Python-" version ".tar.xz"))
               (sha256
                (base32
                 "0jdh9pvx6m6lfz2liwvvhn7vks7qrysqgwn517fkpxb77b33fjn2"))
               (modules '((guix build utils)))
               (snippet
                '(begin
                   ;; Delete the bundled copy of libexpat.
                   (delete-file-recursively "Modules/expat")
                   (substitute* "Modules/Setup.dist"
                     ;; Link Expat instead of embedding the bundled one.
                     (("^#pyexpat.*") "pyexpat pyexpat.c -lexpat\n"))
                   ;; Delete windows binaries
                   (for-each delete-file
                             (find-files "Lib/distutils/command" ".*.exe$"))
                   (for-each delete-file
                             (find-files "Lib/ensurepip" ".*.whl$")))))))
    (inputs
     `(,@(%boot0-inputs)
       ("expat" ,expat-sans-tests)))              ;remove OpenSSL, zlib, etc.
    (native-inputs                                ;and pkg-config
     `())
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       ;; Running the tests won't work because we lack several required
       ;; modules (OpenSSL, etc).
       #:tests? #f
       ;; Disable features that cannot be built at this stage.
       #:configure-flags '("--without-ensurepip" "--without-threads")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-lib-shells
           (lambda _
             (substitute* '("Lib/subprocess.py"
                            "Lib/distutils/tests/test_spawn.py"
                            "Lib/test/support/__init__.py"
                            "Lib/test/test_subprocess.py")
               (("/bin/sh") (which "sh")))))
         (add-before 'configure 'disable-modules
           (lambda _
             (substitute* "setup.py"
               ;; Disable ctypes, since it requires libffi.
               (("extensions\\.append\\(ctypes\\)") "")
               ;; Prevent the 'ossaudiodev' extension from being
               ;; built, since it requires Linux headers.
               (("'linux', ") ""))))
         (add-after 'install 'remove-tests
           (lambda* (#:key outputs #:allow-other-keys)
             (delete-file-recursively
               (string-append (assoc-ref outputs "out")
                              "/lib/python"
                              ,(version-major+minor version)
                              "/test"))))
         ,@(if (system-hurd?)
               `((add-before 'build 'fix-regen
                   (lambda* (#:key inputs #:allow-other-keys)
                     (let ((libc (assoc-ref inputs "libc")))
                       (substitute* "Lib/plat-generic/regen"
                         (("/usr/include/")
                          (string-append libc "/include/")))))))
               '()))))
    (native-search-paths
     (list (search-path-specification
            (variable "PYTHONPATH")
            (files (list (string-append
                           "lib/python"
                           (version-major+minor version)
                           "/site-packages"))))))))

(define/system-dependent ld-wrapper-boot0
  ;; The first 'ld' wrapper, defined with 'define/system-dependent' because
  ;; its calls '%boot0-inputs', whose result depends on (%current-system)
  ;;
  ;; We need this so binaries on Hurd will have libmachuser and libhurduser
  ;; in their RUNPATH, otherwise validate-runpath will fail.
  (make-ld-wrapper "ld-wrapper-boot0"
                   #:target boot-triplet
                   #:binutils binutils-boot0
                   #:guile %bootstrap-guile
                   #:bash (car (assoc-ref (%boot0-inputs) "bash"))
                   #:guile-for-build %bootstrap-guile))

(define (%boot1-inputs)
  ;; 2nd stage inputs.
  `(("gcc" ,gcc-boot0)
    ("ld-wrapper-cross" ,ld-wrapper-boot0)
    ("binutils-cross" ,binutils-boot0)
    ,@(alist-delete "binutils" (%boot0-inputs))))

(define/system-dependent glibc-final-with-bootstrap-bash
  ;; The final libc, "cross-built".  If everything went well, the resulting
  ;; store path has no dependencies.  Actually, the really-final libc is
  ;; built just below; the only difference is that this one uses the
  ;; bootstrap Bash.
  (let ((libc (libc-for-target (%current-system))))
    (package
      (inherit libc)
      (name "glibc-intermediate")
      (outputs (delete "debug" (package-outputs libc)))
      (source (bootstrap-origin (package-source libc)))
      (arguments
       `(#:guile ,%bootstrap-guile
         #:implicit-inputs? #f

         ,@(substitute-keyword-arguments (package-arguments libc)
             ((#:configure-flags flags)
              `(append (list ,(string-append "--host=" (boot-triplet))
                             ,(string-append "--build="
                                             (nix-system->gnu-triplet))
                             ,(if (system-hurd?) "--disable-werror"
                                  ""))
                       ,flags))
             ((#:phases phases)
              `(modify-phases ,phases
                 (add-before 'configure 'pre-configure
                   (lambda* (#:key inputs #:allow-other-keys)
                     ;; Don't clobber include paths with the bootstrap libc.
                     (unsetenv "C_INCLUDE_PATH")
                     (unsetenv "CPLUS_INCLUDE_PATH")

                     ;; Tell 'libpthread' where to find 'libihash' on Hurd systems.
                     ,@(if (system-hurd?)
                           '((substitute* '("sysdeps/mach/Makefile"
                                            "sysdeps/mach/hurd/Makefile")
                               (("LDLIBS-pthread.so =.*")
                                (string-append "LDLIBS-pthread.so = "
                                               (assoc-ref %build-inputs "kernel-headers")
                                               "/lib/libihash.a\n"))))
                           '()))))))))
      (propagated-inputs `(("kernel-headers" ,(kernel-headers-boot0))))
      (native-inputs
       `(("bison" ,bison-boot0)
         ("texinfo" ,texinfo-boot0)
         ("perl" ,perl-boot0)
         ("python" ,python-boot0)))
      (inputs
       `( ;; The boot inputs.  That includes the bootstrap libc.  We don't want
         ;; it in $CPATH, hence the 'pre-configure' phase above.
         ,@(%boot1-inputs)

         ;; A native MiG is needed to build Glibc on Hurd.
         ,@(if (system-hurd?)
               `(("mig" ,mig-boot0))
               '())

         ;; Here, we use the bootstrap Bash, which is not satisfactory
         ;; because we don't want to depend on bootstrap tools.
         ("static-bash" ,@(assoc-ref (%boot0-inputs) "bash")))))))

(define (cross-gcc-wrapper gcc binutils glibc bash)
  "Return a wrapper for the pseudo-cross toolchain GCC/BINUTILS/GLIBC
that makes it available under the native tool names."
  (package (inherit gcc)
    (name (string-append (package-name gcc) "-wrapped"))
    (source #f)
    (build-system trivial-build-system)
    (outputs '("out"))
    (arguments
     (list
      #:guile %bootstrap-guile
      #:modules '((guix build utils))
      #:builder #~(begin
                    (use-modules (guix build utils))

                    (let* ((binutils (assoc-ref %build-inputs "binutils"))
                           (gcc      (assoc-ref %build-inputs "gcc"))
                           (libc     (assoc-ref %build-inputs "libc"))
                           (bash     (assoc-ref %build-inputs "bash"))
                           (out      (assoc-ref %outputs "out"))
                           (bindir   (string-append out "/bin"))
                           (triplet  #$(boot-triplet)))
                      (define (wrap-program program)
                        ;; GCC-BOOT0 is a libc-less cross-compiler, so it
                        ;; needs to be told where to find the crt files and
                        ;; the dynamic linker.
                        (call-with-output-file program
                          (lambda (p)
                            (format p "#!~a/bin/bash
exec ~a/bin/~a-~a -B~a/lib -Wl,-dynamic-linker -Wl,~a/~a \"$@\"~%"
                                    bash
                                    gcc triplet program
                                    libc libc
                                    #$(glibc-dynamic-linker))))

                        (chmod program #o555))

                      (mkdir-p bindir)
                      (with-directory-excursion bindir
                        (for-each (lambda (tool)
                                    (symlink (string-append binutils "/bin/"
                                                            triplet "-" tool)
                                             tool))
                                  '("ar" "ranlib"))
                        (for-each wrap-program '("gcc" "g++")))))))
    (native-inputs
     `(("binutils" ,binutils)
       ("gcc" ,gcc)
       ("libc" ,glibc)
       ("bash" ,bash)))
    (inputs '())))

(define/system-dependent gcc-boot0-intermediate-wrapped
  ;; Make the cross-tools GCC-BOOT0 and BINUTILS-BOOT0 available under the
  ;; non-cross names.
  (cross-gcc-wrapper gcc-boot0 binutils-boot0
                     glibc-final-with-bootstrap-bash
                     (car (assoc-ref (%boot1-inputs) "bash"))))

(define static-bash-for-glibc
  ;; A statically-linked Bash to be used by GLIBC-FINAL in system(3) & co.
  (package
    (inherit static-bash)
    (source (bootstrap-origin (package-source static-bash)))
    (inputs `(("gcc" ,gcc-boot0-intermediate-wrapped)
              ("libc" ,glibc-final-with-bootstrap-bash)
              ("libc:static" ,glibc-final-with-bootstrap-bash "static")
              ,@(fold alist-delete (%boot1-inputs)
                      '("gcc" "libc"))))
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile

       ,@(substitute-keyword-arguments (package-arguments static-bash)
           ((#:configure-flags flags #~'())
            ;; Add a '-L' flag so that the pseudo-cross-ld of
            ;; BINUTILS-BOOT0 can find libc.a.
            #~(append #$flags
                      (list (string-append "LDFLAGS=-static -L"
                                           (assoc-ref %build-inputs
                                                      "libc:static")
                                           "/lib")))))))))

(define gettext-boot0
  ;; A minimal gettext used during bootstrap.
  (package
    (inherit gettext-minimal)
    (name "gettext-boot0")
    ;; Newer versions of GNU gettext depends on libxml2 and ncurses.  To
    ;; simplify the dependency chain, we stick to this version here.
    (version "0.19.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gettext/gettext-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0hsw28f9q9xaggjlsdp2qmbp2rbd1mp0njzan2ld9kiqwkq2m57z"))))
    (inputs (%boot1-inputs))                      ;zero dependencies
    (arguments
     `(#:implicit-inputs? #f
       #:guile ,%bootstrap-guile
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  ;; Build only the tools.
                  (add-after 'unpack 'chdir
                    (lambda _
                      (chdir "gettext-tools")))

                  ;; Some test programs require pthreads, which we don't have.
                  (add-before 'configure 'no-test-programs
                    (lambda _
                      (substitute* "tests/Makefile.in"
                        (("^PROGRAMS =.*$")
                         "PROGRAMS =\n")))))))))

(define/system-dependent glibc-final
  ;; The final glibc, which embeds the statically-linked Bash built above.
  ;; Use 'package/inherit' so we get the 'replacement' of 'glibc', if any.
  (let ((libc (libc-for-target (%current-system))))
    (package/inherit libc
      (name "glibc")
      (source (bootstrap-origin (package-source libc)))
      (inputs `(("static-bash" ,static-bash-for-glibc)
                ,@(alist-delete
                   "static-bash"
                   (package-inputs glibc-final-with-bootstrap-bash))))

      ;; This time we need 'msgfmt' to install all the libc.mo files.
      (native-inputs `(,@(package-native-inputs glibc-final-with-bootstrap-bash)
                       ("gettext" ,gettext-boot0)))

      (propagated-inputs
       (package-propagated-inputs glibc-final-with-bootstrap-bash))

      ;; The final libc only refers to itself, but the 'debug' output contains
      ;; references to GCC-BOOT0 and to the Linux headers.  XXX: Would be great
      ;; if 'allowed-references' were per-output.
      (arguments
       `(#:allowed-references
         (,(gexp-input gcc-boot0 "lib")
          ,(kernel-headers-boot0)
          ,static-bash-for-glibc
          ,@(if (system-hurd?)
                `(,gnumach-headers-boot0
                  ,hurd-headers-boot0)
                '())
          ,@(package-outputs glibc-final-with-bootstrap-bash))
         ,@(package-arguments glibc-final-with-bootstrap-bash))))))

(define/system-dependent gcc-boot0-wrapped
  ;; Make the cross-tools GCC-BOOT0 and BINUTILS-BOOT0 available under the
  ;; non-cross names.
  (cross-gcc-wrapper gcc-boot0 binutils-boot0 glibc-final
                     (car (assoc-ref (%boot1-inputs) "bash"))))

(define (%boot2-inputs)
  ;; 3rd stage inputs.
  `(("libc" ,glibc-final)
    ("libc:static" ,glibc-final "static")
    ("gcc" ,gcc-boot0-wrapped)
    ,@(fold alist-delete (%boot1-inputs) '("libc" "gcc" "linux-libre-headers"))))

(define libstdc++
  ;; Intermediate libstdc++ that will allow us to build the final GCC
  ;; (remember that GCC-BOOT0 cannot build libstdc++.)
  (let ((lib (make-libstdc++ gcc-boot0)))
    (package
      (inherit lib)
      (source (bootstrap-origin (package-source lib)))
      (arguments
       `(#:guile ,%bootstrap-guile
         #:implicit-inputs? #f
         #:allowed-references ("out")

         ;; XXX: libstdc++.so NEEDs ld.so for some reason.
         #:validate-runpath? #f

         ;; All of the package arguments from 'make-libstdc++
         ;; except for the configure-flags.
         ,@(package-arguments lib)
         #:configure-flags `("--disable-shared"
                             "--disable-libstdcxx-dual-abi"
                             "--disable-libstdcxx-threads"
                             "--disable-libstdcxx-pch"
                             ,(string-append "--with-gxx-include-dir="
                                             (assoc-ref %outputs "out")
                                             "/include"))))
      (outputs '("out"))
      (inputs (%boot2-inputs))
      (synopsis "GNU C++ standard library (intermediate)"))))

(define binutils-final
  (package
    (inherit binutils)
    (source (bootstrap-origin (package-source binutils)))
    (arguments
     (append (list #:guile %bootstrap-guile
                   #:implicit-inputs? #f
                   #:allowed-references `("out" ,glibc-final
                                          ,(this-package-native-input
                                            "libstdc++")
                                          ,@(if (target-powerpc? (%current-system))
                                                (list static-bash-for-glibc)
                                                '())))
             (substitute-keyword-arguments (package-arguments binutils)
               ((#:configure-flags flags #~'())
                ;; For gprofng, tell the build system where to look for libstdc++.
                #~(append #$flags
                          (list (string-append "LDFLAGS=-L"
                                               #$(this-package-native-input
                                                  "libstdc++")
                                               "/lib")))))))
    (native-inputs (list bison-boot0
                         libstdc++))              ;for gprofng
    (inputs
     (match (%current-system)
       ((? target-powerpc?)
        `(("bash" ,static-bash-for-glibc)
          ,@(%boot2-inputs)))
       (_ (%boot2-inputs))))))

(define zlib-final
  ;; Zlib used by GCC-FINAL.
  (package
    (inherit zlib)
    (source (bootstrap-origin (package-source zlib)))
    (arguments
     `(#:guile ,%bootstrap-guile
       #:implicit-inputs? #f
       #:allowed-references ("out" ,glibc-final)
       ,@(package-arguments zlib)))
    (inputs (%boot2-inputs))))

(define/system-dependent ld-wrapper-boot3
  ;; A linker wrapper that uses the bootstrap Guile.
  (make-ld-wrapper "ld-wrapper-boot3"
                   #:binutils binutils-final
                   #:guile %bootstrap-guile
                   #:bash (car (assoc-ref (%boot2-inputs) "bash"))
                   #:guile-for-build %bootstrap-guile))

(define gcc-final
  ;; The final GCC.
  (package (inherit gcc-boot0)
    (name "gcc")

    ;; XXX: Currently #:allowed-references applies to all the outputs but the
    ;; "debug" output contains disallowed references, notably
    ;; linux-libre-headers.  Disable the debugging output to work around that.
    (outputs (delete "debug" (package-outputs gcc-boot0)))

    (arguments
     (cons*
      #:guile %bootstrap-guile
      #:implicit-inputs? #f

      #:allowed-references `("out" "lib" ,zlib-final
                             ,glibc-final ,static-bash-for-glibc)

      ;; Things like libasan.so and libstdc++.so NEED ld.so for some
      ;; reason, but it is not in their RUNPATH.  This is a false
      ;; positive, so turn it off.
      #:validate-runpath? #f

      ;; Additional modules for the libstdc++ phase below.
      #:modules `((srfi srfi-1)
                  (srfi srfi-26)
                  ,@%default-gnu-modules)

      (substitute-keyword-arguments (package-arguments gcc)
        ((#:make-flags flags)
         ;; Since $LIBRARY_PATH is not honored, add the relevant flags.
         #~(let ((zlib (assoc-ref %build-inputs "zlib")))
             (map (lambda (flag)
                    (if #$(if (target-hurd64?)
                              #~(and (string? flag)
                                     (string-prefix? "LDFLAGS=" flag))
                              #~(string-prefix? "LDFLAGS=" flag))
                        (string-append flag " -L"
                                       (assoc-ref %build-inputs "libstdc++")
                                       "/lib -L" zlib "/lib -Wl,-rpath="
                                       zlib "/lib")
                        flag))
                  #$(if (target-hurd64?)
                        `(cons
                          (string-append
                           ;;Convince gmp's configure that gcc works
                           "STAGE_CC_WRAPPER=" (getcwd) "/build/gcc.sh")
                          ,flags)
                        flags))))
        ((#:configure-flags flags)
         (if (target-hurd64?)
             #~(append
                #$flags
                (list #$(string-append
                         ;;Convince gmp's configure that gcc works
                         "CC=gcc"
                         " -Wno-implicit-function-declaration")
                      "--disable-plugin"))
             flags))
        ;; Build again GMP & co. within GCC's build process, because it's hard
        ;; to do outside (because GCC-BOOT0 is a cross-compiler, and thus
        ;; doesn't honor $LIBRARY_PATH, which breaks `gnu-build-system'.)
        ((#:phases phases)
         #~(modify-phases #$phases
             (add-after 'unpack 'unpack-gmp&co
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((gmp  (assoc-ref %build-inputs "gmp-source"))
                       (mpfr (assoc-ref %build-inputs "mpfr-source"))
                       (mpc  (assoc-ref %build-inputs "mpc-source")))

                   ;; To reduce the set of pre-built bootstrap inputs, build
                   ;; GMP & co. from GCC.
                   (for-each (lambda (source)
                               (invoke "tar" "xvf" source))
                             (list gmp mpfr mpc))

                   ;; Create symlinks like `gmp' -> `gmp-x.y.z'.
                   #$@(map (lambda (lib)
                             ;; Drop trailing letters, as gmp-6.0.0a unpacks
                             ;; into gmp-6.0.0.
                             #~(symlink #$(string-trim-right
                                           (package-full-name lib "-")
                                           char-set:letter)
                                        #$(package-name lib)))
                           (list gmp-6.0 mpfr mpc)))))
             (add-after 'unpack 'fix-build-with-external-libstdc++
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((libstdc++ (assoc-ref inputs "libstdc++")))
                   ;; Fix a regression in GCC 11 where the libstc++ input
                   ;; shadows glibc headers when building libstdc++.  An
                   ;; upstream fix was added in GCC 11.3.0, but it only
                   ;; hides system include directories, not those on
                   ;; CPLUS_INCLUDE_PATH.  See discussion at
                   ;; <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=100017>.
                   (substitute* "libstdc++-v3/src/c++17/Makefile.in"
                     (("AM_CXXFLAGS = ")
                      (string-append "CPLUS_INCLUDE_PATH = "
                                     (string-join
                                      (remove (cut string-prefix? libstdc++ <>)
                                              (string-split
                                               (getenv "CPLUS_INCLUDE_PATH")
                                               #\:))
                                      ":")
                                     "\nAM_CXXFLAGS = "))))))
             #$@(if (target-hurd64?)
                    #~((add-after 'configure 'create-stage-wrapper
                         (lambda _
                           (with-output-to-file "gcc.sh"
                             (lambda _
                               (format #t "#! ~a/bin/bash
exec \"$@\" \
    -Wno-error \
    -Wno-implicit-function-declaration"
                                       #$static-bash-for-glibc)))
                           (chmod "gcc.sh" #o555))))
                    #~()))))))

    ;; This time we want Texinfo, so we get the manual.  Add
    ;; STATIC-BASH-FOR-GLIBC so that it's used in the final shebangs of
    ;; scripts such as 'mkheaders' and 'fixinc.sh' (XXX: who cares about these
    ;; scripts?).
    (native-inputs `(("texinfo" ,texinfo-boot0)
                     ("perl" ,perl-boot0) ;for manpages
                     ("static-bash" ,static-bash-for-glibc)
                     ,@(package-native-inputs gcc-boot0)))

    (inputs `(("gmp-source" ,(bootstrap-origin (package-source gmp-6.0)))
              ("mpfr-source" ,(package-source mpfr))
              ("mpc-source" ,(package-source mpc))
              ("ld-wrapper" ,ld-wrapper-boot3)
              ("binutils" ,binutils-final)
              ("libstdc++" ,libstdc++)
              ("zlib" ,zlib-final)
              ,@(%boot2-inputs)))))

(define (%boot3-inputs)
  ;; 4th stage inputs.
  `(("gcc" ,gcc-final)
    ("ld-wrapper" ,ld-wrapper-boot3)
    ,@(alist-delete "gcc" (%boot2-inputs))))

(define bash-final
  ;; Link with `-static-libgcc' to make sure we don't retain a reference
  ;; to the bootstrap GCC.  Use "bash-minimal" to avoid an extra dependency
  ;; on Readline and ncurses.
  (let ((bash (static-libgcc-package bash-minimal)))
    (package
      (inherit bash)
      (source (bootstrap-origin (package-source bash)))
      (inputs (%boot3-inputs))
      (arguments
       `(#:implicit-inputs? #f
         #:guile ,%bootstrap-guile

         #:disallowed-references ,(assoc-ref (%boot3-inputs) "coreutils&co")

         ,@(package-arguments bash))))))

(define (%boot4-inputs)
  ;; Now use the final Bash.
  `(("bash" ,bash-final)
    ,@(alist-delete "bash" (%boot3-inputs))))

(define with-boot4
  (package-with-explicit-inputs %boot4-inputs %bootstrap-guile))

(define-public guile-final
  ;; This package must be public because other modules refer to it.  However,
  ;; mark it as hidden so that 'fold-packages' ignores it.
  (let ((parent (with-boot4 (hidden-package
                             (package-with-bootstrap-guile guile-3.0/pinned)))))
    (package
      (inherit parent)
      (inputs
       (modify-inputs (package-inputs parent)
         (delete "libxcrypt")))
      (arguments
       (substitute-keyword-arguments (package-arguments parent)
         ((#:phases phases #~%standard-phases)
          #~(modify-phases #$phases
              (delete 'add-libxcrypt-reference-pkgconfig))))))))

(define-public glibc-utf8-locales-final
  ;; Now that we have GUILE-FINAL, build the UTF-8 locales.  They are needed
  ;; by the build processes afterwards so their 'scm_to_locale_string' works
  ;; with the full range of Unicode codepoints (remember
  ;; 'scm_to_locale_string' is called every time a string is passed to a C
  ;; function.)
  (package
    (inherit glibc-utf8-locales)
    (properties `((hidden? . #t)
                  ,@(package-properties glibc-utf8-locales)))
    (native-inputs
     `(("glibc" ,glibc-final)
       ("gzip" ,(with-boot4 gzip))))))

(define-public glibc-utf8-locales-final/hurd
  ;; Locales for the libc version used on GNU/Hurd.
  (package
    (inherit glibc-utf8-locales/hurd)
    (properties `((hidden? . #t)
                  ,@(package-properties glibc-utf8-locales/hurd)))
    (native-inputs
     `(("glibc" ,glibc-final)
       ("gzip" ,(with-boot4 gzip))))))

(define-public ld-wrapper
  ;; The final 'ld' wrapper, which uses the final Guile and Binutils.
  (make-ld-wrapper "ld-wrapper"
                   #:binutils binutils-final
                   #:guile guile-final
                   #:bash bash-final))

(define-public ld-gold-wrapper
  (make-ld-wrapper "ld-gold-wrapper"
                   #:binutils binutils-gold
                   #:linker "ld.gold"
                   #:guile guile-final
                   #:bash bash-final))

;; There used to be a "stage 5" including a variant of the
;; 'glibc-utf8-locales' package.  This is no longer necessary since 'glibc'
;; embeds the "C.UTF-8" locale, but these aliases are kept for convenience.
(define %boot5-inputs %boot4-inputs)
(define with-boot5 with-boot4)

(define (make-gnu-make-final)
  "Compute the final GNU Make, which uses the final Guile."
  (let ((pkg-config (package
                      (inherit %pkg-config)       ;the native pkg-config
                      (inputs `(("guile" ,guile-final)
                                ,@(%boot5-inputs)))
                      (arguments
                       `(#:implicit-inputs? #f
                         ,@(package-arguments %pkg-config))))))
    (package
      (inherit (package-with-bootstrap-guile gnu-make))
      (inputs `(("guile" ,guile-final)
                ,@(%boot5-inputs)))
      (native-inputs `(("pkg-config" ,pkg-config)))
      (arguments
       `(#:implicit-inputs? #f
         ,@(package-arguments gnu-make))))))


(define coreutils-final
  ;; The final Coreutils.  Treat them specially because some packages, such as
  ;; Findutils, keep a reference to the Coreutils they were built with.
  (with-boot5 (package-with-bootstrap-guile
               (package
                 (inherit coreutils-minimal)
                 (name "coreutils")))
              ;; Use the final Guile, linked against the
              ;; final libc with working iconv, so that
              ;; 'substitute*' works well when touching
              ;; test files in Gettext.
              ))

(define grep-final
  ;; The final grep.  Gzip holds a reference to it (via zgrep), so it must be
  ;; built before gzip.
  (let ((grep (with-boot5 (package-with-bootstrap-guile grep))))
    (package/inherit grep
      (arguments (strip-keyword-arguments '(#:configure-flags)
                                          (package-arguments grep)))
      (inputs (alist-delete "pcre2" (package-inputs grep)))
      (native-inputs `(("perl" ,perl-boot0))))))

(define xz-final
  ;; The final xz.  We need to replace the bootstrap xz with a newer one
  ;; before we get to the %final-inputs so file doesn't try to link to it.
  (let ((xz (with-boot5 (package-with-bootstrap-guile xz))))
    (package/inherit xz)))

(define (%boot6-inputs)
  ;; Now use the final Coreutils.
  `(("coreutils" ,coreutils-final)
    ("grep" ,grep-final)
    ("xz" ,xz-final)
    ,@(fold alist-delete (%boot5-inputs)
            '("coreutils" "xz"))))

(define with-boot6
  (package-with-explicit-inputs %boot6-inputs))

(define sed-final
  ;; The final sed.
  (let ((sed (with-boot6 (package-with-bootstrap-guile sed))))
    (package/inherit sed (native-inputs `(("perl" ,perl-boot0))))))

(define-public %final-inputs
  ;; The 'glibc-final' package is not the same depending on what system is
  ;; targeted, so this whole list must be parameterized.
  (mlambda (system)
    ;; Final derivations used as implicit inputs by 'gnu-build-system'.  We
    ;; still use 'package-with-bootstrap-guile' so that the bootstrap tools are
    ;; used for origins that have patches, thereby avoiding circular
    ;; dependencies.
    (parameterize ((%current-system system))
      (let ((finalize (compose with-boot6
                               package-with-bootstrap-guile)))
        `(,@(map (match-lambda
                   ((name package)
                    (list name (finalize package))))
                 `(("tar" ,tar)
                   ("gzip" ,gzip)
                   ("bzip2" ,bzip2)
                   ("file" ,file)
                   ("diffutils" ,diffutils)
                   ("patch" ,patch/pinned)
                   ("findutils" ,findutils)
                   ("gawk" ,(package/inherit gawk
                              (native-inputs
                               (list (if (target-hurd?)
                                         glibc-utf8-locales-final/hurd
                                         glibc-utf8-locales-final)))))
                   ("zstd" ,zstd)))
          ("sed" ,sed-final)
          ("grep" ,grep-final)
          ("xz" ,xz-final)
          ("coreutils" ,coreutils-final)
          ("make" ,(make-gnu-make-final))
          ("bash" ,bash-final)
          ("ld-wrapper" ,ld-wrapper)
          ("binutils" ,binutils-final)
          ("gcc" ,gcc-final)
          ("libc" ,glibc-final)
          ("libc:static" ,glibc-final "static"))))))

(define-public canonical-package
  (let ((name->package (mlambda (system)
                         (fold (lambda (input result)
                                 (match input
                                   ((_ package . outputs)
                                    (vhash-cons (package-full-name package)
                                                package result))))
                               vlist-null
                               `(("guile" ,guile-final)
                                 ("glibc-utf8-locales"
                                  ,(if (target-hurd? system)
                                       glibc-utf8-locales-final/hurd
                                       glibc-utf8-locales-final))
                                 ,@(%final-inputs system))))))
    (lambda (package)
      "Return the 'canonical' variant of PACKAGE---i.e., if PACKAGE is one of
the implicit inputs of 'gnu-build-system', return that one, otherwise return
PACKAGE.

The goal is to avoid duplication in cases like GUILE-FINAL vs. GUILE-2.2,
COREUTILS-FINAL vs. COREUTILS, etc."
      ;; XXX: This doesn't handle dependencies of the final inputs, such as
      ;; libunistring, GMP, etc.
      (match (vhash-assoc (package-full-name package)
                          (name->package (%current-system)))
        ((_ . canon)
         ;; In general we want CANON, except if we're cross-compiling: CANON
         ;; uses explicit inputs, so it is "anchored" in the bootstrapped
         ;; process, with dependencies on things that cannot be
         ;; cross-compiled.
         (if (%current-target-system)
             package
             canon))
        (_ package)))))


;;;
;;; GCC toolchain.
;;;

;; Using the following procedure, a gcc toolchain targeting glibc-2.27 can be
;; instantiated like this:
;;
;; (define-public gcc-glibc-2.27-toolchain
;;   (make-gcc-toolchain gcc glibc-2.27))

(define* (make-gcc-toolchain gcc
                            #:optional
                            (libc #f))
  "Return a complete toolchain for GCC. If LIBC is specified, target that libc."
  (let ((gcc (if libc (make-gcc-libc gcc libc) gcc))
        (libc (if libc libc glibc-final)))
    (package
      (name (string-append (package-name gcc) "-toolchain"))
      (version (package-version gcc))
      (source #f)
      (build-system trivial-build-system)
      (arguments
       '(#:modules ((guix build union))
         #:builder (begin
                     (use-modules (ice-9 match)
                                  (srfi srfi-1)
                                  (srfi srfi-26)
                                  (guix build union))

                     (let ((out (assoc-ref %outputs "out")))
                       (union-build out
                                    (filter-map (match-lambda
                                                  (("libc-debug" . _) #f)
                                                  (("libc-static" . _) #f)
                                                  ((_ . directory) directory))
                                                %build-inputs))

                       (union-build (assoc-ref %outputs "debug")
                                    (list (assoc-ref %build-inputs
                                                     "libc-debug")))
                       (union-build (assoc-ref %outputs "static")
                                    (list (assoc-ref %build-inputs
                                                     "libc-static")))))))

      (native-search-paths
       (append (package-native-search-paths gcc)
               (package-native-search-paths libc))) ;GUIX_LOCPATH
      (search-paths
       (append (package-search-paths gcc)
               (package-search-paths libc)))

      ;; Copy the 'compiler-cpu-architectures' property and other things that
      ;; may be useful, but not the 'hidden?' property.
      (properties (alist-delete 'hidden? (package-properties gcc)))

      (license (package-license gcc))
      (synopsis "Complete GCC tool chain for C/C++ development")
      (description
       "This package provides a complete GCC tool chain for C/C++ development to
be installed in user profiles.  This includes GCC, as well as libc (headers and
binaries, plus debugging symbols in the @code{debug} output), and Binutils.  GCC
is the GNU Compiler Collection.")
      (home-page "https://gcc.gnu.org/")
      (outputs '("out" "debug" "static"))

      ;; The main raison d'être of this "meta-package" is (1) to conveniently
      ;; install everything that we need, and (2) to make sure ld-wrapper comes
      ;; before Binutils' ld in the user's profile.
      (inputs `(("gcc" ,gcc)
                ("ld-wrapper" ,(car (assoc-ref (%final-inputs (%current-system))
                                               "ld-wrapper")))
                ("binutils" ,binutils-final)
                ("gcc-lib" ,gcc "lib")
                ("libc" ,libc)
                ("libc-debug" ,libc "debug")
                ("libc-static" ,libc "static"))))))


(define-public gcc-toolchain-4.8
  (make-gcc-toolchain gcc-4.8))

(define-public gcc-toolchain-4.9
  (make-gcc-toolchain gcc-4.9))

(define-public gcc-toolchain-5
  (make-gcc-toolchain gcc-5))

(define-public gcc-toolchain-6
  (make-gcc-toolchain gcc-6))

(define-public gcc-toolchain-7
  (make-gcc-toolchain gcc-7))

(define-public gcc-toolchain-8
  (make-gcc-toolchain gcc-8))

(define-public gcc-toolchain-9
  (make-gcc-toolchain gcc-9))

(define-public gcc-toolchain-10
  (make-gcc-toolchain gcc-10))

(define-public gcc-toolchain-11
    (make-gcc-toolchain gcc-11))

(define-public gcc-toolchain-12
  (make-gcc-toolchain gcc-12))

(define-public gcc-toolchain-13
  (make-gcc-toolchain gcc-13))

(define-public gcc-toolchain-14
  (make-gcc-toolchain gcc-14))

;; The default GCC
(define-public gcc-toolchain
  (if (host-hurd64?)
      gcc-toolchain-14
      gcc-toolchain-11))

(define-public gcc-toolchain-aka-gcc
  ;; It's natural for users to try "guix install gcc".  This package
  ;; automatically "redirects" them to 'gcc-toolchain'.
  (deprecated-package "gcc" gcc-toolchain))


(define-public gdc-toolchain-10
  (package (inherit (make-gcc-toolchain gdc-10))
    (synopsis "Complete GCC tool chain for D lang development")
    (description "This package provides a complete GCC tool chain for
D lang development to be installed in user profiles.  This includes
gdc, as well as libc (headers and binaries, plus debugging symbols
in the @code{debug} output), and binutils.")))

(define-public gdc-toolchain-11
  (package (inherit (make-gcc-toolchain gdc-11))
    (synopsis "Complete GCC tool chain for D lang development")
    (description "This package provides a complete GCC tool chain for
D lang development to be installed in user profiles.  This includes
gdc, as well as libc (headers and binaries, plus debugging symbols
in the @code{debug} output), and binutils.")))

;; Provide the Fortran toolchain package only for the version of gfortran that
;; is used by Guix internally to build Fortran libraries, because combining
;; code compiled with different versions can cause problems.

(define-public gfortran-toolchain
  (package (inherit (make-gcc-toolchain gfortran))
    (synopsis "Complete GCC tool chain for Fortran development")
    (description "This package provides a complete GCC tool chain for
Fortran development to be installed in user profiles.  This includes
gfortran, as well as libc (headers and binaries, plus debugging symbols
in the @code{debug} output), and binutils.")))


;;; commencement.scm ends here
