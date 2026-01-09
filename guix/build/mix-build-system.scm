;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Pierre-Henry Fröhring <contact@phfrohring.com>
;;; Copyright © 2024-2026 Igorj Gorjaĉev <igor@goryachev.org>
;;; Copyright © 2024, 2025 Giacomo Leidi <therewasa@fishinthecalculator.me>
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

;; Commentary:
;;
;; Code:

(define-module (guix build mix-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:export (mix-build
            %standard-phases))

;; The Elixir version is constant as soon as it is computable from the current
;; execution.  It is a X.Y string where X and Y are respectively the major and
;; minor version number of the Elixir used in the build.
(define %elixir-version (make-parameter "X.Y"))

(define %git-version-rx
  (make-regexp "^(.*)-[0-9]+(\\.[0-9]+)?(\\.[0-9]+)?-[0-9]+\\..+$"))

(define* (elixir-libdir path #:optional (version (%elixir-version)))
  "Return the path where all libraries under PATH for a specified Elixir
VERSION are installed."
  (string-append path "/lib/elixir/" version))

(define %elixir-prefix "elixir-")

(define %beam-prefix "beam-")

(define %vendorize-env-var "GUIX_MIX_VENDOR_DIR")

(define* (strip-prefix name #:optional (prefix %elixir-prefix))
  "Return NAME without the prefix PREFIX."
  (if (string-prefix? prefix name)
      (string-drop name (string-length prefix))
      name))

(define (mix-build-dir mix-build-root mix-env)
  "Return the directory where build artifacts are to be installed according to
en environment MIX-ENV in the current directory.  MIX-BUILD-ROOT depends on the
package arguments.  See: https://hexdocs.pm/mix/1.15/Mix.html#module-environment-variables"
  (string-append mix-build-root "/" mix-env "/lib"))

(define (elixir-version inputs)
  "Return an X.Y string where X and Y are respectively the major and minor version number of PACKAGE.
Example: /gnu/store/…-elixir-1.14.0 → 1.14"
  ((compose
    (cute string-join <> ".")
    (cute take <> 2)
    (cute string-split <> #\.)
    strip-prefix
    strip-store-file-name)
   (assoc-ref inputs "elixir")))

(define* (unpack #:key source mix-path #:allow-other-keys)
  "Unpack SOURCE in the working directory, and change directory within the
source.  When SOURCE is a directory, copy it in a sub-directory of the current
working directory."
  (let ((gnu-unpack (assoc-ref gnu:%standard-phases 'unpack)))
    (gnu-unpack #:source source)
    (when (file-exists? "contents.tar.gz")
      (invoke "tar" "xvf" "contents.tar.gz"))))

(define (list-directories dir)
  "List absolute paths of directories directly under the directory DIR."
  (map (cute string-append dir "/" <>)
       (scandir
        dir (lambda (filename)
              (and (not (member filename '("." "..")))
                   (directory-exists? (string-append dir "/" filename)))))))

(define* (unpack-vendorize #:key vendorize? vendor-dir inputs
                           #:allow-other-keys)
  "Unpack vendored packages."
  (define (inputs->beam-inputs inputs)
    "Filter using the label part from INPUTS."
    (filter-map (lambda (input)
                  (match input
                    ((name . file)
                     (and (beam-package? name) file))))
                inputs))
  (define (beam-input->upstream-name beam-input)
    "Convert BEAM-INPUT into upstream package name."
    ((compose
      (cute package-name->elixir-name <> %beam-prefix)
      (cute substring <> 33)
      basename)
     beam-input))
  (define (copy-or-unpack-beam-package beam-input dep-dir)
    "Copy contents, if BEAM-INPUT is a checkout package, otherwise unpack it."
    (or (directory-exists? dep-dir)
        (if (file-is-directory? beam-input)
            (copy-recursively beam-input dep-dir)
            (begin
              (mkdir-p dep-dir)
              (invoke "sh" "-c"
                      (string-append "tar -xOf " beam-input
                                     " contents.tar.gz"
                                     " | tar -xz -C " dep-dir))))))
  (and
   vendorize?
   (begin
     (mkdir-p vendor-dir)
     (setenv %vendorize-env-var (canonicalize-path vendor-dir))
     (let ((beam-inputs (delete-duplicates (inputs->beam-inputs inputs))))
       (unless (null? beam-inputs)
         (for-each
          (lambda (beam-input)
            (let* ((upstream-name (beam-input->upstream-name beam-input))
                   (dep-dir (string-append vendor-dir "/" upstream-name)))
              (copy-or-unpack-beam-package beam-input dep-dir)))
          beam-inputs))))))

(define* (symlink-vendorize #:key vendorize? vendor-symlinks?
                            #:allow-other-keys)
  (and
   vendorize?
   vendor-symlinks?
   (let* ((vendor-dir (getenv %vendorize-env-var))
          (deps-dir (list-directories vendor-dir)))
     (for-each
      (lambda (dep-dir)
        (let ((snapshot (string-contains dep-dir "_snapshot")))
          (and
           snapshot
           (let ((canonical
                  (string-drop-right
                   dep-dir (- (string-length dep-dir) snapshot))))
             (symlink dep-dir canonical)))))
      deps-dir))))

(define (beam-package? name)
  (string-prefix? %beam-prefix name))

(define* (set-mix-env #:key inputs mix-path mix-exs #:allow-other-keys)
  "Set environment variables.
See: https://hexdocs.pm/mix/1.15.7/Mix.html#module-environment-variables"
  (setenv "MIX_ARCHIVES" "archives")
  (setenv "MIX_BUILD_ROOT" "_build")
  (setenv "MIX_DEPS_PATH" "deps")
  (setenv "MIX_EXS" mix-exs)
  (setenv "MIX_HOME" (getcwd))
  (setenv "MIX_PATH" (or mix-path ""))
  (setenv "MIX_REBAR3" (string-append (assoc-ref inputs "rebar3") "/bin/rebar3"))
  ;; Set deterministic compiler option.
  (setenv "ERL_COMPILER_OPTIONS" "deterministic")
  ;; Add Erlang dependencies in Elixir's load path.
  (setenv "ERL_LIBS"
          (string-join (search-path-as-list
                        `("lib/erlang/lib")
                        (map (match-lambda
                               ((label . package) package))
                             inputs))
                       ":")))

(define* (set-elixir-version #:key inputs #:allow-other-keys)
  "Store the version number of the Elixir input in a parameter."
  (%elixir-version (elixir-version inputs))
  (format #t "Elixir version: ~a~%" (%elixir-version)))


(define* (build #:key mix-environments vendorize?
                #:allow-other-keys)
  "Builds the Mix project."
  (for-each (lambda (mix-env)
              (setenv "MIX_ENV" mix-env)
              (if vendorize?
                  (invoke "mix" "release")
                  (invoke "mix" "compile" "--no-prune-code-paths"
                          "--no-deps-check")))
            mix-environments))

(define* (check #:key (tests? #t) (test-flags '())
                vendorize?
                #:allow-other-keys)
  "Test the Mix project."
  (if tests?
      (let ((maybe-no-deps-check-flag
             (if vendorize? '() '("--no-deps-check"))))
        (setenv "MIX_ENV" "test")
        (apply invoke
               (append '("mix" "do" "compile")
                       maybe-no-deps-check-flag
                       '("--no-prune-code-paths" "+" "test")
                       maybe-no-deps-check-flag
                       test-flags)))
      (format #t "tests? = ~a~%" tests?)))

(define* (remove-mix-dirs . _)
  "Remove all .mix/ directories.
We do not want to copy them to the installation directory."
  (for-each delete-file-recursively
            (find-files "." (file-name-predicate "\\.mix$") #:directories? #t)))

(define* (package-name->elixir-name name+ver #:optional (prefix %elixir-prefix))
  "Convert the Guix package NAME-VER to the corresponding Elixir name-version
format.  Example: elixir-a-pkg-1.2.3 -> a_pkg or elixir-a-pkg-0.0.0-0.e51e36e
-> a_pkg"
  (define git-version? (regexp-exec %git-version-rx name+ver))
  ((compose
    (cute string-join <> "_")
    (cute drop-right <> (if git-version? 2 1))
    (cute string-split <> #\-))
   (strip-prefix name+ver prefix)))

(define* (install #:key
                  inputs
                  outputs
                  name
                  build-per-environment
                  #:allow-other-keys)
  "Install build artifacts in the store."
  (if (beam-package? name)
      (let* ((out (assoc-ref outputs "out"))
             (excluded '("CHECKSUM" "contents.tar.gz" "environment-variables"
                         "metadata.config" "VERSION"))
             (files
              (filter
               (lambda (f)
                 (not (member f (cons* "." ".." excluded))))
               (scandir "."))))
        (mkdir-p out)
        (apply invoke "cp" "-r" (append files (list out))))
      (let* ((lib-name (package-name->elixir-name name))
             (lib-dir (string-append (elixir-libdir (assoc-ref outputs "out")) "/" lib-name))
             (root (getenv "MIX_BUILD_ROOT"))
             (env (if build-per-environment "prod" "shared")))
        (mkdir-p lib-dir)
        (copy-recursively (string-append (mix-build-dir root env) "/" lib-name) lib-dir
                          #:follow-symlinks? #t))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (add-after 'install-locale 'set-mix-env set-mix-env)
    (add-after 'set-mix-env 'set-elixir-version set-elixir-version)
    (replace 'unpack unpack)
    (add-after 'unpack 'unpack-vendorize unpack-vendorize)
    (add-after 'unpack-vendorize 'symlink-vendorize symlink-vendorize)
    (replace 'build build)
    (replace 'check check)
    (add-before 'install 'remove-mix-dirs remove-mix-dirs)
    (replace 'install install)))

(define* (mix-build #:key inputs (phases %standard-phases)
                    #:allow-other-keys #:rest args)
  "Build the given Mix package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; mix-build-system.scm ends here
