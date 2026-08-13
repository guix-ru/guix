;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Andrew Wong <wongandj@icloud.com>
;;; Copyright © 2025 Alexis Simon <alexis.simon@runbox.com>
;;; Copyright © 2026 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2026 Liliana Marie Prikler <liliana.prikler@gmail.com>
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

(define-module (gnu packages typst)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages rust-crates)
  #:use-module (gnu packages tls))

(define-public typst
  (package
    (name "typst")
    (version "0.15.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/typst/typst")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y727lyicn3ciq36mdpbcg8d09naff39y1i52467mlmr11p0l9xa"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:imported-modules (append %copy-build-system-modules
                                 %cargo-build-system-modules)
      #:modules '((guix build cargo-build-system)
                  ((guix build copy-build-system) #:prefix copy:)
                  (guix build utils))
      #:cargo-install-paths ''("crates/typst-cli")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-version-string
            (lambda _
              (setenv "TYPST_VERSION" #$(package-version this-package))))
          (add-after 'unpack 'fix-dev-assets
            (lambda _
              (substitute* "Cargo.toml"
                (("typst-dev-assets = \\{[^}]*\\}")
                 (string-append "typst-dev-assets = {version=\""
                                #$version
                                "\"}")))))
          (add-after 'configure 'configure-artifacts
            (lambda _
              (setenv "GEN_ARTIFACTS" (string-append (getcwd) "/artifacts"))))
          (add-after 'install 'install-artifacts
            (lambda args
               (apply (assoc-ref copy:%standard-phases 'install)
                      #:install-plan
                      '(("artifacts/typst.bash"
                         "share/bash-completion/completions/typst")
                        ("artifacts/typst.elv"
                         "share//elvish/lib/typst")
                        ("artifacts/typst.fish"
                         "share/fish/vendor_completions.d/")
                        ("artifacts/_typst"
                         "share/zsh/site-functions/")
                        ("artifacts/" "share/man/man1/"
                         #:include-regexp ("\\.1$")))
                      args))))))
    (inputs (cons* openssl (cargo-inputs 'typst)))
    (native-search-paths
     (list (search-path-specification
            (variable "TYPST_PACKAGE_PATH")
            (files '("share/typst/packages"))
            (separator #f))
           (search-path-specification
            (variable "TYPST_FONT_PATHS")
            (files '("share/fonts" "share/texmf-dist/fonts")))))
    (home-page "https://typst.app/")
    (synopsis "LaTeX-like typesetting system")
    (description
     "Typst is a markup-based typesetting system that is designed to be as
powerful as LaTeX while being much easier to learn and use.  Features include
built-in markup for math typesetting, bibliography management and other common
tasks, an extensible scripting system for uncommon tasks, incremental
compilation, and intuitive error messages.")
    (license license:asl2.0)))
