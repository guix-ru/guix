;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Pierre-Henry Fröhring <contact@phfrohring.com>
;;; Copyright © 2025 Giacomo Leidi <therewasa@fishinthecalculator.me>
;;; Copyright © 2025 Igorj Gorjaĉev <igor@goryachev.org>
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
;; Standard build procedure for Elixir packages using 'mix'.  This is
;; implemented as an extension of 'gnu-build-system'.
;;
;; Code:

(define-module (guix build-system mix)
  #:use-module (guix build mix-build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system)
  #:use-module (guix diagnostics)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix search-paths)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (mix-build-system
            hexpm-uri
            beam-name->package-name
            hexpm-source
            define-mix-inputs
            mix-inputs))

(define (default-elixir-hex)
  (@* (gnu packages elixir) elixir-hex))

(define (default-rebar3)
  (@* (gnu packages erlang) rebar3))

(define (default-elixir)
  (@* (gnu packages elixir) elixir))

(define* (strip-prefix name #:optional (prefix "elixir-"))
  "Return NAME without the prefix PREFIX."
  (if (string-prefix? prefix name)
      (string-drop name (string-length prefix))
      name))

(define (hexpm-uri name version)
  "Return the URI where to fetch the sources of a Hex package NAME at VERSION.
NAME is the name of the package which should look like: elixir-pkg-name-X.Y.Z
See: https://github.com/hexpm/specifications/blob/main/endpoints.md"
  ((compose
    (cute string-append "https://repo.hex.pm/tarballs/" <> "-" version ".tar")
    (cute string-replace-substring <> "-" "_")
    strip-prefix)
   name))

(define (beam-name->package-name name)
  (downstream-package-name "beam-" name))

;; NOTE: Only use this procedure in (gnu packages beam-packages).
(define* (hexpm-source package-name hexpm-name hexpm-version hexpm-hash
                       #:key (patches '()) (snippet #f))
  (origin
    (method url-fetch)
    (uri (hexpm-uri hexpm-name hexpm-version))
    (file-name
     (string-append "beam-" package-name "-" hexpm-version ".tar"))
    (sha256 (base32 hexpm-hash))
    (modules '((guix build utils)))
    (patches patches)
    (snippet snippet)))

(define-syntax define-mix-inputs
  (syntax-rules (=>)
    ((_ lookup inputs ...)
     (define lookup
       (let ((table (make-hash-table)))
         (letrec-syntax ((record
                          (syntax-rules (=>)
                            ((_) #t)
                            ((_ (name => lst) rest (... ...))
                             (begin
                               (hashq-set! table 'name (filter identity lst))
                               (record rest (... ...)))))))
           (record inputs ...)
           (lambda (name)
             "Return the inputs for NAME."
             (hashq-ref table name))))))))

(define* (mix-inputs name #:key (module '(gnu packages beam-packages)))
  "Lookup Mix inputs for NAME defined in MODULE, return an empty list if
unavailable."
  (let ((lookup (module-ref (resolve-interface module) 'lookup-mix-inputs)))
    (or (lookup name)
        (begin
          (warning (G_ "no Mix inputs available for '~a'~%") name)
          '()))))

;; A number of environment variables specific to the Mix build system are
;; reflected here.  They are documented at
;; https://hexdocs.pm/mix/Mix.html#module-environment-variables.  Other
;; parameters located in mix.exs are defined at
;; https://hexdocs.pm/mix/Mix.Project.html#module-configuration
(define* (mix-build name
                    inputs
                    #:key
                    source
                    (tests? #t)
                    (test-flags ''())
                    (vendorize? #f)
                    (vendor-symlinks? #t)
                    (vendor-dir "guix-vendor")
                    (mix-path #f) ;See MIX_PATH.
                    (mix-exs "mix.exs") ;See MIX_EXS.
                    (build-per-environment #t) ;See :build_per_environment.
                    (phases '%standard-phases)
                    (outputs '("out"))
                    (search-paths '())
                    (system (%current-system))
                    (guile #f)
                    (imported-modules `((guix build mix-build-system)
                                        ,@%default-gnu-imported-modules))
                    (modules '((guix build mix-build-system)
                               (guix build utils))))
  "Build SOURCE using Elixir, and with INPUTS."

  ;; Check the documentation of :build_per_environment here:
  ;; https://hexdocs.pm/mix/Mix.Project.html#module-configuration And
  ;; "Environments" here:
  ;; https://hexdocs.pm/mix/Mix.html#module-environments
  (define mix-environments
    (if build-per-environment
        `("prod" ,@(if tests? '("test") '()))
        '("shared")))

  (define builder
    (with-imported-modules imported-modules
      #~(begin

          (use-modules #$@(sexp->gexp modules))

          #$(with-build-variables inputs outputs
              #~(mix-build #:name #$name
                           #:source #+source
                           #:system #$system
                           #:tests? #$tests?
                           #:test-flags #$test-flags
                           #:vendorize? #$vendorize?
                           #:vendor-symlinks? #$vendor-symlinks?
                           #:vendor-dir #$vendor-dir
                           #:mix-path #$mix-path
                           #:mix-exs #$mix-exs
                           #:mix-environments '#$mix-environments
                           #:build-per-environment #$build-per-environment
                           #:phases #$(if (pair? phases)
                                          (sexp->gexp phases)
                                          phases)
                           #:outputs %outputs
                           #:search-paths '#$(sexp->gexp
                                              (map
                                               search-path-specification->sexp
                                               search-paths))
                           #:inputs
                           %build-inputs)))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system
                                                  #:graft? #f)))
    (gexp->derivation name
                      builder
                      #:system system
                      #:graft? #f       ;consistent with 'gnu-build'
                      #:target #f
                      #:guile-for-build guile)))

(define* (lower name
                #:key
                (elixir (default-elixir))
                (elixir-hex (default-elixir-hex))
                (inputs '())
                (native-inputs '())
                (propagated-inputs '())
                (rebar3 (default-rebar3))
                (tests? #t)
                outputs
                source
                system
                target
                #:allow-other-keys #:rest arguments)
  "Return a bag for NAME."
  (let ((private-keywords
         '(#:inputs #:native-inputs
           #:outputs #:system #:target
           #:elixir #:elixir-hex
           #:rebar3 #:erlang))
        (build-inputs
         `(,@(standard-packages)
           ("erlang" ,(lookup-package-input elixir "erlang"))
           ("rebar3" ,rebar3)
           ("elixir" ,elixir)
           ("elixir-hex" ,elixir-hex)
           ,@inputs
           ,@native-inputs)))
  (bag (name name)
       (system system)
       (build-inputs build-inputs)
       (host-inputs (if target inputs '()))
       (outputs outputs)
       (build mix-build)
       (arguments (strip-keyword-arguments private-keywords arguments)))))

(define mix-build-system
  (build-system (name 'mix)
                (description "The standard Mix build system")
                (lower lower)))

;;; mix.scm ends here
