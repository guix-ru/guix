;;; GNU Guix --- Functional package management for GNU
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

(define-module (guix build-system typst)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (srfi srfi-1)
  #:export (%typst-build-system-modules
            default-typst
            typst-build
            typst-build-system))

(define (default-typst)
  "Return the default Typst package, resolved lazily."
  (@* (gnu packages typst) typst))

(define %typst-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build typst-build-system)
    (guix build toml)
    ,@%copy-build-system-modules))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (namespace "preview")
                build-backend test-backend
                (tests? (not target))
                (test-flags '())
                (typst (default-typst))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:typst #:inputs #:native-inputs))

  (and (not target)                               ;XXX: no cross-compilation
       (bag
         (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs

                        ;; Keep the standard inputs of 'gnu-build-system'.
                        ,@(standard-packages)))
         (build-inputs `(("typst" ,typst)
                         ,@native-inputs))
         (outputs outputs)
         (build typst-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (typst-build name inputs
                      #:key source
                      (tests? #t)
                      (build-backend #f)
                      (test-backend #f)
                      (test-flags ''())
                      (phases '%standard-phases)
                      (outputs '("out"))
                      (search-paths '())
                      (system (%current-system))
                      (guile #f)
                      (imported-modules %typst-build-system-modules)
                      (modules '((guix build typst-build-system)
                                 (guix build utils)))
                      allowed-references
                      disallowed-references)
  "Build SOURCE using TYPST, and with INPUTS."
  (define build
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))

          #$(with-build-variables inputs outputs
              #~(typst-build
                 #:name #$name
                 #:source #+source
                 #:system #$system
                 #:build-backend #$build-backend
                 #:test-backend #$test-backend
                 #:test-flags #$test-flags
                 #:tests? #$tests?
                 #:phases #$(if (pair? phases)
                                (sexp->gexp phases)
                                phases)
                 #:outputs %outputs
                 #:search-paths '#$(sexp->gexp
                                    (map search-path-specification->sexp
                                         search-paths))
                 #:inputs %build-inputs)))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name build
                      #:system system
                      #:graft? #f                 ;consistent with 'gnu-build'
                      #:target #f
                      #:guile-for-build guile
                      #:allowed-references allowed-references
                      #:disallowed-references disallowed-references)))

(define typst-build-system
  (build-system
    (name 'typst)
    (description "Build system for typst packages")
    (lower lower)))
