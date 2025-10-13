;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2019 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2021 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2024 Daniel Khodabakhsh <d.khodabakhsh@gmail.com>
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

(define-module (guix build-system node)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:export (%node-build-system-modules
            node-build
            node-build-system))

(define %node-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build node-build-system)
    (json)
    (json builder)
    (json parser)
    (json record)
    ,@%default-gnu-imported-modules))

(define (default-node)
  "Return the default Node package, resolved lazily."
  (@* (gnu packages node) node-lts))

(define (default-guile-json)
  "Return the default guile-json package, resolved lazily."
  (@* (gnu packages guile) guile-json-4))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (node (default-node))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:node #:inputs #:native-inputs))

  (and (not target)                    ;XXX: no cross-compilation
       (bag
         (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs
                        ;; Keep the standard inputs of 'gnu-build-system'.
                        ,@(standard-packages)))
         (build-inputs `(("node" ,node)
                         ;; Many packages with native addons need
                         ;; libuv headers. The libuv version must
                         ;; be exactly the same as for the node
                         ;; package we are adding implicitly,
                         ;; so we take care of adding libuv, too.
                         ("libuv" ,@(assoc-ref (package-inputs node) "libuv"))
                         ,@native-inputs))
         (outputs outputs)
         (build node-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (node-build name inputs
                     #:key
                     source
                     (npm-flags ''())
                     (test-target "test")
                     (tests? #t)
                     (phases '%standard-phases)
                     (outputs '("out"))
                     (search-paths '())
                     (system (%current-system))
                     (guile #f)
                     (guile-json (default-guile-json))
                     (imported-modules %node-build-system-modules)
                     (modules '((guix build node-build-system)
                                (guix build utils))))
  "Build SOURCE using NODE and INPUTS."
  (define builder
    (with-extensions (list guile-json)
      (with-imported-modules imported-modules
        #~(begin
            (use-modules #$@(sexp->gexp modules))
            (node-build #:name #$name
                        #:source #+source
                        #:system #$system
                        #:npm-flags #$npm-flags
                        #:test-target #$test-target
                        #:tests? #$tests?
                        #:phases #$phases
                        #:outputs #$(outputs->gexp outputs)
                        #:search-paths '#$(sexp->gexp
                                           (map search-path-specification->sexp
                                                search-paths))
                        #:inputs #$(input-tuples->gexp inputs))))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:guile-for-build guile)))

(define node-build-system
  (build-system
    (name 'node)
    (description "The Node build system")
    (lower lower)))
