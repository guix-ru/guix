;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Lars-Dominik Braun <lars@6xq.net>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
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

(define-module (guix build-system pyproject)
  #:use-module ((gnu packages) #:select (search-auxiliary-file))
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (srfi srfi-1)
  #:export (%pyproject-build-system-modules
            default-python
            default-sanity-check.py
            pyproject-build
            pyproject-build-system
            pyproject-guile-json))

;; Commentary:
;;
;; Standard build procedure for Python packages using 'pyproject.toml'.
;; This is implemented as an extension of 'python-build-system'.
;;
;; Code:

(define %pyproject-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build pyproject-build-system)
    (guix build toml)
    ,@%python-build-system-modules))

(define (default-python)
  "Return the default Python package, resolved lazily."
  ;; We are using python-sans-pip-wrapper, because it does not contain
  ;; setuptools. This allows us to skip the dependency on setuptools for
  ;; packages which don’t need it. And it allows us to more easily swap
  ;; out setuptools if a different version is required.
  ;; Using python-toolchain here might cause dependency cycles.
  (@* (gnu packages python) python-sans-pip-wrapper))

(define (pyproject-guile-json)
  "Return the default guile-json package, resolved lazily."
  (@* (gnu packages guile) guile-json-4))

;; TODO: On the next iteration of python-team, migrate the sanity-check to
;; importlib_metadata instead of setuptools.
(define (default-sanity-check.py)
  (local-file (search-auxiliary-file "python/sanity-check.py")))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (python (default-python))
                (sanity-check.py (default-sanity-check.py))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:python #:inputs #:native-inputs #:sanity-check.py))

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
         (build-inputs `(("python" ,python)
                         ("sanity-check.py" ,sanity-check.py)
                         ,@native-inputs))
         (outputs (append outputs '(wheel)))
         (build pyproject-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (pyproject-build name inputs
                          #:key source
                          (tests? #t)
                          (configure-flags ''())
                          (backend-path #f)
                          (build-backend #f)
                          (test-backend #f)
                          (test-flags ''())
                          (phases '%standard-phases)
                          (outputs '("out" "wheel"))
                          (search-paths '())
                          (system (%current-system))
                          (guile #f)
                          (guile-json (pyproject-guile-json))
                          (imported-modules %pyproject-build-system-modules)
                          (modules '((guix build pyproject-build-system)
                                     (guix build utils)))
                          allowed-references
                          disallowed-references)
  "Build SOURCE using PYTHON, and with INPUTS."
  (define build
    (with-extensions (list guile-json)
      (with-imported-modules imported-modules
        #~(begin
            (use-modules #$@(sexp->gexp modules))

            #$(with-build-variables inputs outputs
                #~(pyproject-build
                   #:name #$name
                   #:source #+source
                   #:configure-flags #$configure-flags
                   #:system #$system
                   #:backend-path #$backend-path
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
                   #:inputs %build-inputs))))))


  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name build
                      #:system system
                      #:graft? #f                 ;consistent with 'gnu-build'
                      #:target #f
                      #:guile-for-build guile
                      #:allowed-references allowed-references
                      #:disallowed-references disallowed-references)))

(define pyproject-build-system
  (build-system
    (name 'pyproject)
    (description "The PEP517-compliant Python build system")
    (lower lower)))

;;; pyproject.scm ends here
