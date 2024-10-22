;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2024 Greg Hogan <code@greghogan.com>
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

(define-module (guix build cmake-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-34)
  #:export (%standard-phases
            cmake-build))

;; Commentary:
;;
;; Builder-side code of the standard cmake build procedure.
;;
;; Code:

(define* (configure #:key outputs (configure-flags '()) (out-of-source? #t)
                    build-type target generator
                    #:allow-other-keys)
  "Configure the given package."
  (let* ((out        (assoc-ref outputs "out"))
         (abs-srcdir (getcwd))
         (srcdir     (if out-of-source?
                         (string-append "../" (basename abs-srcdir))
                         ".")))
    (format #t "source directory: ~s (relative from build: ~s)~%"
            abs-srcdir srcdir)
    (when out-of-source?
      (mkdir "../build")
      (chdir "../build"))
    (format #t "build directory: ~s~%" (getcwd))

    (let ((args `(,srcdir
                  ,@(if generator
                        (list (string-append "-G" generator))
                        '())
                  ,@(if build-type
                        (list (string-append "-DCMAKE_BUILD_TYPE="
                                             build-type))
                        '())
                  ,(string-append "-DCMAKE_INSTALL_PREFIX=" out)
                  ;; ensure that the libraries are installed into /lib
                  "-DCMAKE_INSTALL_LIBDIR=lib"
                  ;; add input libraries to rpath
                  "-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE"
                  ;; add (other) libraries of the project itself to rpath
                  ,(string-append "-DCMAKE_INSTALL_RPATH=" out "/lib")
                  ;; enable verbose output from builds
                  "-DCMAKE_VERBOSE_MAKEFILE=ON"
                  ,@configure-flags)))
      (format #t "running 'cmake' with arguments ~s~%" args)
      (apply invoke "cmake" args))))

(define %test-suite-log-regexp
  ;; Name of test suite log files as commonly found in CMake.
  "^LastTest\\.log$")

(define* (check #:key (tests? #t) (parallel-tests? #t)
                (test-suite-log-regexp %test-suite-log-regexp)
                #:allow-other-keys)
  (if tests?
      (guard (c ((invoke-error? c)
                 ;; Dump the test suite log to facilitate debugging.
                 (display "\nTest suite failed, dumping logs.\n"
                          (current-error-port))
                 (gnu:dump-file-contents "." test-suite-log-regexp)
                 (raise c)))
        (apply invoke "ctest" "--output-on-failure"
               `(,@(if parallel-tests?
                       `("-j" ,(number->string (parallel-job-count)))
                       ;; When unset CMake defers to the build system.
                       '("-j" "1")))))
      (format #t "test suite not run~%")))

(define %standard-phases
  ;; Everything is as with the GNU Build System except for the `configure'
  ;; and 'check' phases.
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (replace 'check check)
    (replace 'configure configure)))

(define* (cmake-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; cmake-build-system.scm ends here
