;;; GNU Guix --- Functional package management for GNU
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

(define-module (guix scripts import mix)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix read-print)
  #:use-module (guix scripts)
  #:use-module (guix import mix)
  #:use-module (guix scripts import)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:export (guix-import-mix))


;;;
;;; Command-line options.
;;;

(define %default-options
  '())

(define (show-help)
  (display (G_ "Usage: guix import mix PACKAGE-NAME
Import package dependencies from its 'mix.lock'.\n"))
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (display (G_ "
  -f, --lockfile=FILE    import dependencies from FILE, a 'mix.lock' file"))
  (newline)
  (show-bug-report-information))

(define %options
  ;; Specification of the command-line options.
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix import mix")))
         (option '(#\f "lockfile") #f #t
                 (lambda (opt name arg result)
                   (if (file-exists? arg)
                       (alist-cons 'lockfile arg result)
                       (leave (G_ "file '~a' does not exist~%") arg))))
         %standard-import-options))


;;;
;;; Entry point.
;;;

(define (guix-import-mix . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (parse-command-line args %options (list %default-options)
                        #:build-options? #f))

  (let* ((opts (parse-options))
         (lockfile (assoc-ref opts 'lockfile))
         (file-to-insert (assoc-ref opts 'file-to-insert))
         (args (filter-map (match-lambda
                             (('argument . value)
                              value)
                             (_ #f))
                           (reverse opts))))
    (match args
      ((spec)
       (define-values (name version)
         (package-name->name+version spec))

       (if lockfile
           (let ((source-expressions
                  _
                  (mix-lock->expressions lockfile name)))
             (when file-to-insert
               (let* ((source-expressions
                       mix-inputs-entry
                       (mix-lock->expressions lockfile name))
                      (term (first mix-inputs-entry))
                      (mix-inputs
                       `(define-mix-inputs lookup-mix-inputs
                          ,@(sort
                             (cons mix-inputs-entry
                                   (extract-mix-inputs
                                    file-to-insert #:exclude term))
                             (lambda (a b)
                               (string< (symbol->string (first a))
                                        (symbol->string (first b)))))))
                      (_
                       (and=> (find-mix-inputs-location file-to-insert)
                              delete-expression))
                      (port (open-file file-to-insert "a")))
                 (pretty-print-with-comments port mix-inputs)
                 (newline port)
                 (close-port port)))
             source-expressions)))
      (()
       (leave (G_ "too few arguments~%")))
      ((many ...)
       (leave (G_ "too many arguments~%"))))))
