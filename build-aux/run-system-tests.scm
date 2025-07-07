;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Mathieu Othacehe <othacehe@gnu.org>
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

(use-modules (gnu)
             (gnu tests)
             (gnu packages package-management)
             (guix)
             (guix colors)
             ((guix build utils)
                #:select (delete-file-recursively))
             (guix store)
             (guix packages)
             (guix utils)
             (guix ui)
             (guix build syscalls)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 getopt-long)
             (ice-9 match)
             (ice-9 exceptions)
             (ice-9 pretty-print)
             (ice-9 rdelim)
             (web uri))

;; Version of the script.
(define script-version "0.1")

;; Directory for the system test logs.
(define tmpdir (mkdtemp! "/tmp/system-tests-XXXXXX"))

;; Save the stdout and stderr ports.
(define output-port (duplicate-port (current-output-port) "w0"))
(define error-port (duplicate-port (current-error-port) "w0"))

;; List of all the system tests, sorted alphabetically.
(define all-sorted-tests
  (sort (all-system-tests)
        (lambda (a b)
          (string<? (system-test-name a) (system-test-name b)))))

(define (test->log-file test)
  "Return the name of the log file for the given system TEST."
  (format #f "~a/~a.log" tmpdir (system-test-name test)))

(define (print-failure)
  "Print a failure message."
  (format output-port
          (colorize-string "FAIL" (color RED BOLD)))
  (format output-port "~%"))

(define (print-success)
  "Print a success message."
  (format output-port
          (colorize-string "OK" (color GREEN BOLD)))
  (format output-port "~%"))

(define* (run-system-test test #:key redirect-output?)
  "Run the given system-test and redirect the test output to a file.
Return true if the test is successful and false otherwise."
  (define log-port
    (and redirect-output?
         (open-output-file (test->log-file test))))

  (dynamic-wind
    (lambda ()
      (when redirect-output?
        (redirect-port log-port (current-output-port))
        (redirect-port log-port (current-warning-port))
        (redirect-port log-port (current-error-port))))
    (lambda ()
      (parameterize ((current-build-output-port
                      (if redirect-output?
                          log-port
                          (current-build-output-port))))
        (let ((store (open-connection)))
          (run-with-store store
            (mlet %store-monad
                ((drv (lower-object test)))
              (return
               (guard (c ((store-protocol-error? c)
                          (print-failure)
                          #f))
                 (and (build-derivations store (list drv))
                      (print-success)))))))))
    (lambda ()
      (and redirect-output?
           (close-port log-port)))))

(define* (print-results results #:key keep-logs)
  "Use RESULTS, a list of booleans, to print out the test results.  If
some tests are failing, exit with the error code 1.  Otherwise, exit with
success."
  (let* ((ok-count (count identity results))
         (fail-count (- (length results) ok-count)))
    (format output-port "~%")
    (if keep-logs
        (format output-port "Test results are available in ~a~%" tmpdir)
        (delete-file-recursively tmpdir))
    (if (= fail-count 0)
        (begin
          (format output-port "All system tests are successful.~%")
          (exit 0))
        (begin
          (format output-port "~a failing test~a~%" fail-count
                  (if (> fail-count 1) "s" ""))
          (exit 1)))))

(define* (run-tests tests #:key keep-logs redirect-output?)
  "Run all the given TESTS.  If KEEP-LOGS is set to true, keep the system test
log files."
  (let* ((count (length tests))
         (indexes (iota count)))
    (print-results
     (map (lambda (index test)
            (format output-port "Running ~a (~a/~a): "
                    (system-test-name test)
                    (1+ index) count)
            (force-output output-port)
            (run-system-test test #:redirect-output? redirect-output?))
          indexes tests)
     #:keep-logs keep-logs)))

(define (subset-tests tests subset)
  "Return the subset of TESTS that are also in the given SUBSET list."
  (filter
   (lambda (test)
     (let ((name (system-test-name test)))
       (member name subset)))
   tests))

(define (exclude-tests tests excludes)
  "Return the given TESTS list without the tests that are part of the EXCLUDES
list."
  (filter
   (lambda (test)
     (let ((name (system-test-name test)))
       (not (member name excludes))))
   tests))

(define (show-help)
  (display "Usage: run-system-tests.scm [OPTIONS]
Run the system tests.\n")
  (display " --tests=TEST1,TEST2,...           run only TEST1 and TEST2")
  (newline)
  (display " --exclude-tests=TEST1,TEST2,...   run all tests but TEST1 and TEST2")
  (newline)
  (display " --list-tests                      list all the system tests")
  (newline)
  (display " -n, --no-redirect                 do not redirect the test logs")
  (newline)
  (display " -K, --keep-logs                   keep the test logs")
  (newline)
  (display " -h, --help                        display this help and exit")
  (newline)
  (display " -V, --version                     display verson information and exit")
  (newline))

(define %options
  '((tests                          (value #t))
    (exclude-tests                  (value #t))
    (list-tests                     (value #f))
    (keep-logs    (single-char #\K) (value #f))
    (no-redirect  (single-char #\n) (value #f))
    (help         (single-char #\h) (value #f))
    (version      (single-char #\V) (value #f))))

(define (main . args)
  (define guix-source
    (canonicalize-path
     (string-append (current-source-directory) "/..")))

  ;; Use the local Guix as the current-guix-package so that we do not compute
  ;; the Guix derivation over and over.
  (define guix-local
    (package
      (inherit guix)
      (source (local-file guix-source #:recursive? #t))
      (arguments
     (substitute-keyword-arguments (package-arguments guix)
       ((#:tests? #f #f) #f)))))

  (let* ((opts (getopt-long (command-line) %options))
         (option (cut option-ref opts <> <>))
         (keep-logs (option 'keep-logs #f))
         (no-redirect (option 'no-redirect #f)))
    (cond
     ((option 'help #f)
      (show-help))
     ((option 'version #f)
      (format #t "run-system-tests.scm ~A~%" script-version))
     ((option 'list-tests #f)
      (format #t "System tests:~%~{- ~a~%~}"
              (map (lambda (test)
                     (system-test-name test))
                   all-sorted-tests)))
     (else
      (let* ((subset
              (and=> (option 'tests #f)
                     (cut string-split <> #\,)))
             (excludes
              (and=> (option 'exclude-tests #f)
                     (cut string-split <> #\,)))
             (tests
              (cond
               (excludes (exclude-tests all-sorted-tests excludes))
               (subset (subset-tests all-sorted-tests subset))
               (else all-sorted-tests))))
        (parameterize ((current-guix-package guix-local))
          (run-tests tests
                     #:keep-logs keep-logs
                     #:redirect-output? (not no-redirect))))))))

(apply main (cdr (command-line)))
