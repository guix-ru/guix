;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Ekaitz Zarraga <ekaitz@elenq.tech>
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

(define-module (guix build zig-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (guix build zig-utils)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            zig-build
            zig-source-install-path))

;; Interesting guide here:
;; https://github.com/riverwm/river/blob/master/PACKAGING.md

(define (zig-source-install-path output)
  (string-append output "/src/zig/" (strip-store-file-name output)))

(define (zig-input-install-path input)
  (zig-source-install-path
   (dirname (dirname (dirname (canonicalize-path input))))))

(define (zig-arguments)
  (define version-major+minor
    (let* ((port (open-input-pipe "zig version"))
           (str  (read-line port)))
      (close-pipe port)
      (take (string-split str #\.) 2)))
  (define (version>=? a b-major b-minor)
    (let ((a-major (string->number (first a)))
          (a-minor (string->number (second a))))
      (or (> a-major b-major)
          (and (= a-major b-major)
               (>= a-minor b-minor)))))
  `(("parallel-jobs" .
     ,(lambda (jobs)
        (cond
         ((version>=? version-major+minor 0 11)
          (list (string-append "-j" (number->string jobs))))
         (else '()))))
    ("release-type" .
     ,(lambda (type)
        (cond
         ((version>=? version-major+minor 0 11)
          (list (string-append "-Doptimize=Release" (string-capitalize type))))
         (else
          (list (string-append "-Drelease-" type))))))))

;; `zig fetch --name=NAME` overwrites dependency NAME in build.zig.zon.
(define* (unpack-dependencies #:key (skip-build? #f) #:allow-other-keys)
  "Extract Zig dependencies from build.zig.zon, search them from packages
within GUIX_ZIG_PACKAGE_PATH and unpack.  Note that this phase asserts
dependency names start with \"zig-\"."
  (define (extract-zig-dependency line)
    (let* ((pattern1 (string-match "\\.@\"(zig-.*)\" *=" line))
           (pattern2 (string-match "\\.(zig-.*) *=" line))
           (matched (and=> (or pattern1 pattern2)
                           (cut vector-ref <> 2)))
           (extract-line
            (match-lambda
              ((start . end)
               (substring line start end)))))
      (and=> matched extract-line)))
  (define zig-dependencies
    (if (and (file-exists? "build.zig.zon")
             (not skip-build?))
        (let* ((port (open-file "build.zig.zon" "r" #:encoding "utf-8"))
               (result
                (let loop ((line (read-line port))
                           (lines '()))
                  (if (eof-object? line)
                      lines
                      (loop (read-line port)
                            (or (and=> (extract-zig-dependency line)
                                       (cut cons <> lines))
                                lines))))))
          (close-port port)
          result)
        '()))
  (define (zig-inputs)
    (append-map
     (lambda (directory)
       (map (lambda (input-name)
              (cons input-name
                    (string-append directory "/" input-name)))
            (scandir directory (negate (cut member <> '("." ".."))))))
     (or (and=> (getenv "GUIX_ZIG_PACKAGE_PATH")
                (cut string-split <> #\:))
         '())))
  (for-each
   (lambda (dependency-name)
     (let ((pattern (string-append "^" dependency-name "[-.][0-9]")))
       (for-each
        (match-lambda
          ((input-name . input-path)
           (when (string-match pattern input-name)
             (let ((call `("zig" "fetch" ,(zig-input-install-path input-path)
                           ,(string-append "--save=" dependency-name))))
               (format #t "running: ~s~%" call)
               (apply invoke call)))))
        (zig-inputs))))
   zig-dependencies))

(define* (build #:key
                zig-build-flags
                zig-build-target
                ;; "safe", "fast" or "small", empty for a "debug" build.
                zig-release-type
                parallel-build?
                skip-build?
                #:allow-other-keys)
  "Build a given Zig package."
  (when (not skip-build?)
    (setenv "DESTDIR" "out")
    (let* ((arguments (zig-arguments))
           (call `("zig" "build"
                   "--prefix"             ""            ;; Don't add /usr
                   "--prefix-lib-dir"     "lib"
                   "--prefix-exe-dir"     "bin"
                   "--prefix-include-dir" "include"
                   ,(string-append "-Dtarget=" (zig-target zig-build-target))
                   ,@(if parallel-build?
                         ((assoc-ref arguments "parallel-jobs")
                          (parallel-job-count))
                         ((assoc-ref arguments "parallel-jobs")
                          1))
                   ,@(if zig-release-type
                         ((assoc-ref arguments "release-type")
                          zig-release-type)
                         '())
                   ,@zig-build-flags)))
      (format #t "running: ~s~%" call)
      (apply invoke call))))

(define* (check #:key tests?
                zig-test-flags
                target
                parallel-tests?
                #:allow-other-keys)
  "Run all the tests"
  (when (and tests? (not target))
    (let ((old-destdir (getenv "DESTDIR")))
      (setenv "DESTDIR" "test-out") ;; Avoid colisions with the build output
      (let* ((arguments (zig-arguments))
             (call `("zig" "build" "test"
                     ,@(if parallel-tests?
                           ((assoc-ref arguments "parallel-jobs")
                            (parallel-job-count))
                           ((assoc-ref arguments "parallel-jobs")
                            1))
                     ,@zig-test-flags)))
        (format #t "running: ~s~%" call)
        (apply invoke call))
      (if old-destdir
        (setenv "DESTDIR" old-destdir)
        (unsetenv "DESTDIR")))))

(define* (install #:key outputs install-source? #:allow-other-keys)
  "Install a given Zig package."
  (let* ((out (assoc-ref outputs "out"))
         (source-install-path (zig-source-install-path out)))
    (when (file-exists? "out")
      (copy-recursively "out" out)
      (delete-file-recursively "out"))
    (when install-source?
      (mkdir-p source-install-path)
      (copy-recursively "." source-install-path))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (replace 'configure zig-configure)
    (add-after 'configure 'unpack-dependencies unpack-dependencies)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)))


(define* (zig-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given Zig package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
