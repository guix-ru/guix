;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Magali Lemes <magalilemes00@gmail.com>
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

(define-module (guix scripts git log)
  #:use-module (git)
  #:use-module ((guix channels)
                #:select (%default-guix-channel
                          channel-url))
  #:use-module ((guix git) #:select (url-cache-directory))
  #:use-module (guix scripts)
  #:use-module (guix ui)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:export (guix-git-log))


(define %options
  (list (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))

        (option '("checkout-path") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'checkout-path? #t result)))
        (option '("oneline") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'oneline? #t result)))))

(define %default-options
  '())

(define (show-help)
  (display (G_ "Usage: guix git log [OPTIONS...]
Show Guix commit logs.\n"))
  (display (G_ "
      --checkout-path    show checkout path"))
  (display (G_ "
      --oneline          show short hash and summary of five first commits"))
  (display (G_ "
  -h, --help             display this help and exit"))
  (newline)
  (show-bug-report-information))

(define (show-checkout-path)
  (display (url-cache-directory (channel-url %default-guix-channel)))
  (newline))

(define commit-short-id
  (compose (cut string-take <> 7) oid->string commit-id))

(define (show-commit commit)
    (format #t (G_ "~a ~a~%") (commit-short-id commit) (commit-summary commit)))

;; currently showing 5 latest commits
(define (get-commits path)
  (let* ((repository (repository-open path))
         (latest-commit (commit-lookup repository (reference-target (repository-head repository)))))
    (for-each show-commit (take
                           (let loop ((commit latest-commit)
                                      (res (list latest-commit)))
                             (match (commit-parents commit)
                                    (() (reverse res))
                                    ((head . tail)
                                     (loop head (cons head res)))))
                           5))))

(define (guix-git-log . args)
  (define options
    (parse-command-line args %options (list %default-options)))

  (let ((checkout-path? (assoc-ref options 'checkout-path?))
        (oneline?       (assoc-ref options 'oneline?)))
    (with-error-handling
      (cond
       (checkout-path?
        (show-checkout-path))
       (oneline?
        (let ((cache (url-cache-directory (channel-url %default-guix-channel))))
          (get-commits cache)))))))
