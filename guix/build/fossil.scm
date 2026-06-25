;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025-2026 Nguyễn Gia Phong <cnx@loang.net>
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
;;;
;;; Commentary:
;;;
;;; This is the build-side support code of (guix fossil-download).
;;; It allows a source tarball to be generated from a Fossil repository
;;; at a specific check-in.
;;;
;;; Code:

(define-module (guix build fossil)
  #:use-module ((guix build download)
                #:select (%download-methods url-fetch))
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (web uri)
  #:export (fossil-fetch fossil-fetch-url))

(define* (fossil-fetch uri check-in file #:key (fossil-command "fossil"))
  "Fetch CHECK-IN from URI into a tarball FILE using @command{fossil clone}
followed by @command{fossil tarball}.  CHECK-IN must be a valid
Fossil check-in name.  Return #t on success, #f otherwise."
  (setenv "FOSSIL_HOME" "/tmp")
  (let ((name (basename (strip-store-file-name file) ".tar.gz")))
    (guard (c ((invoke-error? c)
               (report-invoke-error c)
               #f))
      (invoke fossil-command
        "tarball" check-in file "--name" name "-R"
        (case (uri-scheme (string->uri-reference uri))
          ((file http https)            ;clone the repository first
           (match-let ((repository (simple-format #f "/tmp/~a.fossil" name))
                       ((input . output) (pipe)))
             ;; Trust the TLS certificate of the server,
             ;; since we'll later verify the tarball's checksum.
             (display "y" output)
             (close-port output)
             (with-input-from-port input
               (cut invoke fossil-command "clone"
                    "--no-open" "--once" uri repository))
             (close-port input)
             repository))
          ((ssh)                        ;TODO: authentication for SSH
           (let ((message (string-append "fetching a Fossil repository"
                                         " through SSH is not supported: "
                                         uri)))
             (raise (condition (&message (message message))))))
          ((#f) uri))))))               ;local file

(define* (fossil-fetch-url uri check-in file
                           #:key
                           (download-methods '())
                           (disarchive-mirrors '())
                           (hashes '()))
  "Fetch CHECK-IN from URI into a tarball FILE using url-fetch
with the specified DOWNLOAD-METHODS.  CHECK-IN must be a valid
Fossil check-in name.  Return #t on success, #f otherwise.

DISARCHIVE-MIRRORS and HASHES are only used when 'disarchive
is in DOWNLOAD_METHODS.  DISARCHIVE-MIRRORS must be a list of strings,
and HASHES must be a list of symbol/bytevector pair."
  (and (memq (uri-scheme (string->uri-reference uri))
             '(http https))
       (parameterize ((%download-methods download-methods))
         (url-fetch (simple-format #f "~a/tarball/~a/~a"
                      uri check-in (strip-store-file-name file))
                    file
                    #:verify-certificate? #f
                    #:disarchive-mirrors disarchive-mirrors
                    #:hashes hashes))))
