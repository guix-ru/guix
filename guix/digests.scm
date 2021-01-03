;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix digests)
  #:use-module (gcrypt hash)
  #:use-module (guix base32)
  #:use-module ((guix store) #:select (%store-prefix))
  #:use-module (guix serialization)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:export (digest?
            digest-type
            digest-size
            digest-content

            digest-entry?
            digest-entry-name
            digest-entry-value

            store-deduplication-link
            file-tree-digest
            file-digest
            restore-digest

            digest->sexp
            sexp->digest))

;;; Commentary:
;;;
;;; This module implements "digests", which can be thought of as
;;; content-addressed archives.  A digest describes a directory (recursively),
;;; symlink, or regular file; in lieu of actual file contents, it contains the
;;; hash of those contents.
;;;
;;; Code:

;; Digest of a file.
(define-record-type <digest>
  (digest type size content)
  digest?
  (type    digest-type)                 ;'regular | 'executable | ...
  (size    digest-size)                 ;integer
  (content digest-content))             ;hash | symlink target | entries

;; Directory entry for a digest with type = 'directory.
(define-record-type <digest-entry>
  (digest-entry name value)
  digest-entry?
  (name    digest-entry-name)
  (value   digest-entry-value))

(define* (file-tree-digest file
                           #:key
                           file-type+size
                           file-port
                           symlink-target
                           directory-entries
                           (postprocess-entries
                            filter/sort-directory-entries)
                           (hash-algorithm (hash-algorithm sha256)))
  "Return a digest of FILE.  The calling convention is the same as for
'write-file-tree'."
  (let dump ((file file))
    (define-values (type size)
      (file-type+size file))

    (define (nar-hash)
      (let ((port get-hash (open-hash-port hash-algorithm)))
        (write-file-tree file port
                         #:file-type+size (lambda _ (values type size))
                         #:file-port file-port)
        (force-output port)
        (let ((hash (get-hash)))
          (close-port port)
          hash)))

    (match type
      ((or 'regular 'executable)
       (digest type size
               (list (hash-algorithm-name hash-algorithm) (nar-hash))))
      ('symlink
       (digest 'symlink 0 (symlink-target file)))
      ('directory
       (let ((entries (postprocess-entries (directory-entries file))))
         (digest 'directory 0
                 (map (lambda (entry)
                        (digest-entry entry
                                      (dump (string-append file "/" entry))))
                      entries)))))))

(define* (file-digest file
                      #:key (select? (const #t)))
  "Return a digest for FILE, recursing into it and its sub-directories and
discarding files that do not pass SELECT?."
  (file-tree-digest file
                    ;; FIXME: deduplicate arguments
                    #:file-type+size
                    (lambda (file)
                      (let* ((stat (lstat file))
                             (size (stat:size stat)))
                        (case (stat:type stat)
                          ((directory)
                           (values 'directory size))
                          ((regular)
                           (values (if (zero? (logand (stat:mode stat)
                                                      #o100))
                                       'regular
                                       'executable)
                                   size))
                          (else
                           (values (stat:type stat) size)))))
                    #:file-port (cut open-file <> "r0b")
                    #:symlink-target readlink

                    #:directory-entries
                    (lambda (directory)
                      ;; 'scandir' defaults to 'string-locale<?' to sort files,
                      ;; but this happens to be case-insensitive (at least in
                      ;; 'en_US' locale on libc 2.18.)  Conversely, we want
                      ;; files to be sorted in a case-sensitive fashion.
                      (define basenames
                        (scandir directory (negate (cut member <> '("." "..")))
                                 string<?))

                      (filter-map (lambda (base)
                                    (let ((file (string-append directory
                                                               "/" base)))
                                      (and (select? file (lstat file))
                                           base)))
                                  basenames))))

(define (store-deduplication-link hash)
  "Return the file name in the content-addressed store for HASH, a nar hash."
  (string-append (%store-prefix) "/.links/"
                 (bytevector->nix-base32-string hash)))

(define (copy-file-from-store digest target)
  "Attempt to copy DIGEST from the content-addressed store into TARGET.
Return #t on success, and #f if DIGEST could not be found."
  (match (digest-content digest)
    (('sha256 hash)
     (let* ((address (store-deduplication-link hash))
            (perms   (match (digest-type digest)
                       ('executable #o555)
                       ('regular    #O444)))
            (stat    (stat address #f)))
       (and stat (= (stat:perms stat) perms)
            (= (stat:size stat) (digest-size digest))
            (begin
            (catch 'system-error
              (lambda ()
                (link address target))
              (lambda args
                (if (= EXDEV (system-error-errno args))
                    (begin
                      (copy-file address target)
                      (chmod target perms)
                      (utime target 1 1 0 0)
                      #t))))))))
    (_
     #f)))

(define* (restore-digest digest target
                         #:key
                         (copy-file copy-file-from-store))
  "Restore DIGEST into directory TARGET.  Copy files from the local
content-addressed store using COPY-FILE.  Return the list of target
directory/digest pairs for all the digests for which 'copy-file' returned
false."
  (let loop ((target  target)
             (digest  digest)
             (missing '()))
    (match digest
      (($ <digest> 'directory _ (entries ...))
       (mkdir target)
       (let ((missing* (fold (lambda (entry missing)
                               (match entry
                                 (($ <digest-entry> name value)
                                  (loop (string-append target "/" name)
                                        value missing))))
                             missing
                             entries)))
         ;; If there are were missing files among ENTRIES, leave TARGET
         ;; untouched so that the caller can eventually create files
         ;; therein.
         (unless (eq? missing missing*)
           (chmod target #o555)
           (utime target 1 1 0 0))
         missing*))
      (($ <digest> (or 'regular 'executable))
       (if (copy-file digest target)
           missing
           (cons (cons target digest) missing)))
      (($ <digest> 'symlink _ source)
       (symlink source target)
       (utime target 1 1 0 0 AT_SYMLINK_NOFOLLOW)
       missing))))

(define (digest->sexp digest)
  "Return an sexp serialization of DIGEST."
  (define (->sexp digest)
    (match digest
      (($ <digest> 'directory _ entries)
       `(d ,@(map (match-lambda
                    (($ <digest-entry> name digest)
                     `(,name ,(->sexp digest))))
                  entries)))
      (($ <digest> (and type (or 'executable 'regular)) size
                   (algorithm hash))
       `(,(if (eq? type 'executable) 'x 'f) ,size
         (,algorithm ,(bytevector->nix-base32-string hash))))
      (($ <digest> 'symlink _ target)
       `(l ,target))))

  `(digest (version 0)
           ,(->sexp digest)))

(define (sexp->digest sexp)
  "Return a digest deserialized from SEXP."
  (define (->digest sexp)
    (match sexp
      (('x size (algorithm hash) _ ...)
       (digest 'executable size (list algorithm hash)))
      (('f size (algorithm hash) _ ...)
       (digest 'regular size
               (list algorithm (nix-base32-string->bytevector hash))))
      (('d entries ...)
       (digest 'directory 0
               (map (match-lambda
                      ((name digest)
                       (digest-entry name (->digest digest))))
                    entries)))
      (('l target)
       (digest 'symlink 0 target))))

  (match sexp
    (('digest ('version 0) sexp)
     (->digest sexp))))
