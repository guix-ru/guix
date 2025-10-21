;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Maxim Cournoyer <maxim@guixotic.coop>
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

(define-module (guix build io)
  #:use-module (guix build syscalls)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs enums)
  #:use-module (rnrs io ports)
  #:use-module (system foreign)
  #:export (file->bytevector)
  ;; For convenience.
  #:re-export (protection
               protection-set
               mmap-flag
               mmap-flag-set))

;;;
;;; Memory mapped files.
;;;

(define* (file->bytevector file #:key
                           (protections (protection-set read))
                           (flags (if (enum-set-member? (protection write)
                                                        protections)
                                      (mmap-flag-set shared)
                                      (mmap-flag-set private)))
                           (offset 0))
  "Return a bytevector object that is backed by a memory mapped FILE.  This
avoids eagerly copying the full file contents into memory, instead letting the
kernel lazily page it in on demand.  The underlying memory map is
automatically unmapped when the bytevector is no longer referenced.  Refer to
the documentation of `mmap' for details about the accepted arguments."
  (let* ((mode (format #f "rb~:[~;+~]"
                       (and (enum-set-member? (protection write) protections)
                            (enum-set-member? (mmap-flag shared) flags))))
         (port (open-file file mode)))
    (call-with-port port
      (lambda (port)
        (mmap (fileno port) (- (stat:size (stat file)) offset)
              #:protections protections #:flags flags #:offset offset)))))
