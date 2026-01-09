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

(define-module (guix import mix mix-lock)
  #:use-module (ice-9 peg)
  #:export (mix-lock-string->scm

            entry-git
            entry-hexpm
            entry-name
            git-commit
            git-url
            hexpm-build-system
            hexpm-build-systems
            hexpm-name
            hexpm-version
            mix-lock
            mix-lock-entry))

;;;
;;; PEG parser for ‘mix.lock’.
;;;

(define (mix-lock-string->scm str)
  (peg:tree (search-for-pattern mix-lock str)))

;; Auxiliar peg patterns
(define-peg-pattern numeric-char body
  (range #\0 #\9))

(define-peg-pattern lowercase-char body
  (range #\a #\z))

(define-peg-pattern uppercase-char body
  (range #\A #\Z))

(define-peg-pattern alphabetic-char body
  (or lowercase-char uppercase-char))

(define-peg-pattern alphanumeric-char body
  (or alphabetic-char numeric-char))

(define-peg-pattern space-char body
  (+ (or " " "\t" "\n" "\r")))

(define-peg-pattern opt-space-char body
  (* space-char))

;; string: "string"
(define-peg-pattern string-char body
  (or alphanumeric-char
      space-char
      "_" "." "~" "=" "<" ">" "/" ":" "-"))

(define-peg-pattern string body
  (and (ignore "\"")
       (* string-char)
       (ignore "\"")))

;; atom: :hex or true | false | nil
(define-peg-pattern atom-char body
  (or alphanumeric-char "_" "@" "?" "!"))

(define-peg-pattern atom body
  (or
   (and (ignore ":")
        (+ atom-char))
   "true" "false" "nil"))

;; list: [ ... ]
(define-peg-pattern list body
  (and (ignore "[")
       (* (and
           (ignore (? ", "))
           (or value key-value)))
       (ignore "]")))

;; tuple: { ... }
(define-peg-pattern tuple body
  (and (ignore "{")
       (* (and
           (ignore (? ", "))
           value))
       (ignore "}")
       (ignore (? "\n"))))

;; syntactic sugar for [{key, value} ...]
(define-peg-pattern key-value body
  (ignore
   (and (+ atom-char) ": " value)))

;; value may be string, atom, tuple, list
(define-peg-pattern value body
  (and (ignore opt-space-char)
       (or string atom tuple list)
       (ignore opt-space-char)))

;; ignore several constructions
(define-peg-pattern ignore-string body
  (ignore
   (and "\""
        (* (or string-char
               space-char))
        "\"")))

(define-peg-pattern ignore-comma-space body
  (ignore ", "))

(define-peg-pattern ignore-list body
  (ignore (and "["
               (* (and
                   (? ", ")
                   (or value key-value)))
               "]")))

;; exported symbols
(define-peg-pattern entry-name all
  string)

(define-peg-pattern hexpm-name all
  atom)

(define-peg-pattern hexpm-version all
  string)

(define-peg-pattern hexpm-checksum all
  string)

(define-peg-pattern hexpm-build-system all
  (capture (+ (or alphanumeric-char "_" "-"))))

(define-peg-pattern hexpm-build-systems all
  (and (ignore "[")
       (capture
        (* (and (ignore ":")
                (capture hexpm-build-system)
                (ignore (? ", ")))))
       (ignore "]")))

(define-peg-pattern git-url all
  string)

(define-peg-pattern git-commit all
  string)

(define-peg-pattern entry-hexpm all
  (and
   (ignore ": {:hex, ")
   hexpm-name
   ignore-comma-space
   (capture hexpm-version)
   ignore-comma-space
   ignore-string
   ignore-comma-space
   (capture hexpm-build-systems)
   ignore-comma-space
   ignore-list
   ignore-comma-space
   ignore-string
   ignore-comma-space
   (capture hexpm-checksum)
   (ignore "}")))

(define-peg-pattern entry-git all
  (and
   (ignore ": {:git, ")
   git-url
   ignore-comma-space
   git-commit
   ignore-comma-space
   ignore-list
   (ignore "}")))

(define-peg-pattern mix-lock-entry all
  (and (ignore (? "  "))
       (capture entry-name)
       (or
        entry-hexpm
        entry-git)
       (ignore (? ",\n"))))

;; mix.lock
(define-peg-pattern mix-lock all
  (and (ignore "%{\n")
       (* mix-lock-entry)
       (ignore "}")))
