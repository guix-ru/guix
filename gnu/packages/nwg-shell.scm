;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Sughosha <sughosha@disroot.org>
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

(define-module (gnu packages nwg-shell)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages))

(define-public nwg-shell-wallpapers
  (package
    (name "nwg-shell-wallpapers")
    (version "1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nwg-piotr/nwg-shell-wallpapers")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b1k4fnkgzsnq64ziv5bhp930dplacpad6vzqf3w6pabp9xp3skv"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~`(("wallpapers" "share/backgrounds/nwg-shell")
          ("README.md" "share/doc/nwg-shell-wallpapers"))))
    (home-page "https://github.com/nwg-piotr/nwg-shell-wallpapers")
    (synopsis "Selection of wallpapers contributed to the nwg-shell project")
    (description
     "nwg-shell-wallpapers is a selection of wallpapers contributed to the
nwg-shell project.")
    (license license:cc0)))
