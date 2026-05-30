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

(define-module (gnu packages kde-xyz)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg))

(define-public kwin-effects-rounded-corners
  (package
    (name "kwin-effects-rounded-corners")
    (version "0.8.6")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
          (url "https://github.com/matinlotfali/KDE-Rounded-Corners")
          (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03d13vg7qcbw3sc1j25r5dilhyinjr58k3487zc1cn18smp2iydz"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f)) ;requires a running kwin session
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kcmutils
           kconfigwidgets
           ki18n
           kwin
           kwindowsystem
           libepoxy
           libxcb
           qtbase
           wayland))
    (home-page "https://github.com/matinlotfali/KDE-Rounded-Corners")
    (synopsis "Round the corners for your windows in KDE Plasma")
    (description
     "This package provides a KWin effect for KDE Plasma that rounds the
corners of your windows and adds an outline around them without much affecting
the performance.")
    (license license:gpl3+)))

(define-public plasma-applet-chatai
  (package
    (name "plasma-applet-chatai")
    (version "2.2.3")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
          (url "https://github.com/DenysMb/ChatAI-Plasmoid")
          (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wbqwngwczzdivrz5yp6jwwskskh6fhsz9gld51q0qwg98zfpg5q"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("." "share/plasma/plasmoids/ChatAI-Plasmoid"
           #:include-regexp ("metadata\\.json" "contents/")))))
    (propagated-inputs
     (list kdeclarative
           kcmutils
           kirigami
           libplasma
           knotifications
           qtbase
           qtdeclarative
           qtwebengine))
    (home-page "https://store.kde.org/p/2163340")
    (synopsis "Range of chatbots for KDE Plasma")
    (description
     "This package provides a widget for KDE Plasma that provides a range of
chatbots such as @uref{https://duck.ai/, Duck.ai}.")
    (license license:gpl2+)))
