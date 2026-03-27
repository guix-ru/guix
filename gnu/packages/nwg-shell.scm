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
  #:use-module (guix build-system pyproject)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages python-build))

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

(define-public nwg-hello
  (package
    (name "nwg-hello")
    (version "0.4.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nwg-piotr/nwg-hello")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0di77z7sblphk755x96wrl1khfms57g7kvaqril87nxfwh9595aw"))
       (modules '((guix build utils)))
       ;; Replace systemd commands with elogind commands.
       (snippet
        '(substitute* '("nwg_hello/main.py"
                        "nwg-hello-default.json"
                        "sway-config")
           (("\"systemctl (-i |)") "\"loginctl ")
           (("'systemctl (-i |)") "'loginctl ")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;no tests exist in source
      #:modules '((guix build pyproject-build-system)
                  (guix build utils)
                  (srfi srfi-26))                     ;for cute
      #:phases
      #~(modify-phases %standard-phases
          (delete 'sanity-check)
          (add-after 'unpack 'patch-paths
            (lambda _
              ;; Set file name to the default config and stylesheet.
              (substitute* "nwg_hello/main.py"
                (("\\/etc\\/nwg-hello\\/nwg-hello-default\\.(css|json)" all)
                 (string-append #$output all)))
              ;; Set file name to the default icons.
              (substitute* "nwg_hello/ui.py"
                (("\\/usr(.*\\{icon_name\\}-default\\.svg)" _ suffix)
                 (string-append #$output suffix)))
              ;; Look for sessions, icons, backgrounds and commands in the
              ;; system profile.
              (substitute* '("nwg_hello/main.py"
                             "nwg_hello/tools.py"
                             "nwg_hello/ui.py"
                             "nwg-hello-default.css"
                             "nwg-hello-default.json")
                (("\\/usr") "/run/current-system/profile"))
              (substitute* '("nwg-hello-default.css"
                             "nwg-hello-default.json")
                (("\\/usr") #$output))))
          (add-after 'install 'install-data
            (lambda _
              (mkdir-p (string-append #$output "/share"))
              (copy-recursively "img"
                                (string-append #$output "/share/nwg-hello"))
              (install-file "nwg.jpg"
                            (string-append #$output "/share/nwg-hello"))
              (for-each (cute install-file <>
                              (string-append #$output "/etc/nwg-hello"))
                        '("nwg-hello-default.json"
                          "nwg-hello-default.css"
                          "hyprland.conf"
                          "sway-config"))
              (install-file "README.md"
                            (string-append #$output "/share/doc/nwg-hello"))))
          (add-after 'create-entrypoints 'wrap-program
            (lambda _
              (wrap-program (string-append #$output "/bin/nwg-hello")
                `("PATH" prefix
                  (,(dirname (which "getent"))))
                `("GI_TYPELIB_PATH" =
                  (,(getenv "GI_TYPELIB_PATH")))))))))
    (native-inputs
     (list gobject-introspection
           python-setuptools))
    (inputs
     (list bash-minimal
           gtk+
           gtk-layer-shell
           python-pygobject))
    (home-page "https://nwg-piotr.github.io/nwg-shell/nwg-hello")
    (synopsis "GTK3-based greeter for greetd")
    (description
     "nwg-hello is a GTK3-based greeter for the @command{greetd} daemon, written
in Python.  It is meant to work under a Wayland compositor, like sway or
Hyprland.

This application is a part of the nwg-shell project.")
    (license license:expat)))
