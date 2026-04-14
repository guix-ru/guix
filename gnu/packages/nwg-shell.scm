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
  #:use-module (guix build-system go)
  #:use-module (guix build-system pyproject)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages golang-graphics)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages wm))

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

(define-public nwg-drawer
  (package
    (name "nwg-drawer")
    (version "0.7.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nwg-piotr/nwg-drawer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f347n1f9nzqr2ryc49nv74kz6qyh6yk8g2b0rdbd3flgx4kap8v"))
       (patches
        (search-patches "nwg-drawer-0.7.5-fallback-paths.patch"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/nwg-piotr/nwg-drawer"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (with-directory-excursion "src/github.com/nwg-piotr/nwg-drawer"
                (substitute* '("main.go" "tools.go")
                  (("\\/usr\\/bin\\/env") (search-input-file inputs "/bin/env"))
                  (("\\/usr\\/share") (string-append #$output "/share"))))))
          (add-after 'install 'install-data
            (lambda _
              (mkdir-p (string-append #$output "/share/nwg-drawer"))
              (with-directory-excursion (string-append "src/github.com/"
                                                       "nwg-piotr/"
                                                       "nwg-drawer")
                (copy-recursively "desktop-directories"
                                  (string-append #$output "/share/"
                                                 "nwg-drawer/"
                                                 "desktop-directories"))
                (copy-recursively "img"
                                  (string-append #$output "/share/"
                                                 "nwg-drawer/img"))
                (install-file "drawer.css"
                              (string-append #$output "/share/nwg-drawer"))
                (install-file "README.md"
                              (string-append #$output
                                             "/share/doc/nwg-drawer")))))
          (add-after 'install 'wrap-program
            (lambda _
              (wrap-program (string-append #$output "/bin/nwg-drawer")
                `("PATH" prefix
                  (,(dirname (which "xdg-open"))))
                `("GI_TYPELIB_PATH" =
                  (,(getenv "GI_TYPELIB_PATH")))))))))
    (native-inputs
     (list gobject-introspection
           go-github-com-allan-simon-go-singleinstance
           go-github-com-diamondburned-gotk4
           go-github-com-diamondburned-gotk4-layer-shell
           go-github-com-expr-lang-expr
           go-github-com-fsnotify-fsnotify
           go-github-com-google-shlex
           go-github-com-joshuarubin-go-sway
           go-github-com-sirupsen-logrus
           pkg-config))
    (inputs
     (list bash-minimal
           coreutils-minimal
           gtk+
           gtk-layer-shell
           xdg-utils))       ;for 'xdg-open'
    (home-page "https://nwg-piotr.github.io/nwg-shell/nwg-drawer")
    (synopsis "Application drawer for wlroots-based Wayland compositors")
    (description
     "nwg-drawer is an application launcher.  It's being developed with sway and
Hyprland in mind, but should also work with other wlroots-based Wayland
compositors.

This application is a part of the nwg-shell project.")
    (license license:expat)))

(define-public nwg-menu
  (package
    (name "nwg-menu")
    (version "0.1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nwg-piotr/nwg-menu")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19z29v3iym9z0cpw2bhgvc2l5v0x6jgihvrri8ib78ghfgs7rwyx"))
       (modules '((guix build utils)))
       ;; Replace systemd commands with elogind commands.
       (snippet
        '(substitute* "main.go"
           (("systemctl ((-i |)poweroff|reboot|suspend)" _ arg)
            (string-append "loginctl " arg))))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/nwg-piotr/nwg-menu"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (with-directory-excursion (string-append "src/github.com/"
                                                       "nwg-piotr/nwg-menu")
                (substitute* "main.go"
                  (("\\/usr(\\/share\\/nwg-menu)" _ suffix)
                   (string-append #$output suffix)))
                (substitute* "tools.go"
                  (("\\/usr\\/bin\\/env") (search-input-file inputs "/bin/env"))
                  (("\\/usr\\/share") (string-append #$output "/share"))))))
          (add-after 'install 'install-data
            (lambda _
              (mkdir-p (string-append #$output "/share/nwg-menu"))
              (with-directory-excursion (string-append "src/github.com/"
                                                       "nwg-piotr/nwg-menu")
                (copy-recursively "desktop-directories"
                                  (string-append #$output "/share/nwg-menu/"
                                                 "desktop-directories"))
                (install-file "menu-start.css"
                              (string-append #$output "/share/nwg-menu"))
                (install-file "README.md"
                              (string-append #$output
                                             "/share/doc/nwg-menu")))))

          (add-after 'install 'wrap-program
            (lambda _
              (wrap-program (string-append #$output "/bin/nwg-menu")
                `("GI_TYPELIB_PATH" =
                  (,(getenv "GI_TYPELIB_PATH")))))))))
    (native-inputs
     (list gobject-introspection
           go-github-com-allan-simon-go-singleinstance
           go-github-com-dlasky-gotk3-layershell
           go-github-com-gotk3-gotk3
           go-github-com-joshuarubin-go-sway
           go-github-com-sirupsen-logrus
           pkg-config))
    (inputs
     (list bash-minimal
           coreutils-minimal
           gtk+
           gtk-layer-shell))
    (home-page "https://nwg-piotr.github.io/nwg-shell/nwg-menu")
    (synopsis "MenuStart plugin to nwg-panel")
    (description
     "nwg-menu provides the MenuStart plugin to nwg-panel.  It also may be used
standalone, however, with a little help from command line arguments.

This application is a part of the nwg-shell project.")
    (license license:expat)))

(define-public azote
  (package
    (name "azote")
    (version "1.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nwg-piotr/azote")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ka1i3z6ycj8y74lpblxxbmjval8zbskgfs2gnpgwgsypxlqckg1"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;no tests exist in source
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda _
              (with-directory-excursion "azote"
                (substitute* "main.py"
                  (("\\/usr\\/bin\\/env bash")
                   (string-append #$(this-package-input "bash-minimal")
                                  "/bin/bash"))
                  (("\\/usr(\\/share\\/azote)" _ suffix)
                   (string-append #$output suffix))
                  (("\\/usr(\\/share\\/applications)" _ suffix)
                   (string-append #$output suffix)))
                (substitute* "tools.py"
                  (("\\/usr(\\/share\\/backgrounds\\/sway)" _ suffix)
                   (string-append #$(this-package-input "sway") suffix))
                  (("\\/usr(\\/share\\/backgrounds\\/nwg-shell)" _ suffix)
                   (string-append #$(this-package-input "nwg-shell-wallpapers")
                                  suffix))
                  ;; Looking for mimeinfo.cache in a single directory does not
                  ;; suite well with Guix.  Preventing this, Azote uses of the
                  ;; default feh command to preview the wallpapers.
                  (("\\/usr(\\/share\\/applications)" _ suffix)
                   (string-append #$output suffix))))))
          (add-after 'create-entrypoints 'install-data
            (lambda _
              (with-directory-excursion "dist"
                (install-file "azote.desktop"
                              (string-append #$output "/share/applications"))
                (install-file "azote.svg"
                              (string-append #$output "/share/pixmaps"))
                (for-each (lambda (file)
                            (install-file file
                                          (string-append #$output
                                                         "/share/azote")))
                          (find-files "." "indicator.*\\.png$")))
              (install-file "README.md"
                            (string-append #$output "/share/doc/Azote"))))
          (add-after 'create-entrypoints 'wrap-program
            (lambda _
              (wrap-program (string-append #$output "/bin/azote")
                `("PATH" ":" prefix
                  (,(dirname (which "cp"))
                   ,(dirname (which "feh"))
                   ,(dirname (which "find"))
                   ,(dirname (which "grim"))
                   ;; TODO: The current version of imagemagick does not contain
                   ;; the magick command.  Update it and then add this:
                   ;; ,(dirname (which "magick"))
                   ,(dirname (which "slurp"))
                   ,(dirname (which "swaybg"))
                   ,(dirname (which "wlr-randr"))))
                `("GI_TYPELIB_PATH" =
                  (,(getenv "GI_TYPELIB_PATH")))))))))
    ;; These packages are propagated for compositors like Sway or Hyprland to
    ;; load the backgrounds set by Azote.
    (propagated-inputs
     (list gdk-pixbuf                  ;for GDK_PIXBUF_MODULE_FILE
           `(,libavif "pixbuf-loader") ;for loading AVIF backgrounds
           libheif                     ;for loading HEIF backgrounds
           `(,libjxl "pixbuf-loader")  ;for loading JXL backgrounds
           swaybg))                    ;for ~/.azotebg and ~/.azotebg-hyprland
    (native-inputs
     (list gobject-introspection
           python-setuptools))
    (inputs
     (list bash-minimal
           coreutils-minimal
           feh
           findutils
           grim
           gtk+
           imagemagick
           libappindicator
           nwg-shell-wallpapers
           python-pillow
           python-pillow-avif-plugin
           python-pillow-heif
           python-pillow-jxl-plugin
           python-pycairo
           python-pygobject
           python-send2trash
           python-xlib
           slurp
           sway
           swaybg
           wlr-randr))
    (home-page "https://nwg-piotr.github.io/nwg-shell/azote")
    (synopsis "Wallpaper manager")
    (description
     "Azote is a GTK+ 3 based picture browser and background setter, as the
frontend to the @command{swaybg} (sway/Wayland) and @command{feh} (X windows)
commands.

This application is a part of the nwg-shell project.")
    (license (list license:gpl3
                   license:bsd-3)))) ;azote/colorthief.py
