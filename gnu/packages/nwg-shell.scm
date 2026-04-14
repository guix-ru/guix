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
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages golang-graphics)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages music)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))

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

(define-public nwg-bar
  (package
    (name "nwg-bar")
    (version "0.1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nwg-piotr/nwg-bar")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yds8sivilirw2cqkq621z731l4hragzgpajjls79f4zkwlrdpz4"))
       (patches
        (search-patches
         ;; TODO: Remove this patch in the next update.
         "nwg-bar-0.1.6-fix-silent-errors.patch"
         "nwg-bar-0.1.6-fallback-paths.patch"))
       (modules '((guix build utils)))
       ;; Replace systemd commands with elogind commands.
       (snippet
        '(substitute* "config/bar.json"
           (("\"systemctl (-i |)") "\"loginctl ")))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/nwg-piotr/nwg-bar"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda _
              (with-directory-excursion "src/github.com/nwg-piotr/nwg-bar"
                (substitute* '("tools.go" "config/bar.json")
                  (("\\/usr\\/share") (string-append #$output "/share"))))))
          (add-after 'install 'install-data
            (lambda _
              (mkdir-p (string-append #$output "/share"))
              (with-directory-excursion (string-append "src/github.com/"
                                                       "nwg-piotr/nwg-bar")
                (copy-recursively "config"
                                  (string-append #$output "/share/nwg-bar"))
                (copy-recursively "images"
                                  (string-append #$output "/share/nwg-bar/"
                                                 "images"))
                (install-file "README.md"
                              (string-append #$output "/share/doc/nwg-bar")))))
          (add-after 'install 'wrap-program
            (lambda _
              (wrap-program (string-append #$output "/bin/nwg-bar")
                `("GI_TYPELIB_PATH" =
                  (,(getenv "GI_TYPELIB_PATH")))))))))
    (native-inputs
     (list gobject-introspection
           go-github-com-allan-simon-go-singleinstance
           go-github-com-dlasky-gotk3-layershell
           go-github-com-gotk3-gotk3
           go-github-com-joshuarubin-go-sway
           pkg-config))
    (inputs
     (list bash-minimal
           gtk+
           gtk-layer-shell))
    (home-page "https://github.com/nwg-piotr/nwg-bar")
    (synopsis "GTK3-based button bar for wlroots-based compositors")
    (description
     "nwg-bar is a Golang replacement to the @command{nwgbar} command (a part of
nwg-launchers), with some improvements.  Originally aimed at sway, works with
wlroots-based compositors only.

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

(define-public nwg-look
  (package
    (name "nwg-look")
    (version "1.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nwg-piotr/nwg-look")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1z4hybdfd8d98hy8az7z580a0fx4laf0vfrknjnlpk6xss159mbh"))))
    (build-system go-build-system)
    (arguments
     (list
       ;Tests fail due to a call having arguments but no formatting
      ;; directives and wrong types of int and float64.
      #:tests? #f
      #:install-source? #f
      #:import-path "github.com/nwg-piotr/nwg-look"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda _
              (substitute* "src/github.com/nwg-piotr/nwg-look/tools.go"
                (("\\/usr\\/share") (string-append #$output "/share")))))
          (add-after 'install 'install-data
            (lambda _
              (with-directory-excursion (string-append "src/github.com/"
                                                       "nwg-piotr/nwg-look")
                (install-file "stuff/main.glade"
                              (string-append #$output "/share/nwg-look"))
                (install-file "stuff/nwg-look.desktop"
                              (string-append #$output "/share/applications"))
                (install-file "stuff/nwg-look.svg"
                              (string-append #$output "/share/pixmaps"))
                (copy-recursively "langs"
                                  (string-append #$output "/share/nwg-look/"
                                                 "langs"))
                (install-file "README.md"
                              (string-append #$output
                                             "/share/doc/nwg-look")))))
          (add-after 'install 'wrap-program
            (lambda _
              (wrap-program (string-append #$output "/bin/nwg-look")
                `("PATH" ":" prefix
                  (,(dirname (which "gsettings"))
                   ,(dirname (which "xcur2png"))))
                `("GI_TYPELIB_PATH" =
                  (,(getenv "GI_TYPELIB_PATH")))))))))
    (native-inputs
     (list gobject-introspection
           go-github-com-gotk3-gotk3
           go-github-com-sirupsen-logrus
           pkg-config))
    (inputs
     (list bash-minimal
           coreutils-minimal
           `(,glib "bin")    ;for 'gsettings'
           gtk+
           xcur2png))
    (home-page "https://nwg-piotr.github.io/nwg-shell/nwg-look")
    (synopsis "GTK3 settings editor for wlroots environment")
    (description
     "nwg-look is a GTK3 settings editor, designed to work properly in
wlroots-based Wayland environment.

This application is a part of the nwg-shell project.")
    (license license:expat)))

(define-public nwg-displays
  (package
    (name "nwg-displays")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nwg-piotr/nwg-displays")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wgmj933fx4kl83q71phzm67wmkzfp543mb956dzbc5hkrac0nvv"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;no tests exist in source
      #:phases
      #~(modify-phases %standard-phases
          (delete 'sanity-check)
          (add-after 'unpack 'patch-paths
            (lambda _
              (with-directory-excursion "nwg_displays"
                (substitute* "tools.py"
                  (("\\//usr(.*nwg-displays\\/.svg)" _ suffix)
                   (string-append #$output suffix)))
                (substitute* "wallpaper_manager/wallpaper_manager.py"
                  (("\\/usr\\/bin\\/env bash")
                   (string-append #$(this-package-input "bash-minimal")
                                  "/bin/bash"))))))
          (add-after 'create-entrypoints 'install-data
            (lambda _
              (install-file "nwg-displays.desktop"
                            (string-append #$output "/share/applications"))
              (install-file "nwg-displays.svg"
                            (string-append #$output "/share/pixmaps"))
              (install-file "README.md"
                            (string-append #$output
                                           "/share/doc/nwg-displays"))))
          (add-after 'create-entrypoints 'wrap-programs
            (lambda _
              (for-each (lambda (file)
                          (wrap-program file
                            `("PATH" ":" prefix
                              (,(dirname (which "notify-send"))
                               ,(dirname (which "swaybg"))))
                            `("GI_TYPELIB_PATH" =
                              (,(getenv "GI_TYPELIB_PATH")))))
                        (find-files (string-append #$output "/bin"))))))))
    (native-inputs
     (list gobject-introspection))
    (inputs
     (list bash-minimal
           gtk+
           gtk-layer-shell
           libnotify
           python-pygobject
           python-i3ipc
           swaybg))
    (home-page "https://github.com/nwg-piotr/nwg-displays")
    (synopsis "GUI display configuration tool for wlroots compositors")
    (description
     "nwg-displays is an output management utility for sway and Hyprland Wayland
compositor, inspired by wdisplays and wlay.")
    (license license:expat)))

(define-public nwg-readme-browser
  (package
    (name "nwg-readme-browser")
    (version "0.1.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nwg-piotr/nwg-readme-browser")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "039f8xdm2i2dyvhk8nw7z3pl552mfg7ncl2amsfqpnn8v6y6vd1q"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;no tests exist in source
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda _
              ;; Set the default doc directory to the system doc directory.
              (substitute* "nwg_readme_browser/main.py"
                (("\\/usr") "/run/current-system/profile"))))
          (add-after 'create-entrypoints 'install-data
            (lambda _
              (install-file "nwg-readme-browser.desktop"
                            (string-append #$output "/share/applications"))
              (install-file "nwg-readme-browser.svg"
                            (string-append #$output "/share/pixmaps"))
              (install-file "README.md"
                            (string-append #$output
                                           "/share/doc/nwg-readme-browser"))))
          (add-after 'create-entrypoints 'wrap-program
            (lambda _
              (wrap-program (string-append #$output "/bin/nwg-readme-browser")
                `("GI_TYPELIB_PATH" =
                  (,(getenv "GI_TYPELIB_PATH"))))))
          (add-before 'sanity-check 'check-setup
            (lambda _
              (setenv "HOME" (getcwd)))))))
    (native-inputs
     (list gobject-introspection
           python-setuptools))
    (inputs
     (list adwaita-icon-theme
           bash-minimal
           gtk+
           python-docutils
           python-markdown2
           python-pygobject
           webkitgtk-for-gtk3))
    (home-page "https://nwg-piotr.github.io/nwg-shell/nwg-readme-browser")
    (synopsis "WebKitGTK-based README file browser")
    (description
     "nwg-readme-browser was conceived as @acronym{RTFM, Read The Fucking
Manual} with a graphical user interface.  It searches a configurable path for
@file{README.*} files, and displays them in @code{WebKit2.WebView}.  It supports
@file{.md}, @file{.rst}, @file{.html} and plain text.  It does not support
@file{.pdf} format.  Although the program was written with nwg-shell for sway
and Hyprland in mind, it may also be used standalone.

This application is a part of the nwg-shell project.")
    (license license:expat)))

(define-public nwg-clipman
  (package
    (name "nwg-clipman")
    (version "0.2.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nwg-piotr/nwg-clipman")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ha3yvdxcid0fgsx28cibc6n20qikc1x9y9hpjv26jc2dkikql0r"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;no tests exist in source
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'create-entrypoints 'install-data
            (lambda _
              (install-file "nwg-clipman.desktop"
                            (string-append #$output "/share/applications"))
              (install-file "nwg-clipman.svg"
                            (string-append #$output "/share/pixmaps"))
              (install-file "README.md"
                            (string-append #$output
                                           "/share/doc/nwg-clipman"))))
          (add-after 'create-entrypoints 'wrap-program
            (lambda _
              (wrap-program (string-append #$output "/bin/nwg-clipman")
                `("PATH" ":" prefix
                  (,(dirname (which "cliphist"))
                   ,(dirname (which "wl-copy"))))
                `("GI_TYPELIB_PATH" =
                  (,(getenv "GI_TYPELIB_PATH"))))))
          (add-before 'sanity-check 'check-setup
            (lambda _
              (setenv "HOME" (getcwd)))))))
    (native-inputs
     (list gobject-introspection
           python-setuptools))
    (inputs
     (list bash-minimal
           cliphist
           gtk+
           gtk-layer-shell
           python-pygobject
           wl-clipboard))
    (home-page "https://github.com/nwg-piotr/nwg-clipman")
    (synopsis "Clipboard manager for nwg-shell")
    (description
     "nwg-clipman is a GTK3-based GUI for @command{cliphist}.  It provides
access to previously copied items, as well as management of the clipboard
history from a window opened on @code{gtk-layer-shell}.  The program is intended
for use with sway, Hyprland and other wlroots-based Wayland compositors.

This application is a part of the nwg-shell project.")
    (license license:expat)))

(define-public nwg-icon-picker
  (package
    (name "nwg-icon-picker")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nwg-piotr/nwg-icon-picker")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b7mds438br3labjl2c3lpgga5xmypp1a58ygf96kawy5s2wjv8s"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;no tests exist in source
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'create-entrypoints 'install-data
            (lambda _
              (install-file "nwg-icon-picker.desktop"
                            (string-append #$output "/share/applications"))
              (install-file "nwg-icon-picker.svg"
                            (string-append #$output "/share/pixmaps"))
              (install-file "README.md"
                            (string-append #$output
                                           "/share/doc/nwg-icon-picker"))))
          (add-after 'create-entrypoints 'wrap-program
            (lambda _
              (wrap-program (string-append #$output "/bin/nwg-icon-picker")
                `("XDG_DATA_DIRS" prefix
                  (,(string-append #$output "/share")))
                `("GI_TYPELIB_PATH" =
                  (,(getenv "GI_TYPELIB_PATH")))))))))
    (propagated-inputs
     (list hicolor-icon-theme))
    (native-inputs
     (list gobject-introspection
           python-setuptools))
    (inputs
     (list bash-minimal
           gtk+
           python-pygobject))
    (home-page "https://github.com/nwg-piotr/nwg-icon-picker")
    (synopsis "GTK icon chooser with a text search option")
    (description
     "nwg-icon-picker is intended to work as the icon picker for nwg-panel, but
it may be used standalone.  It displays a window to choose an icon with a
textual search entry, and returns the icon name.  You can also open a file from
the search result in GIMP or Inkscape, if installed.

This application is a part of the nwg-shell project.")
    (license license:expat)))

(define-public gopsuinfo
  (package
    (name "gopsuinfo")
    (version "0.1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nwg-piotr/gopsuinfo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nv70mgnmq941cc8wldk59kmdkjmwz2bga2f1v5qh3c3p8ww7c64"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/nwg-piotr/gopsuinfo"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/github.com/nwg-piotr/gopsuinfo/gopsuinfo.go"
                (("\\/usr") #$output))))
          (add-after 'install 'install-data
            (lambda _
              (mkdir-p (string-append #$output "/share/gopsuinfo"))
              (with-directory-excursion (string-append "src/github.com/"
                                                       "nwg-piotr/gopsuinfo")
                (copy-recursively "icons_dark"
                                  (string-append #$output "/share/gopsuinfo/"
                                                 "icons_dark"))
                (copy-recursively "icons_light"
                                  (string-append #$output "/share/gopsuinfo/"
                                                 "icons_light"))
                (install-file "README.md"
                              (string-append #$output
                                             "/share/doc/gopsuinfo"))))))))
    (native-inputs
     (list go-github-com-shirou-gopsutil))
    (home-page "https://github.com/nwg-piotr/gopsuinfo")
    (synopsis "Display system usage info as text")
    (description
     "gopsuinfo is a Go version of the @command{psuinfo} python script.  It
provides @command{gopsuinfo}, a gopsutil-based command, to display system usage
info as text in panels like Waybar or icon/text in tint2 and nwg-panel
executors.

This application is a part of the nwg-shell project.")
    (license (list license:bsd-2 license:expat)))) ;dual-licensed

(define-public nwg-panel
  (package
    (name "nwg-panel")
    (version "0.10.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nwg-piotr/nwg-panel")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iy528zvk26k48dd8rd1qr69y52nvvhvfv4zfvf821n26r33dwad"))
       (modules '((guix build utils)))
       ;; Replace systemd commands with elogind commands.
       (snippet
        '(substitute* '("nwg_panel/config.py"
                        "nwg_panel/main.py"
                        "nwg_panel/config/config"
                        "nwg_panel/modules/menu_start.py")
           (("\"systemctl (-i |)") "\"loginctl ")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;no tests exist in source
      #:modules '((guix build pyproject-build-system)
                  (guix build utils)
                  (srfi srfi-26))                     ;for cute
      #:phases
      #~(modify-phases %standard-phases
          ;; Sanity check requires a running sway or Hyprland session.
          (delete 'sanity-check)
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Set default backgrounds directory.
              (substitute* "nwg_panel/config.py"
                (("\\/usr(\\/share\\/backgrounds)" _ suffix)
                 (string-append #$(this-package-input "nwg-shell-wallpapers")
                                suffix)))
              ;; Patch the default data directory.
              (substitute* '("nwg_panel/icons.py"
                             "nwg_panel/modules/pinned.py")
                (("\\/usr\\/share") (string-append #$output "/share")))
              ;; Fix the dbus service.
              (substitute* "nwg-panel.service"
                (("\\/bin\\/sh") (search-input-file inputs "/bin/bash"))
                (("\\/usr") #$output))))
          (add-after 'create-entrypoints 'install-data
            (lambda _
              (for-each (cute install-file <>
                              (string-append #$output "/share/applications"))
                        '("nwg-panel-config.desktop" "nwg-processes.desktop"))
              (for-each (cute install-file <>
                              (string-append #$output "/share/pixmaps"))
                        '("nwg-panel.svg" "nwg-shell.svg" "nwg-processes.svg"))
              (install-file "nwg-panel.service"
                            (string-append #$output "/share/dbus-1/services"))
              (install-file "README.md"
                            (string-append #$output "/share/doc/nwg-panel"))))
          (add-after 'create-entrypoints 'wrap-programs
            (lambda _
              (for-each (lambda (file)
                          (wrap-program file
                            `("PATH" ":" prefix
                              (,(dirname (which "brightnessctl"))
                               ,(dirname (which "curl"))
                               ,(dirname (which "foot"))
                               ,(dirname (which "gopsuinfo"))
                               ,(dirname (which "htop"))
                               ,(dirname (which "nwg-icon-picker"))
                               ,(dirname (which "nwg-menu"))
                               ,(dirname (which "notify-send"))
                               ,(dirname (which "pactl"))
                               ,(dirname (which "pkill"))
                               ,(dirname (which "playerctl"))
                               ,(dirname (which "swaync-client"))
                               ,(dirname (which "wlr-randr"))))
                            `("GI_TYPELIB_PATH" =
                              (,(getenv "GI_TYPELIB_PATH")))))
                        (find-files (string-append #$output "/bin"))))))))
    (native-inputs
     (list gobject-introspection
           python-setuptools))
    (inputs
     (list bash-minimal
           curl                 ;for the default executors
           brightnessctl        ;for controlling brightness
           foot                 ;for the default executors
           gopsuinfo            ;for the default executors
           gtk+
           gtk-layer-shell
           hicolor-icon-theme
           htop                 ;for the default executors
           libappindicator
           libdbusmenu
           libnotify
           nwg-icon-picker      ;for chosing icons for buttons
           nwg-menu             ;optional plugin
           nwg-shell-wallpapers ;for random wallpaper module
           playerctl            ;optional plugin
           procps
           pulseaudio           ;for controlling audio
           python-pycairo
           python-dasbus
           python-i3ipc
           python-psutil
           python-pygobject
           python-requests
           swaynotificationcenter
           wlr-randr))
    (home-page "https://nwg-piotr.github.io/nwg-shell/nwg-panel")
    (synopsis "GTK3-based panel for sway and Hyprland")
    (description
     "nwg-panel is a GTK3-based panel for sway and Hyprland Wayland compositors.
The panel is equipped with a graphical configuration program that frees the user
from the need to manually edit configuration files.

This application is a part of the nwg-shell project.")
    (license license:expat)))

(define-public nwg-dock
  (package
    (name "nwg-dock")
    (version "0.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nwg-piotr/nwg-dock")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gagfm2igbwdsi9f4gs7lmvqjmxvdp0zkm6yl9b0qi7wjnb3hsb2"))
       (patches
        (search-patches "nwg-dock-0.4.3-fallback-paths.patch"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: nwg-dock/tools.go:980:15: github.com/sirupsen/logrus.Errorf call
      ;; has arguments but no formatting directives
      #:tests? #f
      #:install-source? #f
      #:import-path "github.com/nwg-piotr/nwg-dock"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda _
              (with-directory-excursion "src/github.com/nwg-piotr/nwg-dock"
                (substitute* '("main.go" "tools.go")
                  (("\\/usr\\/share") (string-append #$output "/share"))))))
          (add-after 'install 'install-data
            (lambda _
              (mkdir-p (string-append #$output "/share"))
              (with-directory-excursion (string-append "src/github.com/"
                                                       "nwg-piotr/nwg-dock")
                (copy-recursively "config"
                                  (string-append #$output "/share/nwg-dock"))
                (copy-recursively "images"
                                  (string-append #$output
                                                 "/share/nwg-dock/images"))
                (install-file "README.md"
                              (string-append #$output
                                             "/share/doc/nwg-dock")))))
          (add-after 'install 'wrap-program
            (lambda _
              (wrap-program (string-append #$output "/bin/nwg-dock")
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
           gtk+
           gtk-layer-shell))
    (home-page "https://nwg-piotr.github.io/nwg-shell/nwg-dock")
    (synopsis "GTK3 based dock for sway")
    (description
     "nwg-dock is a fully configurable (w/ command line arguments and CSS) GTK3
based dock, written in Go, aimed exclusively at sway Wayland compositor.  It
features pinned buttons, task buttons, the workspace switcher and the launcher
button.

This application is a part of the nwg-shell project.")
    (license license:expat)))

(define-public nwg-dock-hyprland
  (package
    (name "nwg-dock-hyprland")
    (version "0.4.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nwg-piotr/nwg-dock-hyprland")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cfbrapdiw8qwfcbmyqz6k0q5ny8balzmpwsnp9b3rny2w0cvqkb"))
       (patches
        (search-patches "nwg-dock-hyprland-0.4.8-fallback-paths.patch"))))
    (build-system go-build-system)
    (arguments
     (list
       ;Tests fail due to a call having arguments but no formatting
      ;; directives.
      #:tests? #f
      #:install-source? #f
      #:import-path "github.com/nwg-piotr/nwg-dock-hyprland"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda _
              (with-directory-excursion (string-append "src/github.com/"
                                                       "nwg-piotr/"
                                                       "nwg-dock-hyprland")
                (substitute* '("main.go" "tools.go")
                  (("\\/usr\\/share") (string-append #$output "/share"))))))
          (add-after 'install 'install-data
            (lambda _
              (mkdir-p (string-append #$output "/share"))
              (with-directory-excursion (string-append "src/github.com/"
                                                       "nwg-piotr/"
                                                       "nwg-dock-hyprland")
                (copy-recursively "config"
                                  (string-append #$output
                                                 "/share/nwg-dock-hyprland"))
                (copy-recursively "images"
                                  (string-append #$output "/share/"
                                                 "nwg-dock-hyprland/images"))
                (install-file "README.md"
                              (string-append #$output "/share/doc/"
                                             "nwg-dock-hyprland")))))
          (add-after 'install 'wrap-program
            (lambda _
              (wrap-program (string-append #$output "/bin/nwg-dock-hyprland")
                `("GI_TYPELIB_PATH" =
                  (,(getenv "GI_TYPELIB_PATH")))))))))
    (native-inputs
     (list gobject-introspection
           go-github-com-allan-simon-go-singleinstance
           go-github-com-diamondburned-gotk4
           go-github-com-diamondburned-gotk4-layer-shell
           go-github-com-sirupsen-logrus
           pkg-config))
    (inputs
     (list bash-minimal
           gtk+
           gtk-layer-shell))
    (home-page "https://github.com/nwg-piotr/nwg-dock-hyprland")
    (synopsis "GTK3 based dock for Hyprland")
    (description
     "nwg-dock-hyprland is a fully configurable (w/ command line arguments and
CSS) GTK3 based dock, written in Go, aimed exclusively at Hyprland Wayland
compositor.  It features pinned buttons, task buttons, the workspace switcher
and the launcher button.

This application is a part of the nwg-shell project.")
    (license license:expat)))

(define-public typobuster
  (package
    (name "typobuster")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nwg-piotr/typobuster")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n8nqfwj9ih1vdzvmlnjp463rnr66ca30sdc7a82g42brmlkk5wv"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;no tests exist in source
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda _
              (substitute* "typobuster/tools.py"
                (("\\/usr\\/share") (string-append #$output "/share")))))
          (add-after 'create-entrypoints 'install-data
            (lambda _
              (install-file "typobuster.desktop"
                            (string-append #$output "/share/applications"))
              (install-file "typobuster.svg"
                            (string-append #$output "/share/pixmaps"))
              (install-file "README.md"
                            (string-append #$output "/share/doc/typobuster"))))
          (add-after 'create-entrypoints 'wrap-program
            (lambda _
              (wrap-program (string-append #$output "/bin/typobuster")
                `("PATH" prefix
                  (,(dirname (which "gsettings"))))
                `("GI_TYPELIB_PATH" prefix
                  (,(getenv "GI_TYPELIB_PATH")))
                `("XDG_DATA_DIRS" prefix
                  (,(string-append #$(this-package-input "gtksourceview")
                                   "/share")))))))))
    (propagated-inputs
     ;; avoid runtime error:
     ;; No such schema “org.gnome.desktop.interface”
     (list gsettings-desktop-schemas))
    (native-inputs
     (list gobject-introspection
           python-setuptools))
    (inputs
     (list bash-minimal
           `(,glib "bin")     ;for gsettings
           gtk+
           gtksourceview-4
           python-pycairo
           python-pygobject))
    (home-page "https://nwg-piotr.github.io/nwg-shell/typobuster")
    (synopsis "Text editor with text transformations and auto-correction")
    (description
     "Typobuster is a simplified text editor with a wide selection of
transformations and automatic correction of common typos.

This application is a part of the nwg-shell project.")
    (license license:gpl3)))

(define-public nwg-shell-config
  (package
    (name "nwg-shell-config")
    (version "0.5.64")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nwg-piotr/nwg-shell-config")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sfh4sbpjf7bf5r2n96jwafzmwwc8yfhlfn5lsms5fq2f3srxyk2"))
       (modules '((guix build utils)))
       ;; Replace systemd commands with elogind commands.
       (snippet
        '(with-directory-excursion "nwg_shell_config"
           ;; The skeleton files in the store directory is read-only, so do not
           ;; install them preserving file permissions.
           (substitute* '("hud.py" "tools.py")
             (("copy2") "copyfile"))
           (substitute* '("locker.py"
                          "main_hyprland.py"
                          "main_sway.py"
                          "ui_components.py"
                          "shell/custom"
                          "shell/custom-hyprland"
                          "shell/settings"
                          "shell/settings-hyprland")
             (("\"systemctl (-i |)") "\"loginctl ")
             (("'systemctl (-i |)") "'loginctl "))
           ;; Add support for icecat.
           (substitute* "ui_components.py"
             (("(.*)\"firefox\": \"(MOZ_ENABLE_WAYLAND=1 )firefox\"," all
               indent env)
              (string-append all "\n" indent "\"icecat\": \"" env "icecat\","))
             (("\"firefox\", " all) (string-append all "\"icecat\", ")))))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;no tests exist in source
      #:modules '((guix build pyproject-build-system)
                  (guix build utils)
                  (srfi srfi-26))                     ;for cute
      #:phases
      #~(modify-phases %standard-phases
          ;; Sanity check requires a running sway or Hyprland session.
          (delete 'sanity-check)
          (add-after 'unpack 'patch-paths
            (lambda _
              (with-directory-excursion "nwg_shell_config"
                ;; Replace the default backgrounds directory.
                (substitute* '("locker.py"
                               "main_hyprland.py"
                               "main_sway.py"
                               "tools.py"
                               "shell/settings"
                               "shell/settings-hyprland")
                  (("\\/usr(\\/share\\/backgrounds)" _ suffix)
                   (string-append #$(this-package-input "nwg-shell-wallpapers")
                                  suffix)))
                ;; Patch path to the pixmaps directory.
                (substitute* "tools.py"
                  (("\\/usr\\/share\\/pixmaps")
                   (string-append #$output "/share/pixmaps")))
                ;; Patch path to swaync
                (substitute* '("main_sway.py" "main_hyprland.py")
                  (("\\/etc\\/xdg\\/swaync" all)
                   (string-append #$(this-package-input
                                     "swaynotificationcenter") all))))))
          (add-after 'install 'install-data
            (lambda _
              (install-file "nwg-shell-config.desktop"
                            (string-append #$output "/share/applications"))
              (for-each (cute install-file <>
                              (string-append #$output "/share/pixmaps"))
                        '("nwg-shell-config.svg"
                          "nwg-shell-update.svg"
                          "nwg-shell-translate.svg"
                          "nwg-update-noupdate.svg"
                          "nwg-update-available.svg"
                          "nwg-update-checking.svg"
                          "nwg-screenshot.svg"
                          "nwg-3.svg"
                          "nwg-2.svg"
                          "nwg-1.svg"
                          "nwg-workspace.svg"))
              (install-file "README.md"
                            (string-append #$output
                                           "/share/doc/nwg-shell-config"))))
          (add-after 'create-entrypoints 'wrap-programs
            (lambda _
              (with-directory-excursion (string-append #$output "/bin")
                (for-each (lambda (file)
                            (wrap-program file
                              `("PATH" ":" prefix
                                (,(string-append #$output "/bin")
                                 ,(dirname (which "foot"))
                                 ,(dirname (which "gtklock"))
                                 ,(dirname (which "killall"))
                                 ,(dirname (which "notify-send"))
                                 ,(dirname (which "pkill"))
                                 ,(dirname (which "playerctl"))
                                 ,(dirname (which "swappy"))
                                 ,(dirname (which "swayidle"))
                                 ,(dirname (which "swaync"))
                                 ,(dirname (which "wlsunset"))))
                              `("GI_TYPELIB_PATH" =
                                (,(getenv "GI_TYPELIB_PATH")))))
                          (find-files "." "nwg.*")))))
          (add-after 'wrap 'install-nwg-system-update
            (lambda _
              ;; TODO: Add an argument to nwg-system-update for updating
              ;; Guix and then add a code for it in
              ;; nwg_shell_config/update_indicator.py.
              (install-file "nwg-system-update"
                            (string-append #$output "/bin")))))))
    (native-inputs
     (list gobject-introspection
           python-setuptools))
    (inputs
     (list bash-minimal
           foot
           gtk+
           gtk-layer-shell
           gtklock
           libappindicator
           libnotify
           nwg-shell-wallpapers
           playerctl
           procps
           psmisc
           python-geopy
           python-i3ipc
           python-pygobject
           python-psutil
           python-requests
           swappy
           swayidle
           swaynotificationcenter
           wlsunset))
    (home-page "https://nwg-piotr.github.io/nwg-shell/nwg-shell-config")
    (synopsis "nwg-shell configuration utility")
    (description
     "nwg-shell-config utility provides a graphical user interface for
configuring sway and Hyprland Wayland compositors in nwg-shell.

This application is a part of the nwg-shell project.")
    (license license:expat)))

;; nwg-shell meta package
(define-public nwg-shell
  (package
    (name "nwg-shell")
    (version "0.5.50")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nwg-piotr/nwg-shell")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qb4k1w3ycp0n8gwfnk2nmxn8gxa6aab1hg3527x514yck3w1zcp"))
       (modules '((guix build utils)))
       (snippet
        '(with-directory-excursion "nwg_shell"
           ;; The skeleton files in /gnu/store directory are read-only, so make
           ;; the installer install the configuration files without preserving
           ;; permissions and then make make azotebg executable.
           (substitute* "installer.py"
             (("copy, copy2") "copyfile, copy")
             (("copy\\(") "copyfile(")
             (("copy2\\(") "copyfile(")
             (("import os" all) (string-append all "\nimport stat"))
             (("(.* )copyfile\\(.*\\(.*\"azotebg\"\\), bcg\\)" all indent)
              (string-append all "\n" indent "st = os.stat(bcg)" "\n" indent
                             "os.chmod(bcg, st.st_mode | stat.S_IEXEC)"))
             ;; Copy .azotebg to .azotebg-hyprland preserving permissions.
             (("copyfile(\\(.*\"\\.azotebg-hyprland\"\\))" _ suffix)
              (string-append "copy" suffix)))
           ;; Replace systemd commands with elogind commands.
           (substitute* '("installer.py"
                          "skel/data/nwg-shell-config/settings"
                          "skel/data/nwg-shell-config/settings-hyprland"
                          "skel/config/sway/config"
                          "skel/config/nwg-panel/hyprland-0"
                          "skel/config/nwg-panel/hyprland-1"
                          "skel/config/nwg-panel/hyprland-2"
                          "skel/config/nwg-panel/hyprland-3"
                          "skel/config/nwg-panel/preset-0"
                          "skel/config/nwg-panel/preset-1"
                          "skel/config/nwg-panel/preset-2"
                          "skel/config/nwg-panel/preset-3"
                          "skel/config/hypr/hyprland.conf")
             (("\"systemctl (-i |)") "\"loginctl "))
           (substitute* '("skel/config/sway/config"
                          "skel/config/hypr/hyprland.conf")
             ;; Disable importing environment variables for systemd.
             ((".*systemctl --user import-environment.*") "")
             ;; Disable updating environment variables for systemd.
             ((".*dbus-update-activation-environment --systemd.*") "")
             ;; Disable executing polkit-gnome-authentication-agent-1.  Since it
             ;; is not available in $PATH, it is difficult to predict in which
             ;; profile it is installed.  So let users enable it and set the
             ;; path manually.
             (("exec (= |)\\/usr\\/lib\\/polkit-gnome" all)
              (string-append "#" all)))
           ;; Replace firefox with icecat.
           (substitute* '("skel/config/foot/foot.ini"
                          "skel/config/sway/config")
             (("firefox") "icecat"))
           (substitute* "installer.py"
             ;; Add an entry for icecat browser.
             (("\"firefox\", " all) (string-append all "\"icecat\", "))
             ;; Add support to the icecat browser.
             (("    \"firefox\": \"firefox\"," all)
              (string-append all "\n    \"icecat\": \"icecat\",\n"))
             ;; Remove Arch Linux related line from the warning.
             ((".*on a fresh Arch Linux installation.*") ""))))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;no tests exist in source
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda _
              (with-directory-excursion "nwg_shell/skel"
                ;; Patch path to the nwg-shell backgrounds directory.
                (substitute* '("data/nwg-shell-config/settings"
                               "data/nwg-shell-config/settings-hyprland"
                               "config/nwg-panel/hyprland-0"
                               "config/nwg-panel/preset-0")
                  (("\\/usr(\\/share\\/backgrounds\\/nwg-shell)" _ suffix)
                   (string-append #$(this-package-input "nwg-shell-wallpapers")
                                  suffix)))
                ;; Patch path to backgrounds directory.
                (substitute* "stuff/azotebg"
                  (("\\/usr") #$output)))))
          (add-after 'create-entrypoints 'install-scripts
            (lambda _
              (install-file "scripts/screenshot"
                            (string-append #$output "/bin"))))
          (add-after 'install-scripts 'install-data
            (lambda _
              (install-file "nwg-shell.jpg"
                            (string-append #$output "/share/backgrounds"))
              (install-file "README.md"
                            (string-append #$output
                                           "/share/doc/nwg-shell"))))
          (add-after 'install-data 'wrap-programs
            (lambda _
              (with-directory-excursion (string-append #$output "/bin")
                (wrap-program "nwg-shell-installer"
                 `("PATH" prefix
                   (,(dirname (which "localectl")))))
                (wrap-program (string-append #$output "/bin/screenshot")
                 `("PATH" ":" prefix
                   (,(dirname (which "echo"))
                    ,(dirname (which "grim"))
                    ,(dirname (which "jq"))
                    ,(dirname (which "notify-send"))
                    ,(dirname (which "slurp"))
                    ,(dirname (which "swappy"))
                    ,(dirname (which "wl-copy"))
                    ,(dirname (which "xdg-user-dir"))))))
              )))))
    (propagated-inputs
     (list adwaita-icon-theme
           azote
           blueman
           brightnessctl
           cliphist
           font-abattis-cantarell
           foot
           gdk-pixbuf                  ;for GDK_PIXBUF_MODULE_FILE
           gnome-themes-extra
           gopsuinfo
           gsettings-desktop-schemas
           gtklock
           i3-autotiling
           kstatusnotifieritem
           `(,libavif "pixbuf-loader") ;for loading AVIF backgrounds
           libheif                     ;for loading HEIF backgrounds
           `(,libjxl "pixbuf-loader")  ;for loading JXL backgrounds
           network-manager-applet
           nwg-bar
           nwg-clipman
           nwg-displays
           nwg-dock
           nwg-dock-hyprland
           nwg-drawer
           nwg-icon-picker
           nwg-look
           nwg-menu
           nwg-panel
           nwg-readme-browser
           nwg-shell-config
           nwg-shell-wallpapers
           papirus-icon-theme
           pulseaudio
           playerctl
           swaybg
           swayidle
           swaynotificationcenter
           wl-clipboard
           wlsunset))
    (native-inputs
     (list python-setuptools))
    (inputs
     (list bash-minimal
           coreutils-minimal
           grim
           jq
           libnotify
           localed
           slurp
           swappy
           wl-clipboard
           xdg-user-dirs))
    (home-page "https://nwg-piotr.github.io/nwg-shell/")
    (synopsis "GTK3-based shell for sway and Hyprland Wayland compositors")
    (description
     "nwg-shell is a GTK3-based shell for sway and Hyprland Wayland compositors.
The project provides a common configuration tool (nwg-shell-config) that allows
you to configure the system in a graphical UI, and a range of components such as
nwg-panel (system panel), nwg-drawer (application launcher), nwg-dock (system
dock) or nwg-menu (XDG-style menu).  It also includes several native tools as
nwg-look (look and feel GTK settings editor), nwg-displays (display
configuration tool), Azote (wallpaper manager), nwg-clipman (clipboard history
manager), nwg-icon-picker (icon browser with textual search), nwg-readme-browser
(documentation viewer) and nwg-hello (login manager).  Scripts and utilities
such as autotiling (script for sway to automatically switch the horizontal /
vertical window split orientation) and gopsuinfo (a command to display system
usage info) are used in the background.  The shell also utilizes third party
software as swaync (notification center), gtklock / swaylock (screen lockers)
and more.

This package acts as a metapackage and installer of default configuration
files.")
    (license (list license:expat
                   license:cc-by4.0)))) ;for the graphics
