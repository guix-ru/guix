;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019, 2020, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu installer)
  #:use-module (guix discovery)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix utils)
  #:use-module (guix ui)
  #:use-module ((guix self) #:select (make-config.scm))
  #:use-module (guix describe)
  #:use-module (guix channels)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:autoload (gnu installer compute) (compute-keymap-step compute-locale-step)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages connman)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:autoload   (gnu packages gnupg) (guile-gcrypt)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages text-editors)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (gnu system locale)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (web uri)
  #:export (installer-program))

(define module-to-import?
  ;; Return true for modules that should be imported.  For (gnu system …) and
  ;; (gnu packages …) modules, we simply add the whole 'guix' package via
  ;; 'with-extensions' (to avoid having to rebuild it all), which is why these
  ;; modules are excluded here.
  (match-lambda
    (('guix 'config) #f)
    (('gnu 'installer _ ...) #t)
    (('gnu 'build _ ...) #t)
    (('guix 'build _ ...) #t)
    (('guix 'read-print) #t)
    (_ #f)))

(define* (installer-steps #:key dry-run?)
  (let ((keymap-step (compute-keymap-step 'default dry-run?))
        (locale-step (compute-locale-step
                      #:locales-name "locales"
                      #:iso639-languages-name "iso639-languages"
                      #:iso3166-territories-name "iso3166-territories"
                      #:dry-run? dry-run?))
        (timezone-data #~(string-append #$tzdata
                                        "/share/zoneinfo/zone.tab")))
    #~(lambda (current-installer)
        ((installer-parameters-menu current-installer)
         (lambda ()
           ((installer-parameters-page current-installer)
            (lambda _
              (#$(compute-keymap-step 'param dry-run?)
               current-installer)))))
        (compute-steps current-installer
                       #:keymap-step #$keymap-step
                       #:logo-file #$(local-file "installer/aux-files/logo.txt")
                       #:locale-step #$locale-step
                       #:timezone-data #$timezone-data
                       #:pci-database #$(file-append pciutils
                                                     "/share/hwdata/pci.ids.gz")
                       #:dry-run? #$dry-run?))))

(define (provenance-sexp)
  "Return an sexp representing the currently-used channels, for logging
purposes."
  (match (match (current-channels)
           (() (and=> (repository->guix-channel (dirname (current-filename)))
                      list))
           (channels channels))
    (#f
     (warning (G_ "cannot determine installer provenance~%"))
     'unknown)
    ((channels ...)
     (map (lambda (channel)
            (let* ((uri (string->uri (channel-url channel)))
                   (url (if (or (not uri) (eq? 'file (uri-scheme uri)))
                            "local checkout"
                            (channel-url channel))))
             `(channel ,(channel-name channel) ,url ,(channel-commit channel))))
          channels))))

(define* (installer-program #:key dry-run?)
  "Return a file-like object that runs the given INSTALLER."
  (define init-gettext
    ;; Initialize gettext support, so that installer messages can be
    ;; translated.
    #~(begin
        (bindtextdomain "guix" (string-append #$guix "/share/locale"))
        (textdomain "guix")
        (setlocale LC_ALL "")))

  (define set-installer-path
    ;; Add the specified binary to PATH for later use by the installer.
    #~(let* ((inputs
              '#$(list bash             ;start subshells
                       connman          ;call connmanctl
                       cryptsetup
                       dosfstools       ;mkfs.fat
                       e2fsprogs        ;mkfs.ext4
                       lvm2-static      ;dmsetup
                       btrfs-progs
                       jfsutils         ;jfs_mkfs
                       ntfs-3g          ;mkfs.ntfs
                       xfsprogs         ;mkfs.xfs
                       kbd              ;chvt
                       util-linux       ;mkwap
                       nano
                       shadow
                       tar              ;dump
                       gzip             ;dump
                       coreutils)))
        (with-output-to-port (%make-void-port "w")
          (lambda ()
            (set-path-environment-variable "PATH" '("bin" "sbin") inputs)))))

  (define steps (installer-steps #:dry-run? dry-run?))
  (define modules
    (scheme-modules*
     (string-append (current-source-directory) "/..")
     "gnu/installer"))

  (define installer-builder
    ;; Note: Include GUIX as an extension to get all the (gnu system …), (gnu
    ;; packages …), etc. modules.
    (with-extensions (list guile-gcrypt guile-newt
                           guile-parted guile-bytestructures
                           guile-json-3 guile-git guile-webutils
                           guile-gnutls
                           guile-zlib           ;for (gnu build linux-modules)
                           guile-zstd           ;for (gnu build linux-modules)
                           (current-guix))
      (with-imported-modules `(,@(source-module-closure
                                  `(,@modules
                                    (gnu services herd)
                                    (guix build utils))
                                  #:select? module-to-import?)
                               ((guix config) => ,(make-config.scm)))
        #~(begin
            (use-modules (gnu installer)
                         (gnu installer compute)
                         (gnu installer record)
                         (gnu installer keymap)
                         (gnu installer steps)
                         (gnu installer dump)
                         (gnu installer final)
                         (gnu installer hostname)
                         (gnu installer kernel)
                         (gnu installer locale)
                         (gnu installer parted)
                         (gnu installer services)
                         (gnu installer timezone)
                         (gnu installer user)
                         (gnu installer utils)
                         (gnu installer newt)
                         ((gnu installer newt keymap)
                          #:select (keyboard-layout->configuration))
                         (gnu services herd)
                         (guix i18n)
                         (guix build utils)
                         (guix utils)
                         ((system repl debug)
                          #:select (terminal-width))
                         (ice-9 match)
                         (ice-9 textual-ports))

            ;; Enable core dump generation.
            (setrlimit 'core #f #f)
            (unless #$dry-run?
              (call-with-output-file "/proc/sys/kernel/core_pattern"
                (lambda (port)
                  (format port %core-dump))))

            ;; Initialize gettext support so that installers can use
            ;; (guix i18n) module.
            #$init-gettext

            ;; Add some binaries used by the installers to PATH.
            #$set-installer-path

            ;; Arrange for language and territory name translations to be
            ;; available.  We need them at run time, not just compile time,
            ;; because some territories have several corresponding languages
            ;; (e.g., "French" is always displayed as "français", but
            ;; "Belgium" could be translated to Dutch, French, or German.)
            (bindtextdomain "iso_639-3"           ;languages
                            #+(file-append iso-codes "/share/locale"))
            (bindtextdomain "iso_3166-1"          ;territories
                            #+(file-append iso-codes "/share/locale"))

            ;; Likewise for XKB keyboard layout names.
            (bindtextdomain "xkeyboard-config"
                            #+(file-append xkeyboard-config "/share/locale"))

            ;; Initialize 'terminal-width' in (system repl debug)
            ;; to a large-enough value to make backtrace more
            ;; verbose.
            (terminal-width 200)

            (define current-installer newt-installer)
            (define steps (#$steps current-installer))

            (installer-log-line "installer provenance: ~s"
                                '#$(provenance-sexp))

            (dynamic-wind
              (installer-init current-installer)
              (lambda ()
                (parameterize
                    ((%run-command-in-installer
                      (if #$dry-run?
                          dry-run-command
                          (installer-run-command current-installer))))
                  (catch #t
                    (lambda ()
                      (define results
                        (run-installer-steps
                         #:rewind-strategy 'menu
                         #:menu-proc (installer-menu-page current-installer)
                         #:steps steps
                         #:dry-run? #$dry-run?))

                      (let ((result (result-step results 'final)))
                        (unless #$dry-run?
                         (match (result-step results 'final)
                           ('success
                            ;; We did it!  Let's reboot!
                            (sync)
                            (stop-service 'root))
                           (_
                            ;; The installation failed, exit so that it is
                            ;; restarted by login.
                            #f)))))
                    (const #f)
                    (lambda (key . args)
                      (installer-log-line "crashing due to uncaught exception: ~s ~s"
                                          key args)
                      (define dump-dir
                        (prepare-dump key args #:result %current-result))

                      (define user-abort?
                        (match args
                          (((? user-abort-error? obj)) #t)
                          (_ #f)))

                      (define action
                        (if user-abort?
                            'dump
                            ((installer-exit-error current-installer)
                             (get-string-all
                              (open-input-file
                               (string-append dump-dir
                                              "/installer-backtrace"))))))

                      (match action
                        ('dump
                         (let* ((dump-files
                                 ((installer-dump-page current-installer)
                                  dump-dir))
                                (dump-archive
                                 (make-dump dump-dir dump-files)))
                           ((installer-report-page current-installer)
                            dump-archive)))
                        (_ #f))
                      (exit 1)))))

              (installer-exit current-installer))))))

  (program-file
   "installer"
   #~(begin
       ;; Set the default locale to install unicode support.  For
       ;; some reason, unicode support is not correctly installed
       ;; when calling this in 'installer-builder'.
       (setenv "LANG" "en_US.UTF-8")
       (execl #$(program-file "installer-real" installer-builder
                              #:guile guile-3.0-latest)
              "installer-real"))))
