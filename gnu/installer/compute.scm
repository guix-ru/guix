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

(define-module (gnu installer compute)
  #:use-module (ice-9 match)

  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module ((guix self) #:select (make-config.scm))
  #:use-module (guix ui)
  #:use-module (guix utils)

  #:autoload (gnu installer hostname) (hostname->configuration)
  #:autoload (gnu installer kernel) (kernel->configuration)
  #:autoload (gnu installer locale) (locale->configuration)
  #:autoload (gnu installer newt) (newt-installer)
  #:autoload (gnu installer newt keymap) (keyboard-layout->configuration)
  #:autoload (gnu installer parted) (user-partitions->configuration)
  #:use-module (gnu installer record)
  #:autoload (gnu installer services) (system-services->configuration)
  #:autoload (gnu installer steps) (%installer-configuration-file
                                    installer-step
                                    result-step
                                    run-installer-steps)
  #:autoload (gnu installer timezone) (posix-tz->configuration)
  #:autoload (gnu installer user) (users->configuration)
  #:use-module (gnu installer utils)

  #:use-module (gnu packages base)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages xorg)
  #:use-module (gnu system locale)

  #:export (compute-keymap-step
            compute-locale-step
            compute-steps
            run-installer))

(define not-config?
  ;; Select (guix …) and (gnu …) modules, except (guix config).
  (match-lambda
    (('guix 'config) #f)
    (('guix _ ...) #t)
    (('gnu _ ...) #t)
    (_ #f)))

(define* (build-compiled-file name locale-builder)
  "Return a file-like object that evaluates the gexp LOCALE-BUILDER and store
its result in the scheme file NAME. The derivation will also build a compiled
version of this file."
  (define set-utf8-locale
    #~(begin
        (setenv "LOCPATH"
                #$(file-append
                   (libc-utf8-locales-for-target) "/lib/locale/"
                   (version-major+minor
                    (package-version (libc-utf8-locales-for-target)))))
        (setlocale LC_ALL "en_US.utf8")))

  (define builder
    (with-extensions (list guile-json-3)
      (with-imported-modules `(,@(source-module-closure
                                  '((gnu installer locale))
                                  #:select? not-config?)
                               ((guix config) => ,(make-config.scm)))
        #~(begin
            (use-modules (gnu installer locale))

            ;; The locale files contain non-ASCII characters.
            #$set-utf8-locale

            (mkdir #$output)
            (let ((locale-file
                   (string-append #$output "/" #$name ".scm"))
                  (locale-compiled-file
                   (string-append #$output "/" #$name ".go")))
              (call-with-output-file locale-file
                (lambda (port)
                  (write #$locale-builder port)))
              (compile-file locale-file
                            #:output-file locale-compiled-file))))))
  (computed-file name builder))

(define apply-locale
  ;; Install the specified locale.
  (with-imported-modules (source-module-closure '((gnu services herd)))
    #~(lambda (locale)
        (false-if-exception
         (setlocale LC_ALL locale))

        ;; Restart the documentation viewer so it displays the manual in
        ;; language that corresponds to LOCALE.  Make sure that nothing is
        ;; printed on the console.
        (parameterize ((shepherd-message-port
                        (%make-void-port "w")))
          (stop-service 'term-tty2)
          (start-service 'term-tty2 (list locale))))))

(define* (compute-locale-step #:key
                              locales-name
                              iso639-languages-name
                              iso3166-territories-name
                              dry-run?)
  "Return a gexp that run the locale-page of INSTALLER, and install the
selected locale. The list of locales, languages and territories passed to
locale-page are computed in derivations named respectively LOCALES-NAME,
ISO639-LANGUAGES-NAME and ISO3166-TERRITORIES-NAME. Those lists are compiled,
so that when the installer is run, all the lengthy operations have already
been performed at build time."
  (define (compiled-file-loader file name)
    #~(load-compiled
       (string-append #$file "/" #$name ".go")))

  (let* ((supported-locales #~(supported-locales->locales
                               #+(glibc-supported-locales)))

         ;; Note: Use the latest version of 'iso-codes', including
         ;; Guix-specific changes, so that all languages known to glibc and
         ;; returned by 'glibc-supported-locales'.
         (iso-codes #~(string-append #$iso-codes "/share/iso-codes/json/"))
         (iso639-3 #~(string-append #$iso-codes "iso_639-3.json"))
         (iso639-5 #~(string-append #$iso-codes "iso_639-5.json"))
         (iso3166 #~(string-append #$iso-codes "iso_3166-1.json"))
         (locales-file (build-compiled-file
                        locales-name
                        #~`(quote ,#$supported-locales)))
         (iso639-file (build-compiled-file
                       iso639-languages-name
                       #~`(quote ,(iso639->iso639-languages
                                   #$supported-locales
                                   #$iso639-3 #$iso639-5))))
         (iso3166-file (build-compiled-file
                        iso3166-territories-name
                        #~`(quote ,(iso3166->iso3166-territories #$iso3166))))
         (locales-loader (compiled-file-loader locales-file
                                               locales-name))
         (iso639-loader (compiled-file-loader iso639-file
                                              iso639-languages-name))
         (iso3166-loader (compiled-file-loader iso3166-file
                                               iso3166-territories-name)))
    #~(lambda (current-installer)
        (let ((result
               ((installer-locale-page current-installer)
                #:supported-locales #$locales-loader
                #:iso639-languages #$iso639-loader
                #:iso3166-territories #$iso3166-loader
                #:dry-run? #$dry-run?)))
          (if #$dry-run?
              '()
              (#$apply-locale result))
          result))))

(define apply-keymap
  ;; Apply the specified keymap. Use the default keyboard model.
  #~(match-lambda
      ((layout variant options)
       (kmscon-update-keymap (default-keyboard-model)
                             layout variant options))))

(define (compute-keymap-step context dry-run?)
  "Return a gexp that runs the keymap-page of INSTALLER and install the
selected keymap."
  #~(lambda (current-installer)
      (let ((result
             (call-with-values
                 (lambda ()
                   (xkb-rules->models+layouts
                    (string-append #$xkeyboard-config
                                   "/share/X11/xkb/rules/base.xml")))
               (lambda (models layouts)
                 ((installer-keymap-page current-installer)
                  layouts '#$context #$dry-run?)))))
        (and result (#$apply-keymap result))
        result)))

(define* (compute-steps current-installer #:key
                        dry-run?
                        keymap-step
                        locale-step
                        (logo-file "/dev/null")
                        pci-database
                        timezone-data)
  (list
   ;; Ask the user to choose a locale among those supported by
   ;; the glibc.  Install the selected locale right away, so that
   ;; the user may benefit from any available translation for the
   ;; installer messages.
   (installer-step
    (id 'locale)
    (description (G_ "Locale"))
    (compute (lambda _
               (and locale-step
                    (locale-step current-installer))))
    (configuration-formatter locale->configuration))

   ;; Welcome the user and ask them to choose between manual
   ;; installation and graphical install.
   (installer-step
    (id 'welcome)
    (compute (lambda _
               ((installer-welcome-page current-installer)
                logo-file
                #:pci-database pci-database))))

   ;; Ask the user to select a timezone under glibc format.
   (installer-step
    (id 'timezone)
    (description (G_ "Timezone"))
    (compute (lambda _
               (and=> timezone-data
                      (installer-timezone-page current-installer))))
    (configuration-formatter posix-tz->configuration))

   ;; The installer runs in a kmscon virtual terminal where loadkeys
   ;; won't work. kmscon uses libxkbcommon as a backend for keyboard
   ;; input. It is possible to update kmscon current keymap by sending
   ;; it a keyboard model, layout, variant and options, in a somehow
   ;; similar way as what is done with setxkbmap utility.
   ;;
   ;; So ask for a keyboard model, layout and variant to update the
   ;; current kmscon keymap.  For non-Latin layouts, we add an
   ;; appropriate second layout and toggle via Alt+Shift.
   (installer-step
    (id 'keymap)
    (description (G_ "Keyboard mapping selection"))
    (compute (lambda _
               (if (or dry-run? (not keymap-step))
                   '("en" "US" #f)
                   (keymap-step current-installer))))
    (configuration-formatter keyboard-layout->configuration))

   ;; Ask the user to input a hostname for the system.
   (installer-step
    (id 'hostname)
    (description (G_ "Hostname"))
    (compute (lambda _
               ((installer-hostname-page current-installer))))
    (configuration-formatter hostname->configuration))

   ;; Ask the user to select the kernel for the system,
   ;; for x86 systems only.
   (installer-step
    (id 'kernel)
    (description (G_ "Kernel"))
    (compute (lambda _
               (if (target-x86?)
                   ((installer-kernel-page current-installer))
                   '())))
    (configuration-formatter (lambda (result)
                               (kernel->configuration result dry-run?))))

   ;; Provide an interface above connmanctl, so that the user can select
   ;; a network susceptible to acces Internet.
   (installer-step
    (id 'network)
    (description (G_ "Network selection"))
    (compute (lambda _
               (if dry-run?
                   '()
                   ((installer-network-page current-installer))))))

   ;; Ask whether to enable substitute server discovery.
   (installer-step
    (id 'substitutes)
    (description (G_ "Substitute server discovery"))
    (compute (lambda _
               (if dry-run?
                   '()
                   ((installer-substitutes-page current-installer))))))

   ;; Prompt for users (name, group and home directory).
   (installer-step
    (id 'user)
    (description (G_ "User creation"))
    (compute (lambda _
               ((installer-user-page current-installer))))
    (configuration-formatter users->configuration))

   ;; Ask the user to choose one or many desktop environment(s).
   (installer-step
    (id 'services)
    (description (G_ "Services"))
    (compute (lambda _
               ((installer-services-page current-installer))))
    (configuration-formatter system-services->configuration))

   ;; Run a partitioning tool allowing the user to modify
   ;; partition tables, partitions and their mount points.
   ;; Do this last so the user has something to boot if any
   ;; of the previous steps didn't go as expected.
   (installer-step
    (id 'partition)
    (description (G_ "Partitioning"))
    (compute (lambda _
               (if dry-run?
                   '()
                   ((installer-partitioning-page current-installer)))))
    (configuration-formatter user-partitions->configuration))

   (installer-step
    (id 'final)
    (description (G_ "Configuration file"))
    (compute
     (lambda (result prev-steps)
       ((installer-final-page current-installer)
        result prev-steps dry-run?))))))

(define* (run-installer #:key dry-run?)
  "To the installer from Guile without building it:
    ./pre-inst-env guile -c '((@ (gnu installer compute) run-installer) #:dry-run? #t)'
when using #:dry-run? #t, no root access is required and the LOCALE, KEYMAP,
and PARTITION pages are skipped."
  (let* ((dry-run? (and dry-run? 'guile))
         (current-installer newt-installer)
         (logo-file (string-append "gnu/installer/aux-files/logo.txt"))
         (steps (compute-steps current-installer
                               #:logo-file logo-file
                               #:dry-run? dry-run?)))
    (catch #t
      (lambda _
        ((installer-init current-installer))
        (parameterize ((%run-command-in-installer
                             (if dry-run?
                                 dry-run-command
                                 (installer-run-command current-installer)))
                            (%installer-configuration-file
                             (if dry-run?
                                 "config.scm"
                                 (%installer-configuration-file))))
          (let* ((results (run-installer-steps
                           #:rewind-strategy 'menu
                           #:menu-proc (installer-menu-page current-installer)
                           #:steps steps
                           #:dry-run? dry-run?)))
            (result-step results 'final))))
      (const #f)
      (lambda (key . args)
        ((installer-exit current-installer))
        (display-backtrace (make-stack #t) (current-error-port))
        (apply throw key args)))))
