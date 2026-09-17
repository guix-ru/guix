;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2022 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2022 dan <i@dan.games>
;;; Copyright © 2026 c4droid <c4droid@foxmail.com>
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

(define-module (gnu system images wsl2)
  #:use-module (gnu bootloader)
  #:use-module (gnu image)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system image)
  #:use-module (gnu system shadow)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:select (fsdg-compatible))
  #:export (wsl-boot-program
            make-wsl-os
            make-wsl-image
            wsl2-image))

(define (wsl-boot-program user)
  "Program that runs the system boot script, then starts a login shell as
USER."
  (program-file
   "wsl-boot-program"
   (with-imported-modules '((guix build syscalls))
     #~(begin
         (use-modules (guix build syscalls))
         (unless (file-exists? "/run/current-system")
           (let ((shepherd-socket "/var/run/shepherd/socket"))
             ;; Clean up this file so we can wait for it later.
             (when (file-exists? shepherd-socket)
               (delete-file shepherd-socket))

             ;; Child process boots the system and is replaced by shepherd.
             (when (zero? (primitive-fork))
               (let* ((system-generation
                       (readlink "/var/guix/profiles/system"))
                      (system (readlink
                               (string-append
                                (if (absolute-file-name? system-generation)
                                    ""
                                    "/var/guix/profiles/")
                                system-generation))))
                 (setenv "GUIX_NEW_SYSTEM" system)
                 (execl #$(file-append guile-3.0 "/bin/guile")
                        "guile"
                        "--no-auto-compile"
                        (string-append system "/boot"))))

             ;; Parent process waits for shepherd before continuing.
             (while (not (file-exists? shepherd-socket))
               (sleep 1))))

         (let* ((pw (getpw #$user))
                (shell (passwd:shell pw))
                (sudo #+(file-append sudo "/bin/sudo"))
                (args (cdr (command-line)))
                (uid (passwd:uid pw))
                (gid (passwd:gid pw))
                (runtime-dir (string-append "/run/user/"
                                            (number->string uid))))
           ;; Save the value of $PATH set by WSL.  Useful for finding
           ;; Windows binaries to run with WSL's binfmt interop.
           (setenv "WSLPATH" (getenv "PATH"))

           ;; /run is mounted with the nosuid flag by WSL.  This prevents
           ;; /run/privileged/bin from working.  Remount it without this flag
           ;; as a workaround.  See:
           ;; https://github.com/microsoft/WSL/issues/8716.
           (mount #f "/run" #f
                  MS_REMOUNT
                  #:update-mtab? #f)

           ;; Create XDG_RUNTIME_DIR for the login user.
           (unless (file-exists? runtime-dir)
             (mkdir runtime-dir)
             (chown runtime-dir uid gid))
           (setenv "XDG_RUNTIME_DIR" runtime-dir)

           ;; Start login shell as user.
           (apply execl sudo "sudo"
                  "--preserve-env=WSLPATH,XDG_RUNTIME_DIR"
                  "-u" #$user
                  "--"
                  shell "-l" args))))))

(define dummy-package
  (package
    (name "dummy")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:target #f
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((out (assoc-ref %outputs "out"))
                          (dummy (string-append out "/dummy")))
                     (mkdir-p out)
                     (call-with-output-file dummy
                       (const #t))))))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license (fsdg-compatible "dummy"))))

(define dummy-bootloader
  (bootloader
    (name 'dummy-bootloader)
    (package dummy-package)
    (configuration-file "/var/lib/wsl-bootloader.cfg")
    (configuration-file-generator
     (lambda (. _rest)
       (computed-file "dummy-bootloader"
                      #~(call-with-output-file #$output
                          (lambda (port) (display "" port))))))
    (installer #~(const #t))))

(define dummy-kernel dummy-package)

(define (dummy-initrd . _rest)
  (plain-file "dummy-initrd" ""))

(define* (wsl-conf-file #:key
                        (default-user "guest")
                        (hostname "gnu")
                        (automount? #t)
                        (mount-root "/mnt")
                        (mount-options "metadata,umask=22,fmask=11")
                        (generate-hosts? #t)
                        (generate-resolv? #t)
                        (interop? #t)
                        (append-windows-path? #t)
                        (systemd? #f))
  "Return a plain-file containing the content for /etc/wsl.conf.
This configuration file is read by the WSL kernel when starting the
distribution.  It controls the default user, systemd mode, automount
settings, network behavior, and interop features."
  (plain-file
   "wsl.conf"
   (string-append
    "[user]\n"
    "default=" default-user "\n\n"
    "[boot]\n"
    "systemd=" (if systemd? "true" "false") "\n\n"
    "[automount]\n"
    "enabled=" (if automount? "true" "false") "\n"
    "root=" mount-root "\n"
    "options=" mount-options "\n\n"
    "[network]\n"
    "hostname=" hostname "\n"
    "generateHosts=" (if generate-hosts? "true" "false") "\n"
    "generateResolvConf=" (if generate-resolv? "true" "false") "\n\n"
    "[interop]\n"
    "enabled=" (if interop? "true" "false") "\n"
    "appendWindowsPath=" (if append-windows-path? "true" "false") "\n")))

(define* (wsl-distribution-conf #:key
                                (name "guix")
                                (default-uid 1000)
                                (oobe-command #f)
                                (icon #f)
                                (terminal? #t))
  "Return a plain-file containing the content for /etc/wsl-distribution.conf.
This file consumed by the WSL installer (e.g., when double-clicking a .wsl
file or using 'wsl --install --from-file') to determine the distribution name,
default UID, OOBE command, shortcut settings, and Windows Terminal integration."
  (plain-file
   "wsl-distribution.conf"
   (string-append
    "[oobe]\n"
    (if oobe-command
        (string-append "command=" oobe-command "\n")
        "")
    "defaultUid=" (number->string default-uid) "\n"
    "defaultName=" name "\n\n"
    "[shortcut]\n"
    "enabled=true\n"
    (if icon (string-append "icon=" icon "\n") "")
    "\n[windowsterminal]\n"
    "enabled=" (if terminal? "true" "false") "\n")))

(define* (make-wsl-os #:key
                      (user "guest")
                      (uid 1000)
                      (hostname "gnu")
                      (extra-conf '()))
  "Return an operating system suitable for use as WSL2 distribution.
USER, UID, and HOSTNAME are the default user account name, its UID, and the
system host name.  EXTRA-CONF is an optional list of additional files to be
placed under /etc; each element is a pair (FILE-NAME . FILE-LIKE), where
FILE-NAME is a string and FILE-LIKE is a file-like object (e.g., a plain-file
or local-file).  These files will be installed alongside wsl.conf and
wsl-distribution.conf in the generated image."
  (operating-system
    (host-name hostname)
    (timezone "Etc/UTC")
    (bootloader (bootloader-configuration
                  (bootloader dummy-bootloader)))
    (kernel dummy-kernel)
    (initrd dummy-initrd)
    (initrd-modules '())
    (firmware '())
    (file-systems '())
    (users (cons* (user-account
                    (name user)
                    (uid uid)
                    (group "users")
                    (supplementary-groups '("wheel"))
                    (password "")
                    (comment "WSL user"))
                  (user-account
                    (inherit %root-account)
                    (shell (wsl-boot-program user)))
                  %base-user-accounts))
    (services
     (list
      (service guix-service-type)
      (simple-service 'wsl-meta etc-service-type
                      `(("wsl.conf" ,(wsl-conf-file #:default-user user
                                                    #:hostname (operating-system-host-name this-operating-system)))
                        ("wsl-distribution.conf" ,(wsl-distribution-conf #:name "guix"
                                                                         #:default-uid uid))
                        ,@extra-conf))
      (service special-files-service-type
               `(("/bin/sh" ,(file-append bash "/bin/bash"))
                 ("/bin/mount" ,(file-append util-linux "/bin/mount"))
                 ("/usr/bin/env" ,(file-append coreutils "/bin/env"))))))))

(define* (make-wsl-image #:key
                         (user "guest")
                         (uid 1000)
                         (hostname "gnu")
                         (extra-conf '()))
  "Return an image record for a WSL2 distribution.
All keyword arguments are passed to 'make-wsl-os'."
  (image
    (inherit
     (os->image (make-wsl-os #:user user
                             #:uid uid
                             #:hostname hostname
                             #:extra-conf extra-conf)
                #:type wsl2-image-type))
    (name 'wsl-image)))

(define wsl2-image
  (make-wsl-image))

wsl2-image
