;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Maya Tomasek <maya.tomasek@disroot.org>
;;; Copyright © 2023 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2023 Felix Lechner <felix.lechner@lease-up.com>
;;; Copyright © 2024 Justin Veilleux <terramorpha@cock.li>
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

(define-module (gnu packages zig-xyz)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system zig)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages man)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages zig))

(define-public river
  (package
    (name "river")
    (version "0.3.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/river/river")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "023c3kqqmnd1vs5q7p1xgspkqzc59v61rpp615c06lpswi0a6him"))))
    (build-system zig-build-system)
    (arguments
     (list #:install-source? #f
           #:zig-release-type "safe"
           #:zig-build-flags
           #~(list "-Dpie" "-Dxwayland")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-path
                 (lambda _
                   (substitute* "build.zig"
                     (("/bin/sh") (which "sh")))))
               (add-after 'install 'install-wayland-session
                 (lambda _
                   (let ((wayland-sessions
                          (string-append #$output "/share/wayland-sessions")))
                     (mkdir-p wayland-sessions)
                     (install-file "contrib/river.desktop"
                                   wayland-sessions)))))))
    (inputs
     (list libevdev
           zig-wayland
           zig-wlroots
           zig-xkbcommon))
    (native-inputs
     (list pkg-config
           scdoc))
    (home-page "https://isaacfreund.com/software/river/")
    (synopsis "Dynamic tiling Wayland compositor")
    (description
     "River is a dynamic tiling Wayland compositor with flexible
runtime configuration.  It can run nested in an X11/Wayland session or also
directly from a tty using KMS/DRM.")
    (license license:gpl3)))

(define-public tigerbeetle
  (package
    (name "tigerbeetle")
    (version "0.13.35")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tigerbeetledb/tigerbeetle")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x8msknvq8s6vnlczq5fxmaiqvig2sbcv60c3x8zbgr28dsqpmll"))))
    (build-system zig-build-system)
    (arguments
     (list
      #:zig zig-0.9
      #:install-source? #f
      #:zig-release-type "safe"))
    (synopsis "Distributed financial accounting database")
    (description "TigerBeetle is a financial accounting database designed for
mission-critical safety and performance for financial services.")
    (home-page "https://github.com/tigerbeetledb/tigerbeetle")
    (license license:asl2.0)))

(define-public waylock
  (package
    (name "waylock")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/ifreund/waylock")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jlq23cb5069sa5ipshhj82a9rn30frflwi9skp2kplqpxm15wwd"))))
    (build-system zig-build-system)
    (arguments
     (list
      #:install-source? #f
      ;; No tests.
      #:tests? #f
      #:zig-release-type "safe"
      #:zig-build-flags
      #~(list "-Dpie")))
    (inputs (list linux-pam zig-wayland zig-xkbcommon))
    (native-inputs (list pkg-config scdoc))
    (home-page "https://codeberg.org/ifreund/waylock")
    (synopsis "Wayland screen locker")
    (description
     "Waylock is a small screen locker for Wayland compositors implementing the
@code{ext-session-lock-v1} protocol.")
    (license license:expat)))

(define-public zig-diffz
  (let ((commit "420fcb22306ffd4c9c3c761863dfbb6bdbb18a73")
        (revision "0"))
    (package
      (name "zig-diffz")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ziglibs/diffz")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0rbcprl2c1kbd7xfwdqycz8r5grm069fcy6fafi14cnak77i0xyi"))))
      (build-system zig-build-system)
      (arguments (list #:skip-build? #t))
      (synopsis "Implementation of go-diff's diffmatchpatch in Zig")
      (description
       "This package provides a Zig implementation of @code{diffmatchpatch} in
@code{go-github-com-sergi-go-diff}.")
      (home-page "https://github.com/ziglibs/diffz")
      (license license:expat))))

(define-public zig-doctest
  (let ((commit "725a93787cb1e24e2a183971d52f4ea12d9e1757")
        (revision "0"))
    (package
      (name "zig-doctest")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/kristoff-it/zig-doctest")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "05xk4vz5lzv3660phlyhnpd7g8j49zlv3hfsa8bl0flj3mvmhvzc"))))
      (build-system zig-build-system)
      (arguments
       (list #:install-source? #f
             ;; No test suite.
             #:tests? #f
             #:zig-release-type "safe"))
      (synopsis "Tool for testing Zig code snippets")
      (description
       "@command{doctest} is a code rendering tool being able to provide
syntax highlighting and run code snippets to ensure they behave as expected.")
      (home-page "https://github.com/kristoff-it/zig-doctest")
      (license license:expat))))

(define-public zig-httpz
  ;; No releases, latest commit from zig-0.13 branch.
  (let ((commit "7d2ddae87af9b110783085c0ea6b03985faa4584")
        (revision "0"))
    (package
      (name "zig-httpz")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/karlseguin/http.zig")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "02hixvyx1r04lg0nzvhkyrqwcwm8m8rs8hm01n2nzw6jv935frh8"))
         (snippet
          (rename-zig-dependencies
           '(("metrics" . "zig-metrics")
             ("websocket" . "zig-websocket"))))))
      (build-system zig-build-system)
      (arguments
       (list #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'fix-paths
                   (lambda _
                     (substitute* "example/simple.zig"
                       (("example/index.html" file)
                        (string-append
                         (zig-source-install-path #$output) "/" file))))))))
      (propagated-inputs (list zig-metrics zig-websocket))
      (home-page "https://github.com/karlseguin/http.zig")
      (synopsis "HTTP/1.1 server for Zig")
      (description "This package provides an HTTP/1.1 server for Zig.")
      (license license:expat))))

(define-public zig-ini
  ;; No releases.
  (let ((commit "e18d36665905c1e7ba0c1ce3e8780076b33e3002")
        (revision "0"))
    (package
      (name "zig-ini")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ziglibs/ini")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0b1688g6kvcrbr8pbl3glv09fpd27p5z2bif8jmn6km6myq5fs1y"))))
      (build-system zig-build-system)
      (home-page "https://github.com/ziglibs/ini")
      (synopsis "INI parser library")
      (description
       "This package provides an INI parser library with:
@itemize
@item Raw record reading.
@item Leading/trailing whitespace removal.
@item Comments based on @code{;} and @code{#}.
@item C and Zig API.
@end itemize")
      (license license:expat))))

(define-public zig-known-folders
  (let ((commit "1cceeb70e77dec941a4178160ff6c8d05a74de6f")
        (revision "0"))
    (package
      (name "zig-known-folders")
      (version (git-version "0.7.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ziglibs/known-folders")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1kr58ragd6nk29ps0fwc4r3zxv2javkiq4vny4zwx6wqqid98nld"))))
      (build-system zig-build-system)
      (synopsis "Zig library to access well-known folders")
      (description
       "This package provides a Zig library for accessing well-known folders
across several operating systems.")
      (home-page "https://github.com/ziglibs/known-folders")
      (license license:expat))))

(define-public zig-logz
  ;; No releases, latest commit from zig-0.13 branch.
  (let ((commit "5d5bbaeb98ad30cda30c60ab0f2d1a7fb4890676")
        (revision "0"))
    (package
      (name "zig-logz")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/karlseguin/log.zig")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "01xihyvyx3rpv0kvjh6mg1b99d6agq683q4iyn39nwqb6ma0i0sz"))
                (snippet
                 (rename-zig-dependencies
                  '(("metrics" . "zig-metrics"))))))
      (build-system zig-build-system)
      (propagated-inputs (list zig-metrics))
      (home-page "https://github.com/karlseguin/log.zig")
      (synopsis "Structured logger for Zig")
      (description
       "@code{logz} is an opinionated structured logger that outputs to stdout,
stderr, a file or a custom writer using logfmt or JSON.  It aims to minimize
runtime memory allocation by using a pool of pre-allocated loggers.")
      (license license:expat))))

(define-public zig-metrics
  ;; No releases, latest commit from zig-0.13 branch.
  (let ((commit "fcf9e94fa54a20f4954e9821801c32e44d407a2f")
        (revision "0"))
    (package
      (name "zig-metrics")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/karlseguin/metrics.zig")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1vvfy97ak5xwvwv3rkqsgsj644298d3c75645386qf4psija6a25"))
                (modules '((guix build utils)))
                (snippet
                 #~(substitute* "example/lib/metrics.zig"
                     (("try (Metrics.Active.init)" _ keep) keep)))))
      (build-system zig-build-system)
      (home-page "https://github.com/karlseguin/metrics.zig")
      (synopsis "Prometheus metrics library")
      (description
       "@code{metrics} is a Prometheus metrics library with support for
@code{counters}, @code{gauges} and @code{histograms} metric types and their
labeled variants.")
      (license license:expat))))

(define-public zig-pixman
  (package
    (name "zig-pixman")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/ifreund/zig-pixman")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0il6nw51kf08bcxpf45n7h78k1iyfi1zarcvpb7n19g2r48dkiyd"))))
    (build-system zig-build-system)
    (arguments (list #:skip-build? #t))
    (propagated-inputs (list pixman))
    (synopsis "Zig bindings for Pixman")
    (description "This package provides Zig bindings for @code{pixman}.")
    (home-page "https://codeberg.org/ifreund/zig-pixman")
    (license license:expat)))

(define-public zig-wayland
  (package
    (name "zig-wayland")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/ifreund/zig-wayland")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1cf5085f6c0yly4fcr49jry3mh12bybw98x5lvickl6w5gxsvy3n"))))
    (build-system zig-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'configure 'fix-cross-compilation
                 (lambda _
                   (substitute* "build.zig"
                     (("pkg-config") (getenv "PKG_CONFIG"))))))))
    (propagated-inputs (list wayland wayland-protocols))
    (native-inputs (list pkg-config wayland))
    (synopsis "Zig Wayland bindings and protocol scanner")
    (description
     "This package provides Zig bindings for @code{wayland} and a @code{Scanner}
interface.")
    (home-page "https://codeberg.org/ifreund/zig-wayland")
    (license license:expat)))

(define-public zig-websocket
  ;; No releases, latest commit from zig-0.13 branch.
  (let ((commit "cf89cb8b9c61c99982ced19da54890983e226245")
        (revision "0"))
    (package
      (name "zig-websocket")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/karlseguin/websocket.zig")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1q85pvc1dgyj5zdpk3xav1zgj6857p0nygndz3nc5xlm260hrck8"))))
      (build-system zig-build-system)
      (home-page "https://github.com/karlseguin/websocket.zig")
      (synopsis "WebSocket implementation for Zig")
      (description "This package provides a Zig WebSocket server.")
      (license license:expat))))

(define-public zig-wlroots
  (package
    (name "zig-wlroots")
    (version "0.18.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/ifreund/zig-wlroots")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0aa9xck8al1n27dzc9zkm7xp88g87x7h5cl2jlpyxjgbgahancr8"))))
    (build-system zig-build-system)
    (arguments
     (list #:zig-build-flags
           #~(list "-Denable-tests")
           #:zig-test-flags
           #~(list "-Denable-tests")))
    (propagated-inputs
     (list wlroots
           zig-pixman
           zig-wayland
           zig-xkbcommon))
    (native-inputs
     (list pkg-config
           wlroots))
    (synopsis "Zig bindings for wlroots")
    (description "This package provides Zig bindings for @code{wlroots}.")
    (home-page "https://codeberg.org/ifreund/zig-wlroots")
    (license license:expat)))

(define-public zig-xkbcommon
  (package
    (name "zig-xkbcommon")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/ifreund/zig-xkbcommon")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16f59n7l2gcpnq8gb4v8skr4jhb2l6ax75rna92nqzj15f4ikqag"))))
    (build-system zig-build-system)
    (arguments (list #:skip-build? #t))
    (propagated-inputs (list libxkbcommon))
    (synopsis "Zig bindings for libxkbcommon")
    (description "This package provides Zig bindings for @code{libxkbcommon}.")
    (home-page "https://codeberg.org/ifreund/zig-xkbcommon")
    (license license:expat)))

(define-public zig-zls-0.10
  (package
    (name "zig-zls")
    (version "0.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zigtools/zls")
                    (commit version)
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lsks7h3z2m4psyn9mwdylv1d6a9i3z54ssadiz76w0clbh8ch9k"))))
    (build-system zig-build-system)
    (inputs (list zig-0.10 python))
    (arguments
     (list #:zig zig-0.10
           #:install-source? #f
           #:zig-release-type "safe"
           ;; The tests fail with memory leaks.
           #:tests? #f))
    (synopsis "Zig language server")
    (description
     "Zig Language Server is a language server implementing the @acronym{LSP,
Language Server Protocol} for the Zig programming language.")
    (home-page "https://github.com/zigtools/zls")
    (license license:expat)))

(define-public zig-zls-0.12
  (package
    (inherit zig-zls-0.10)
    (name "zig-zls")
    (version "0.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zigtools/zls")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ini1ifa9b0v2ika3sqsiiv2p7v9npfslss45280yxwn2pjqmn7n"))
       (snippet
        (rename-zig-dependencies
         '(("diffz" . "zig-diffz")
           ("known_folders" . "zig-known-folders"))))))
    (build-system zig-build-system)
    (arguments
     (let ((version-data-path
            #~(string-append "-Dversion_data_path="
                             #+(package-source (this-package-input "zig"))
                             "/doc/langref.html.in")))
       (list #:zig (this-package-input "zig")
             #:install-source? #f
             #:zig-release-type "safe"
             #:zig-build-flags
             #~(list #$version-data-path "-Dpie")
             #:zig-test-flags
             #~(list #$version-data-path))))
    (inputs (list zig-0.12 zig-diffz zig-known-folders))))

(define-public zig-zls-0.13
  (let ((base zig-zls-0.12))
    (package
      (inherit base)
      (name "zig-zls")
      (version "0.13.0")
      (source (origin
                (inherit (package-source base))
                (uri (git-reference
                      (url "https://github.com/zigtools/zls")
                      (commit version)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1dbg06v136yjcs9grc6xwsmv0cm39c0sdkh5vzn7h1qxxka6ixlp"))))
      (build-system zig-build-system)
      (inputs
       (modify-inputs (package-inputs base)
         (replace "zig" zig-0.13))))))

(define-public zig-zul
  ;; No releases.
  (let ((commit "ae0c27350c0db6b460f22cba30b6b0c4a02d1ffd")
        (revision "0"))
    (package
      (name "zig-zul")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/karlseguin/zul")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0h0581hhm6xzhb0s43aa357x4c4cxfvfpy24q4f5ss9jrgfc1g52"))
                (modules '((guix build utils)))
                (snippet
                 ;; XXX: Some of the four http.Request/Response tests need to be
                 ;; skipped, otherwise ConnectionRefused error occurs.  (However
                 ;; all of them pass when running independently.)
                 #~(substitute* "src/http.zig"
                     ((".*http\\.(Request|Response).*" line)
                      (string-append
                       line "if (true) return error.SkipZigTest;"))))))
      (build-system zig-build-system)
      (home-page "https://www.goblgobl.com/zul/")
      (synopsis "Zig utility library")
      (description
       "@code{zul} enhances Zig's standard library by wrapping common tasks
(e.g., reading lines from a file) with simpler APIs and adding new functionality
(e.g., @code{UUID} type).")
      (license license:expat))))
