;;; Copyright © 2023 Steve George <steve@futurile.net>
;;; Copyright © 2024 Roman Scherer <roman@burningswell.com>
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

(define-module (gnu packages crates-audio)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-gtk)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config))

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;

(define-public rust-asio-sys-0.2
  (package
    (name "rust-asio-sys")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "asio-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "16lbavksj2aasadyxbdnbrll6a1m8cwl4skbxgbvr1ma2wpwv82c"))))
    (build-system cargo-build-system)
    (arguments
     (list #:skip-build? #t
           #:cargo-inputs `(("rust-bindgen" ,rust-bindgen-0.56)
                            ("rust-cc" ,rust-cc-1)
                            ("rust-num-derive" ,rust-num-derive-0.3)
                            ("rust-num-traits" ,rust-num-traits-0.2)
                            ("rust-once-cell" ,rust-once-cell-1)
                            ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/RustAudio/cpal/")
    (synopsis
     "Low-level interface and binding generation for the Steinberg ASIO SDK")
    (description
     "Low-level interface and binding generation for the Steinberg ASIO SDK.")
    (license license:asl2.0)))

(define-public rust-jack-0.10
  (package
    (name "rust-jack")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jack" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0djs3j0icxbzbivhj73vgjrvjw6ncpfak2vyxjcbn4wvl9ajcwnf"))))
    (build-system cargo-build-system)
    (arguments
     (list #:tests? #f
           #:cargo-inputs
           `(("rust-bitflags" ,rust-bitflags-1)
             ("rust-jack-sys" ,rust-jack-sys-0.4)
             ("rust-lazy-static" ,rust-lazy-static-1)
             ("rust-libc" ,rust-libc-0.2)
             ("rust-log" ,rust-log-0.4))
           #:cargo-development-inputs
           `(("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5))))
    (native-inputs (list pkg-config))
    (inputs (list jack-2))
    (home-page "https://github.com/RustAudio/rust-jack")
    (synopsis "Real time audio and midi with JACK")
    (description "Real time audio and midi with JACK.")
    (license license:expat)))

(define-public rust-jack-0.8
  (package
    (inherit rust-jack-0.10)
    (name "rust-jack")
    (version "0.8.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "jack" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0lz10s0n2gy128m65pf96is9ip00vfgvnkfja0y9ydmv24pw2ajx"))))
    (arguments
     (list #:tests? #f
           #:cargo-inputs `(("rust-bitflags" ,rust-bitflags-1)
                            ("rust-jack-sys" ,rust-jack-sys-0.2)
                            ("rust-lazy-static" ,rust-lazy-static-1)
                            ("rust-libc" ,rust-libc-0.2)
                            ("rust-log" ,rust-log-0.4)
                            ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5))))))

(define-public rust-jack-sys-0.4
  (package
    (name "rust-jack-sys")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jack-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17vaq4i8q5nx39rjqx9sixqn1xraf1vxs3bmrf618v8nzxchbmz9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; cannot find value `library` in this scope
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libloading" ,rust-libloading-0.7)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (native-inputs (list pkg-config))
    (inputs (list jack-2))
    (home-page "https://github.com/RustAudio/rust-jack/tree/main/jack-sys")
    (synopsis "Low-level binding to the JACK audio API")
    (description "Low-level binding to the JACK audio API.")
    (license (list license:expat license:asl2.0))))

(define-public rust-jack-sys-0.2
  (package
    (inherit rust-jack-sys-0.4)
    (name "rust-jack-sys")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "jack-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1h9c9za19nyr1prx77gkia18ia93f73lpyjdiyrvmhhbs79g54bv"))))
    (build-system cargo-build-system)
    (arguments
     (list #:cargo-inputs `(("rust-lazy-static" ,rust-lazy-static-1)
                            ("rust-libc" ,rust-libc-0.2)
                            ("rust-libloading" ,rust-libloading-0.6)
                            ("rust-pkg-config" ,rust-pkg-config-0.3))))))

(define-public rust-lewton-0.10
  (package
    (name "rust-lewton")
    (version "0.10.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "lewton" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0c60fn004awg5c3cvx82d6na2pirf0qdz9w3b93mbcdakbglhyvp"))))
    (build-system cargo-build-system)
    (arguments
     (list #:cargo-inputs
           `(("rust-byteorder" ,rust-byteorder-1)
             ("rust-futures" ,rust-futures-0.1)
             ("rust-ogg" ,rust-ogg-0.8)
             ("rust-tinyvec" ,rust-tinyvec-1)
             ("rust-tokio-io" ,rust-tokio-io-0.1))
           #:cargo-development-inputs
           `(("rust-alto" ,rust-alto-3)
             ("rust-ogg" ,rust-ogg-0.8))))
    (home-page "https://github.com/RustAudio/lewton")
    (synopsis "Pure Rust Vorbis decoder")
    (description "A pure Rust Vorbis decoder.  Vorbis is a free and open
source audio format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-librespot-audio-0.4
  (package
    (name "rust-librespot-audio")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "librespot-audio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "178djijj7fkg5ca5rhk10rvy9gs797gikvackh5qxsp1al9s6xn1"))))
    (build-system cargo-build-system)
    (arguments
     (list #:cargo-inputs
           `(("rust-aes-ctr" ,rust-aes-ctr-0.6)
             ("rust-byteorder" ,rust-byteorder-1)
             ("rust-bytes" ,rust-bytes-1)
             ("rust-futures-util" ,rust-futures-util-0.3)
             ("rust-librespot-core" ,rust-librespot-core-0.4)
             ("rust-log" ,rust-log-0.4)
             ("rust-tempfile" ,rust-tempfile-3)
             ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The audio fetching logic for Librespot")
    (description
     "Part of Librespot, an open source client library for Spotify.  This
package contains the audio fetching logic.")
    (license license:expat)))

(define-public rust-librespot-connect-0.4
  (package
    (name "rust-librespot-connect")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "librespot-connect" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1v6k20173hx27g34d24vkb4a67av7dbr3mfmng64b51y8imgpyjg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-form-urlencoded" ,rust-form-urlencoded-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-librespot-core" ,rust-librespot-core-0.4)
                       ("rust-librespot-discovery" ,rust-librespot-discovery-0.4)
                       ("rust-librespot-playback" ,rust-librespot-playback-0.4)
                       ("rust-librespot-protocol" ,rust-librespot-protocol-0.4)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-protobuf" ,rust-protobuf-2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "Discovery and Spotify Connect logic for Librespot")
    (description
     "Librespot is an open source client library for Spotify.  This package
contains the discovery and Spotify Connect logic.")
    (license license:expat)))

(define-public rust-librespot-core-0.4
  (package
    (name "rust-librespot-core")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "librespot-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vaxnnlnsx8bmphiikm4kb99795jch0xxifr0azl9rl8b3r4jqq4"))))
    (build-system cargo-build-system)
    (arguments
     (list #:cargo-test-flags
           ``("--release" "--"
              "--skip=test_connection"
              "--skip=test_apresolve"
              "--skip=test_apresolve_port_443")
           #:cargo-inputs
           `(("rust-aes" ,rust-aes-0.6)
             ("rust-base64" ,rust-base64-0.13)
             ("rust-byteorder" ,rust-byteorder-1)
             ("rust-bytes" ,rust-bytes-1)
             ("rust-form-urlencoded" ,rust-form-urlencoded-1)
             ("rust-futures-core" ,rust-futures-core-0.3)
             ("rust-futures-util" ,rust-futures-util-0.3)
             ("rust-hmac" ,rust-hmac-0.11)
             ("rust-http" ,rust-http-0.2)
             ("rust-httparse" ,rust-httparse-1)
             ("rust-hyper" ,rust-hyper-0.14)
             ("rust-hyper-proxy" ,rust-hyper-proxy-0.9)
             ("rust-librespot-protocol" ,rust-librespot-protocol-0.4)
             ("rust-log" ,rust-log-0.4)
             ("rust-num-bigint" ,rust-num-bigint-0.4)
             ("rust-num-integer" ,rust-num-integer-0.1)
             ("rust-num-traits" ,rust-num-traits-0.2)
             ("rust-once-cell" ,rust-once-cell-1)
             ("rust-pbkdf2" ,rust-pbkdf2-0.8)
             ("rust-priority-queue" ,rust-priority-queue-1)
             ("rust-protobuf" ,rust-protobuf-2)
             ("rust-rand" ,rust-rand-0.8)
             ("rust-serde" ,rust-serde-1)
             ("rust-serde-json" ,rust-serde-json-1)
             ("rust-sha-1" ,rust-sha-1-0.10)
             ("rust-shannon" ,rust-shannon-0.2)
             ("rust-thiserror" ,rust-thiserror-1)
             ("rust-tokio" ,rust-tokio-1)
             ("rust-tokio-stream" ,rust-tokio-stream-0.1)
             ("rust-tokio-util" ,rust-tokio-util-0.7)
             ("rust-url" ,rust-url-2)
             ("rust-uuid" ,rust-uuid-1)
             ("rust-vergen" ,rust-vergen-3))
           #:cargo-development-inputs
           `(("rust-env-logger" ,rust-env-logger-0.9)
             ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The core functionality provided by librespot")
    (description
     "Part of Librespot, an open source client library for
Spotify.  This package contains core functionality, such as authentication,
channel and session.")
    (license license:expat)))

(define-public rust-librespot-discovery-0.4
  (package
    (name "rust-librespot-discovery")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "librespot-discovery" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01igbv0xf3vj046jvblbr09cgmv25mlfajyb2903cl31iz8pga1a"))))
    (build-system cargo-build-system)
    (arguments
     (list #:cargo-inputs
           `(("rust-aes-ctr" ,rust-aes-ctr-0.6)
             ("rust-base64" ,rust-base64-0.13)
             ("rust-form-urlencoded" ,rust-form-urlencoded-1)
             ("rust-futures-core" ,rust-futures-core-0.3)
             ("rust-hmac" ,rust-hmac-0.11)
             ("rust-hyper" ,rust-hyper-0.14)
             ("rust-libmdns" ,rust-libmdns-0.7)
             ("rust-librespot-core" ,rust-librespot-core-0.4)
             ("rust-log" ,rust-log-0.4)
             ("rust-rand" ,rust-rand-0.8)
             ("rust-serde-json" ,rust-serde-json-1)
             ("rust-sha-1" ,rust-sha-1-0.9)
             ("rust-thiserror" ,rust-thiserror-1)
             ("rust-tokio" ,rust-tokio-1)
             ("rust-dns-sd" ,rust-dns-sd-0.1))
           #:cargo-development-inputs
           `(("rust-futures" ,rust-futures-0.3)
             ("rust-hex" ,rust-hex-0.4)
             ("rust-simple-logger" ,rust-simple-logger-2)
             ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The discovery logic of Librespot")
    (description "Part of Librespot, an open source client library for
Spotify.  This package contains the discovery logic.")
    (license license:expat)))

(define-public rust-librespot-metadata-0.4
  (package
    (name "rust-librespot-metadata")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "librespot-metadata" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "07626b84cghd3jabdvyqhn1v0lax9p1hhz6ldw2r4l6brcgkd03b"))))
    (build-system cargo-build-system)
    (arguments
     (list #:cargo-inputs
           `(("rust-async-trait" ,rust-async-trait-0.1)
             ("rust-byteorder" ,rust-byteorder-1)
             ("rust-librespot-core" ,rust-librespot-core-0.4)
             ("rust-librespot-protocol" ,rust-librespot-protocol-0.4)
             ("rust-log" ,rust-log-0.4)
             ("rust-protobuf" ,rust-protobuf-2))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The metadata elements of Librespot")
    (description "Part of Librespot, an open source client library for
Spotify.  This package contains the metadata logic.")
    (license license:expat)))

(define-public rust-librespot-playback-0.4
  (package
    (name "rust-librespot-playback")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "librespot-playback" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dygnzldvkv1qpagr9nl62hmqh0xfcf4lsva37j0xxy7pjws142i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-alsa" ,rust-alsa-0.6)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-cpal" ,rust-cpal-0.13)
        ("rust-futures-executor" ,rust-futures-executor-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-glib" ,rust-glib-0.15)
        ("rust-gstreamer" ,rust-gstreamer-0.18)
        ("rust-gstreamer-app" ,rust-gstreamer-app-0.18)
        ("rust-gstreamer-audio" ,rust-gstreamer-audio-0.18)
        ("rust-jack" ,rust-jack-0.10)
        ("rust-lewton" ,rust-lewton-0.10)
        ("rust-libpulse-binding" ,rust-libpulse-binding-2)
        ("rust-libpulse-simple-binding" ,rust-libpulse-simple-binding-2)
        ("rust-librespot-audio" ,rust-librespot-audio-0.4)
        ("rust-librespot-core" ,rust-librespot-core-0.4)
        ("rust-librespot-metadata" ,rust-librespot-metadata-0.4)
        ("rust-log" ,rust-log-0.4)
        ("rust-ogg" ,rust-ogg-0.8)
        ("rust-parking-lot" ,rust-parking-lot-0.12)
        ("rust-portaudio-rs" ,rust-portaudio-rs-0.3)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rand-distr" ,rust-rand-distr-0.4)
        ("rust-rodio" ,rust-rodio-0.15)
        ("rust-sdl2" ,rust-sdl2-0.35)
        ("rust-shell-words" ,rust-shell-words-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-zerocopy" ,rust-zerocopy-0.6))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "Audio playback for Librespot")
    (description "Audio playback for Librespot, an open source client
library for Spotify.")
    (license license:expat)))

(define-public rust-librespot-protocol-0.4
  (package
    (name "rust-librespot-protocol")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "librespot-protocol" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "17xkvhlxfkjh1z79pvq22nrxi99hcxnzafg0pdkymh3a3733lvax"))))
    (build-system cargo-build-system)
    (arguments
     (list
       #:cargo-inputs
       `(("rust-protobuf" ,rust-protobuf-2)
         ("rust-glob" ,rust-glob-0.3)
         ("rust-protobuf-codegen-pure" ,rust-protobuf-codegen-pure-2))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The protobuf logic for communicating with Spotify servers")
    (description "Part of Librespot, an open source, Spotify client library.
This package contains the protobuf logic.")
    (license license:expat)))

(define-public rust-lv2-0.6
  (package
    (name "rust-lv2")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xh4hjfh2w5rhzbk0g9845k25f6fxrv7xqpkr09p0x57b200qc41"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lv2-atom" ,rust-lv2-atom-2)
                       ("rust-lv2-core" ,rust-lv2-core-3)
                       ("rust-lv2-midi" ,rust-lv2-midi-1)
                       ("rust-lv2-state" ,rust-lv2-state-2)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-lv2-time" ,rust-lv2-time-0.1)
                       ("rust-lv2-units" ,rust-lv2-units-0.1)
                       ("rust-lv2-urid" ,rust-lv2-urid-2)
                       ("rust-lv2-worker" ,rust-lv2-worker-0.1)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Safe, fast, and ergonomic framework to create LV2 plugins in Rust")
    (description "This package provides a safe, fast, and ergonomic framework
to create LV2 plugins in Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-atom-2
  (package
    (name "rust-lv2-atom")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-atom" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wd9rgsn8sag8wyhjccmnn82gx4w1yyiav52nyvk579l21xlw6wm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lv2-core" ,rust-lv2-core-3)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-lv2-units" ,rust-lv2-units-0.1)
                       ("rust-urid" ,rust-urid-0.1))
       #:cargo-development-inputs (("rust-lv2-urid" ,rust-lv2-urid-2))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Rust LV2 Atom handling library")
    (description "This package provides a Rust LV2 Atom handling library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-core-3
  (package
    (name "rust-lv2-core")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pj9l15zwqwj2h83f3xfpwxsj70vvhkw52gyzkljafvrbx1h00fm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lv2-core-derive" ,rust-lv2-core-derive-2)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Rust LV2 core library")
    (description "This package provides the Rust LV2 core library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-core-derive-2
  (package
    (name "rust-lv2-core-derive")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-core-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12w3l41jzargrcywz13hbmaazfw4ix2sljl3601h6jfbdrw8zybv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Procedural macros for lv2-core")
    (description "This package provides Procedural macros for lv2-core.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-midi-1
  (package
    (name "rust-lv2-midi")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-midi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0x0glbrfri1glgcrmvc6i1jfv6azhpqvp4ibk5cihsq3s2yfc8xd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `wmidi`
       #:cargo-inputs (("rust-lv2-atom" ,rust-lv2-atom-2)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1)
                       ("rust-wmidi" ,rust-wmidi-3))
       #:cargo-development-inputs (("rust-lv2-core" ,rust-lv2-core-3)
                                   ("rust-lv2-units" ,rust-lv2-units-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Rust LV2 MIDI processing library")
    (description "This package provides a Rust LV2 MIDI processing library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-state-2
  (package
    (name "rust-lv2-state")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-state" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nm0fc7cb4rkmfsvvr4xbac4qf0j7wl2gws3qrcflx057i2lpsb5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lv2-atom" ,rust-lv2-atom-2)
                       ("rust-lv2-core" ,rust-lv2-core-3)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))
       #:cargo-development-inputs (("rust-lv2-urid" ,rust-lv2-urid-2)
                                   ("rust-mktemp" ,rust-mktemp-0.4))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Rust LV2 state handling library")
    (description "This package provides a Rust LV2 state handling library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-sys-2
  (package
    (name "rust-lv2-sys")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0c4f59mrjyy0z0wf033wp648df0sc6zirrcd6kndqj9nvvkzkl4x"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Rust LV2 C header bindings")
    (description "This package provides Rust LV2 C header bindings.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-time-0.1
  (package
    (name "rust-lv2-time")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-time" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wznk17vvn5dph6r47vjwmf7g98pb6ij2fdhizdk95sf2qvkf82c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Rust LV2 wrapper for LV2 time types")
    (description "This package provides a Rust LV2 wrapper for LV2 time types.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-units-0.1
  (package
    (name "rust-lv2-units")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-units" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fdamp3hxdr36hqi1j6y01rz1x17if1ibzr7rr4nrabidw74gf82"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Rust LV2 wrapper of LV2 unit types")
    (description "This package provides a Rust LV2 wrapper of LV2 unit types.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-urid-2
  (package
    (name "rust-lv2-urid")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-urid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s2fcb0nyn54ml6azkbhnnxghy898x1q5vs5qgdznrhy9m20624c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lv2-core" ,rust-lv2-core-3)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Rust LV2 URID handling library")
    (description "This package provides a LV2 URID handling library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-worker-0.1
  (package
    (name "rust-lv2-worker")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-worker" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14crsrnjyarra9ipma6lhaj4gpfadvippzr134nkn0z3y30ip4fj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Tests panic
       #:cargo-inputs (("rust-lv2-core" ,rust-lv2-core-3)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Rust LV2 work offloading library")
    (description "This package provides a Rust LV2 work offloading library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ogg-0.8
  (package
    (name "rust-ogg")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ogg" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0vjxmqcv9252aj8byy70iy2krqfjknfcxg11lcyikj11pzlb8lb9"))))
    (build-system cargo-build-system)
    (arguments
     (list #:cargo-inputs
           `(("rust-byteorder" ,rust-byteorder-1)
             ("rust-bytes" ,rust-bytes-0.4)
             ("rust-futures" ,rust-futures-0.1)
             ("rust-tokio-io" ,rust-tokio-io-0.1))
           #:cargo-development-inputs
           `(("rust-rand" ,rust-rand-0.3))))
    (home-page "https://github.com/RustAudio/ogg")
    (synopsis "Ogg container decoder and encoder written in pure Rust")
    (description "An Ogg decoder and encoder.  Implements the xiph.org Ogg
spec in pure Rust.")
    (license license:expat)))

(define-public rust-portaudio-rs-0.3
  (package
    (name "rust-portaudio-rs")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "portaudio-rs" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qnmc7amk0fzbcs985ixv0k4955f0fmpkhrl9ps9pk3cz7pvbdnd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-portaudio-sys" ,rust-portaudio-sys-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list portaudio alsa-lib))
    (home-page "https://github.com/RustAudio/rust-portaudio")
    (synopsis "Rust bindings for PortAudio a cross-platfomr audio library")
    (description "Rusting bindings for PortAudio an open source, cross-platform
audio I/O library.")
    (license license:expat)))

(define-public rust-portaudio-sys-0.1
  (package
    (name "rust-portaudio-sys")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "portaudio-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xdpywirpr1kqkbak7hnny62gmsc93qgc3ij3j2zskrvjpxa952i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (native-inputs (list pkg-config))
    (inputs (list portaudio alsa-lib))
    (home-page "https://github.com/RustAudio/rust-portaudio")
    (synopsis "Bindings for PortAudio a cross-platform audio library")
    (description "Bindings for PortAudio an open source, cross-platform audio
I/O library.")
    (license license:expat)))

(define-public rust-rodio-0.15
  (package
    (name "rust-rodio")
    (version "0.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rodio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07kkrx0hxfcqgkpg0lrh9355bj1rl0k65nwsk3qwdri6yvlkj2gc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-claxon" ,rust-claxon-0.4)
                       ("rust-cpal" ,rust-cpal-0.13)
                       ("rust-hound" ,rust-hound-3)
                       ("rust-lewton" ,rust-lewton-0.10)
                       ("rust-minimp3" ,rust-minimp3-0.5)
                       ("rust-symphonia" ,rust-symphonia-0.4))
       #:cargo-development-inputs (("rust-quickcheck" ,rust-quickcheck-0.9))))
    (native-inputs (list pkg-config))
    (inputs (list alsa-lib))
    (home-page "https://github.com/RustAudio/rodio")
    (synopsis "Pure Rust audio playback library")
    (description "Audio playback library written in pure Rust that supports
many formats including AAC, FLAC, MP3, MP4 and WAV.")
    (license (list license:expat license:asl2.0))))

(define-public rust-urid-0.1
  (package
    (name "rust-urid")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "urid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "195672gs136vczn1r4hkjg5vfa7vdzr26bzv6lwhk0z7cvbvaa38"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-urid-derive" ,rust-urid-derive-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Library for idiomatic URID support")
    (description "This package provides Library for idiomatic URID support.")
    (license (list license:expat license:asl2.0))))

(define-public rust-urid-derive-0.1
  (package
    (name "rust-urid-derive")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "urid-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i1nf0sgq4ai051h17s9msaavl3jfzdmdlsy8455pr88y0pfx7l1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Procedural macros for urid")
    (description "This package provides procedural macros for urid.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wmidi-3
  (package
    (name "rust-wmidi")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wmidi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kxnbs18nmpzm2hfwaaa5h2s77cmk5w53srzxqmrqlkdpdcrjafa"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/RustAudio/wmidi")
    (synopsis "Midi parsing library")
    (description "This package provides a Midi parsing library.")
    (license license:expat)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
