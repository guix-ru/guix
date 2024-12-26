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
  #:use-module (gnu packages crates-io)
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

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
