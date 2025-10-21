;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2017 rsiddharth <s@ricketyspace.net>
;;; Copyright © 2017, 2019, 2023 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2025 Maxim Cournoyer <maxim@guixotic.coop>
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

(define-module (gnu packages haskell-crypto)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public ghc-asn1-types
  (package
    (name "ghc-asn1-types")
    (version "0.3.4")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "asn1-types" version))
              (sha256
               (base32
                "1a119qxhxhr0yn37r26dkydm6g5kykdkx98ghb59i4ipa6i95vkq"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "asn1-types")))
    (inputs
     (list ghc-memory ghc-hourglass))
    (home-page "https://github.com/vincenthz/hs-asn1-types")
    (synopsis "ASN.1 types for Haskell")
    (description
     "The package provides the standard types for dealing with the ASN.1
format.")
    (license license:bsd-3)))

(define-public ghc-asn1-encoding
  (package
    (name "ghc-asn1-encoding")
    (version "0.9.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "asn1-encoding" version))
       (sha256
        (base32 "02nsr30h5yic1mk7znf0q4z3n560ip017n60hg7ya25rsfmxxy6r"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "asn1-encoding")))
    (inputs (list ghc-hourglass ghc-asn1-types))
    (native-inputs (list ghc-tasty ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision ("2"
                         "16503ryhq15f2rfdav2qnkq11dg2r3vk3f9v64q9dmxf8dh8zv97")))
    (home-page "https://github.com/vincenthz/hs-asn1")
    (synopsis "ASN1 data reader and writer in RAW, BER and DER forms")
    (description
     "This package provides a reader and writer for ASN1 data in raw form with
supports for high level forms of ASN1 (BER, and DER).")
    (license license:bsd-3)))

(define-public ghc-asn1-parse
  (package
    (name "ghc-asn1-parse")
    (version "0.9.5")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "asn1-parse" version))
              (sha256
               (base32
                "17pk8y3nwv9b9i5j15qlmwi7fmq9ab2z4kfpjk2rvcrh9lsf27wg"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "asn1-parse")))
    (inputs
     (list ghc-asn1-types ghc-asn1-encoding))
    (home-page "https://github.com/vincenthz/hs-asn1")
    (synopsis "Simple monadic parser for ASN1 stream types")
    (description
     "This package provides a simple monadic parser for ASN1 stream types,
when ASN1 pattern matching is not convenient.")
    (license license:bsd-3)))

(define-public ghc-crypto-api
  (package
    (name "ghc-crypto-api")
    (version "0.13.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "crypto-api" version))
       (sha256
        (base32 "19bsmkqkpnvh01b77pmyarx00fic15j4hvg4pzscrj4prskrx2i9"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "crypto-api")))
    (inputs (list ghc-cereal ghc-tagged ghc-entropy))
    (arguments
     `(#:cabal-revision ("1"
                         "1z6n1sa5pn3iqvqjrd8hv4bc2pxzsrhm5sh0l8z7g9lbqp6w0wp5")))
    (home-page "https://github.com/TomMD/crypto-api")
    (synopsis "Provides generic interface for cryptographic operations
for Haskell")
    (description
     "This Haskell package provides a generic interface for
cryptographic operations (hashes, ciphers, randomness).

Maintainers of hash and cipher implementations are encouraged to add instances
for the classes defined in @code{Crypto.Classes}.  @code{Crypto} users are
similarly encouraged to use the interfaces defined in the @code{Classes} module.

Any concepts or functions of general use to more than one cryptographic
algorithm (ex: padding) is within scope of this package.")
    (license license:bsd-3)))

(define-public ghc-crypto-api-tests
  (package
    (name "ghc-crypto-api-tests")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "crypto-api-tests" version))
       (sha256
        (base32
         "0w3j43jdrlj28jryp18hc6q84nkl2yf4vs1hhgrsk7gb9kfyqjpl"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "crypto-api-tests")))
    (inputs (list ghc-test-framework-quickcheck2
                  ghc-crypto-api
                  ghc-cereal
                  ghc-test-framework
                  ghc-test-framework-hunit
                  ghc-hunit
                  ghc-quickcheck))
    (home-page "https://github.com/TomMD/crypto-api-tests")
    (synopsis "Test framework and KATs for cryptographic operations for Haskell")
    (description "This Haskell package provides a test framework for hash and
cipher operations using the crypto-api interface.  Known answer tests (KATs)
for common cryptographic algorithms are included.")
    (license license:bsd-3)))

(define-public ghc-cryptohash
  (package
    (name "ghc-cryptohash")
    (version "0.11.9")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cryptohash" version))
       (sha256
        (base32
         "1yr2iyb779znj79j3fq4ky8l1y8a600a2x1fx9p5pmpwq5zq93y2"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cryptohash")))
    (inputs
     (list ghc-byteable
           ghc-cryptonite
           ghc-memory
           ghc-hunit
           ghc-quickcheck
           ghc-tasty
           ghc-tasty-quickcheck
           ghc-tasty-hunit))
    (home-page "https://github.com/vincenthz/hs-cryptohash")
    (synopsis "Collection of cryptographic hashes in Haskell")
    (description
     "A collection of crypto hashes, with a practical incremental and one-pass,
pure APIs, with performance close to the fastest implementations available in
other languages.  The implementations are made in C with a haskell FFI wrapper
that hides the C implementation.")
    (license license:bsd-3)))

(define-public ghc-cryptohash-md5
  (package
    (name "ghc-cryptohash-md5")
    (version "0.11.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cryptohash-md5" version))
       (sha256
        (base32 "018g13hkmq5782i24b4518hcd926fl6x6fh5hd7b9wlxwc5dn21v"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cryptohash-md5")))
    (native-inputs (list ghc-base16-bytestring ghc-puremd5 ghc-tasty
                         ghc-tasty-quickcheck ghc-tasty-hunit))
    (arguments
     `(#:cabal-revision ("6"
                         "0m7f9mgw4w9vcch37ja9zgbn0knq8rjppqr9x2ylkcdxfdnmhhif")))
    (home-page "https://github.com/haskell-hvr/cryptohash-md5")
    (synopsis "MD5 implementation for Haskell")
    (description "This Haskell package provides implementation of MD5.")
    (license license:bsd-3)))

(define-public ghc-cryptohash-sha1
  (package
    (name "ghc-cryptohash-sha1")
    (version "0.11.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cryptohash-sha1" version))
       (sha256
        (base32 "0h9jl9v38gj0vnscqx7xdklk634p05fa6z2pcvknisq2mnbjq154"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cryptohash-sha1")))
    (native-inputs (list ghc-base16-bytestring ghc-sha ghc-tasty
                         ghc-tasty-quickcheck ghc-tasty-hunit))
    (arguments
     `(#:cabal-revision ("6"
                         "1cxdw2y10z4v0mz91wki7f233jziipx85j4qy5msig4nv3djkprb")))
    (home-page "https://github.com/haskell-hvr/cryptohash-sha1")
    (synopsis "SHA-1 implementation for Haskell")
    (description
     "This Haskell package provides an incremental and one-pass,
pure API to the @uref{https://en.wikipedia.org/wiki/SHA-1, SHA-1 hash algorithm},
including @uref{https://en.wikipedia.org/wiki/HMAC, HMAC support}, with
performance close to the fastest implementations available in other languages.

The implementation is made in C with a haskell FFI wrapper that hides
the C implementation.")
    (license license:bsd-3)))

(define-public ghc-cryptohash-sha256
  (package
    (name "ghc-cryptohash-sha256")
    (version "0.11.102.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cryptohash-sha256" version))
       (sha256
        (base32 "1xkb7iqplbw4fy1122p79xf1zcb7k44rl0wmfj1q06l7cdqxr9vk"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cryptohash-sha256")))
    (native-inputs (list ghc-base16-bytestring ghc-sha ghc-tasty
                         ghc-tasty-quickcheck ghc-tasty-hunit))
    (arguments
     `(#:cabal-revision ("6"
                         "01s12sl5mxvraj5fj3cs0pkb03pm8xpjz13y09dpl7i6rv6f578f")))
    (home-page "https://github.com/haskell-hvr/cryptohash-sha256")
    (synopsis "SHA-256 implementation for Haskell")
    (description
     "This Haskell package provides an incremental and
one-pass, pure API to the @uref{https://en.wikipedia.org/wiki/SHA-2,
SHA-256 cryptographic hash algorithm}, with performance close to the
fastest implementations available in other languages.

The implementation is made in C with a haskell FFI wrapper that hides
the C implementation.")
    (license license:bsd-3)))

(define-public ghc-cryptonite
  (package
    (name "ghc-cryptonite")
    (version "0.30")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cryptonite" version))
       (sha256
        (base32 "07bb97iszhnrfddh5ql6p3dqd0c13xycjw5n2kljw7d0ia59q2an"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cryptonite")))
    (inputs (list ghc-memory ghc-basement))
    (native-inputs (list ghc-tasty ghc-tasty-quickcheck ghc-tasty-hunit
                         ghc-tasty-kat))
    (home-page "https://github.com/haskell-crypto/cryptonite")
    (synopsis "Cryptography primitives")
    (description
     "This package is a repository of cryptographic primitives for Haskell.
It supports a wide range of symmetric ciphers, cryptographic hash functions,
public key algorithms, key derivation numbers, cryptographic random number
generators, and more.")
    (license license:bsd-3)))

(define-public ghc-curve25519
  (package
  (name "ghc-curve25519")
  (version "0.2.7")
  (source
   (origin
     (method url-fetch)
     (uri (hackage-uri "curve25519" version))
     (sha256
      (base32 "1p8b1lppkvc19974hr43lcqdi4nj55j2nf7gsnp8dn7gyf23aayq"))))
  (build-system haskell-build-system)
  (native-inputs
   (list ghc-hunit
         ghc-quickcheck
         ghc-tagged
         ghc-test-framework
         ghc-test-framework-hunit
         ghc-test-framework-quickcheck2))
  (inputs
   (list ghc-crypto-api))
  (properties '((upstream-name . "curve25519")))
  (home-page "https://github.com/acw/curve25519")
  (synopsis "Fast implementations of the curve25519 elliptic curve primitives")
  (description
   "This module provides Haskell bindings and extensions to the curve25519-donna
codebase.  It's a pretty straightforward implementation of the basic
cryptographic routines you'd want from a project that uses curve25519: key
generation, and key agreement.  For further functionality, you'll want to look
elsewhere.")
  (license license:bsd-3)))

(define-public ghc-digest
  (package
    (name "ghc-digest")
    (version "0.0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "digest" version))
       (sha256
        (base32 "163418p50bqdxn8ajzj6x3455xbch9jq1w6psbkdz5qzw017py6b"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "digest")))
    (arguments
     `(#:extra-directories ("zlib")))
    (inputs (list zlib))
    (native-inputs (list pkg-config))
    (home-page "http://hackage.haskell.org/package/digest")
    (synopsis "Various cryptographic hashes for bytestrings")
    (description
     "This package provides efficient cryptographic hash implementations for
strict and lazy bytestrings.  For now, CRC32 and Adler32 are supported; they
are implemented as FFI bindings to efficient code from zlib.")
    (license license:bsd-3)))

(define-public ghc-entropy
  (package
    (name "ghc-entropy")
    (version "0.4.1.11")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "entropy" version))
       (sha256
        (base32 "1slj5n783k2amsx2hqs5qwvcw4rgfhdhgmiyg7cii0wg4w0mmpgm"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "entropy")))
    (home-page "https://github.com/TomMD/entropy")
    (synopsis "Provides platform independent entropy source for Haskell")
    (description "This Haskell package provides a platform independent method
to obtain cryptographically strong entropy.")
    (license license:bsd-3)))

(define-public ghc-pem
  (package
    (name "ghc-pem")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "pem" version))
              (sha256
               (base32
                "1m7qjsxrd8m88cvkqmr8kscril500j2a9y0iynvksjyjkhdlq33p"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "pem")))
    (inputs
     (list ghc-basement ghc-memory))
    (native-inputs
     (list ghc-test-framework ghc-test-framework-quickcheck2
           ghc-test-framework-hunit ghc-hunit ghc-quickcheck))
    (home-page "https://github.com/vincenthz/hs-pem")
    (synopsis "Privacy Enhanced Mail (PEM) format reader and writer")
    (description
     "This library provides readers and writers for the @dfn{Privacy Enhanced
Mail} (PEM) format.")
    (license license:bsd-3)))

(define-public ghc-puremd5
  (package
    (name "ghc-puremd5")
    (version "2.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "pureMD5" version))
       (sha256
        (base32
         "0qwkvxwi9wh6knn69rg2hvc8ngmv1if77kmpcnp0xqr0l30fwavq"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "pureMD5")))
    (inputs (list ghc-cereal ghc-crypto-api ghc-tagged))
    (native-inputs (list ghc-crypto-api-tests ghc-quickcheck
                         ghc-test-framework ghc-test-framework-quickcheck2
                         ghc-pretty-hex))
    (home-page "https://github.com/TomMD/pureMD5")
    (synopsis "Haskell implementation of the MD5 hash algorithm")
    (description "This package provides a Haskell-only implementation of
the MD5 digest (hash) algorithm.  This now supports the @code{crypto-api} class
interface.")
    (license license:bsd-3)))

(define-public ghc-rsa
  (package
    (name "ghc-rsa")
    (version "2.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "RSA" version))
       (sha256
        (base32 "0hchsqrxpfw7mqrqwscfy8ig1w2di6w3nxpzi873w0gibv2diibj"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "RSA")))
    (inputs (list ghc-crypto-api ghc-crypto-pubkey-types ghc-sha))
    (native-inputs (list ghc-quickcheck ghc-tagged ghc-test-framework
                         ghc-test-framework-quickcheck2))
    (arguments
     `(#:cabal-revision ("2"
                         "090yzm99fmh7c6z4m9hbkasqhc3xlw104g2b6wnk77n1abd13ryj")))
    (home-page "http://hackage.haskell.org/package/RSA")
    (synopsis
     "Implementation of RSA, using the padding schemes of PKCS#1 v2.1.")
    (description
     "This library implements the RSA encryption and signature algorithms for
arbitrarily-sized @code{ByteStrings}.  While the implementations work, they are
not necessarily the fastest ones on the planet.  Particularly key generation.
The algorithms included are based of RFC 3447, or the Public-Key Cryptography
Standard for RSA, version 2.1 (a.k.a, PKCS#1 v2.1).")
    (license license:bsd-3)))

(define-public ghc-sha
  (package
    (name "ghc-sha")
    (version "1.6.4.4")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "SHA" version))
              (sha256
               (base32
                "0i4b2wjisivdy72synal711ywhx05mfqfba5n65rk8qidggm1nbb"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "SHA")))
    (native-inputs
     (list ghc-quickcheck ghc-test-framework
           ghc-test-framework-quickcheck2))
    (home-page "https://hackage.haskell.org/package/SHA")
    (synopsis "SHA suite of message digest functions")
    (description
     "This library implements the SHA suite of message digest functions,
according to NIST FIPS 180-2 (with the SHA-224 addendum), as well as the
SHA-based HMAC routines.  The functions have been tested against most of the
NIST and RFC test vectors for the various functions.  While some attention has
been paid to performance, these do not presently reach the speed of well-tuned
libraries, like OpenSSL.")
    (license license:bsd-3)))

(define-public ghc-x509
  (package
    (name "ghc-x509")
    (version "1.7.7")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "x509" version))
              (sha256
               (base32
                "1zk8lll1hmzl5xvrd16dhyz25151y59xhsqp2mm1wgymwl7r5ijr"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "x509")))
    (inputs (list ghc-memory
                  ghc-hourglass
                  ghc-pem
                  ghc-asn1-types
                  ghc-asn1-encoding
                  ghc-asn1-parse
                  ghc-cryptonite))
    (native-inputs (list ghc-tasty ghc-tasty-quickcheck))
    (home-page "http://github.com/vincenthz/hs-certificate")
    (synopsis "X509 reader and writer")
    (description
     "This library provides functions to read and write X509 certificates.")
    (license license:bsd-3)))

(define-public ghc-x509-store
  (package
    (name "ghc-x509-store")
    (version "1.6.9")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "x509-store" version))
              (sha256
               (base32
                "1nn8ql7vkp4qgf2msm600sr6ranpsajbhq0sc4c0l6pk1i9174n5"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "x509-store")))
    (inputs (list ghc-pem ghc-asn1-types ghc-asn1-encoding ghc-cryptonite
                  ghc-x509))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
    (home-page "http://github.com/vincenthz/hs-certificate")
    (synopsis "X.509 collection accessing and storing methods")
    (description
     "This package provides functions for accessing and storing X.509
collections, certificates, revocation lists, and exception lists.")
    (license license:bsd-3)))

(define-public ghc-x509-validation
  (package
    (name "ghc-x509-validation")
    (version "1.6.12")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "x509-validation" version))
              (sha256
               (base32
                "1j7is28ljz4yxwxz5ax3x7ykgwkr38dx46bw7vgj4arkk7hl93hd"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "x509-validation")))
    (inputs (list ghc-memory
                  ghc-hourglass
                  ghc-data-default-class
                  ghc-pem
                  ghc-asn1-types
                  ghc-asn1-encoding
                  ghc-x509
                  ghc-x509-store
                  ghc-cryptonite))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
    (home-page "http://github.com/vincenthz/hs-certificate")
    (synopsis "X.509 certificate and revocation list validation")
    (description
     "This package provides functions for X.509 certificate and revocation
list validation.")
    (license license:bsd-3)))

(define-public ghc-x509-system
  (package
    (name "ghc-x509-system")
    (version "1.6.7")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "x509-system" version))
              (sha256
               (base32
                "049bdaxrih49nkhkyl2342qnbx2f0q99z8rld648bz1kkgyizz38"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "x509-system")))
    (inputs (list ghc-pem ghc-x509 ghc-x509-store))
    (home-page "http://github.com/vincenthz/hs-certificate")
    (synopsis "Handle system X.509 accessors and storage")
    (description
     "This package provides a library to handle system accessors and storage
for X.509 certificates.")
    (license license:bsd-3)))

(define-public ghc-crypto-cipher-types
  (package
    (name "ghc-crypto-cipher-types")
    (version "0.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "crypto-cipher-types" version))
       (sha256
        (base32
         "03qa1i1kj07pfrxsi7fiaqnnd0vi94jd4jfswbmnm4gp1nvzcwr0"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "crypto-cipher-types")))
    (inputs (list ghc-byteable ghc-securemem))
    (home-page "https://github.com/vincenthz/hs-crypto-cipher")
    (synopsis "Generic cryptography cipher types for Haskell")
    (description "This Haskell package provides basic typeclasses and types
for symmetric ciphers.")
    (license license:bsd-3)))

(define-public ghc-cipher-aes
  (package
    (name "ghc-cipher-aes")
    (version "0.2.11")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cipher-aes" version))
       (sha256
        (base32
         "05ahz6kjq0fl1w66gpiqy0vndli5yx1pbsbw9ni3viwqas4p3cfk"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cipher-aes")))
    (inputs (list ghc-byteable ghc-securemem ghc-crypto-cipher-types))
    (native-inputs (list ghc-quickcheck ghc-test-framework
                         ghc-test-framework-quickcheck2
                         ghc-crypto-cipher-tests))
    (home-page "https://github.com/vincenthz/hs-cipher-aes")
    (synopsis "AES cipher implementation with advanced mode of operations for
Haskell")
    (description "This Haskell package provides AES cipher implementation.

The modes of operations available are ECB (Electronic code book), CBC (Cipher
block chaining), CTR (Counter), XTS (XEX with ciphertext stealing),
GCM (Galois Counter Mode).

The AES implementation uses AES-NI when available (on x86 and x86-64
architecture), but fallback gracefully to a software C implementation.

The software implementation uses S-Boxes, which might suffer for cache timing
issues.  However do notes that most other known software implementations,
including very popular one (openssl, gnutls) also uses similar
implementation.  If it matters for your case, you should make sure you have
AES-NI available, or you'll need to use a different implementation.")
    (license license:bsd-3)))

(define-public ghc-crypto-pubkey-types
  (package
    (name "ghc-crypto-pubkey-types")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "crypto-pubkey-types" version))
       (sha256
        (base32 "0q0wlzjmpx536h1zcdzrpxjkvqw8abj8z0ci38138kpch4igbnby"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "crypto-pubkey-types")))
    (inputs (list ghc-asn1-types ghc-asn1-encoding))
    (home-page "http://github.com/vincenthz/hs-crypto-pubkey-types")
    (synopsis "Generic cryptography Public keys algorithm types")
    (description "Generic cryptography public keys algorithm types.")
    (license license:bsd-3)))

(define-public ghc-crypto-random
  (package
    (name "ghc-crypto-random")
    (version "0.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "crypto-random" version))
       (sha256
        (base32
         "0139kbbb2h7vshf68y3fvjda29lhj7jjwl4vq78w4y8k8hc7l2hp"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "crypto-random")))
    (inputs (list ghc-securemem ghc-vector))
    (home-page "https://github.com/vincenthz/hs-crypto-random")
    (synopsis "Simple cryptographic random related types for Haskell")
    (description "Simple cryptographic random related types: a safe
abstraction for CPRNGs.")
    (license license:bsd-3)))

(define-public ghc-crypton
  (package
    (name "ghc-crypton")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "crypton" version))
       (sha256
        (base32 "06h1qjvqd8gmiyzvh31sc061vcgns101l0774c44a8k44015925l"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "crypton")))
    (inputs (list ghc-memory ghc-basement))
    (native-inputs (list ghc-tasty ghc-tasty-quickcheck ghc-tasty-hunit
                         ghc-tasty-kat))
    (home-page "https://github.com/kazu-yamamoto/crypton")
    (synopsis "Cryptography Primitives sink")
    (description
     "This package provides a repository of cryptographic primitives.

@itemize @bullet
@item Symmetric ciphers: AES, DES, 3DES, CAST5, Blowfish, Twofish, Camellia,
  RC4, Salsa, XSalsa, ChaCha.
@item Hash: SHA1, SHA2, SHA3, SHAKE, MD2, MD4, MD5, Keccak, Skein, Ripemd,
 Tiger, Whirlpool, Blake2.
@item MAC: HMAC, KMAC, Poly1305
@item Asymmetric crypto: DSA, RSA, DH, ECDH, ECDSA, ECC, Curve25519, Curve448,
  Ed25519, Ed448
@item Key Derivation Function: PBKDF2, Scrypt, HKDF, Argon2, BCrypt,
  BCryptPBKDF
@item Cryptographic Random generation: System Entropy, Deterministic Random
  Generator
@item Data related: Anti-Forensic Information Splitter (AFIS)
@end itemize")
    (license license:bsd-3)))

(define-public ghc-crypton-x509
  (package
    (name "ghc-crypton-x509")
    (version "1.7.7")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "crypton-x509" version))
       (sha256
        (base32 "10pkva9wvm6ih48bprxlnyhnn3nr6xq9dhkrk4hfxpsaij6f9v7g"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "crypton-x509")))
    (inputs (list ghc-memory
                  ghc-hourglass
                  ghc-pem
                  ghc-asn1-types
                  ghc-asn1-encoding
                  ghc-asn1-parse
                  ghc-crypton))
    (native-inputs (list ghc-tasty ghc-tasty-quickcheck))
    (home-page "https://github.com/kazu-yamamoto/crypton-certificate")
    (synopsis "X509 reader and writer")
    (description "This package provides generic X509 support for Haskell.")
    (license license:bsd-3)))

(define-public ghc-crypton-x509-store
  (package
    (name "ghc-crypton-x509-store")
    (version "1.6.11")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "crypton-x509-store" version))
       (sha256
        (base32 "07vq7f883cm5krqz2kc0qkh9ks54jknrwdqvfqsk91s12b693a83"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "crypton-x509-store")))
    (inputs (list ghc-pem ghc-asn1-types ghc-asn1-encoding ghc-crypton
                  ghc-crypton-x509))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
    (home-page "https://github.com/kazu-yamamoto/crypton-certificate")
    (synopsis "X.509 collection accessing and storing methods")
    (description
     "This package provides methods for accessing and storing X.509
collections of certificates, certificate revocation lists, and exception
lists.")
    (license license:bsd-3)))

(define-public ghc-crypton-x509-system
  (package
    (name "ghc-crypton-x509-system")
    (version "1.6.7")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "crypton-x509-system" version))
       (sha256
        (base32 "1jilnr9715njlx1hqvg5lrsrwk12r04maypmh18di0sybwg2cdm4"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "crypton-x509-system")))
    (inputs (list ghc-pem ghc-crypton-x509 ghc-crypton-x509-store))
    (home-page "https://github.com/kazu-yamamoto/crypton-certificate")
    (synopsis "Handle per-operating-system X.509 accessors and storage")
    (description
     "This package provides tools for operating system dependent X.509 stores,
storage methods, and accessors.")
    (license license:bsd-3)))

(define-public ghc-crypton-x509-validation
  (package
    (name "ghc-crypton-x509-validation")
    (version "1.6.14")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "crypton-x509-validation" version))
       (sha256
        (base32 "07b09kgrd3m5ahxpj458r5ycd30bz8ldwjwf19mdcnfv4x0kj3pd"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "crypton-x509-validation")))
    (inputs (list ghc-memory
                  ghc-hourglass
                  ghc-data-default
                  ghc-pem
                  ghc-asn1-types
                  ghc-asn1-encoding
                  ghc-crypton-x509
                  ghc-crypton-x509-store
                  ghc-crypton
                  ghc-iproute))
    (native-inputs (list ghc-tasty ghc-tasty-hunit))
    (home-page "https://github.com/kazu-yamamoto/crypton-certificate")
    (synopsis "X.509 Certificate and CRL validation")
    (description "This package provides Haskell tools for X.509 certificate
and @dfn{Certificates revocation list} (CRL) validation.")
    (license license:bsd-3)))

(define-public ghc-crypton-socks
  (package
    (name "ghc-crypton-socks")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "crypton-socks" version))
       (sha256
        (base32 "0fwzlvndyxjwhxambxdjzzm63yvb6jzsg53dkv3i6x17biz0hdm8"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "crypton-socks")))
    (inputs (list ghc-cereal ghc-network ghc-network-bsd))
    (home-page "http://github.com/mpilgrem/crypton-socks")
    (synopsis "SOCKS Protocol Version 5")
    (description
     "This package provides a library implementing SOCKS Protocol Version 5.")
    (license license:bsd-3)))

(define-public ghc-crypto-token
  (package
    (name "ghc-crypto-token")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "crypto-token" version))
       (sha256
        (base32 "18xphlvn6wzyi6lqrvm5vic97pkcbiph1938p5l1mb22rj5215zm"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "crypto-token")))
    (inputs (list ghc-crypton ghc-memory ghc-network-byte-order))
    (native-inputs (list ghc-hspec hspec-discover))
    (home-page "http://hackage.haskell.org/package/crypto-token")
    (synopsis "crypto tokens")
    (description "Encrypted tokens/tickets to keep state in the client side.")
    (license license:bsd-3)))

(define-public ghc-cprng-aes
  (package
    (name "ghc-cprng-aes")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cprng-aes" version))
       (sha256
        (base32
         "1wr15kbmk1g3l8a75n0iwbzqg24ixv78slwzwb2q6rlcvq0jlnb4"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cprng-aes")))
    (inputs (list ghc-byteable ghc-crypto-random ghc-cipher-aes))
    (home-page "https://github.com/vincenthz/hs-cprng-aes")
    (synopsis "Crypto Pseudo Random Number Generator using AES in counter mode
in Haskell")
    (description "Simple crypto pseudo-random-number-generator with really
good randomness property.

Using ent, a randomness property maker on one 1Mb sample:

@itemize
@item Entropy = 7.999837 bits per byte.
@item Optimum compression would reduce the size of this 1048576 byte file by 0
percent.
@item Chi square distribution for 1048576 samples is 237.02.
@item Arithmbetic mean value of data bytes is 127.3422 (127.5 = random).
@item Monte Carlo value for Pi is 3.143589568 (error 0.06 percent).
@end itemize

Compared to urandom with the same sampling:

@itemize
@item Entropy = 7.999831 bits per byte.
@item Optimum compression would reduce the size of this 1048576 byte file by 0
percent.
@item Chi square distribution for 1048576 samples is 246.63.
@item Arithmetic mean value of data bytes is 127.6347 (127.5 = random).
@item Monte Carlo value for Pi is 3.132465868 (error 0.29 percent).
@end itemize")
    (license license:bsd-3)))

(define-public ghc-ed25519
  (package
    (name "ghc-ed25519")
    (version "0.0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "ed25519" version))
       (sha256
        (base32 "0v8msqvgzimhs7p5ri25hrb1ni2wvisl5rmdxy89fc59py79b9fq"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "ed25519")))
    (native-inputs (list ghc-quickcheck ghc-doctest))
    (arguments
     `(#:cabal-revision ("9"
                         "0y5vfdcys18waa5k5zxiq9n8jq0l05j76riml23i0gxmwgdhsmgi")))
    (home-page "https://github.com/thoughtpolice/hs-ed25519/")
    (synopsis "Ed25519 cryptographic signatures")
    (description
     "This package provides a simple, fast, self-contained
copy of the Ed25519 public-key signature system with a clean interface.
It also includes support for detached signatures, and thorough
documentation on the design and implementation, including usage
guidelines.")
    (license license:expat)))

(define-public ghc-tls
  (package
    (name "ghc-tls")
    (version "2.1.8")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "tls" version))
              (sha256
               (base32
                "1csdy3426lva1a5f7gh9qm96vzfraqj91jbxlm79wbf1jsdzfhsq"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tls")))
    (inputs (list ghc-asn1-types
                  ghc-asn1-encoding
                  ghc-base16-bytestring
                  ghc-cereal
                  ghc-crypton
                  ghc-crypton-x509
                  ghc-crypton-x509-store
                  ghc-crypton-x509-validation
                  ghc-data-default
                  ghc-memory
                  ghc-network
                  ghc-serialise
                  ghc-transformers
                  ghc-unix-time
                  ghc-zlib))
    (native-inputs (list ghc-quickcheck
                         ghc-asn1-types
                         ghc-async
                         ghc-crypton
                         ghc-crypton-x509
                         ghc-crypton-x509-validation
                         ghc-hourglass
                         ghc-hspec
                         ghc-serialise
                         hspec-discover))
    (home-page "http://github.com/haskell-tls/hs-tls")
    (synopsis "TLS/SSL protocol native implementation (Server and Client)")
    (description
     "Native Haskell TLS and SSL protocol implementation for server and client.
This provides a high-level implementation of a sensitive security protocol,
eliminating a common set of security issues through the use of the advanced
type system, high level constructions and common Haskell features.  Currently
implement the SSL3.0, TLS1.0, TLS1.1 and TLS1.2 protocol, and support RSA and
Ephemeral (Elliptic curve and regular) Diffie Hellman key exchanges, and many
extensions.")
    (license license:bsd-3)))

(define-public ghc-hsopenssl
  (package
    (name "ghc-hsopenssl")
    (version "0.11.7.9")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "HsOpenSSL" version))
       (sha256
        (base32 "0z3nyqlcgqw4ncn90gs6bznyj3lrqpjv83nzs4854knm15x6cbsa"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "HsOpenSSL")))
    (arguments
     (list #:extra-directories (list "openssl")
           #:configure-flags
           #~(list "--ghc-options=-optc=-Wno-incompatible-pointer-types")))
    (inputs (list ghc-network openssl))
    (home-page "https://github.com/haskell-cryptography/HsOpenSSL")
    (synopsis "Partial OpenSSL binding for Haskell")
    (description
     "HsOpenSSL is an OpenSSL binding for Haskell.  It can
generate RSA and DSA keys, read and write PEM files, generate message
digests, sign and verify messages, encrypt and decrypt messages.  It has
also some capabilities of creating SSL clients and servers.  This
package is in production use by a number of Haskell based systems and
stable.  You may also be interested in the tls package,
@uref{http://hackage.haskell.org/package/tls}, which is a pure Haskell
implementation of SSL.")
    (license license:public-domain)))

(define-public ghc-hsopenssl-x509-system
  (package
  (name "ghc-hsopenssl-x509-system")
  (version "0.1.0.4")
  (source
   (origin
     (method url-fetch)
     (uri (hackage-uri "HsOpenSSL-x509-system" version))
     (sha256
      (base32 "15mp70bqg1lzp971bzp6wym3bwzvxb76hzbgckygbfa722xyymhr"))))
  (build-system haskell-build-system)
  (inputs
   (list ghc-hsopenssl))
  (properties '((upstream-name . "HsOpenSSL-x509-system")))
  (home-page "https://github.com/redneb/HsOpenSSL-x509-system")
  (synopsis "Use the system's native CA certificate store with HsOpenSSL")
  (description
   "This package provides a cross-platform library that tries to find
a (reasonable) CA certificate bundle that can be used with HsOpenSSL to verify
the certificates of remote peers.  It is for HsOpenSSL what x509-system is for
the tls package, and borrows some ideas from x509-system.")
  (license license:bsd-3)))

(define-public ghc-openssl-streams
  (package
    (name "ghc-openssl-streams")
    (version "1.2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "openssl-streams" version))
       (sha256
        (base32 "10pnnpzgb5xr811kc9qdk7h2cgn6hk2yiyhnzz8f8p0fjzc0pwjm"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "openssl-streams")))
    (inputs (list ghc-hsopenssl ghc-io-streams ghc-network))
    (native-inputs (list ghc-hunit ghc-test-framework ghc-test-framework-hunit))
    (arguments
     `(#:cabal-revision ("3"
                         "1vizl9yajkbd59rrnfb38xwq7hfj36avbv6vwqnkhbvz3qaj3dwg")))
    (home-page "https://hackage.haskell.org/package/openssl-streams")
    (synopsis "OpenSSL network support for io-streams")
    (description "This library contains io-streams routines for secure
networking using OpenSSL (by way of HsOpenSSL).")
    (license license:bsd-3)))

(define-public ghc-cryptonite-conduit
  (package
    (name "ghc-cryptonite-conduit")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "cryptonite-conduit" version))
       (sha256
        (base32
         "1bldcmda4xh52mw1wfrjljv8crhw3al7v7kv1j0vidvr7ymnjpbh"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "cryptonite-conduit")))
    (inputs
     (list ghc-conduit
           ghc-conduit-extra
           ghc-cryptonite
           ghc-exceptions
           ghc-memory
           ghc-resourcet))
    (native-inputs
     (list ghc-conduit-combinators ghc-tasty ghc-tasty-hunit
           ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision
       ("1" "1hh2nzfz4qpxgivfilgk4ll416lph8b2fdkzpzrmqfjglivydfmz")))
    (home-page "https://github.com/haskell-crypto/cryptonite-conduit")
    (synopsis "Cryptonite bridge for conduit")
    (description "This package provides conduit interfaces for some of
cryptonite's implementations of cryptographic primitives.")
    (license license:bsd-3)))
