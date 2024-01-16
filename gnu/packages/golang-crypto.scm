;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
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

(define-module (gnu packages golang-crypto)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-check))

;;; Commentary:
;;;
;;; Golang modules (libraries) related to Cryptography: encryption algorithms,
;;; hashing functions, TLS, key management, digital signatures, password
;;; hashing etc.
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

(define-public go-github-com-multiformats-go-multihash
  (let ((commit "97cdb562a04c6ef66d8ed40cd62f8fbcddd396d6")
        (revision "0"))
    (package
      (name "go-github-com-multiformats-go-multihash")
      (version (git-version "1.0.8" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/multiformats/go-multihash")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "02wd9akrwy4y5m0nig9m24p14bjjgb4n1djydrq8cm4yhbvjrrk0"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/multiformats/go-multihash"))
      (native-inputs
       (list go-github-com-mr-tron-base58
             go-github-com-gxed-hashland-keccakpg
             go-github-com-minio-blake2b-simd
             go-github-com-minio-sha256-simd
             go-github-com-spaolacci-murmur3
             go-golang-org-x-crypto))
      (home-page "https://github.com/multiformats/go-multihash")
      (synopsis "Multihash implementation in Go")
      (description "Multihash implementation in Go.")
      (license license:expat))))

(define-public go-github-com-riobard-go-bloom
  (let ((commit "cdc8013cb5b3eb0efebec85f0e904efccac42df9")
        (revision "0"))
    (package
      (name "go-github-com-riobard-go-bloom")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/riobard/go-bloom")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "10a8ixh6zw52df2imxrzgxi82zc1j5hqnv5smjp818qwdn1a1rhj"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/riobard/go-bloom"))
      (home-page "https://github.com/riobard/go-bloom")
      (synopsis "Bloom filter in Go")
      (description "Go-Bloom implements bloom filter using double hashing.")
      (license license:asl2.0))))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
