;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Arthur Rodrigues <arthurhdrodrigues@proton.me>
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

(define-module (gnu packages docker)
  #:use-module (guix deprecation))

;; XXX: Deprecated on <2026-09-18>.
(define-deprecated/public-alias go-github-com-compose-spec-compose-go-v2
  (@ (gnu packages containers) go-github-com-compose-spec-compose-go-v2))

(define-deprecated/public-alias go-github-com-docker-go-events
  (@ (gnu packages containers) go-github-com-docker-go-events))

(define-deprecated/public-alias go-github-com-docker-go-metrics
  (@ (gnu packages containers) go-github-com-docker-go-metrics))

(define-deprecated/public-alias go-github-com-moby-policy-helpers
  (@ (gnu packages containers) go-github-com-moby-policy-helpers))

(define-deprecated/public-alias python-docker
  (@ (gnu packages containers) python-docker))

(define-deprecated/public-alias python-docker-5
  (@ (gnu packages containers) python-docker-5))

(define-deprecated/public-alias python-dockerpty
  (@ (gnu packages containers) python-dockerpty))

(define-deprecated/public-alias docker-compose
  (@ (gnu packages containers) docker-compose))

(define-deprecated/public-alias docker-policy-helper
  (@ (gnu packages containers) docker-policy-helper))

(define-deprecated/public-alias python-docker-pycreds
  (@ (gnu packages containers) python-docker-pycreds))

(define-deprecated/public-alias python-udocker
  (@ (gnu packages containers) python-udocker))

(define-deprecated/public-alias containerd
  (@ (gnu packages containers) containerd))

(define-deprecated/public-alias docker-libnetwork-cmd-proxy
  (@ (gnu packages containers) docker-libnetwork-cmd-proxy))

(define-deprecated/public-alias docker
  (@ (gnu packages containers) docker))

(define-deprecated/public-alias docker-cli
  (@ (gnu packages containers) docker-cli))

(define-deprecated/public-alias cqfd
  (@ (gnu packages containers) cqfd))

(define-deprecated/public-alias tini
  (@ (gnu packages containers) tini))

(define-deprecated/public-alias docker-registry
  (@ (gnu packages containers) docker-registry))
