;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Igorj Gorjaĉev <igor@goryachev.org>
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

(define-module (test-mix)
  #:use-module (guix import mix)
  #:use-module (guix base32)
  #:use-module (guix build-system mix)
  #:use-module (gcrypt hash)
  #:use-module (guix tests)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match))

(define test-mix-lock-file
  "\
%{
  \"gen_stage\": {:hex, :gen_stage, \"1.3.2\", \
\"7c77e5d1e97de2c6c2f78f306f463bca64bf2f4c3cdd606affc0100b89743b7b\", \
[:mix], [], \"hexpm\", \
\"0ffae547fa777b3ed889a6b9e1e64566217413d018cabd825f786e843ffe63e7\"},
  \"eini\": {:hex, :eini_beam, \"2.2.4\", \
\"02143b1dce4dda4243248e7d9b3d8274b8d9f5a666445e3d868e2cce79e4ff22\", \
[:rebar3], [], \"hexpm\", \
\"12de479d144b19e09bb92ba202a7ea716739929afdf9dff01ad802e2b1508471\"},
  \"erlydtl\": {:git, \"https://github.com/manuel-rubio/erlydtl.git\", \
\"dffa1a73ee2bfba14195b8b3964c39f007ff1284\", []},
}")

(define temp-file
  (string-append "t-mix-" (number->string (getpid))))

(test-begin "mix")

(test-assert "mix-lock-file-import"
  (begin
    (call-with-output-file temp-file
      (lambda (port)
        (display test-mix-lock-file port)))
    (mock
     ((guix scripts download) guix-download
      (lambda _
        (format #t "~a~%~a~%"
                "/gnu/store/m43vixiijc26ni5p9zvbvjrs311h4fsm-erlydtl-dffa1a7"
                "1jhcfh0idadlh9999kjzx1riqjw0k05wm6ii08xkjvirhjg0vawh")))
     (let-values
         (((source-expressions beam-inputs-entry)
           (mix-lock->expressions temp-file "test")))
       (and
        (match source-expressions
          ('((define beam-gen-stage-1.3.2
               (hexpm-source
                "gen_stage" "gen_stage" "1.3.2"
                "1rv3zqzq8vkqby1bvjhqs09p88b68pkf3fd6i7c3wyvpz93ybyhg"))
             (define beam-eini-2.2.4
               (hexpm-source
                "eini" "eini_beam" "2.2.4"
                "0wc4a2qy40nq3bqdzygxka93jrvixakh58ibp6dy06ab2jflgphj"))
             (define beam-erlydtl-snapshot.dffa1a7
               (origin
                 (method git-fetch)
                 (uri (git-reference
                        (url "https://github.com/manuel-rubio/erlydtl.git")
                        (commit "dffa1a73ee2bfba14195b8b3964c39f007ff1284")))
                 (file-name (git-file-name "beam-erlydtl" "snapshot.dffa1a7"))
                 (sha256
                  (base32
                   "1jhcfh0idadlh9999kjzx1riqjw0k05wm6ii08xkjvirhjg0vawh")))))
           #t)
          (x
           (pk 'fail (pretty-print-with-comments (current-output-port) x) #f)))
        (match beam-inputs-entry
          (`(test => (list beam-gen-stage-1.3.2
                           beam-eini-2.2.4
                           beam-erlydtl-snapshot.dffa1a7))
           #t)
          (x
           (pk 'fail x #f))))))))

(test-end "mix")

(false-if-exception (delete-file temp-file))
