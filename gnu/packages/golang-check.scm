;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2019 Brian Leung <bkleung89@gmail.com>
;;; Copyright © 2019 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Joseph LaFreniere <joseph@lafreniere.xyz>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2023 Benjamin <benjamin@uvy.fr>
;;; Copyright © 2023 Felix Lechner <felix.lechner@lease-up.com>
;;; Copyright © 2023 Fries <fries1234@protonmail.com>
;;; Copyright © 2023 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2024 Greg Hogan <code@greghogan.com>
;;; Copyright © 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2024 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2024 Herman Rimm <herman@rimm.ee>
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

(define-module (gnu packages golang-check)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-xyz))

;;; Commentary:
;;;
;;; Golang packages providing tools to unit-test, mock, assert, and lint
;;; processes for the Golang itself. They may provide executables and
;;; libraries, for which there are marked sections.
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

;;;
;;; Libraries:
;;;

(define-public go-atomicgo-dev-assert
  (package
    (name "go-atomicgo-dev-assert")
    (version "0.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/atomicgo/assert")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ra5bx3w6vynwbxgsz5knibk2xwmfi6654fsi29zsmk77f39g8vv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "atomicgo.dev/assert"))
    (home-page "https://atomicgo.dev/assert")
    (synopsis "Go package with tons of assertions")
    (description
     "Package assert provides obj set of assertion functions.  Every assertion
function returns obj boolean.  This package does not integrate into the
testing package automatically and requires to check the returning boolean
value and call @code{t.Fatal()} if the assertion fails.")
    (license license:expat)))

(define-public go-github-com-alecthomas-assert-v2
  (package
    (name "go-github-com-alecthomas-assert-v2")
    (version "2.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/assert")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05jak1v9s2wrwrn6ar0s4388f7qg15q0qfmhfcswgl88720196z3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/alecthomas/assert/v2"))
    (propagated-inputs
     (list go-github-com-alecthomas-repr
           go-github-com-hexops-gotextdiff))
    (home-page "https://github.com/alecthomas/assert/")
    (synopsis "Go assertion library")
    (description "Assertion library that:
@itemize
@item makes spotting differences in equality much easier
@item uses repr and diffmatchpatch to display structural differences in colour
@item aborts tests on first assertion failure
@end itemize")
    (license license:expat)))

(define-public go-github-com-caarlos0-testfs
  (package
    (name "go-github-com-caarlos0-testfs")
    (version "0.4.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/caarlos0/testfs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g7acw554f2d4y35qipdz5c627w83jxmq1z32d7nkpchzj0y7rf1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/caarlos0/testfs"))
    (home-page "https://github.com/caarlos0/testfs")
    (synopsis "Golang @code{fs.FS} implementation to be used inside tests")
    (description
     "Package testfs provides a simple @code{fs.FS} which is contained in a
test (using testing.TB's @code{TempDir}) and with a few helper methods.")
    (license license:expat)))

(define-public go-github-com-cheekybits-is
  (let ((commit "68e9c0620927fb5427fda3708222d0edee89eae9")
        (revision "0"))
    (package
      (name "go-github-com-cheekybits-is")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cheekybits/is")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1mkbyzhwq3rby832ikq00nxv3jnckxsm3949wkxd8ya9js2jmg4d"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/cheekybits/is"))
      (home-page "https://github.com/cheekybits/is")
      (synopsis "Mini testing helper for Go")
      (description "A mini testing helper for Go.

@itemize
@item It has a simple interface (@command{is.OK} and @command{is.Equal}).
@item It plugs into existing Go toolchain (uses @command{testing.T}).
@item It's obvious for newcomers.
@item It also gives you @command{is.Panic} and @command{is.PanicWith} helpers
- because testing panics is ugly.
@end itemize\n")
      (license license:expat))))

(define-public go-github-com-chzyer-test
  (package
    (name "go-github-com-chzyer-test")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chzyer/test")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1axdlcnx2qjsn5wsr2pr1m0w0a8k4nk5kkrngh742fgh81vzzy8s"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Tests relating to a in-memory disk fail due to a Segfault see
      ;; <https://github.com/chzyer/test/issues/4>.
      #:tests? #f
      #:import-path "github.com/chzyer/test"))
    (propagated-inputs
     (list go-github-com-chzyer-logex))
    (home-page "https://github.com/chzyer/test")
    (synopsis "Testing library for Go")
    ;; Description is not provided, see
    ;; <https://github.com/chzyer/test/issues/3>.
    (description
     "A testing library for Go programs.")
    (license license:expat)))

(define-public go-github-com-corpix-uarand
  (package
    (name "go-github-com-corpix-uarand")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/corpix/uarand")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ad5k1h2qpam2cfalwkjigrwg1yc45dny10n08qmqix1gxyjillc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/corpix/uarand"))
    (native-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/corpix/uarand")
    (synopsis "Random user-agent generator for Golang")
    (description
     "This package implements a functionality to generate random user-agent
strings which may be used in mock tests.")
    (license license:unlicense)))

(define-public go-github-com-davecgh-go-spew
  (package
    (name "go-github-com-davecgh-go-spew")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/davecgh/go-spew")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hka6hmyvp701adzag2g26cxdj47g21x6jz4sc6jjz1mn59d474y"))))
    (build-system go-build-system)
    (arguments
     '(#:unpack-path "github.com/davecgh/go-spew"
       #:import-path "github.com/davecgh/go-spew/spew"))
    (home-page "https://github.com/davecgh/go-spew")
    (synopsis "Deep pretty printer for Go data structures to aid in debugging")
    (description "Package @command{spew} implements a deep pretty printer
for Go data structures to aid in debugging.

A quick overview of the additional features spew provides over the built-in
printing facilities for Go data types are as follows:

@itemize
@item Pointers are dereferenced and followed.
@item Circular data structures are detected and handled properly.
@item Custom Stringer/error interfaces are optionally invoked, including on
unexported types.
@item Custom types which only implement the Stringer/error interfaces via a
pointer receiver are optionally invoked when passing non-pointer variables.
@item Byte arrays and slices are dumped like the hexdump -C command which
includes offsets, byte values in hex, and ASCII output (only when using Dump
style).
@end itemize")
    (license license:isc)))

(define-public go-github-com-dgryski-go-ddmin
  (package
    (name "go-github-com-dgryski-go-ddmin")
    (version "0.0.0-20210904190556-96a6d69f1034")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dgryski/go-ddmin")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rgv4km7nffsjlyc4jkzy68mzhy38l7fdv7h5szv36wri7cx7n77"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dgryski/go-ddmin"))
    (home-page "https://github.com/dgryski/go-ddmin")
    (synopsis "Delta-minimization algorithm in Golang")
    (description
     "Package ddmin implements the
@url{https://users.cs.utah.edu/~regehr/papers/mintest.pdf, delta-minimization}
test minimization algorithm.")
    (license license:bsd-2)))

(define-public go-github-com-elgris-jsondiff
  (package
    (name "go-github-com-elgris-jsondiff")
    (version "0.0.0-20160530203242-765b5c24c302")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elgris/jsondiff")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jm1q0s531hmkqdx8jqphfpmzysn44aphkpwlzqwp3hkz89g4d4q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/elgris/jsondiff"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-mgutz-ansi))
    (home-page "https://github.com/elgris/jsondiff")
    (synopsis "JSON diff diagnostics for Golang")
    (description
     "This package provides a simple little tool that produces readable diff
of 2 JSON-able and convertible to @code{map[string]interface{}} objects.
Useful for diagnostics or debugging")
    (license license:expat)))

(define-public go-github-com-felixge-fgprof
  (package
    (name "go-github-com-felixge-fgprof")
    (version "0.9.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/felixge/fgprof")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00h4kphvmbcdgad0wmqbaclc4a1pipdb55ay41mwh6cnkdjjvhp0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/felixge/fgprof"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-google-pprof))
    (home-page "https://github.com/felixge/fgprof")
    (synopsis "Sampling profiler for Golang")
    (description
     "@code{fgprof} is a sampling Go profiler providing analyze On-CPU as well
as @url{http://www.brendangregg.com/offcpuanalysis.html, Off-CPU} (e.g. I/O)
time together.

Go's builtin sampling CPU profiler can only show On-CPU time, but it's better
than fgprof at that.  Go also includes tracing profilers that can analyze I/O,
but they can't be combined with the CPU profiler.

fgprof is designed for analyzing applications with mixed I/O and CPU
workloads.  This kind of profiling is also known as wall-clock profiling.")
    (license license:expat)))

;; XXX: The project looks like abandoned, see
;; <https://github.com/frankban/quicktest/issues/172>, remove when nothing
;; depends on it.
(define-public go-github-com-frankban-quicktest
  (package
    (name "go-github-com-frankban-quicktest")
    (version "1.14.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/frankban/quicktest")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gnh9v4cg39pw8y356299zhh5jmq2p4cf9945lfiqsjsk7h6ld70"))))
    (build-system go-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-skip" (string-join
                       (list "TestReportOutput"
                             "TestIndirectReportOutput"
                             "TestMultilineReportOutput"
                             "TestCmpReportOutput"
                             "TestTopLevelAssertReportOutput")
                       "|"))
      #:import-path "github.com/frankban/quicktest"))
    (propagated-inputs
     (list go-github-com-google-go-cmp
           go-github-com-kr-pretty))
    (home-page "https://github.com/frankban/quicktest")
    (synopsis "Quick helpers for testing Go applications")
    (description
     "Package quicktest provides a collection of Go helpers for writing
tests.")
    (license license:expat)))

(define-public go-github-com-go-quicktest-qt
  (package
    (name "go-github-com-go-quicktest-qt")
    (version "1.101.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-quicktest/qt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mc10cszgm760aw82jyrgvld5dqcfnrsjy9zx1dzf9px34d8vlgx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-quicktest/qt"
      #:test-flags
      #~(list "-skip" (string-join
                       ;; Tests failing with assertion error or could not find
                       ;; test files.
                       (list "TestReportOutput"
                             "TestIndirectReportOutput"
                             "TestMultilineReportOutput"
                             "TestCmpReportOutput"
                             "TestTopLevelAssertReportOutput")
                       "|"))))
    (propagated-inputs
     (list go-github-com-google-go-cmp
           go-github-com-kr-pretty))
    (home-page "https://github.com/go-quicktest/qt")
    (synopsis "Qt: quicker Go tests")
    (description
     "Package qt implements assertions and other helpers wrapped around the
standard library's testing types.")
    (license license:expat)))

(define-public go-github-com-go-test-deep
  (package
    (name "go-github-com-go-test-deep")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-test/deep")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vjzmd4gc3zr9hlpzhhq4g1d0k6rbhply10vdl49gvir4dzmzzrl"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/go-test/deep"))
    (home-page "https://github.com/go-test/deep")
    (synopsis "Human-friendly deep variable equality in Go")
    (description
     "The deep package provides the deep.Equal function which is like
reflect.DeepEqual but returns a list of differences.  This is helpful
when comparing complex types like structures and maps.")
    (license license:expat)))

(define-public go-github-com-golang-mock
  (package
    (name "go-github-com-golang-mock")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/mock")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hara8j0x431njjhqxfrg1png7xa1gbrpwza6ya4mwlx76hppap4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/golang/mock"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Workaround for go-build-system's lack of Go modules
          ;; support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v"
                          ;; Network access required
                          "-skip" "TestFileParser|TestImportsOfFile"
                          "./..."))))))))
    (propagated-inputs
     (list go-golang-org-x-tools go-golang-org-x-mod))
    (home-page "https://github.com/golang/mock")
    (synopsis "Mocking framework for Golang")
    (description
     "gomock is a mocking framework for the @url{http://golang.org/,Go
programming language}.  It integrates well with Go's built-in @code{testing}
package, but can be used in other contexts too.")
    (license license:asl2.0)))

(define-public go-github-com-golangplus-testing
  (package
    (name "go-github-com-golangplus-testing")
    (version "1.0.0")
    (home-page "https://github.com/golangplus/testing")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a29m4zplf9m14k74lrb55dids2l17vx28sv0g3y3qcv1xygksiv"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/golangplus/testing"))
    (propagated-inputs
     (list go-github-com-golangplus-fmt))
    (synopsis "Additions to Go's standard testing package")
    (description "This package provides additions to Go's stdlib testing.")
    (license license:bsd-3)))

(define-public go-github-com-google-gofuzz
  (package
    (name "go-github-com-google-gofuzz")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/gofuzz")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19bykk6y9d1ivylxchkx1r1d02xrh3wfvvd02zvr5qv5ippv78ag"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/gofuzz"))
    (home-page "https://github.com/google/gofuzz")
    (synopsis "Fuzz testing library for Go")
    (description
     "Gofuzz is a library for populationg Go objects with random values for
the purpose of fuzz testing.")
    (license license:asl2.0)))

;; XXX: Placing to (gnu package profiling) creates some failing cycles.
(define-public go-github-com-google-pprof
  (package
    (name "go-github-com-google-pprof")
    (version "0.0.0-20240402174815-29b9bb013b0f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/pprof")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09l37q5dql0q0zj8amlnrynajfvp1vrp846q5vgiwsbwz9b78f18"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ; source only package
      #:import-path "github.com/google/pprof"
      #:phases
      #~(modify-phases %standard-phases
          ;; To make this package smaller to use as a library where just
          ;; source is required.
          (delete 'build))))
    (propagated-inputs
     (list go-github-com-chzyer-readline
           go-github-com-ianlancetaylor-demangle
           go-golang-org-x-sys))
    (home-page "https://github.com/google/pprof")
    (synopsis "Visualization and analysis of profiling data")
    (description
     "@code{pprof} is a tool for visualization and analysis of profiling data.

It reads a collection of profiling samples in profile.proto format and
generates reports to visualize and help analyze the data.  It can generate
both text and graphical reports (through the use of the dot visualization
package).")
    (license (list
              ;; For go code: LICENSE
              license:asl2.0
              ;; For svgpan: third_party/svgpan/LICENSE
              ;; original source <https://code.google.com/archive/p/svgpan/>.
              license:bsd-3
              ;; For d3flamegraph: third_party/d3flamegraph/D3_LICENSE
              ;;
              ;; Bundle of d3-flame-graph and d3-selection JavaScript library
              ;; (NPM) <https://www.npmjs.com/package/d3-flame-graph> and
              ;; <https://www.npmjs.com/package/d3-selection>.
              license:asl2.0 ; for bundle and d3-flame-graph
              license:isc    ; for d3-selection
              ))))

(define-public go-github-com-hexops-gotextdiff
  (package
    (name "go-github-com-hexops-gotextdiff")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hexops/gotextdiff")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vgq6w0cfhr76qlczgm5khsj1wnjkva0vhkh3qspaa1nkfw3jny1"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/hexops/gotextdiff"))
    (home-page "https://github.com/hexops/gotextdiff")
    (synopsis "Unified text diffing in Go")
    (description
     "This package provides a library to generate unified diffs.")
    (license license:bsd-3)))

(define-public go-github-com-icrowley-fake
  (package
    (name "go-github-com-icrowley-fake")
    (version "0.0.0-20240710202011-f797eb4a99c0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/icrowley/fake")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zb2rdck6fwjw1ib7k6gm9d9mnsig4cr2807k7v5z6nwqka1pcw1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/icrowley/fake"))
    (propagated-inputs
     (list go-github-com-corpix-uarand))
    (home-page "https://github.com/icrowley/fake")
    (synopsis "Fake data generator for Golang")
    (description
     "Package fake is the fake data generatror for go (Golang), heavily
inspired by forgery and @url{https://github.com/ffaker/ffaker,ffaker} Ruby
gems.")
    (license license:expat)))

(define-public go-github-com-jackc-fake
  (package
    (inherit go-github-com-icrowley-fake)
    (name "go-github-com-jackc-fake")
    (version "0.0.0-20150926172116-812a484cc733")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jackc/fake")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f32qgzhx7pl3s0g4v916z21kfyh5v1dv28aakxisiw23936wf68"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Module name has not been changed after been forked upstream.
            (substitute* (find-files "." "\\.go$")
              (("github.com/icrowley/fake") "github.com/jackc/fake"))))))
    (arguments
     (list
      #:import-path "github.com/jackc/fake"))
    (home-page "https://github.com/jackc/fake")
    (description
     "This package is an alternative fork of @url{github.com/icrowley/fake}
used in go-github-com-jackc-pgx.")
    (license license:expat)))

(define-public go-github-com-jackc-pgmock
  (package
    (name "go-github-com-jackc-pgmock")
    (version "0.0.0-20210724152146-4ad1a8207f65")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jackc/pgmock")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "189hp5fkvavwgg7z0z9b9xj88ypsphvb7s4dpwa5aj42jm39nqha"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jackc/pgmock"))
    (native-inputs
     (list go-github-com-jackc-pgconn-bootstrap
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-jackc-pgproto3-v2))
    (home-page "https://github.com/jackc/pgmock")
    (synopsis "PostgreSQL server mocking library")
    (description
     "This package implements a functionality to mock a PostgreSQL server.")
    (license license:expat)))

(define-public go-github-com-jackc-pgmock-bootstrap
  (hidden-package
   (package
     (inherit go-github-com-jackc-pgmock)
     (arguments
      (list #:tests? #f
            #:import-path "github.com/jackc/pgmock"
            #:phases
            #~(modify-phases %standard-phases
                (delete 'build))))
      (native-inputs '()))))

(define-public go-github-com-jacobsa-oglematchers
  (package
    (name "go-github-com-jacobsa-oglematchers")
    (version "0.0.0-20150720000706-141901ea67cd")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jacobsa/oglematchers")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09ff5x6vbhd9zl1z4yzyk573ifh16rry38q1rx986kbz4hqkmniq"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jacobsa/oglematchers"
       ;; break loop with with go-github-com-jacobsa-ogletest
       #:tests? #f))
    (home-page "https://github.com/jacobsa/oglematchers")
    (synopsis "Matchers for Go testing framework")
    (description
     "Package oglematchers provides a set of matchers useful in a testing or
mocking framework.  These matchers are inspired by and mostly compatible with
Google Test for C++ and Google JS Test.")
    (license license:asl2.0)))

(define-public go-github-com-jacobsa-oglemock
  (package
    (name "go-github-com-jacobsa-oglemock")
    (version "0.0.0-20150831005832-e94d794d06ff")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jacobsa/oglemock")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "14yxf8ykwdwkcccksl6741xgzcf8qykyi58kp4maxpgscqhdl8rq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jacobsa/oglemock"
      ;; break loop with with go-github-com-jacobsa-ogletest
      #:tests? #f))
    (native-inputs
     (list go-github-com-jacobsa-oglematchers))
    (home-page "https://github.com/jacobsa/oglemock")
    (synopsis "Mocking framework for unit tests")
    (description
     "Package oglemock provides a mocking framework for unit tests.")
    (license license:asl2.0)))

(define-public go-github-com-jacobsa-ogletest
  (package
    (name "go-github-com-jacobsa-ogletest")
    (version "0.0.0-20170503003838-80d50a735a11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jacobsa/ogletest")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lbwbxzr75g65q07ry5k4kglxqs3ym7xkvqznzm55rm3qk76v83r"))))
    (build-system go-build-system)
    (arguments
     (list
       #:import-path "github.com/jacobsa/ogletest"
       #:test-flags #~(list "-skip" "TestGoldenFiles")))
    (propagated-inputs
     (list go-github-com-jacobsa-oglematchers
           go-github-com-jacobsa-oglemock
           go-github-com-jacobsa-reqtrace
           go-golang-org-x-net))
    (home-page "https://github.com/jacobsa/ogletest")
    (synopsis "Expressive unit tests")
    (description
     "Package ogletest provides a framework for writing expressive unit tests.
It integrates with the builtin testing package, so it works with the gotest
command.  Unlike the testing package which offers only basic capabilities for
signalling failures, it offers ways to express expectations and get nice
failure messages automatically.")
    (license license:asl2.0)))

(define-public go-github-com-jacobsa-reqtrace
  (package
    (name "go-github-com-jacobsa-reqtrace")
    (version "0.0.0-20150505043853-245c9e0234cb")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jacobsa/reqtrace")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zfyijig10896v42rvxka1n4wn6lijqz40y2281187l7mq8vv5jn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jacobsa/reqtrace"))
    (propagated-inputs
     (list go-golang-org-x-net))
    (home-page "https://github.com/jacobsa/reqtrace")
    (synopsis "Simple request tracing framework")
    (description
     "Package reqtrace contains a very simple request tracing framework.")
    (license license:asl2.0)))

(define-public go-github-com-jbenet-go-cienv
  (package
    (name "go-github-com-jbenet-go-cienv")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jbenet/go-cienv")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qyfjvr8n5chpb5zi6r9cf0danrwds3k5lbf7vp7ygcl6wnm0vmv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jbenet/go-cienv"))
    (home-page "https://github.com/jbenet/go-cienv")
    (synopsis "CI system environment variables")
    (description
     "Package @code{cienv} implements some helper functions to use during tests.
Many times certain facilities are not available, or tests must run
differently.")
    (license license:expat)))

(define-public go-github-com-marvinjwendt-testza
  (package
    (name "go-github-com-marvinjwendt-testza")
    (version "0.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/MarvinJWendt/testza")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mqvs9142wx3a352yj0zxcm8f3mclyqzzxjlpn1rsb3vrskgs8v9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/MarvinJWendt/testza"
      #:phases
      #~(modify-phases %standard-phases
          ;; An error that should be nil is not nil.  Error message: "creating
          ;; snapshot failed: <...> permission denied
          (add-before 'check 'writable-test-file
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/"
                                                       import-path
                                                       "/testdata/snapshots")
                (for-each make-file-writable
                          (list "TestSnapshotCreate_file_content.testza"
                                "TestSnapshotCreate_file_content_string.testza"))))))))
    (propagated-inputs
     (list go-atomicgo-dev-assert
           go-github-com-sergi-go-diff
           go-github-com-davecgh-go-spew
           go-github-com-klauspost-cpuid-v2
           go-github-com-pterm-pterm))
    (home-page "https://github.com/MarvinJWendt/testza")
    (synopsis "Full-featured test framework for Golang")
    (description
     "Package testza is a full-featured testing framework for Go.  It
integrates with the default test runner, so you can use it with the standard
@code{go test} tool.  Testza contains easy to use methods, like assertions,
output capturing, mocking, and much more.")
    (license license:expat)))

(define-public go-github-com-matryer-is
  (package
    (name "go-github-com-matryer-is")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/matryer/is")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04wgh8j2n19a5a4p8jjnya6yl5dm07kbbcz8gq6gj980bd9fk1ir"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/matryer/is"))
    (home-page "https://github.com/matryer/is")
    (synopsis "Lightweight testing mini-framework for Golang")
    (description
     "Package is provides a lightweight extension to the standard library's
testing capabilities.")
    (license license:expat)))

(define-public go-github-com-onsi-ginkgo
  (package
    (name "go-github-com-onsi-ginkgo")
    (version "1.16.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/onsi/ginkgo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hh6n7q92y0ai8k6rj2yzw6wwxikhyiyk4j92zgvf1zad0gmqqmz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/onsi/ginkgo"
      #:test-flags #~(list "-skip" "TestIntegration")))
    (propagated-inputs
     (list go-github-com-go-task-slim-sprig
           go-github-com-nxadm-tail
           go-github-com-onsi-gomega
           go-golang-org-x-sys
           go-golang-org-x-tools))
    (home-page "https://github.com/onsi/ginkgo")
    (synopsis "BDD-style testing framework for Go")
    (description
     "Ginkgo is a Behaviour-Driven Development testing framework for Go.  It
builds on top of Go's builtin @code{testing} library and is complemented by the
Gomega matcher library.")
    (license license:expat)))

(define-public go-github-com-onsi-ginkgo-v2
  (package
    (inherit go-github-com-onsi-ginkgo)
    (name "go-github-com-onsi-ginkgo-v2")
    (version "2.22.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/onsi/ginkgo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mla4hr73ykbhl2mr40vzr4fjl97whr17ip907cac78fzch1csn8"))))
    (arguments
     (list
      #:import-path "github.com/onsi/ginkgo/v2"
      #:test-subdirs
      ;; XXX: Most of the tests hang, find out why, keeping bare minimal
      ;; amount.
      #~(list "dsl/..." "extensions/globals" ".")))
    (propagated-inputs
     (list go-github-com-go-logr-logr
           go-github-com-go-task-slim-sprig-v3
           go-github-com-google-pprof
           go-github-com-onsi-gomega
           go-golang-org-x-net
           go-golang-org-x-sys
           go-golang-org-x-tools))))

(define-public go-github-com-onsi-ginkgo-v2-bootstrap
  (hidden-package (package (inherit go-github-com-onsi-ginkgo-v2)
    (name "go-github-com-onsi-ginkgo-v2")
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/onsi/ginkgo/v2"
      #:phases
      #~(modify-phases %standard-phases (delete 'build))))
    (native-inputs '())
    (propagated-inputs
     (list go-github-com-go-logr-logr)))))

(define-public go-github-com-onsi-gomega
  (package
    (name "go-github-com-onsi-gomega")
    (version "1.33.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/onsi/gomega")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jygwi2lz3q7ri85dxdxf187l1hm7r3i0c843l47iakivmld31x1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/onsi/gomega"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-failing-test-files
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
              (delete-file "gexec/build_test.go")))))))
    (native-inputs
     (list go-github-com-onsi-ginkgo-v2-bootstrap))
    (propagated-inputs
     (list go-github-com-golang-protobuf
           go-golang-org-x-net
           go-golang-org-x-sys
           go-golang-org-x-text
           go-google-golang-org-protobuf
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/onsi/gomega")
    (synopsis "Matcher library for Ginkgo")
    (description
     "Gomega is the preferred matcher/assertion library for the Ginkgo test
framework.")
    (license license:expat)))

(define-public go-github-com-otiai10-mint
  (package
    (name "go-github-com-otiai10-mint")
    (version "1.6.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/otiai10/mint")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g5zhz4znp68427p2a1yvrxbq90y7caagdd7zsb4iygnhdszfm7w"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/otiai10/mint"))
    (home-page "https://github.com/otiai10/mint")
    (synopsis "Minimal assertion for Golang testing framework")
    (description
     "Mint (@code{mint.Mint}) is wrapper for @code{*testing.T} blending
testing type to omit repeated @code{t}.")
    (license license:expat)))

(define-public go-github-com-pkg-profile
  (package
    (name "go-github-com-pkg-profile")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pkg/profile")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ifr9gnycjwh7dbvsb5vgs9kzlr548cb4m45zvl8i8lgd3qhppy1"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: Unit tests failing, see
      ;; <https://github.com/pkg/profile/issues/68>.
      #:tests? #f
      #:import-path "github.com/pkg/profile"
      #:phases
      #~(modify-phases %standard-phases
          ;; profile drops a cpu.pprof file inside its source directory
          ;; after tests which makes it unreproducible so we remove it.
          (add-after 'check 'delete-test-file
            (lambda* (#:key import-path tests? #:allow-other-keys)
              (when tests?
                (delete-file (string-append "src/" import-path
                                            "/cpu.pprof"))))))))
    (propagated-inputs
     (list go-github-com-felixge-fgprof))
    (home-page "https://github.com/pkg/profile")
    (synopsis "Simple profiling for Go")
    (description
     "Profile provides a simple way to manage runtime/pprof profiling of your
Go application.")
    (license license:bsd-2)))

(define-public go-github-com-prashantv-gostub
  (package
    (name "go-github-com-prashantv-gostub")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/prashantv/gostub")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "035xf5w4fqlicdbbjcflsqflc0z5gmrn6wr7q41xwqfwfpraf9ah"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/prashantv/gostub"))
    (native-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/prashantv/gostub")
    (synopsis "Stubbing library for Go")
    (description
     "Package gostub is used for stubbing variables in tests, and resetting the
original value once the test has been run.")
    (license license:expat)))

(define-public go-github-com-petermattis-goid
  (let ((commit "bb94eb51e7a772d09cef11768f3248ac25adf9f9")
        (revision "2"))
    (package
      (name "go-github-com-petermattis-goid")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/petermattis/goid")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0hr94frr0rhac4mb9r7ixdgr6hm63rxh6z43rhn2wn7fdy8csw11"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/petermattis/goid"))
      (home-page "https://github.com/petermattis/goid")
      (synopsis "Identify the running goroutine")
      (description
       "This package offers a method of programmatically retrieving the
current goroutine's ID.")
      (license license:asl2.0))))

(define-public go-github-com-sasha-s-go-deadlock
  (package
    (name "go-github-com-sasha-s-go-deadlock")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sasha-s/go-deadlock")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0arb35idnyz4n118xz7p2snazqi35gk1975h1xfk0y4riiks58yz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/sasha-s/go-deadlock"))
    (propagated-inputs
     (list go-github-com-petermattis-goid))
    (home-page "https://github.com/sasha-s/go-deadlock")
    (synopsis "Deadlock detection in go")
    (description
     "This package provides tools for detecting deadlocks at run-time in Go.")
    (license license:asl2.0)))

(define-public go-github-com-stretchr-testify
  (package
    (name "go-github-com-stretchr-testify")
    (version "1.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stretchr/testify")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "12cnhk96h8b3ddlb7jfvwwavzc0j1c2iva92pszl9rv6r571ckzg"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/stretchr/testify"))
    (propagated-inputs
     (list go-github-com-davecgh-go-spew
           go-github-com-pmezard-go-difflib
           go-github-com-stretchr-objx
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/stretchr/testify")
    (synopsis "Go helper library for tests and invariant checking")
    (description "This package provide many tools for testifying that your
code will behave as you intend.

Features include:
@itemize
@item Easy assertions
@item Mocking
@item HTTP response trapping
@item Testing suite interfaces and functions.
@end itemize")
    (license license:expat)))

(define-public go-github-com-stretchr-testify-bootstrap
  (hidden-package
    (package
      (inherit go-github-com-stretchr-testify)
      (arguments
       '(#:import-path "github.com/stretchr/testify"
         #:tests? #f
         #:phases (modify-phases %standard-phases
                    (delete 'build))))
      (propagated-inputs
       (list go-gopkg-in-yaml-v3)))))

(define-public go-github-com-stvp-go-udp-testing
  (package
    (name "go-github-com-stvp-go-udp-testing")
    (version "0.0.0-20201019212854-469649b16807")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stvp/go-udp-testing")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03dzhwnvbshiivbcawaxsl963d8hh18yf3ydvzvhyjgz60g8lxil"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/stvp/go-udp-testing"))
    (home-page "https://github.com/stvp/go-udp-testing")
    (synopsis "UDP test helpers for Golang")
    (description
     "This package implements UDP test helpers.  It lets assert that certain
strings must or must not be sent to a given local UDP listener.")
    (license license:expat)))

(define-public go-github-com-tdewolff-test
  (package
    (name "go-github-com-tdewolff-test")
    (version "1.0.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tdewolff/test")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ihlcnygwdgxq068b29d3n9n1gdbb2j03sc0al1qk5i5dkvgziyx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tdewolff/test"))
    (home-page "https://github.com/tdewolff/test")
    (synopsis "Go test helper functions")
    (description
     "This package implements a few functions that are useful for io testing,
such as readers and writers that fail after N consecutive reads/writes.")
    (license license:expat)))

(define-public go-github-com-tj-assert
  (package
    (name "go-github-com-tj-assert")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tj/assert")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j5swk3fjq1h5fpqkipddz2ccnbidr7qrpm5dpdaflg9q5jnc673"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tj/assert"))
    (propagated-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/tj/assert")
    (synopsis "Golang @code{testify/assert} but as @code{testify/require}")
    (description
     "Package assert implements the same assertions as the
@url{https://github.com/stretchr/testify, assert} package but stops test
execution when a test fails.")
    (license license:expat)))

(define-public go-github-com-smartystreets-goconvey
  (package
    (name "go-github-com-smartystreets-goconvey")
    (version "1.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/smartystreets/goconvey")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s9s7yd4jfwgirnz46kw1sfhgcgsdzfxlca6q16i6ixaqczfaap9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/smartystreets/goconvey"
      #:test-flags
      ;; XXX: Figure out why these test fail.
      #~(list "-skip" (string-join
                       (list "TestShellIntegration"
                             "TestStackModeMultipleInvocationInheritance"
                             "TestStackModeMultipleInvocationInheritance2"
                             "TestStackModeMultipleInvocationInheritance3"
                             "TestWatcher")
                       "|"))))
    (propagated-inputs
     (list go-github-com-jtolds-gls
           go-github-com-smarty-assertions
           go-golang-org-x-tools))
    (home-page "https://github.com/smartystreets/goconvey")
    (synopsis "Go testing tool with both a web and terminal user interface")
    (description
     "GoConvey is a testing tool for Go.  It integrates with go test, can show
test coverage and has a web user interface that will refresh automatically.")
    (license license:expat)))

(define-public go-github-com-smarty-assertions
  (package
    (name "go-github-com-smarty-assertions")
    (version "1.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/smarty/assertions")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kbl6h76mjvqkgszx81allhjzy8j331dbsb090rx134swbqs0pxc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/smarty/assertions"))
    (home-page "https://github.com/smarty/assertions")
    (synopsis "Fluent assertion-style functions")
    (description
     "Package assertions contains the implementations for all assertions which
are referenced in goconvey's
@url{https://github.com/smartystreets/goconvey,@code{convey}} package and
gunit @url{github.com/smarty/gunit,@code{gunit}} for use with the
@code{So(...)}  method.  They can also be used in traditional Go test
functions and even in applications.")
    (license license:expat)))

(define-public go-github-com-smarty-gunit
  (package
    (name "go-github-com-smarty-gunit")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/smarty/gunit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13bcb1aq8yshmi5inn7np5lyqhsyy5hksridi8bxbjq35xrknskr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/smarty/gunit"))
    (home-page "https://github.com/smarty/gunit")
    (synopsis "Golang xUnit-style test fixture test adapter")
    (description
     "Package gunit provides @code{testing} package hooks and convenience
functions for writing tests in an @code{xUnit} style.")
    (license license:expat)))

(define-public go-github-com-viant-assertly
  (package
    (name "go-github-com-viant-assertly")
    (version "0.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/viant/assertly")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mli7kfkaz3k4izx76w14qhq5a8bp6x1zw9471idrhg5wxg1mr1r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/viant/assertly"
      #:test-flags #~(list "-skip" "TestAssertCoalesceWithZero")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              ;; failed to expand macro 1<ds:env[\"USER\"]>3, path:[/]:,
              ;; failed to lookup USER in env.
              (setenv "USER" "guix"))))))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-viant-toolbox))
    (home-page "https://github.com/viant/assertly")
    (synopsis "Data structure testing library)")
    (description
     "This library enables complex data structure testing, specifically:
@itemize
@item realtime transformation or casting of incompatible data types with
directives system
@item consistent way of testing of unordered structures
@item contains, Range, RegExp support on any data structure deeph level
@item switch case directive to provide expected value alternatives based on
actual switch/case input match
@item macro system enabling complex predicate and expression evaluation, and
customization
@end itemize")
    (license license:asl2.0)))

(define-public go-github-com-viant-assertly-bootstrap
  (hidden-package
   (package
     (inherit go-github-com-viant-assertly)
     (arguments
      (list #:tests? #f
            #:import-path "github.com/viant/assertly"
            #:phases
            #~(modify-phases %standard-phases
                (delete 'build))))
      (native-inputs '())
      (propagated-inputs '()))))

(define-public go-go-abhg-dev-requiredfield
  (package
    (name "go-go-abhg-dev-requiredfield")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/abhinav/requiredfield")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15dccs71is06wi8xi3y2nnwpcpqbsh4pas4iggdr514aix8ljknf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.abhg.dev/requiredfield"))
    (propagated-inputs
     (list go-golang-org-x-tools))
    (home-page "https://go.abhg.dev/requiredfield")
    (synopsis "Linter for required struct fields")
    (description
     "This package implements a linter that checks for required fields during
struct initialization.")
    (license license:bsd-3)))

(define-public go-go-abhg-dev-testing-stub
  (package
    (name "go-go-abhg-dev-testing-stub")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/abhinav/stub-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04by4hq9lhmz3ij2rdl053nr76l65q5w8w41khxgr5xak8s63yq6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.abhg.dev/testing/stub"))
    (home-page "https://github.com/abhinav/stub-go/tree")
    (synopsis "Trivial stubbing package for Go")
    (description
     "Package stub provides helper functions to replace global variables for testing,
and restore them afterwards.")
    (license license:bsd-3)))

(define-public go-go-etcd-io-gofail
  (package
    (name "go-go-etcd-io-gofail")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/etcd-io/gofail")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wxjaq1v5w0wjyv84af5cazrmv369i1416ar0dx8r9ag1szcfvpc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.etcd.io/gofail"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/examples")))))))
    (native-inputs (list go-github-com-stretchr-testify))
    (home-page "https://pkg.go.dev/go.etcd.io/gofail")
    (synopsis "Failpoints for go")
    (description
     "This package provides an implementation of
@url{http://www.freebsd.org/cgi/man.cgi?query=fail,failpoints} for Golang.")
    (license license:asl2.0)))

(define-public go-golang-org-sql-mock
  (package
    (name "go-golang-org-sql-mock")
    (version "1.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DATA-DOG/go-sqlmock")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vpvdx9hwmx9gm27aq5r5219xpaxz0gy4q1iqskk4saz05bspn0f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/DATA-DOG/go-sqlmock"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/examples")))))))
    (home-page "https://github.com/DATA-DOG/go-sqlmock")
    (synopsis "Mock library implementing @code{sql/driver}")
    (description
     "This library simulates SQL-driver behavior in tests without requiring a
real database connection.")
    (license license:expat)))

(define-public go-golang-org-x-lint
  (package
    (name "go-golang-org-x-lint")
    (version "0.0.0-20241112194109-818c5a804067")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/lint")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06ni2jpd3s5bzg2qrry58svakkg9k43gkgkrbk8f8x886qnhnimp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "golang.org/x/lint"))
    (propagated-inputs
     (list go-golang-org-x-tools))
    (home-page "https://golang.org/x/lint")
    (synopsis "Linter for Go source code")
    (description
     "This is a linter for Go source code.  Unlike gofmt, it doesn't reformat
the source code, it only prints out style mistakes.")
    (license license:bsd-3)))

;; XXX: Unmaintained since 2020, see
;; <https://github.com/go-check/check/issues/111>.
(define-public go-gopkg-in-check-v1
  (package
    (name "go-gopkg-in-check-v1")
    (version "1.0.0-20201130134442-10cb98267c6c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-check/check")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1jwxndf8rsyx0fgrp47d99rp55yzssmryb92jfj3yf7zd8rjjljn"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Most tests failed.
      #:tests? #f
      #:import-path "gopkg.in/check.v1"))
    (propagated-inputs
     (list go-github-com-kr-pretty))
    (home-page "https://gopkg.in/check.v1")
    (synopsis "Test framework for the Go language")
    (description "This package provides a test library for the Go language.")
    (license license:bsd-2)))

(define-public go-gopkg-in-dnaeon-go-vcr-v3
  (package
    (name "go-gopkg-in-dnaeon-go-vcr-v3")
    (version "3.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gopkg.in/dnaeon/go-vcr.v3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nij7rjbnrbsgjlm7fwpg298qffrgi2ic3wb51vqzxl6s9qkbzrq"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gopkg.in/dnaeon/go-vcr.v3"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Workaround for go-build-system's lack of Go modules
          ;; support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (propagated-inputs
     (list go-gopkg-in-yaml-v3))
    (home-page "https://gopkg.in/dnaeon/go-vcr.v3")
    (synopsis "Record and replay your HTTP interactions")
    (description
     "@@code{go-vcr} simplifies testing by recording your HTTP interactions
and replaying them in future runs in order to provide fast, deterministic and
accurate testing of your code.")
    (license license:bsd-2)))

(define-public go-gopkg-in-dnaeon-go-vcr-v4
  (package
    (inherit go-gopkg-in-dnaeon-go-vcr-v3)
    (name "go-gopkg-in-dnaeon-go-vcr-v4")
    (version "4.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gopkg.in/dnaeon/go-vcr.v4")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p1a4hbk303k2bv9dmaf770dml71zr3260g5z7yd84vzhj8i0rzb"))))
    (arguments
     (list
      #:import-path "gopkg.in/dnaeon/go-vcr.v4"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Workaround for go-build-system's lack of Go modules
          ;; support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))))

(define-public go-gopkg-in-go-playground-assert-v1
  (package
    (name "go-gopkg-in-go-playground-assert-v1")
    (version "1.2.1")
    (home-page "https://github.com/go-playground/assert")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h4amgykpa0djwi619llr3g55p75ia0mi184h9s5zdl8l4rhn9pm"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gopkg.in/go-playground/assert.v1"))
    (synopsis "Basic assertion library used alongside native Go testing")
    (description
     "This package provides basic assertions along with building blocks for
custom assertions to be used alongside native Go testing.")
    (license license:expat)))

(define-public go-github-com-go-playground-assert-v2
  (package
    (inherit go-gopkg-in-go-playground-assert-v1)
    (name "go-github-com-go-playground-assert-v2")
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-playground/assert")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13mb07dxhcy9ydqbracnrpfj682g6sazjpm56yrlbn2jc1yfy44c"))))
    (arguments
     (list #:import-path "github.com/go-playground/assert/v2"))))

(define-public go-github-com-warpfork-go-testmark
  (package
    (name "go-github-com-warpfork-go-testmark")
    (version "0.12.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/warpfork/go-testmark")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06v2x3c5qgbj585a2abksr3hgvgdx61j153rjarqi9cvvzwh1xpr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/warpfork/go-testmark"))
    (propagated-inputs
     (list go-github-com-warpfork-go-fsx))
    (home-page "https://github.com/warpfork/go-testmark")
    (synopsis "Parser for @code{testmark} format")
    (description
     "@code{go-testmark} is a library to parse, patch data and test fixtures from
Markdown files, using the
@url{https://github.com/warpfork/go-testmark?tab=readme-ov-file#what-is-the-testmark-format,
testmark} format, which itself is a subset of Markdown format.")
    (license (list license:asl2.0 license:expat))))

(define-public go-github-com-warpfork-go-wish
  (package
    (name "go-github-com-warpfork-go-wish")
    (version "0.0.0-20220906213052-39a1cc7a02d0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/warpfork/go-wish")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rqbxmqwzy1q2zwy3mszp0li0pg8zzh3j9l8wlzr6p1pq2idallq"))
       (patches (search-patches
                 "go-github-com-warpfork-go-wish-fix-tests.patch"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/warpfork/go-wish"))
    (home-page "https://github.com/warpfork/go-wish")
    (synopsis "Test assertions for Golang")
    (description
     "@code{wish} is a test assertion library for Golang, designed to
gracefully enhance standard library testing package and behaviors of the
@command{go test} command.")
    (license license:expat)))

(define-public go-github-com-zeebo-assert
  (package
    (name "go-github-com-zeebo-assert")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zeebo/assert")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xfklg04ic4xl5q7xy913jzvn2v9bxmrsnm4lyjqznninysgs9xb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zeebo/assert"))
    (home-page "https://github.com/zeebo/assert")
    (synopsis "High-level assertions for tests")
    (description
     "@code{assert} is a testing library that provides high-level assertions API
based on Go @code{testing} library procedures.")
    (license license:cc0)))

(define-public go-go-uber-org-goleak
  (package
    (name "go-go-uber-org-goleak")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uber-go/goleak")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14rvkxh3znp9jzbdjqdkrly3zfg3rmhgg5845biqqrq17w8jvv5s"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.uber.org/goleak"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://pkg.go.dev/go.uber.org/goleak")
    (synopsis "Goroutine leak detector")
    (description
     "Go package to verify that there are no unexpected goroutines running at
the end of a test.")
    (license license:expat)))

(define-public go-go-uber-org-mock
  (package
    (name "go-go-uber-org-mock")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uber-go/mock")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mz1cy02m70mdh7hyaqks8bkh9iyv4jgj6h4psww52nr3b9pnyyy"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: The project contains subdirectory which complicate it's testing
      ;; and it does not produce any binary.
      #:tests? #f
      #:import-path "go.uber.org/mock"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (propagated-inputs
     (list go-golang-org-x-mod go-golang-org-x-tools))
    (home-page "https://pkg.go.dev/go.uber.org/mock")
    (synopsis "Mocking framework for the Golang")
    (description
     "This package provides a mocking framework which integrates well with
built-in @code{testing} package, but can be used in other contexts too.")
    (license license:asl2.0)))

(define-public go-honnef-co-go-tools
  (package
    (name "go-honnef-co-go-tools")
    (version "0.4.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dominikh/go-tools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1n58skq2a0vhsgdfdkyqi00d3vv13kiw9b4mxx6xfyb6ysrdy7d1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "honnef.co/go/tools"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Workaround for go-build-system's lack of Go modules support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (propagated-inputs
     (list go-github-com-burntsushi-toml
           go-golang-org-x-exp-typeparams
           go-golang-org-x-mod
           go-golang-org-x-tools))
    (home-page "https://staticcheck.dev/")
    (synopsis "Staticcheck advanced Go linter library")
    (description
     "This package provides the Go source code for the @code{go-staticcheck}
advanced Go linter.")
    (license license:expat)))

(define-public go-pgregory-net-rapid
  (package
    (name "go-pgregory-net-rapid")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/flyingmutant/rapid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1alyhcbdq6113sfymx7xxmxpcbgvkaipsn15cgjrcqrx8ks1hm5i"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "pgregory.net/rapid"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-failing-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                ;; Remove test file failing on go-1.21, see
                ;; <https://github.com/flyingmutant/rapid/issues/68>.
                (delete-file "strings_example_test.go")))))))
    (home-page "https://pgregory.net/rapid/")
    (synopsis "Go property-based testing library")
    (description
     "@code{Rapid} is a Go library for property-based testing.

Rapid checks that properties you define hold for a large number of
automatically generated test cases.  If a failure is found, rapid
automatically minimizes the failing test case before presenting it.

Features:
@itemize
@item imperative Go API with type-safe data generation using generics
@item data generation biased to explore \"small\" values and edge cases more
thoroughly
@item fully automatic minimization of failing test cases
@item persistence and automatic re-running of minimized failing test cases
@item support for state machine (\"stateful\" or \"model-based\") testing
@item no dependencies outside the Go standard library
@end itemize")
    (license license:mpl2.0)))

;;;
;;; Executables:
;;;

(define-public go-ginkgo
  (package
    (inherit go-github-com-onsi-ginkgo-v2)
    (name "ginkgo")
    (arguments
     (list
       #:import-path "github.com/onsi/ginkgo/ginkgo"
       #:unpack-path "github.com/onsi/ginkgo"
       #:install-source? #f))
    (description
     (string-append (package-description go-github-com-onsi-ginkgo-v2)
                    "  This package provides an command line interface (CLI)
tool."))))

(define-public go-keyify
  (package
    (inherit go-honnef-co-go-tools)
    (name "go-keyify")
    (arguments
     `(#:import-path "honnef.co/go/tools/cmd/keyify"
       #:unpack-path "honnef.co/go/tools"
       #:install-source? #f))
    (synopsis "Transform an unkeyed struct literal into a keyed one in Go")
    (description "This package turns unkeyed struct literals (@code{T{1, 2,
3}}) into keyed ones (@code{T{A: 1, B: 2, C: 3}}) in Go.")))

(define-public go-pgmockproxy
  (package
    (inherit go-github-com-jackc-pgmock)
    (name "go-pgmockproxy")
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/jackc/pgmock/pgmockproxy"
      #:unpack-path "github.com/jackc/pgmock"))
    (description
     "pgmockproxy is a PostgreSQL proxy that logs the messages back and forth
between the PostgreSQL client and server.  This can aid in building a mocking
script by running commands against a real server to observe the results.  It
can also be used to debug applications that speak the PostgreSQL wire protocol
without needing to use a tool like Wireshark.")))

(define-public go-pprof
  (package
    (inherit go-github-com-google-pprof)
    (name "go-pprof")
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/google/pprof"))
    (description
     (string-append (package-description go-github-com-google-pprof)
                    "  This package provides an command line interface (CLI)
tool."))))

(define-public go-staticcheck
  (package
    (inherit go-honnef-co-go-tools)
    (name "go-staticcheck")
    (arguments
     `(#:import-path "honnef.co/go/tools/cmd/staticcheck"
       #:unpack-path "honnef.co/go/tools"
       #:install-source? #f))
    (synopsis "Staticcheck advanced Go linter")
    (description
     "Staticcheck is a state of the art linter for the Go programming language.
Using static analysis, it finds bugs and performance issues, offers
simplifications, and enforces style rules.")))

(define-public go-structlayout
  (package
    (inherit go-honnef-co-go-tools)
    (name "go-structlayout")
    (arguments
     `(#:import-path "honnef.co/go/tools/cmd/structlayout"
       #:unpack-path "honnef.co/go/tools"
       #:install-source? #f))
    (synopsis "Display the layout (field sizes and padding) of structs in Go")
    (description "This package prints the layout of a struct in Go, which is
the byte offset and size of each field, respecting padding.  This information
is printed in human-readable form by default, or as JSON with the @code{-json}
flag.")))

(define-public go-structlayout-optimize
  (package
    (inherit go-honnef-co-go-tools)
    (name "go-structlayout-optimize")
    (arguments
     `(#:import-path "honnef.co/go/tools/cmd/structlayout-optimize"
       #:unpack-path "honnef.co/go/tools"
       #:install-source? #f))
    (synopsis "Reorder struct fields to minimize the amount of padding in Go")
    (description "This package reads @code{go-structlayout} JSON on stdin and
reorders fields to minimize the amount of padding.  It can emit JSON to feed
into @code{go-structlayout-pretty}.")))

(define-public go-structlayout-pretty
  (package
    (inherit go-honnef-co-go-tools)
    (name "go-structlayout-pretty")
    (arguments
     `(#:import-path "honnef.co/go/tools/cmd/structlayout-pretty"
       #:unpack-path "honnef.co/go/tools"
       #:install-source? #f))
    (synopsis "Format the output of go-structlayout with ASCII art in Go")
    (description "This package takes @code{go-structlayout}-like JSON and
prints an ASCII fraphic representing the memory layout.")))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
