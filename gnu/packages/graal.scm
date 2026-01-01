;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Danny Milosavljevic <dannym@friendly-machines.com>
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

(define-module (gnu packages graal)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system maven)
  #:use-module (gnu packages)
  #:use-module (gnu packages maven)
  #:use-module (gnu packages maven-parent-pom)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages java)
  #:use-module (gnu packages python)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages java-compression)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages version-control))

;;;
;;; GraalVM build tool and components
;;;

;; The Universal Permissive License (UPL) 1.0 is an FSF-approved,
;; GPL-compatible, permissive free software license.
;; See: <https://www.fsf.org/blogs/licensing/universal-permissive-license-added-to-license-list>
(define upl1.0
  (license:non-copyleft "https://opensource.org/licenses/UPL"
                        "Universal Permissive License 1.0"))

;;; Shared version and source definitions

(define %graalvm-version "25.0.1")

;; Main graal repository source (used by sdk, truffle)
(define %graal-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/oracle/graal")
          (commit (string-append "vm-" %graalvm-version))))
    (file-name (git-file-name "graal" %graalvm-version))
    (sha256
     (base32 "06m8nbjrjawn5falr7fzgsqqav23zapzhc5f774320cdjbj90zvx"))
    (modules '((guix build utils)))
    (snippet
     #~(begin
         (use-modules (guix build utils))
         ;; Use GCC toolchain instead of clang
         (substitute* '("sdk/mx.sdk/mx_sdk_vm_impl.py"
                        "sdk/mx.sdk/mx_sdk_vm_ng.py")
           (("sdk:LLVM_NINJA_TOOLCHAIN") "mx:DEFAULT_NINJA_TOOLCHAIN"))
         (substitute* '("sdk/mx.sdk/suite.py"
                        "truffle/mx.truffle/suite.py")
           (("\"toolchain\"\\s*:\\s*\"sdk:LLVM_NINJA_TOOLCHAIN\"")
            "\"toolchain\": \"mx:DEFAULT_NINJA_TOOLCHAIN\""))
         ;; Remove clang-specific flags that GCC doesn't understand.
         (substitute* '("sdk/mx.sdk/suite.py"
                        "sdk/mx.sdk/mx_sdk_vm_impl.py"
                        "sdk/mx.sdk/mx_sdk_vm_ng.py")
           (("-stdlib=libc\\+\\+") "")
           (("-static-libstdc\\+\\+") "")
           (("-l:libc\\+\\+abi\\.a") ""))
         ;; Fix missing #include <memory> for GCC.
         (substitute* "sdk/src/org.graalvm.launcher.native/src/launcher.cc"
           (("#include <sys/stat.h>")
            "#include <sys/stat.h>
#include <memory>"))
         ;; Patch the libffi bootstrap Makefile patch to use bash explicitly.
         (substitute* "truffle/src/libffi/patches/others/0001-Add-mx-bootstrap-Makefile.patch"
           (("\\.\\./(\\$\\(SOURCES\\))/configure" all sources)
            (string-append "$(SHELL) ../" sources "/configure")))))))

;;;
;;; MX URL Rewrite Helpers
;;;
;;; The mx build tool downloads dependencies from Maven and other URLs.
;;; MX_URLREWRITES redirects these to Guix-built JARs with computed SHA512 digests.

;; Build MX_URLREWRITES JSON from a list of rewrite specifications.
;; Each spec is: (maven-url path-pattern)
;;   maven-url: the URL mx tries to fetch
;;   path-pattern: pattern for search-input-file, or input key for assoc-ref if no "/"
(define (make-mx-urlrewrites-phase specs)
  #~(lambda* (#:key inputs #:allow-other-keys)
        (use-modules (ice-9 popen) (ice-9 rdelim))
        (define (file-sha512 path)
          (let* ((port (open-pipe* OPEN_READ "sha512sum" "--" path))
                 (line (read-line port)))
            (close-pipe port)
            (car (string-split line #\space))))
        (define (resolve-path path-pattern)
          (if (string-index path-pattern #\/)
              (search-input-file inputs path-pattern)
              (assoc-ref inputs path-pattern)))
        (define (make-rewrite spec)
          (let* ((maven-url (car spec))
                 (path-pattern (cadr spec))
                 (local-path (resolve-path path-pattern))
                 (sha512 (file-sha512 local-path)))
            (format #f "{\"~a\":{\"replacement\":\"file://~a\",\"digest\":\"sha512:~a\"}}"
                    maven-url local-path sha512)))
        (let ((rewrites (string-append "[" (string-join (map make-rewrite '#$specs) ",") "]")))
          (format #t "Setting MX_URLREWRITES:~%~a~%" rewrites)
          (setenv "MX_URLREWRITES" rewrites))))

;; Build install phase that uses `mx paths` to find distribution JARs.
;; dists: list of distribution names (e.g., '("GRAAL_SDK" "WORD"))
;; subdir: output subdirectory under lib/ (default "graal")
(define* (make-mx-install-phase dists #:optional (subdir "graal"))
  #~(lambda* (#:key outputs #:allow-other-keys)
      (use-modules (ice-9 popen) (ice-9 rdelim))
      (define (mx-paths dist)
        (let* ((port (open-pipe* OPEN_READ "mx" "--user-home" (getcwd) "paths" dist))
               (path (read-line port)))
          (close-pipe port)
          path))
      (let* ((out (assoc-ref outputs "out"))
             (lib (string-append out "/lib/" #$subdir)))
        (mkdir-p lib)
        (for-each (lambda (dist)
                    (let ((jar (mx-paths dist)))
                      (format #t "Installing ~a -> ~a~%" dist jar)
                      (install-file jar lib)))
                  '#$dists))))

;; Install mx distributions as Maven artifacts in /lib/m2 format.
;; mapping: list of (mx-dist-name group-id artifact-id [dependencies]) entries
;;   where dependencies is an optional list of (dep-group-id dep-artifact-id)
;; version: the version string for Maven coordinates
(define* (make-mx-install-m2-phase mapping version)
  #~(lambda* (#:key outputs #:allow-other-keys)
      (use-modules (ice-9 popen) (ice-9 rdelim)
                   (sxml simple))
      (define (mx-paths dist)
        (let* ((port (open-pipe* OPEN_READ "mx" "--user-home" (getcwd) "paths" dist))
               (path (read-line port)))
          (close-pipe port)
          path))
      (define (install-maven-artifact mx-dist group-id artifact-id deps)
        (let* ((out (assoc-ref outputs "out"))
               (jar (mx-paths mx-dist))
               (m2-dir (string-append out "/lib/m2/"
                                      (string-join (string-split group-id #\.) "/") "/"
                                      artifact-id "/" #$version))
               (jar-name (string-append artifact-id "-" #$version ".jar"))
               (pom-name (string-append artifact-id "-" #$version ".pom"))
               (dep-sxml (map (lambda (dep)
                                `(dependency
                                  (groupId ,(car dep))
                                  (artifactId ,(cadr dep))
                                  (version ,#$version)))
                              deps)))
          (format #t "Installing ~a -> ~a/~a~%" mx-dist m2-dir jar-name)
          (mkdir-p m2-dir)
          (copy-file jar (string-append m2-dir "/" jar-name))
          ;; Create POM with dependencies for Maven resolution
          (call-with-output-file (string-append m2-dir "/" pom-name)
            (lambda (port)
              (sxml->xml
               `(*TOP*
                 (*PI* xml "version=\"1.0\"")
                 (project
                  (modelVersion "4.0.0")
                  (groupId ,group-id)
                  (artifactId ,artifact-id)
                  (version ,#$version)
                  ,@(if (null? dep-sxml)
                        '()
                        `((dependencies ,@dep-sxml)))))
               port)))))
      (for-each (lambda (entry)
                  (install-maven-artifact (list-ref entry 0)
                                          (list-ref entry 1)
                                          (list-ref entry 2)
                                          (if (> (length entry) 3)
                                              (list-ref entry 3)
                                              '())))
                '#$mapping)))

;; ASM bytecode manipulation library (version 9.7.1)
(define %mx-rewrites-asm
  '(("https://repo1.maven.org/maven2/org/ow2/asm/asm/9.7.1/asm-9.7.1.jar"
     "share/java/asm9.jar")
    ("https://repo1.maven.org/maven2/org/ow2/asm/asm/9.7.1/asm-9.7.1-sources.jar"
     "share/java/asm9-sources.jar")
    ("https://repo1.maven.org/maven2/org/ow2/asm/asm-tree/9.7.1/asm-tree-9.7.1.jar"
     "share/java/asm-tree.jar")
    ("https://repo1.maven.org/maven2/org/ow2/asm/asm-tree/9.7.1/asm-tree-9.7.1-sources.jar"
     "share/java/asm-tree-sources.jar")
    ("https://repo1.maven.org/maven2/org/ow2/asm/asm-analysis/9.7.1/asm-analysis-9.7.1.jar"
     "share/java/asm-analysis.jar")
    ;; asm-analysis sources -> binary (no sources JAR available)
    ("https://repo1.maven.org/maven2/org/ow2/asm/asm-analysis/9.7.1/asm-analysis-9.7.1-sources.jar"
     "share/java/asm-analysis.jar")
    ("https://repo1.maven.org/maven2/org/ow2/asm/asm-util/9.7.1/asm-util-9.7.1.jar"
     "share/java/asm-util8.jar")
    ;; asm-util sources -> binary
    ("https://repo1.maven.org/maven2/org/ow2/asm/asm-util/9.7.1/asm-util-9.7.1-sources.jar"
     "share/java/asm-util8.jar")
    ("https://repo1.maven.org/maven2/org/ow2/asm/asm-commons/9.7.1/asm-commons-9.7.1.jar"
     "share/java/asm-commons8.jar")
    ("https://repo1.maven.org/maven2/org/ow2/asm/asm-commons/9.7.1/asm-commons-9.7.1-sources.jar"
     "share/java/asm-commons-sources.jar")))

;; ANTLR4 parser runtime (version 4.13.2)
(define %mx-rewrites-antlr
  '(("https://repo1.maven.org/maven2/org/antlr/antlr4-runtime/4.13.2/antlr4-runtime-4.13.2.jar"
     "share/java/java-antlr4-runtime.jar")
    ;; sources -> binary
    ("https://repo1.maven.org/maven2/org/antlr/antlr4-runtime/4.13.2/antlr4-runtime-4.13.2-sources.jar"
     "share/java/java-antlr4-runtime.jar")))

;; ICU4J unicode library (mx wants 76.1, we provide 73.2)
(define %mx-rewrites-icu
  '(("https://repo1.maven.org/maven2/com/ibm/icu/icu4j/76.1/icu4j-76.1.jar"
     "share/java/icu4j.jar")
    ("https://search.maven.org/remotecontent?filepath=com/ibm/icu/icu4j/76.1/icu4j-76.1.jar"
     "share/java/icu4j.jar")
    ("https://repo1.maven.org/maven2/com/ibm/icu/icu4j/76.1/icu4j-76.1-sources.jar"
     "share/java/icu4j-sources.jar")
    ("https://repo1.maven.org/maven2/com/ibm/icu/icu4j-charset/76.1/icu4j-charset-76.1.jar"
     "share/java/icu4j-charset.jar")
    ("https://search.maven.org/remotecontent?filepath=com/ibm/icu/icu4j-charset/76.1/icu4j-charset-76.1.jar"
     "share/java/icu4j-charset.jar")
    ("https://repo1.maven.org/maven2/com/ibm/icu/icu4j-charset/76.1/icu4j-charset-76.1-sources.jar"
     "share/java/icu4j-charset-sources.jar")))

;; XZ compression library (version 1.10)
(define %mx-rewrites-xz
  '(("https://repo1.maven.org/maven2/org/tukaani/xz/1.10/xz-1.10.jar"
     "share/java/xz.jar")
    ("https://search.maven.org/remotecontent?filepath=org/tukaani/xz/1.10/xz-1.10.jar"
     "share/java/xz.jar")
    ("https://repo1.maven.org/maven2/org/tukaani/xz/1.10/xz-1.10-sources.jar"
     "share/java/xz-sources.jar")))

;; Ninja build tool
(define %mx-rewrites-ninja
  '(("https://lafo.ssw.uni-linz.ac.at/pub/graal-external-deps/ninja-1.10.2-linux-amd64.zip"
     "share/ninja/ninja.zip")))

;; Hamcrest test matchers
(define %mx-rewrites-hamcrest
  '(("https://repo1.maven.org/maven2/org/hamcrest/hamcrest-core/1.3/hamcrest-core-1.3.jar"
     "lib/m2/org/hamcrest/hamcrest-core/1.3/hamcrest-core-1.3.jar")
    ("https://search.maven.org/remotecontent?filepath=org/hamcrest/hamcrest-core/1.3/hamcrest-core-1.3.jar"
     "lib/m2/org/hamcrest/hamcrest-core/1.3/hamcrest-core-1.3.jar")
    ;; sources -> binary
    ("https://repo1.maven.org/maven2/org/hamcrest/hamcrest-core/1.3/hamcrest-core-1.3-sources.jar"
     "lib/m2/org/hamcrest/hamcrest-core/1.3/hamcrest-core-1.3.jar")
    ("https://search.maven.org/remotecontent?filepath=org/hamcrest/hamcrest-core/1.3/hamcrest-core-1.3-sources.jar"
     "lib/m2/org/hamcrest/hamcrest-core/1.3/hamcrest-core-1.3.jar")))

;; libffi source tarball
(define %mx-rewrites-libffi
  '(("https://github.com/libffi/libffi/releases/download/v3.4.8/libffi-3.4.8.tar.gz"
     "libffi-3.4.8.tar.gz")
    ("https://lafo.ssw.uni-linz.ac.at/pub/graal-external-deps/libffi-3.4.8.tar.gz"
     "libffi-3.4.8.tar.gz")))

;; JLine terminal library (version 3.28.0)
(define %mx-rewrites-jline
  '(("https://repo1.maven.org/maven2/org/jline/jline-terminal/3.28.0/jline-terminal-3.28.0.jar"
     "share/java/jline-terminal.jar")
    ("https://repo1.maven.org/maven2/org/jline/jline-terminal/3.28.0/jline-terminal-3.28.0-sources.jar"
     "share/java/jline-terminal-sources.jar")
    ("https://repo1.maven.org/maven2/org/jline/jline-reader/3.28.0/jline-reader-3.28.0.jar"
     "share/java/jline-reader.jar")
    ("https://repo1.maven.org/maven2/org/jline/jline-reader/3.28.0/jline-reader-3.28.0-sources.jar"
     "share/java/jline-reader-sources.jar")
    ("https://repo1.maven.org/maven2/org/jline/jline-builtins/3.28.0/jline-builtins-3.28.0.jar"
     "share/java/jline-builtins.jar")
    ("https://repo1.maven.org/maven2/org/jline/jline-builtins/3.28.0/jline-builtins-3.28.0-sources.jar"
     "share/java/jline-builtins-sources.jar")
    ("https://repo1.maven.org/maven2/org/jline/jline-terminal-ffm/3.28.0/jline-terminal-ffm-3.28.0.jar"
     "share/java/jline-terminal-ffm.jar")
    ("https://repo1.maven.org/maven2/org/jline/jline-terminal-ffm/3.28.0/jline-terminal-ffm-3.28.0-sources.jar"
     "share/java/jline-terminal-ffm-sources.jar")))

;; JSON library (version 20250517)
(define %mx-rewrites-json
  '(("https://repo1.maven.org/maven2/org/json/json/20250517/json-20250517.jar"
     "share/java/json.jar")
    ("https://repo1.maven.org/maven2/org/json/json/20250517/json-20250517-sources.jar"
     "share/java/json-sources.jar")))

;; BouncyCastle crypto library (version 1.78.1)
(define %mx-rewrites-bouncycastle
  '(("https://repo1.maven.org/maven2/org/bouncycastle/bcprov-jdk18on/1.78.1/bcprov-jdk18on-1.78.1.jar"
     "share/java/bcprov-jdk18on.jar")
    ("https://repo1.maven.org/maven2/org/bouncycastle/bcprov-jdk18on/1.78.1/bcprov-jdk18on-1.78.1-sources.jar"
     "share/java/bcprov-jdk18on-sources.jar")
    ("https://repo1.maven.org/maven2/org/bouncycastle/bcutil-jdk18on/1.78.1/bcutil-jdk18on-1.78.1.jar"
     "share/java/bcutil-jdk18on.jar")
    ("https://repo1.maven.org/maven2/org/bouncycastle/bcutil-jdk18on/1.78.1/bcutil-jdk18on-1.78.1-sources.jar"
     "share/java/bcutil-jdk18on-sources.jar")
    ("https://repo1.maven.org/maven2/org/bouncycastle/bcpkix-jdk18on/1.78.1/bcpkix-jdk18on-1.78.1.jar"
     "share/java/bcpkix-jdk18on.jar")
    ("https://repo1.maven.org/maven2/org/bouncycastle/bcpkix-jdk18on/1.78.1/bcpkix-jdk18on-1.78.1-sources.jar"
     "share/java/bcpkix-jdk18on-sources.jar")))

;; Cap'n Proto runtime (version 0.1.16)
(define %mx-rewrites-capnproto
  '(("https://repo1.maven.org/maven2/org/capnproto/runtime/0.1.16/runtime-0.1.16.jar"
     "share/java/capnproto-runtime.jar")
    ("https://repo1.maven.org/maven2/org/capnproto/runtime/0.1.16/runtime-0.1.16-sources.jar"
     "share/java/capnproto-runtime-sources.jar")))

;; TruffleJWS WebSocket library
(define %mx-rewrites-trufflejws
  '(("https://lafo.ssw.uni-linz.ac.at/pub/graal-external-deps/trufflejws-1.5.7.jar"
     "share/java/trufflejws.jar")
    ("https://lafo.ssw.uni-linz.ac.at/pub/graal-external-deps/trufflejws-1.5.7-src.jar"
     "share/java/trufflejws-sources.jar")))

;; Native source tarballs for graalpy
(define %mx-rewrites-native-sources
  '(("https://lafo.ssw.uni-linz.ac.at/pub/graal-external-deps/graalpython/bzip2-1.0.8.tar.gz"
     "bzip2-1.0.8.tar.gz")
    ("https://lafo.ssw.uni-linz.ac.at/pub/graal-external-deps/xz-5.6.2.tar.gz"
     "xz-5.6.2.tar.gz")))

;; JCodings character encoding library (version 1.0.63)
(define %mx-rewrites-jcodings
  '(("https://repo1.maven.org/maven2/org/jruby/jcodings/jcodings/1.0.63/jcodings-1.0.63.jar"
     "share/java/jcodings.jar")
    ("https://repo1.maven.org/maven2/org/jruby/jcodings/jcodings/1.0.63/jcodings-1.0.63-sources.jar"
     "share/java/jcodings-sources.jar")))

;; VisualVM JFluid Heap library (version 2.1.4)
(define %mx-rewrites-visualvm
  '(("https://repo1.maven.org/maven2/org/graalvm/visualvm/modules/org-graalvm-visualvm-lib-jfluid-heap/2.1.4/org-graalvm-visualvm-lib-jfluid-heap-2.1.4.jar"
     "share/java/visualvm-lib-jfluid-heap.jar")
    ("https://repo1.maven.org/maven2/org/graalvm/visualvm/modules/org-graalvm-visualvm-lib-jfluid-heap/2.1.4/org-graalvm-visualvm-lib-jfluid-heap-2.1.4-sources.jar"
     "share/java/visualvm-lib-jfluid-heap-sources.jar")))

;; Guava core libraries (version 31.0.1-jre)
;; Uses m2 layout from ant-build-system with install-jars
(define %mx-rewrites-guava
  '(("https://repo1.maven.org/maven2/com/google/guava/guava/31.0.1-jre/guava-31.0.1-jre.jar"
     "lib/m2/com/google/guava/guava/31.0.1-jre/guava-31.0.1-jre.jar")
    ;; sources -> binary (no sources jar produced)
    ("https://repo1.maven.org/maven2/com/google/guava/guava/31.0.1-jre/guava-31.0.1-jre-sources.jar"
     "lib/m2/com/google/guava/guava/31.0.1-jre/guava-31.0.1-jre.jar")))

;; JIMFS in-memory file system (version 1.2)
(define %mx-rewrites-jimfs
  '(("https://repo1.maven.org/maven2/com/google/jimfs/jimfs/1.2/jimfs-1.2.jar"
     "share/java/jimfs.jar")
    ("https://repo1.maven.org/maven2/com/google/jimfs/jimfs/1.2/jimfs-1.2-sources.jar"
     "share/java/jimfs-sources.jar")))

;; Java Allocation Instrumenter (version 3.1.0)
(define %mx-rewrites-alloc-instrumenter
  '(("https://repo1.maven.org/maven2/com/google/code/java-allocation-instrumenter/java-allocation-instrumenter/3.1.0/java-allocation-instrumenter-3.1.0.jar"
     "share/java/java-allocation-instrumenter.jar")
    ("https://repo1.maven.org/maven2/com/google/code/java-allocation-instrumenter/java-allocation-instrumenter/3.1.0/java-allocation-instrumenter-3.1.0-sources.jar"
     "share/java/java-allocation-instrumenter-sources.jar")))

;; JUnit test framework (version 4.13.2 required by mx)
(define %mx-rewrites-junit
  '(("https://repo1.maven.org/maven2/junit/junit/4.13.2/junit-4.13.2.jar"
     "share/java/junit.jar")
    ("https://repo1.maven.org/maven2/junit/junit/4.13.2/junit-4.13.2-sources.jar"
     "share/java/junit-sources.jar")))

;; Composed rewrite lists for each package tier
(define %mx-rewrites-regex
  (append %mx-rewrites-asm
          %mx-rewrites-antlr
          %mx-rewrites-icu
          %mx-rewrites-xz
          %mx-rewrites-ninja))

(define %mx-rewrites-truffle
  (append %mx-rewrites-regex
          %mx-rewrites-hamcrest
          %mx-rewrites-libffi
          %mx-rewrites-jline))

(define %mx-rewrites-tools
  (append %mx-rewrites-truffle
          %mx-rewrites-json
          %mx-rewrites-trufflejws))

(define %mx-rewrites-substratevm
  (append %mx-rewrites-truffle
          %mx-rewrites-capnproto))

(define %mx-rewrites-graalpy
  (append %mx-rewrites-truffle
          %mx-rewrites-json
          %mx-rewrites-bouncycastle
          %mx-rewrites-capnproto
          %mx-rewrites-trufflejws
          %mx-rewrites-native-sources))

;; ASM 9.8 - required by mx's internal jacoco code coverage
(define %mx-rewrites-asm-mx
  '(("https://repo1.maven.org/maven2/org/ow2/asm/asm/9.8/asm-9.8.jar"
     "share/java/asm9.jar")
    ("https://repo1.maven.org/maven2/org/ow2/asm/asm/9.8/asm-9.8-sources.jar"
     "share/java/asm9-sources.jar")
    ("https://repo1.maven.org/maven2/org/ow2/asm/asm-tree/9.8/asm-tree-9.8.jar"
     "share/java/asm-tree.jar")
    ("https://repo1.maven.org/maven2/org/ow2/asm/asm-tree/9.8/asm-tree-9.8-sources.jar"
     "share/java/asm-tree-sources.jar")
    ("https://repo1.maven.org/maven2/org/ow2/asm/asm-analysis/9.8/asm-analysis-9.8.jar"
     "share/java/asm-analysis.jar")
    ("https://repo1.maven.org/maven2/org/ow2/asm/asm-analysis/9.8/asm-analysis-9.8-sources.jar"
     "share/java/asm-analysis.jar")
    ("https://repo1.maven.org/maven2/org/ow2/asm/asm-commons/9.8/asm-commons-9.8.jar"
     "share/java/asm-commons.jar")
    ("https://repo1.maven.org/maven2/org/ow2/asm/asm-commons/9.8/asm-commons-9.8-sources.jar"
     "share/java/asm-commons-sources.jar")))

;; JMH 1.21 and dependencies - required by mx for compiler benchmarks
(define %mx-rewrites-jmh-mx
  '(("https://repo1.maven.org/maven2/net/sf/jopt-simple/jopt-simple/4.6/jopt-simple-4.6.jar"
     "lib/m2/net/sf/jopt-simple/jopt-simple/4.6/jopt-simple-4.6.jar")
    ("https://repo1.maven.org/maven2/net/sf/jopt-simple/jopt-simple/4.6/jopt-simple-4.6-sources.jar"
     "lib/m2/net/sf/jopt-simple/jopt-simple/4.6/jopt-simple-4.6.jar")
    ("https://repo1.maven.org/maven2/org/apache/commons/commons-math3/3.2/commons-math3-3.2.jar"
     "lib/m2/org/apache/commons/commons-math3/3.2/commons-math3-3.2.jar")
    ("https://repo1.maven.org/maven2/org/apache/commons/commons-math3/3.2/commons-math3-3.2-sources.jar"
     "lib/m2/org/apache/commons/commons-math3/3.2/commons-math3-3.2.jar")
    ("https://repo1.maven.org/maven2/org/openjdk/jmh/jmh-core/1.21/jmh-core-1.21.jar"
     "lib/m2/org/openjdk/jmh/jmh-core/1.21/jmh-core-1.21.jar")
    ("https://repo1.maven.org/maven2/org/openjdk/jmh/jmh-core/1.21/jmh-core-1.21-sources.jar"
     "lib/m2/org/openjdk/jmh/jmh-core/1.21/jmh-core-1.21.jar")
    ("https://repo1.maven.org/maven2/org/openjdk/jmh/jmh-generator-annprocess/1.21/jmh-generator-annprocess-1.21.jar"
     "lib/m2/org/openjdk/jmh/jmh-generator-annprocess/1.21/jmh-generator-annprocess-1.21.jar")
    ("https://repo1.maven.org/maven2/org/openjdk/jmh/jmh-generator-annprocess/1.21/jmh-generator-annprocess-1.21-sources.jar"
     "lib/m2/org/openjdk/jmh/jmh-generator-annprocess/1.21/jmh-generator-annprocess-1.21.jar")))

;; Comprehensive rewrites for building complete GraalVM CE
(define %mx-rewrites-vm-ce
  (append %mx-rewrites-substratevm       ; truffle + capnproto
          %mx-rewrites-tools             ; truffle + json + trufflejws
          %mx-rewrites-jcodings
          %mx-rewrites-visualvm
          %mx-rewrites-guava
          %mx-rewrites-jimfs
          %mx-rewrites-alloc-instrumenter
          %mx-rewrites-junit
          %mx-rewrites-asm-mx            ; mx's ASM 9.8 for jacoco
          %mx-rewrites-jmh-mx))

;;;
;;; Packages
;;;

;; The mx build tool is used to build all GraalVM projects.
;; It has no external Python dependencies (stdlib only).
(define-public graalvm-mx
  (package
    (name "graalvm-mx")
    (version "7.68.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/graalvm/mx")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0y7qqc8374vq6sg9icfm0jlfx8fb447p9blpm18ji95qrm9ywzx0"))
              (patches (search-patches "graalvm-mx-check-failed-after-join.patch"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("." "lib/mx/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'create-missing-directories
            ;; mx's suite.py defines native projects that expect certain
            ;; directories to exist.  Create them so mx doesn't fail when
            ;; initializing.
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((lib (string-append (assoc-ref outputs "out") "/lib/mx")))
                (mkdir-p (string-append lib "/java/com.oracle.jvmtiasmagent/include")))))
          (add-after 'create-missing-directories 'install-ninja-syntax
            ;; mx needs ninja_syntax Python module for native projects.
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((ninja-syntax (search-input-file inputs "misc/ninja_syntax.py"))
                    (lib (string-append (assoc-ref outputs "out") "/lib/mx")))
                (copy-file ninja-syntax (string-append lib "/ninja_syntax.py")))))
          (add-after 'install-ninja-syntax 'create-wrapper
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (lib (string-append out "/lib/mx"))
                     (python (search-input-file inputs "/bin/python3")))
                (mkdir-p bin)
                (call-with-output-file (string-append bin "/mx")
                  (lambda (port)
                    (format port "#!~a~%exec ~a ~a/mx.py \"$@\"~%"
                            (search-input-file inputs "/bin/bash")
                            python
                            lib)))
                (chmod (string-append bin "/mx")
                       #o755)))))))
    (native-inputs (list (package-source ninja)))
    (inputs (list bash-minimal python-3))
    (home-page "https://github.com/graalvm/mx")
    (synopsis "Build tool for GraalVM projects")
    (description "mx is a command-line tool used for the development of
GraalVM projects.  It provides commands for building, testing, and packaging
polyglot language implementations built on the Truffle framework.")
    (license license:gpl2)))

;; TruffleJWS - WebSocket implementation used by GraalVM tools (chromeinspector)
;; Built from source jar distributed by Oracle at lafo.ssw.uni-linz.ac.at
(define-public java-trufflejws-for-graal
  (package
    (name "java-trufflejws-for-graal")
    (version "1.5.7")
    (source (origin
              (method url-fetch)
              (uri "https://lafo.ssw.uni-linz.ac.at/pub/graal-external-deps/trufflejws-1.5.7-src.jar")
              (sha256
               (base32 "0c6ccyl9s07mimdnscc4g56zkhc31qd6qvhy16vidrj12h8cxgfn"))))
    (build-system ant-build-system)
    (arguments
     (list
      #:jar-name "trufflejws.jar"
      #:source-dir "."
      #:tests? #f  ; no tests in source jar
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              ;; Source is a jar file containing .java files.
              (invoke "unzip" "-q" source)))
          (add-after 'install 'install-sources
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (share (string-append out "/share/java")))
                ;; Copy source jar as sources jar for mx.
                (copy-file #$source
                           (string-append share "/trufflejws-sources.jar"))))))))
    (native-inputs (list unzip))
    (home-page "https://www.graalvm.org")
    (synopsis "WebSocket implementation for GraalVM tools")
    (description "TruffleJWS is a WebSocket library used by GraalVM's
Chrome Inspector and other debugging tools.  It provides WebSocket client
and server implementations for the Truffle framework.")
    (license upl1.0)))

;; GraalVM SDK - standalone foundation with no suite imports.
;; Provides: POLYGLOT, COLLECTIONS, NATIVEIMAGE, WORD, LAUNCHER_COMMON.
(define-public graal-sdk
  (package
    (name "graal-sdk")
    (version %graalvm-version)
    (source %graal-source)
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'chdir-to-sdk
            (lambda _ (chdir "sdk")))
          (add-before 'build 'setup-mx-urlrewrites
            #$(make-mx-urlrewrites-phase %mx-rewrites-jline))
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "JAVA_HOME" (assoc-ref inputs "openjdk"))
              (setenv "MX_PYTHON" (which "python3"))
              ;; Redirect mx's build output and cache to writable locations.
              (setenv "MX_ALT_OUTPUT_ROOT" (string-append (getcwd) "/mxbuild-output"))
              (setenv "MX_CACHE_DIR" (string-append (getcwd) "/mx-cache"))
              ;; Build the core SDK distributions including LAUNCHER_COMMON for language launchers.
              (invoke "mx" "--user-home" (getcwd)
                      "build" "--dependencies"
                      "GRAAL_SDK,WORD,COLLECTIONS,NATIVEIMAGE,POLYGLOT,JLINE3,JNIUTILS,LAUNCHER_COMMON,MAVEN_DOWNLOADER")))
          (replace 'install
            #$(make-mx-install-m2-phase
               ;; Transitive dependencies match upstream Maven Central poms.
               '(("GRAAL_SDK" "org.graalvm.sdk" "graal-sdk")
                 ("POLYGLOT" "org.graalvm.polyglot" "polyglot"
                  (("org.graalvm.sdk" "collections")
                   ("org.graalvm.sdk" "nativeimage")))
                 ("WORD" "org.graalvm.sdk" "word")
                 ("COLLECTIONS" "org.graalvm.sdk" "collections")
                 ("NATIVEIMAGE" "org.graalvm.sdk" "nativeimage"
                  (("org.graalvm.sdk" "word")))
                 ("JNIUTILS" "org.graalvm.sdk" "jniutils")
                 ("LAUNCHER_COMMON" "org.graalvm.sdk" "launcher-common")
                 ("MAVEN_DOWNLOADER" "org.graalvm.sdk" "maven-downloader"))
               %graalvm-version)))))
    (native-inputs
     (list graalvm-mx
           (list openjdk "jdk")
           java-jline-terminal-for-graal-truffle
           java-jline-reader-for-graal-truffle
           java-jline-builtins-for-graal-truffle
           java-jline-terminal-ffm-for-graal-truffle))
    (inputs (list python-3))
    (home-page "https://www.graalvm.org/")
    (synopsis "GraalVM SDK and Polyglot API")
    (description "Foundation libraries for GraalVM including the Polyglot API
for language interoperability.  To embed guest languages in Java applications,
add these JARs to the classpath and use @code{Context.create()} to create a
polyglot context, then @code{context.eval(\"language-id\", \"code\")} to execute
guest language code.  The returned @code{Value} objects provide access to
results via methods like @code{asInt()}, @code{getMember()}, and @code{execute()}.")
    (license upl1.0)))

;; Truffle - Language implementation framework
;; Imports: sdk (as subdir)
;; Provides: TRUFFLE_API, TRUFFLE_NFI, TRUFFLE_DSL_PROCESSOR
(define-public graal-truffle
  (package
    (name "graal-truffle")
    (version %graalvm-version)
    (source %graal-source)
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'chdir-to-truffle
            (lambda _
              (chdir "truffle")))
          (add-before 'build 'setup-mx-urlrewrites
            #$(make-mx-urlrewrites-phase %mx-rewrites-truffle))
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "JAVA_HOME" (assoc-ref inputs "openjdk"))
              (setenv "MX_PYTHON" (which "python3"))
              ;; Redirect mx's build output and cache to writable locations.
              (setenv "MX_ALT_OUTPUT_ROOT" (string-append (getcwd) "/mxbuild-output"))
              (setenv "MX_CACHE_DIR" (string-append (getcwd) "/mx-cache"))
              ;; Fix /bin/sh references in libffi's configure.
              (setenv "CONFIG_SHELL" (which "bash"))
              (setenv "SHELL" (which "bash"))
              ;; Build the Truffle distributions including NFI with libffi.
              ;; Excluded (need LLVM toolchain):
              ;;   TRUFFLE_NFI_NATIVE_GRAALVM_SUPPORT - needs sdk:LLVM_NINJA_TOOLCHAIN
              ;;   TRUFFLE_ATTACH_GRAALVM_SUPPORT - may need JLINE for launcher
              (invoke "mx" "--user-home" (getcwd) "build"
                      "--dependencies" (string-join
                                        '("TRUFFLE_API"
                                          "TRUFFLE_COMPILER"
                                          "TRUFFLE_DSL_PROCESSOR"
                                          "TRUFFLE_ICU4J"
                                          "TRUFFLE_NFI"
                                          "TRUFFLE_NFI_LIBFFI"
                                          "TRUFFLE_NFI_PANAMA"
                                          "TRUFFLE_RUNTIME"
                                          "TRUFFLE_XZ")
                                        ","))))
          (replace 'install
            #$(make-mx-install-m2-phase
               '(("TRUFFLE_API" "org.graalvm.truffle" "truffle-api")
                 ("TRUFFLE_COMPILER" "org.graalvm.truffle" "truffle-compiler")
                 ("TRUFFLE_DSL_PROCESSOR" "org.graalvm.truffle" "truffle-dsl-processor")
                 ("TRUFFLE_ICU4J" "org.graalvm.shadowed" "icu4j")
                 ("TRUFFLE_NFI" "org.graalvm.truffle" "truffle-nfi")
                 ("TRUFFLE_NFI_LIBFFI" "org.graalvm.truffle" "truffle-nfi-libffi")
                 ("TRUFFLE_NFI_PANAMA" "org.graalvm.truffle" "truffle-nfi-panama")
                 ("TRUFFLE_RUNTIME" "org.graalvm.truffle" "truffle-runtime")
                 ("TRUFFLE_XZ" "org.graalvm.shadowed" "xz"))
               %graalvm-version)))))
    (native-inputs
     (list (list "mx" graalvm-mx)
           (list "openjdk" openjdk "jdk")
           (list "git" git-minimal)
           (list "bash" bash)
           (list "java-asm" java-asm-for-graal-truffle)
           (list "java-asm-tree" java-asm-tree-for-graal-truffle)
           (list "java-asm-analysis" java-asm-analysis-for-graal-truffle)
           (list "java-asm-util" java-asm-util-for-graal-truffle)
           (list "java-asm-commons" java-asm-commons-for-graal-truffle)
           (list "java-antlr4-runtime" java-antlr4-runtime-for-graal-truffle)
           (list "java-hamcrest-core" java-hamcrest-core-for-graal-truffle)
           (list "java-icu4j" java-icu4j-for-graal-truffle)
           (list "java-icu4j-charset" java-icu4j-charset-for-graal-truffle)
           (list "java-xz" java-xz-for-graal-truffle)
           (list "java-jline-terminal" java-jline-terminal-for-graal-truffle)
           (list "java-jline-reader" java-jline-reader-for-graal-truffle)
           (list "java-jline-builtins" java-jline-builtins-for-graal-truffle)
           (list "java-jline-terminal-ffm" java-jline-terminal-ffm-for-graal-truffle)
           (list "ninja" ninja-for-graal-truffle)
           (list "libffi-3.4.8.tar.gz" (package-source libffi-for-graal-truffle))))
    (inputs (list python-3))
    (home-page "https://www.graalvm.org/")
    (synopsis "Truffle language implementation framework")
    (description "Truffle is a framework for implementing programming languages
as self-modifying Abstract Syntax Tree (AST) interpreters.  Language implementers
extend @code{Node} classes and use the @code{@@Specialization} annotation from
the Truffle DSL.  Add these JARs to the classpath along with @code{graal-sdk}
to build new language implementations.  Languages built on Truffle achieve high
performance through the Graal JIT compiler when run with @code{-XX:+EnableJVMCI}.")
    (license upl1.0)))

;; Graal Tools - debugging and profiling utilities.
;; This builds the tools suite which imports truffle, so it needs the same
;; URL rewrites and dependencies as graal-truffle.
(define-public graal-tools
  (package
    (name "graal-tools")
    (version %graalvm-version)
    (source %graal-source)
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'chdir-to-tools
            (lambda _ (chdir "tools")))
          (add-before 'build 'setup-mx-urlrewrites
            #$(make-mx-urlrewrites-phase %mx-rewrites-tools))
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "JAVA_HOME" (assoc-ref inputs "openjdk"))
              (setenv "MX_PYTHON" (which "python3"))
              (setenv "MX_ALT_OUTPUT_ROOT" (string-append (getcwd) "/mxbuild-output"))
              (setenv "MX_CACHE_DIR" (string-append (getcwd) "/mx-cache"))
              ;; Build all tools distributions:
              ;; - TRUFFLE_PROFILER: CPU sampler and tracer
              ;; - TRUFFLE_COVERAGE: Code coverage tool
              ;; - INSIGHT: GraalVM Insight for dynamic instrumentation
              ;; - DAP: Debug Adapter Protocol support
              ;; - LSP: Language Server Protocol support
              ;; - CHROMEINSPECTOR: Chrome DevTools Protocol support
              (invoke "mx" "--user-home" (getcwd) "build"
                      "--dependencies"
                      "TRUFFLE_PROFILER,TRUFFLE_COVERAGE,INSIGHT,DAP,LSP,CHROMEINSPECTOR")))
          (replace 'install
            #$(make-mx-install-m2-phase
               '(("TRUFFLE_PROFILER" "org.graalvm.tools" "profiler-tool")
                 ("TRUFFLE_COVERAGE" "org.graalvm.tools" "coverage-tool")
                 ("INSIGHT" "org.graalvm.tools" "insight-tool")
                 ("DAP" "org.graalvm.tools" "dap-tool")
                 ("LSP" "org.graalvm.tools" "lsp-tool")
                 ("CHROMEINSPECTOR" "org.graalvm.tools" "chromeinspector-tool"))
               %graalvm-version)))))
    (native-inputs
     (list (list "mx" graalvm-mx)
           (list "openjdk" openjdk "jdk")
           (list "java-asm" java-asm-for-graal-truffle)
           (list "java-asm-tree" java-asm-tree-for-graal-truffle)
           (list "java-asm-analysis" java-asm-analysis-for-graal-truffle)
           (list "java-asm-util" java-asm-util-for-graal-truffle)
           (list "java-asm-commons" java-asm-commons-for-graal-truffle)
           (list "java-antlr4-runtime" java-antlr4-runtime-for-graal-truffle)
           (list "java-hamcrest-core" java-hamcrest-core-for-graal-truffle)
           (list "java-icu4j" java-icu4j-for-graal-truffle)
           (list "java-icu4j-charset" java-icu4j-charset-for-graal-truffle)
           (list "java-xz" java-xz-for-graal-truffle)
           (list "java-jline-terminal" java-jline-terminal-for-graal-truffle)
           (list "java-jline-reader" java-jline-reader-for-graal-truffle)
           (list "java-jline-builtins" java-jline-builtins-for-graal-truffle)
           (list "java-jline-terminal-ffm" java-jline-terminal-ffm-for-graal-truffle)
           (list "java-json" java-json-for-graal-truffle)
           (list "java-trufflejws" java-trufflejws-for-graal)
           (list "ninja" ninja-for-graal-truffle)
           (list "libffi-3.4.8.tar.gz" (package-source libffi-for-graal-truffle))))
    (inputs (list python-3))
    (home-page "https://www.graalvm.org/")
    (synopsis "GraalVM debugging and profiling tools")
    (description "Development tools for GraalVM Truffle languages:
@itemize
@item CPU Sampler (@code{--cpusampler}): Profile guest language code
@item Code Coverage (@code{--coverage}): Track code coverage
@item GraalVM Insight: Dynamic instrumentation and tracing
@item Debug Adapter Protocol (DAP): IDE debugging integration
@item Language Server Protocol (LSP): IDE language features
@item Chrome DevTools (@code{--inspect}): Browser-based debugging
@end itemize
Use these flags with language launchers like @code{graalpy} or @code{js}.")
    (license upl1.0)))

;; Graal Regex - TRegex regular expression engine
;; This builds the regex suite which imports truffle, so it needs the same
;; URL rewrites and dependencies as graal-truffle.
(define-public graal-regex
  (package
    (name "graal-regex")
    (version %graalvm-version)
    (source %graal-source)
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'chdir-to-regex
            (lambda _ (chdir "regex")))
          (add-before 'build 'setup-mx-urlrewrites
            #$(make-mx-urlrewrites-phase %mx-rewrites-regex))
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "JAVA_HOME" (assoc-ref inputs "openjdk"))
              (setenv "MX_PYTHON" (which "python3"))
              (setenv "MX_ALT_OUTPUT_ROOT" (string-append (getcwd) "/mxbuild-output"))
              (setenv "MX_CACHE_DIR" (string-append (getcwd) "/mx-cache"))
              (invoke "mx" "--user-home" (getcwd) "build"
                      "--dependencies" "TREGEX")))
          (replace 'install
            #$(make-mx-install-m2-phase
               '(("TREGEX" "org.graalvm.regex" "regex"))
               %graalvm-version)))))
    (native-inputs (list graalvm-mx
                         (list openjdk "jdk")
                         ninja-for-graal-truffle
                         java-asm-for-graal-truffle
                         java-asm-tree-for-graal-truffle
                         java-asm-analysis-for-graal-truffle
                         java-asm-util-for-graal-truffle
                         java-asm-commons-for-graal-truffle
                         java-antlr4-runtime-for-graal-truffle
                         java-icu4j-for-graal-truffle
                         java-icu4j-charset-for-graal-truffle
                         java-xz-for-graal-truffle))
    (inputs (list python-3))
    (home-page "https://www.graalvm.org/")
    (synopsis "TRegex regular expression engine for GraalVM")
    (description "TRegex is a high-performance regular expression engine
used internally by GraalVM languages like GraalJS and GraalPy for pattern
matching.  Language implementers can use @code{TruffleRegexCompiler} to compile
regex patterns, or access it via the Truffle interop protocol.  End users
typically do not use this package directly.")
    (license upl1.0)))

;; Graal Compiler - the JIT compiler for GraalVM
;; This builds the compiler suite which imports truffle, regex, and sdk.
(define-public graal-compiler
  (package
    (name "graal-compiler")
    (version %graalvm-version)
    (source %graal-source)
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'chdir-to-compiler
            (lambda _ (chdir "compiler")))
          (add-before 'build 'setup-mx-urlrewrites
            #$(make-mx-urlrewrites-phase %mx-rewrites-truffle))
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "JAVA_HOME" (assoc-ref inputs "openjdk"))
              (setenv "MX_PYTHON" (which "python3"))
              (setenv "MX_ALT_OUTPUT_ROOT" (string-append (getcwd) "/mxbuild-output"))
              (setenv "MX_CACHE_DIR" (string-append (getcwd) "/mx-cache"))
              ;; Build the core compiler distributions.
              ;; GRAAL is the main compiler JAR for use with --upgrade-module-path.
              ;; GRAAL_MANAGEMENT provides JMX management beans.
              (invoke "mx" "--user-home" (getcwd) "build"
                      "--dependencies" "GRAAL,GRAAL_MANAGEMENT")))
          (replace 'install
            #$(make-mx-install-m2-phase
               '(("GRAAL" "org.graalvm.compiler" "compiler")
                 ("GRAAL_MANAGEMENT" "org.graalvm.compiler" "compiler-management"))
               %graalvm-version)))))
    (native-inputs
     (list (list "mx" graalvm-mx)
           (list "openjdk" openjdk "graal-builder-jdk")
           (list "java-asm" java-asm-for-graal-truffle)
           (list "java-asm-tree" java-asm-tree-for-graal-truffle)
           (list "java-asm-analysis" java-asm-analysis-for-graal-truffle)
           (list "java-asm-util" java-asm-util-for-graal-truffle)
           (list "java-asm-commons" java-asm-commons-for-graal-truffle)
           (list "java-antlr4-runtime" java-antlr4-runtime-for-graal-truffle)
           (list "java-hamcrest-core" java-hamcrest-core-for-graal-truffle)
           (list "java-icu4j" java-icu4j-for-graal-truffle)
           (list "java-icu4j-charset" java-icu4j-charset-for-graal-truffle)
           (list "java-xz" java-xz-for-graal-truffle)
           (list "java-jline-terminal" java-jline-terminal-for-graal-truffle)
           (list "java-jline-reader" java-jline-reader-for-graal-truffle)
           (list "java-jline-builtins" java-jline-builtins-for-graal-truffle)
           (list "java-jline-terminal-ffm" java-jline-terminal-ffm-for-graal-truffle)
           (list "ninja" ninja-for-graal-truffle)
           (list "libffi-3.4.8.tar.gz" (package-source libffi-for-graal-truffle))))
    (inputs (list python-3))
    (propagated-inputs
     (list graal-sdk graal-truffle (list openjdk "graal-builder-jdk")))
    (home-page "https://www.graalvm.org/")
    (synopsis "Graal JIT compiler for the JVM")
    (description "The Graal compiler is a high-performance JIT compiler for
the JVM that can be used as a replacement for the C2 compiler.  It provides
optimizations specifically tuned for dynamic languages.  Requires
@code{openjdk} with the @code{graal-builder-jdk} output which has JVMCI exports
configured for Graal.
Example usage:
@example
java -XX:+UnlockExperimentalVMOptions -XX:+EnableJVMCI \\
     --upgrade-module-path=$GUIX_ENVIRONMENT/lib/m2/org/graalvm/compiler/compiler/25.0.1/compiler-25.0.1.jar \\
     -XX:+UseJVMCICompiler YourApp
@end example")
    (license (list license:gpl2+  ; with Classpath exception
                   upl1.0))))

;; SubstrateVM - Ahead-of-time compilation for Java (native-image)
;; This builds the substratevm suite which imports compiler and espresso-shared.
(define-public graal-substratevm
  (package
    (name "graal-substratevm")
    (version %graalvm-version)
    (source %graal-source)
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'chdir-to-substratevm
            (lambda _ (chdir "substratevm")))
          (add-before 'build 'setup-mx-urlrewrites
            #$(make-mx-urlrewrites-phase %mx-rewrites-substratevm))
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "JAVA_HOME" (assoc-ref inputs "openjdk"))
              (setenv "MX_PYTHON" (which "python3"))
              (setenv "MX_ALT_OUTPUT_ROOT" (string-append (getcwd) "/mxbuild-output"))
              (setenv "MX_CACHE_DIR" (string-append (getcwd) "/mx-cache"))
              ;; Build the native-image driver and its dependencies.
              ;; SVM_DRIVER is the native-image building tool.
              ;; SVM is the main SubstrateVM image builder.
              (invoke "mx" "--user-home" (getcwd) "build"
                      "--dependencies" "SVM_DRIVER,SVM")))
          (replace 'install
            #$(make-mx-install-phase '("SVM_DRIVER" "SVM") "svm")))))
    (native-inputs
     (list (list "mx" graalvm-mx)
           (list "openjdk" openjdk "graal-builder-jdk")
           (list "java-asm" java-asm-for-graal-truffle)
           (list "java-asm-tree" java-asm-tree-for-graal-truffle)
           (list "java-asm-analysis" java-asm-analysis-for-graal-truffle)
           (list "java-asm-util" java-asm-util-for-graal-truffle)
           (list "java-asm-commons" java-asm-commons-for-graal-truffle)
           (list "java-antlr4-runtime" java-antlr4-runtime-for-graal-truffle)
           (list "java-hamcrest-core" java-hamcrest-core-for-graal-truffle)
           (list "java-icu4j" java-icu4j-for-graal-truffle)
           (list "java-icu4j-charset" java-icu4j-charset-for-graal-truffle)
           (list "java-xz" java-xz-for-graal-truffle)
           (list "java-jline-terminal" java-jline-terminal-for-graal-truffle)
           (list "java-jline-reader" java-jline-reader-for-graal-truffle)
           (list "java-jline-builtins" java-jline-builtins-for-graal-truffle)
           (list "java-jline-terminal-ffm" java-jline-terminal-ffm-for-graal-truffle)
           (list "ninja" ninja-for-graal-truffle)
           (list "libffi-3.4.8.tar.gz" (package-source libffi-for-graal-truffle))
           (list "java-capnproto-runtime" java-capnproto-runtime-for-graal-truffle)))
    (inputs (list python-3))
    (home-page "https://www.graalvm.org/")
    (synopsis "Ahead-of-time compilation for Java applications")
    (description "SubstrateVM provides the core libraries for ahead-of-time (AOT)
compilation of Java applications.  These JARs are used by the @command{native-image}
tool to compile Java bytecode into standalone native executables.  Note: this
package provides the SVM libraries; the full @command{native-image} tool requires
additional GraalVM components.  The resulting native binaries start instantly
and use less memory than traditional JVM-based applications.")
    (license (list license:gpl2+  ; with Classpath exception
                   upl1.0))))

(define-public graal-wasm
  (package
    (name "graal-wasm")
    (version %graalvm-version)
    (source %graal-source)
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'chdir-to-wasm
            (lambda _
              (chdir "wasm")))
          (add-before 'build 'setup-mx-urlrewrites
            #$(make-mx-urlrewrites-phase %mx-rewrites-truffle))
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "JAVA_HOME" (assoc-ref inputs "openjdk"))
              (setenv "MX_PYTHON" (which "python3"))
              (setenv "MX_ALT_OUTPUT_ROOT" (string-append (getcwd) "/mxbuild-output"))
              (setenv "MX_CACHE_DIR" (string-append (getcwd) "/mx-cache"))
              ;; Build the WebAssembly runtime and launcher.
              (invoke "mx" "--user-home" (getcwd) "build"
                      "--dependencies" "WASM,WASM_LAUNCHER")))
          (replace 'install
            #$(make-mx-install-phase '("WASM" "WASM_LAUNCHER") "wasm")))))
    (native-inputs
     (list (list "mx" graalvm-mx)
           (list "openjdk" openjdk "graal-builder-jdk")
           (list "java-asm" java-asm-for-graal-truffle)
           (list "java-asm-tree" java-asm-tree-for-graal-truffle)
           (list "java-asm-analysis" java-asm-analysis-for-graal-truffle)
           (list "java-asm-util" java-asm-util-for-graal-truffle)
           (list "java-asm-commons" java-asm-commons-for-graal-truffle)
           (list "java-antlr4-runtime" java-antlr4-runtime-for-graal-truffle)
           (list "java-hamcrest-core" java-hamcrest-core-for-graal-truffle)
           (list "java-icu4j" java-icu4j-for-graal-truffle)
           (list "java-icu4j-charset" java-icu4j-charset-for-graal-truffle)
           (list "java-xz" java-xz-for-graal-truffle)
           (list "java-jline-terminal" java-jline-terminal-for-graal-truffle)
           (list "java-jline-reader" java-jline-reader-for-graal-truffle)
           (list "java-jline-builtins" java-jline-builtins-for-graal-truffle)
           (list "java-jline-terminal-ffm" java-jline-terminal-ffm-for-graal-truffle)
           (list "ninja" ninja-for-graal-truffle)
           (list "libffi-3.4.8.tar.gz" (package-source libffi-for-graal-truffle))))
    (inputs (list python-3))
    (home-page "https://www.graalvm.org/webassembly/")
    (synopsis "High-performance WebAssembly runtime for Java")
    (description "GraalWasm is a high-performance embeddable WebAssembly runtime
for Java.  Add these JARs along with @code{graal-sdk} and @code{graal-truffle}
to the classpath, then use @code{Context.newBuilder(\"wasm\").build()} to create
a context and @code{context.eval(Source.newBuilder(\"wasm\", wasmFile).build())}
to load modules.  Access exported functions via @code{getMember(\"exports\")}.
Supports WebAssembly MVP and various post-MVP proposals.")
    (license upl1.0)))

;; Imports: truffle, sdk
;; Provides: ESPRESSO_SHARED
(define-public graal-espresso-shared
  (package
    (name "graal-espresso-shared")
    (version %graalvm-version)
    (source %graal-source)
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'chdir-to-espresso-shared
            (lambda _
              (chdir "espresso-shared")))
          (add-before 'build 'setup-mx-urlrewrites
            #$(make-mx-urlrewrites-phase %mx-rewrites-truffle))
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "JAVA_HOME" (assoc-ref inputs "openjdk"))
              (setenv "MX_PYTHON" (which "python3"))
              (setenv "MX_ALT_OUTPUT_ROOT" (string-append (getcwd)
                                                          "/mxbuild-output"))
              (setenv "MX_CACHE_DIR" (string-append (getcwd)
                                                    "/mx-cache"))
              (invoke "mx" "--user-home" (getcwd)
                      "build"
                      "--dependencies" "ESPRESSO_SHARED")))
          (replace 'install
            #$(make-mx-install-phase '("ESPRESSO_SHARED")
                                     "espresso-shared")))))
    (native-inputs
     (list (list "mx" graalvm-mx)
           (list "openjdk" openjdk "graal-builder-jdk")
           (list "java-asm" java-asm-for-graal-truffle)
           (list "java-asm-tree" java-asm-tree-for-graal-truffle)
           (list "java-asm-analysis" java-asm-analysis-for-graal-truffle)
           (list "java-asm-util" java-asm-util-for-graal-truffle)
           (list "java-asm-commons" java-asm-commons-for-graal-truffle)
           (list "java-antlr4-runtime" java-antlr4-runtime-for-graal-truffle)
           (list "java-hamcrest-core" java-hamcrest-core-for-graal-truffle)
           (list "java-icu4j" java-icu4j-for-graal-truffle)
           (list "java-icu4j-charset" java-icu4j-charset-for-graal-truffle)
           (list "java-xz" java-xz-for-graal-truffle)
           (list "java-jline-terminal" java-jline-terminal-for-graal-truffle)
           (list "java-jline-reader" java-jline-reader-for-graal-truffle)
           (list "java-jline-builtins" java-jline-builtins-for-graal-truffle)
           (list "java-jline-terminal-ffm" java-jline-terminal-ffm-for-graal-truffle)
           (list "ninja" ninja-for-graal-truffle)
           (list "libffi-3.4.8.tar.gz" (package-source libffi-for-graal-truffle))))
    (inputs (list python-3))
    (home-page "https://www.graalvm.org/jdk21/reference-manual/java-on-truffle/")
    (synopsis "Espresso shared code for runtime class loading")
    (description "Shared code used by Espresso (Java on Truffle) for runtime
class loading.  This package provides the @code{org.graalvm.espresso.shared}
module required by the main Espresso runtime.")
    (license license:gpl2))) ;gpl2 only, with classpath exception

;; Espresso - Java on Truffle (Java bytecode interpreter)
;; This builds the espresso suite which imports truffle, tools, sulong, sdk, espresso-shared.
(define-public graal-espresso
  (package
    (name "graal-espresso")
    (version %graalvm-version)
    (source %graal-source)
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 match))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'chdir-to-espresso
            (lambda _
              (chdir "espresso")))
          (add-before 'build 'setup-mx-urlrewrites
            #$(make-mx-urlrewrites-phase %mx-rewrites-truffle))
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "JAVA_HOME" (assoc-ref inputs "openjdk"))
              (setenv "MX_PYTHON" (which "python3"))
              (setenv "MX_ALT_OUTPUT_ROOT" (string-append (getcwd)
                                                          "/mxbuild-output"))
              (setenv "MX_CACHE_DIR" (string-append (getcwd)
                                                    "/mx-cache"))
              ;; Build the Espresso runtime and launcher.
              (invoke "mx" "--user-home" (getcwd)
                      "build"
                      "--dependencies" "ESPRESSO,ESPRESSO_LAUNCHER")))
          (replace 'install
            #$(make-mx-install-phase '("ESPRESSO" "ESPRESSO_LAUNCHER")
                                     "espresso"))
          (add-after 'install 'create-launcher
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (lib (string-append out "/lib/espresso"))
                     (sdk-base (assoc-ref inputs "graal-sdk"))
                     (version #$%graalvm-version)
                     (sdk-jars
                      (map (match-lambda
                             ((group-path artifact)
                              (string-append sdk-base "/lib/m2/" group-path "/"
                                             artifact "/" version "/"
                                             artifact "-" version ".jar")))
                           '(("org/graalvm/sdk" "graal-sdk")
                             ("org/graalvm/polyglot" "polyglot")
                             ("org/graalvm/sdk" "word")
                             ("org/graalvm/sdk" "collections")
                             ("org/graalvm/sdk" "nativeimage")
                             ("org/graalvm/sdk" "jniutils")
                             ("org/graalvm/sdk" "launcher-common"))))
                     (truffle-base (assoc-ref inputs "graal-truffle"))
                     (truffle-jars
                      (map (match-lambda
                             ((group-path artifact)
                              (string-append truffle-base "/lib/m2/" group-path "/"
                                             artifact "/" version "/"
                                             artifact "-" version ".jar")))
                           '(("org/graalvm/truffle" "truffle-api")
                             ("org/graalvm/truffle" "truffle-compiler")
                             ("org/graalvm/truffle" "truffle-nfi")
                             ("org/graalvm/truffle" "truffle-nfi-libffi")
                             ("org/graalvm/truffle" "truffle-nfi-panama")
                             ("org/graalvm/truffle" "truffle-runtime"))))
                     (shared-lib (string-append (assoc-ref inputs "graal-espresso-shared")
                                                "/lib/espresso-shared"))
                     (jdk (assoc-ref inputs "openjdk"))
                     (module-path (string-join (append (list lib)
                                                       sdk-jars
                                                       truffle-jars
                                                       (list shared-lib))
                                               ":")))
                (mkdir-p bin)
                (call-with-output-file (string-append bin "/espresso")
                  (lambda (port)
                    (format port "#!~a
exec ~a/bin/java \\
  --module-path ~a \\
  -m org.graalvm.espresso.launcher/com.oracle.truffle.espresso.launcher.EspressoLauncher \\
  \"$@\"
"
                            (which "bash")
                            jdk module-path)))
                (chmod (string-append bin "/espresso")
                       #o755)))))))
    (native-inputs
     (list (list "mx" graalvm-mx)
           (list "openjdk" openjdk "graal-builder-jdk")
           (list "java-asm" java-asm-for-graal-truffle)
           (list "java-asm-tree" java-asm-tree-for-graal-truffle)
           (list "java-asm-analysis" java-asm-analysis-for-graal-truffle)
           (list "java-asm-util" java-asm-util-for-graal-truffle)
           (list "java-asm-commons" java-asm-commons-for-graal-truffle)
           (list "java-antlr4-runtime" java-antlr4-runtime-for-graal-truffle)
           (list "java-hamcrest-core" java-hamcrest-core-for-graal-truffle)
           (list "java-icu4j" java-icu4j-for-graal-truffle)
           (list "java-icu4j-charset" java-icu4j-charset-for-graal-truffle)
           (list "java-xz" java-xz-for-graal-truffle)
           (list "java-jline-terminal" java-jline-terminal-for-graal-truffle)
           (list "java-jline-reader" java-jline-reader-for-graal-truffle)
           (list "java-jline-builtins" java-jline-builtins-for-graal-truffle)
           (list "java-jline-terminal-ffm" java-jline-terminal-ffm-for-graal-truffle)
           (list "ninja" ninja-for-graal-truffle)
           (list "libffi-3.4.8.tar.gz" (package-source libffi-for-graal-truffle))))
    (inputs
     (list (list "python" python-3)
           (list "openjdk" openjdk "graal-builder-jdk")
           (list "bash" bash-minimal)
           (list "graal-sdk" graal-sdk)
           (list "graal-truffle" graal-truffle)
           (list "graal-espresso-shared" graal-espresso-shared)))
    (home-page "https://www.graalvm.org/jdk21/reference-manual/java-on-truffle/")
    (synopsis "Java bytecode interpreter on Truffle")
    (description "Espresso is a Java bytecode interpreter (Java on Truffle) that
runs Java applications on GraalVM's Truffle framework.  Run the @command{espresso}
launcher to execute Java applications, or embed via the Polyglot API with
@code{Context.newBuilder(\"java\").build()}.  Espresso provides full Java
compatibility while enabling polyglot interoperability with other Truffle
languages.  Supports debugging, HotSwap, and GraalVM tooling integration.")
    (license license:gpl2)))

;; GraalPy - Python 3.12 implementation on GraalVM
(define-public graalpy-community
  (package
    (name "graalpy-community")
    (version %graalvm-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/oracle/graalpython")
                    (commit (string-append "graal-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "19w22gw1ixkgy2c79m9xfhw9xvxl5vc68vddal7vq6dbsyh3g2lh"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 match)
                  (sxml simple))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'fix-cmake-minimum-version
            (lambda _
              ;; pegparser.generator uses cmake 3.0 which is rejected by newer cmake.
              (substitute* "graalpython/com.oracle.graal.python.pegparser.generator/CMakeLists.txt"
                (("cmake_minimum_required\\(VERSION 3\\.0\\)")
                 "cmake_minimum_required(VERSION 3.5)"))))
          (add-after 'unpack 'setup-graal-sources
            (lambda* (#:key inputs #:allow-other-keys)
              ;; mx expects to find the graal repository as a sibling directory.
              ;; The graalpython suite.py imports sdk, truffle, tools, regex from graal.
              ;;
              ;; We MUST use copy-recursively here, NOT symlink, because mx needs
              ;; to write to various directories during the build:
              ;;
              ;; 1. ShadedLibraryProject (mx_sdk_shaded.py): These projects generate
              ;;    shaded/relocated Java bytecode at build time. mx creates output
              ;;    directories like truffle/src/com.oracle.truffle.api.impl.asm/
              ;;    to store the generated .class files. There are ~10 such projects
              ;;    across espresso, sdk, substratevm, and truffle suites.
              ;;
              ;; 2. DefaultNativeProject (mx_native.py): These projects create
              ;;    'include' subdirectories for header files and write build
              ;;    artifacts to various locations within native project directories.
              ;;
              ;; 3. Build outputs: mx writes various build artifacts, caches, and
              ;;    intermediate files throughout the source tree during compilation.
              (let ((graal-src (assoc-ref inputs "graal-25.0.1-checkout")))
                (copy-recursively graal-src "../graal")
                (when (not (file-exists? "../graal/sdk/mx.sdk/suite.py"))
                  (error "graal source not set up correctly")))))
          (add-before 'build 'setup-mx-urlrewrites
            #$(make-mx-urlrewrites-phase %mx-rewrites-graalpy))
          (add-after 'setup-graal-sources 'setup-environment
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((jdk (assoc-ref inputs "openjdk")))
                (setenv "JAVA_HOME" jdk))
              ;; Do NOT set STANDALONE_JAVA_HOME - we want mx to build a full
              ;; jimage with libgraal (libjvmcicompiler.so) baked in.
              (setenv "MX_PYTHON" (which "python3"))
              (setenv "MX_ALT_OUTPUT_ROOT" (string-append (getcwd) "/mxbuild-output"))
              (setenv "MX_CACHE_DIR" (string-append (getcwd) "/mx-cache"))
              ;; CONFIG_SHELL is needed so autoconf-based builds (like libffi)
              ;; use the correct shell instead of /bin/sh which doesn't exist
              (setenv "CONFIG_SHELL" (which "bash"))
              (setenv "SHELL" (which "bash"))
              ;; OpenJDK JVMCI: 25.0.1, GraalVM expects: 25.0.1+8-jvmci-b01
              (setenv "JVMCI_VERSION_CHECK" "ignore")))
          (add-before 'build 'patch-libgraal-env
            (lambda _
              ;; Add svm,ni so native-image is available to build libjvmcicompiler.so.
              (substitute* "mx.graalpython/jvm-ce-libgraal"
                (("COMPONENTS=LibGraal")
                 "COMPONENTS=LibGraal,svm,ni"))
              ;; Remove musl from extra_native_targets to avoid needing musl toolchain.
              (substitute* "../graal/substratevm/mx.substratevm/mx_substratevm.py"
                (("'linux-default-glibc', 'linux-default-musl'")
                 "'linux-default-glibc'"))
              ;; Fix launcher_template.sh shebang - /usr/bin/env doesn't exist in Guix.
              ;; The native-image bash launcher uses this template.
              (let ((bash (which "bash")))
                (substitute* "../graal/sdk/mx.sdk/vm/launcher_template.sh"
                  (("#!/usr/bin/env bash")
                   (string-append "#!" bash))))
              ;; Propagate gcc environment variables through native-image's env sanitization.
              ;; native-image's sanitizeJVMEnvironment (NativeImage.java) strips all env vars
              ;; except a whitelist (PATH, HOME, etc). The -E<varname> flag passes vars through.
              ;; mx already does this for JVMCI_VERSION_CHECK; we add gcc include/library paths.
              (substitute* "../graal/sdk/mx.sdk/mx_sdk_vm_impl.py"
                (("'-EJVMCI_VERSION_CHECK',")
                 (string-append "'-EJVMCI_VERSION_CHECK',\n"
                                "            '-EC_INCLUDE_PATH',\n"
                                "            '-ECPLUS_INCLUDE_PATH',\n"
                                "            '-ELIBRARY_PATH',\n"
                                "            '-ECPATH',  # C include path\n")))))
          (replace 'build
            (lambda _
              ;; Build Python for JVM with Graal JIT compiler using jvm-ce-libgraal env.
              ;; Also build GRAALPYTHON_RESOURCES for Maven embedding use cases.
              (invoke "mx" "--user-home" (getcwd)
                      "--env" "jvm-ce-libgraal"
                      "build"
                      "--target" "GRAALPY_JVM_STANDALONE,GRAALPYTHON_RESOURCES")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (match (find-files (string-append (getcwd) "/mxbuild-output/graalpython")
                                   "^GRAALPY_JVM_STANDALONE$"
                                   #:directories? #t)
                  ((standalone-dir . _)
                   (copy-recursively standalone-dir out))))))
          ;; Install all GraalVM JARs as Maven artifacts in /lib/m2 format.
          ;; This enables maven-build-system packages to depend on graalpy-community.
          (add-after 'install 'install-m2
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (modules-dir (string-append out "/modules"))
                     (version #$%graalvm-version)
                     ;; Mapping: (jar-basename group-id artifact-id)
                     ;; Only install JARs unique to graalpy-community.
                     ;; Other JARs are provided by: graal-sdk, graal-truffle,
                     ;; graal-tools, graal-regex.
                     (jar-mapping
                      '(;; org.graalvm.python - graalpy-specific
                        ("graalpython" "org.graalvm.python" "python-language")
                        ("graalpython-launcher" "org.graalvm.python" "python-launcher")))
                     ;; POM-only metapackages (no JARs, just dependency aggregators)
                     ;; These are needed for Maven dependency resolution.
                     ;; Dependency format: (dependency group artifact [type] [scope])
                     ;; When type/scope are omitted, Maven defaults to jar/compile.
                     (pom-metapackages
                      '(;; org.graalvm.python:python - aggregates python-language + deps
                        ("org.graalvm.python" "python"
                         ((dependency "org.graalvm.python" "python-language")
                          (dependency "org.graalvm.python" "python-resources")
                          (dependency "org.graalvm.truffle" "truffle-runtime")))
                        ;; org.graalvm.polyglot:python - meta-package for embedding
                        ;; This wraps org.graalvm.python:python with type=pom, scope=runtime
                        ("org.graalvm.polyglot" "python"
                         ((dependency "org.graalvm.python" "python" "pom" "runtime"))))))
                ;; Install JARs
                (for-each
                 (match-lambda
                   ((jar-basename group-id artifact-id)
                    (let* ((jar-file (string-append modules-dir "/" jar-basename ".jar"))
                           (m2-dir (string-append out "/lib/m2/"
                                                  (string-join (string-split group-id #\.) "/")
                                                  "/" artifact-id "/" version))
                           (jar-dest (string-append m2-dir "/" artifact-id "-" version ".jar"))
                           (pom-dest (string-append m2-dir "/" artifact-id "-" version ".pom")))
                      (when (file-exists? jar-file)
                        (format #t "Installing ~a -> ~a~%" jar-basename m2-dir)
                        (mkdir-p m2-dir)
                        (copy-file jar-file jar-dest)
                        ;; Create minimal POM for Maven dependency resolution
                        (call-with-output-file pom-dest
                          (lambda (port)
                            (sxml->xml
                             `(*TOP*
                               (*PI* xml "version=\"1.0\"")
                               (project
                                (modelVersion "4.0.0")
                                (groupId ,group-id)
                                (artifactId ,artifact-id)
                                (version ,version)))
                             port)))))))
                 jar-mapping)
                ;; Install POM-only metapackages (no JARs)
                (for-each
                 (match-lambda
                   ((group-id artifact-id deps)
                    (let* ((m2-dir (string-append out "/lib/m2/"
                                                  (string-join (string-split group-id #\.) "/")
                                                  "/" artifact-id "/" version))
                           (pom-dest (string-append m2-dir "/" artifact-id "-" version ".pom")))
                      (format #t "Installing POM metapackage ~a:~a~%" group-id artifact-id)
                      (mkdir-p m2-dir)
                      (call-with-output-file pom-dest
                        (lambda (port)
                          (sxml->xml
                           `(*TOP*
                             (*PI* xml "version=\"1.0\"")
                             (project
                              (modelVersion "4.0.0")
                              (groupId ,group-id)
                              (artifactId ,artifact-id)
                              (version ,version)
                              (packaging "pom")
                              (dependencies
                               ,@(map (match-lambda
                                        (('dependency dep-group dep-artifact)
                                         `(dependency
                                           (groupId ,dep-group)
                                           (artifactId ,dep-artifact)
                                           (version ,version)))
                                        (('dependency dep-group dep-artifact dep-type dep-scope)
                                         `(dependency
                                           (groupId ,dep-group)
                                           (artifactId ,dep-artifact)
                                           (version ,version)
                                           (type ,dep-type)
                                           (scope ,dep-scope))))
                                      deps))))
                           port))))))
                 pom-metapackages)
                ;; Install python-resources JAR from mx build output.
                ;; This JAR contains the Python stdlib and is needed for Maven embedding.
                (let* ((resources-jar
                        (match (find-files (string-append (getcwd) "/mxbuild-output")
                                           "python-resources\\.jar$")
                          ((jar . _) jar)
                          (_ #f)))
                       (m2-dir (string-append out "/lib/m2/org/graalvm/python/python-resources/" version))
                       (jar-dest (string-append m2-dir "/python-resources-" version ".jar"))
                       (pom-dest (string-append m2-dir "/python-resources-" version ".pom")))
                  (if resources-jar
                      (begin
                        (format #t "Installing python-resources.jar from ~a~%" resources-jar)
                        (mkdir-p m2-dir)
                        (copy-file resources-jar jar-dest)
                        (call-with-output-file pom-dest
                          (lambda (port)
                            (sxml->xml
                             `(*TOP*
                               (*PI* xml "version=\"1.0\"")
                               (project
                                (modelVersion "4.0.0")
                                (groupId "org.graalvm.python")
                                (artifactId "python-resources")
                                (version ,version)))
                             port))))
                      (error "python-resources.jar not found in build output")))))))))
    (native-inputs
     (list (list "mx" graalvm-mx)
           ;; Use openjdk graal-builder-jdk output which provides static JDK libraries
           ;; required for SubstrateVM's JvmFuncsFallbacks build task.
           (list "openjdk" openjdk "graal-builder-jdk")
           (list "python" python-3)
           (list "graal-25.0.1-checkout" %graal-source)
           (list "java-asm" java-asm-for-graal-truffle)
           (list "java-asm-tree" java-asm-tree-for-graal-truffle)
           (list "java-asm-analysis" java-asm-analysis-for-graal-truffle)
           (list "java-asm-util" java-asm-util-for-graal-truffle)
           (list "java-asm-commons" java-asm-commons-for-graal-truffle)
           (list "java-antlr4-runtime" java-antlr4-runtime-for-graal-truffle)
           (list "java-hamcrest-core" java-hamcrest-core-for-graal-truffle)
           (list "java-icu4j" java-icu4j-for-graal-truffle)
           (list "java-icu4j-charset" java-icu4j-charset-for-graal-truffle)
           (list "java-xz" java-xz-for-graal-truffle)
           (list "java-jline-terminal" java-jline-terminal-for-graal-truffle)
           (list "java-jline-reader" java-jline-reader-for-graal-truffle)
           (list "java-jline-builtins" java-jline-builtins-for-graal-truffle)
           (list "java-jline-terminal-ffm" java-jline-terminal-ffm-for-graal-truffle)
           (list "java-json" java-json-for-graal-truffle)
           (list "java-capnproto-runtime" java-capnproto-runtime-for-graal-truffle)
           (list "java-bcprov" java-bcprov-for-graalpy)
           (list "java-bcutil" java-bcutil-for-graalpy)
           (list "java-bcpkix" java-bcpkix-for-graalpy)
           (list "java-trufflejws" java-trufflejws-for-graal)
           (list "ninja" ninja-for-graal-truffle)
           (list "cmake" cmake)
           (list "git" git-minimal)
           (list "libffi-3.4.8.tar.gz" (package-source libffi-for-graal-truffle))
           (list "bzip2-1.0.8.tar.gz" (package-source bzip2))
           (list "xz-5.6.2.tar.gz" (package-source xz-for-graal-truffle))))
    (inputs (list zlib
                  bzip2
                  xz))
    (home-page "https://www.graalvm.org/python/")
    (synopsis "Python 3.12 implementation on GraalVM")
    (description "GraalPy is a high-performance Python 3.12 implementation
built on the Truffle framework.  Run @command{graalpy} or @command{python3}
to execute Python scripts.  Use @code{import java} to access Java classes
directly from Python, or embed Python in Java via @code{Context.newBuilder(\"python\")}.
Supports pip for package installation and the system toolchain for C extensions.")
    (license upl1.0)))

(define-public graalvm-ce
  (package
    (name "graalvm-ce")
    (version %graalvm-version)
    (source (origin
              (inherit %graal-source)
              (patches (search-patches "graal-optional-binary-deps.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 match))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'chdir-to-vm
            (lambda _
              (chdir "vm")))
          (add-after 'chdir-to-vm 'setup-environment
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((jdk (assoc-ref inputs "openjdk")))
                (setenv "JAVA_HOME" jdk))
              (setenv "MX_PYTHON" (which "python3"))
              (setenv "MX_ALT_OUTPUT_ROOT" (string-append (getcwd) "/mxbuild-output"))
              (setenv "MX_CACHE_DIR" (string-append (getcwd) "/mx-cache"))
              (setenv "CONFIG_SHELL" (which "bash"))
              (setenv "SHELL" (which "bash"))
              (setenv "JVMCI_VERSION_CHECK" "ignore")))
          (add-before 'build 'patch-build-scripts
            (lambda _
              ;; Fix launcher_template.sh shebang - /usr/bin/env doesn't exist in Guix.
              (let ((bash (which "bash")))
                (substitute* "../sdk/mx.sdk/vm/launcher_template.sh"
                  (("#!/usr/bin/env bash")
                   (string-append "#!" bash))))))
          (add-before 'build 'create-minimal-env
            (lambda _
              ;; Create a minimal env file that builds just the JIT compiler
              ;; without substratevm (which requires LLVM and JAVACPP binaries).
              ;; Note: tflm (truffle-llvm) requires LLVM binaries which we don't have.
              (call-with-output-file "mx.vm/guix-ce"
                (lambda (port)
                  (display "# Minimal GraalVM CE for Guix\n" port)
                  (display "DYNAMIC_IMPORTS=/compiler,/sdk,/truffle,/tools\n" port)
                  (display "COMPONENTS=cmp,gvm,sdk,tfl,tflc\n" port)
                  (display "NATIVE_IMAGES=false\n" port)))))
          (add-before 'build 'setup-mx-urlrewrites
            #$(make-mx-urlrewrites-phase %mx-rewrites-vm-ce))
          (replace 'build
            (lambda _
              ;; Build minimal GraalVM CE with our custom env file.
              (invoke "mx" "--user-home" (getcwd)
                      "--env" "guix-ce"
                      "build")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                ;; Find the built GraalVM distribution
                (match (find-files (string-append (getcwd) "/mxbuild-output")
                                   "^graalvm-ce-java[0-9]+"
                                   #:directories? #t)
                  ((graalvm-dir . _)
                   (copy-recursively graalvm-dir out)))))))))
    (native-inputs
     (list (list "mx" graalvm-mx)
           (list "openjdk" openjdk "graal-builder-jdk")
           (list "python" python-3)
           (list "java-asm" java-asm-for-graal-truffle)
           (list "java-asm-tree" java-asm-tree-for-graal-truffle)
           (list "java-asm-analysis" java-asm-analysis-for-graal-truffle)
           (list "java-asm-util" java-asm-util-for-graal-truffle)
           (list "java-asm-commons" java-asm-commons-for-graal-truffle)
           (list "java-antlr4-runtime" java-antlr4-runtime-for-graal-truffle)
           (list "java-hamcrest-core" java-hamcrest-core-for-graal-truffle)
           (list "java-icu4j" java-icu4j-for-graal-truffle)
           (list "java-icu4j-charset" java-icu4j-charset-for-graal-truffle)
           (list "java-xz" java-xz-for-graal-truffle)
           (list "java-jline-terminal" java-jline-terminal-for-graal-truffle)
           (list "java-jline-reader" java-jline-reader-for-graal-truffle)
           (list "java-jline-builtins" java-jline-builtins-for-graal-truffle)
           (list "java-jline-terminal-ffm" java-jline-terminal-ffm-for-graal-truffle)
           (list "java-json" java-json-for-graal-truffle)
           (list "java-trufflejws" java-trufflejws-for-graal)
           (list "java-capnproto-runtime" java-capnproto-runtime-for-graal-truffle)
           (list "java-jcodings" java-jcodings-for-graal-truffle)
           (list "java-guava" java-guava-for-graal-truffle)
           (list "java-jimfs" java-jimfs-for-graal-truffle)
           (list "java-visualvm-jfluid-heap" java-visualvm-jfluid-heap-for-graal-truffle)
           (list "java-allocation-instrumenter" java-allocation-instrumenter-for-graal-truffle)
           (list "java-junit" java-junit-for-graal)
           (list "ninja" ninja-for-graal-truffle)
           (list "cmake" cmake)
           (list "libffi-3.4.8.tar.gz" (package-source libffi-for-graal-truffle))
           ;; mx internal dependencies (ASM 9.8 for jacoco, JMH for benchmarks)
           (list "java-asm-mx" java-asm-for-graal-mx)
           (list "java-asm-tree-mx" java-asm-tree-for-graal-mx)
           (list "java-asm-analysis-mx" java-asm-analysis-for-graal-mx)
           (list "java-asm-commons-mx" java-asm-commons-for-graal-mx)
           (list "java-jopt-simple-4" java-jopt-simple-4)
           (list "java-commons-math3-mx" java-commons-math3-for-graal-mx)
           (list "java-jmh-mx" java-jmh-for-graal-mx)
           (list "java-jmh-generator-annprocess-mx" java-jmh-generator-annprocess-for-graal-mx)))
    (inputs (list zlib))
    (home-page "https://www.graalvm.org/")
    (synopsis "GraalVM Community Edition with Native Image")
    (description "GraalVM CE is a high-performance JDK with the Graal JIT compiler
and Native Image.  It includes the base GraalVM components: sdk, truffle, compiler,
and substratevm.  Use @command{native-image} to compile Java applications to
standalone executables.")
    (license license:gpl2)))

(define-public graalpy-maven-plugin
  (package
    (name "graalpy-maven-plugin")
    (version "25.0.0")  ; graalpy-extensions version
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/oracle/graalpy-extensions")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0rah2cllx131s4yjfay47pifsv2vgjn92hnhxgib8jwqfd671i80"))))
    (build-system maven-build-system)
    (arguments
     (list
      #:jdk openjdk
      #:tests? #f
      #:exclude '(("org.codehaus.mojo" "flatten-maven-plugin"
                                       "exec-maven-plugin")
                  ("org.apache.maven.plugins" "maven-deploy-plugin"
                                              "maven-gpg-plugin"
                                              "maven-javadoc-plugin"
                                              "maven-source-plugin"
                                              "maven-antrun-plugin"
                                              "maven-assembly-plugin"
                                              "maven-release-plugin"
                                              "maven-enforcer-plugin"
                                              "maven-site-plugin"
                                              "maven-archetype-plugin")
                  ("org.apache.maven.archetype" "archetype-packaging")
                  ("org.sonatype.plugins" "nexus-staging-maven-plugin")
                  ("com.diffplug.spotless" "spotless-maven-plugin")
                  ("com.github.spotbugs" "spotbugs-maven-plugin")
                  ("org.netbeans.tools" "sigtest-maven-plugin"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-shebang
            (lambda _
              (substitute* "org.graalvm.python.embedding.tools/src/main/java/org/graalvm/python/embedding/tools/vfs/VFSUtils.java"
                (("#!/usr/bin/env bash") "#!/bin/sh"))))
          (add-after 'unpack 'fix-plugin-version-lookup
            (lambda _
              ;; The plugin resolves itself to get python-launcher dependency,
              ;; using getGraalPyVersion() which returns the GraalPy runtime
              ;; version.  This fails when plugin version differs from runtime.
              ;; Fix: use the actual plugin version matching our source.
              (substitute* "graalpy-maven-plugin/src/main/java/org/graalvm/python/maven/plugin/AbstractGraalPyMojo.java"
                (("String version = getGraalPyVersion\\(project\\);")
                 (string-append "String version = \"" #$version "\"; // patched for Guix build")))))
          (add-before 'fix-pom-files 'resolve-maven-properties
            (lambda _
              (use-modules (ice-9 textual-ports)
                           (ice-9 regex))
              (for-each
               (lambda (pom)
                 (when (file-exists? pom)
                   ;; First do line-based substitutions
                   (substitute* pom
                     (("\\$\\{project\\.groupId\\}")
                      "org.graalvm.python")
                     (("\\$\\{project\\.python\\.artifact\\}")
                      "python")
                     ;; Resolve dependency version to match graalpy-community.
                     ;; Keep ${revision} as-is (project's own version).
                     (("\\$\\{project\\.polyglot\\.version\\}")
                      #$%graalvm-version))
                   ;; Remove test dependencies (multi-line) - fix-pom-dependencies
                   ;; leaves them unchanged, causing Maven to try resolving them.
                   (let* ((content (call-with-input-file pom get-string-all))
                          (cleaned (regexp-substitute/global
                                    #f
                                    "<dependency>[\n\r\t ]*<groupId>[^<]+</groupId>[\n\r\t ]*<artifactId>[^<]+</artifactId>[\n\r\t ]*<version>[^<]+</version>[\n\r\t ]*<scope>test</scope>[\n\r\t ]*</dependency>"
                                    content
                                    'pre 'post)))
                     (call-with-output-file pom
                       (lambda (port)
                         (display cleaned port))))))
               (find-files "." "pom\\.xml$"))
              ;; Remove submodules that require unavailable packaging extensions.
              (substitute* "pom.xml"
                (("<module>graalpy-archetype-polyglot-app</module>")
                 ""))
))
          (replace 'fix-pom-files
            (lambda* (#:key inputs local-packages exclude #:allow-other-keys)
              (let ((local-packages
                     (pom-local-packages "pom.xml"
                                         #:local-packages local-packages)))
                (format (current-error-port)
                        "Fix pom files with local packages: ~a~%" local-packages)
                (for-each
                 (lambda (pom)
                   (when (file-exists? pom)
                     (chmod pom #o644)
                     (format #t "fixing ~a~%" pom)
                     (fix-pom-dependencies
                      pom (map cdr inputs)
                      #:with-plugins? #t
                      #:with-build-dependencies? #f
                      #:with-modules? #t
                      #:local-packages local-packages
                      #:excludes exclude)))
                 ;; Skip javainterfacegen - it requires pip to download from PyPI.
                 (pom-and-submodules "pom.xml")))))
          (add-before 'build 'set-maven-opts
            (lambda* (#:key inputs #:allow-other-keys)
              ;; cglib needs reflective access to ClassLoader.defineClass.
              ;; Also set graalpy.vfs.venvLauncher to use graalpy-community launcher
              ;; instead of generated one with incomplete classpath.
              (let ((graalpy (search-input-file inputs "bin/graalpy")))
                (setenv "MAVEN_OPTS"
                        (string-append
                         "--add-opens=java.base/java.lang=ALL-UNNAMED "
                         "--add-opens=java.base/java.io=ALL-UNNAMED "
                         "-Dgraalpy.vfs.venvLauncher=" graalpy)))))
          (add-before 'build 'fix-source-date-epoch
            (lambda _
              ;; JDK jar tool's --date option requires dates between 1980-2099.
              ;; Guix sets SOURCE_DATE_EPOCH=1 (1970-01-01), which is invalid.
              ;; Set to 1980-01-02 (315619200) for reproducible builds.
              (setenv "SOURCE_DATE_EPOCH" "315619200")))
          (replace 'build
            (lambda _
              ;; Skip javainterfacegen module - requires pip to download mypy.
              ;; Deactivate sigtest-on-unix profile - runs API signature
              ;; verification (compares public API against snapshot.sigtest
              ;; to catch breaking changes).  Needs exec-maven-plugin which
              ;; we don't have, and doesn't affect the built JARs.
              (invoke "mvn" "package" "-o" "-X"
                      "-DskipJavainterfacegen=true"
                      "-P" "!sigtest-on-unix"
                      (string-append "-Duser.home=" (getenv "HOME"))
                      "-Dmaven.test.skip=true")))
          (replace 'install
            (lambda _
              ;; Use -Dmaven.test.skip=true to skip both compilation AND execution.
              ;; Default -DskipTests only skips execution, causing test compile errors
              ;; when test dependencies (JUnit) are not available.
              (invoke "mvn" "install" "-o" "-e"
                      "-DskipJavainterfacegen=true"
                      "-P" "!sigtest-on-unix"
                      "-Dmaven.test.skip=true"
                      (string-append "-Duser.home=" (getenv "HOME"))))))))
    (inputs
     (list graalpy-community  ; provides python-specific /lib/m2 artifacts
           graal-sdk          ; provides org.graalvm.sdk, org.graalvm.polyglot
           graal-truffle      ; provides org.graalvm.truffle
           graal-tools        ; provides org.graalvm.tools
           graal-regex        ; provides org.graalvm.regex
           apache-parent-pom-34
           maven-parent-pom-34
           maven-parent-pom-43
           maven-parent-pom-44
           maven-plugin-plugin
           maven-compiler-plugin
           ;; Surefire skipped via -Dsurefire.skip=true, exclude to avoid dep chain
           plexus-parent-pom-10  ; parent for plexus-io 3.6.0 and plexus-archiver 4.10.4
           plexus-parent-pom-15  ; for other plexus components
           junit-bom-5.10     ; for plexus-parent-pom-15, sisu 0.9.0.M3
           junit-bom-5.10.2   ; for sisu 0.9.0.M3 via maven-plugin-api 3.9.9
           junit-bom-5.11     ; for java-plexus-java-1, commons-lang3-3.17
           junit-bom-5.11.0   ; for commons-lang3 3.17.0 via maven-resolver-impl
           junit-bom-5.12     ; for maven-parent-pom-44 and apache-parent-pom-34
           maven-common-artifact-filters  ; for surefire 3.5.3
           java-plexus-java-1  ; for surefire 3.5.3
           java-plexus-java
           java-plexus-utils-4   ; 4.0.1 for maven-resources-plugin 3.x
           java-eclipse-sisu-plexus-0.9  ; 0.9.0.M2 for maven-resources-plugin
           java-eclipse-sisu-inject-0.9  ; 0.9.0.M3 for maven-resources-plugin
           java-slf4j-api        ; for surefire 3.5.3
           java-snakeyaml-2
           java-commons-compress  ; 1.21 for surefire 3.5.3
           java-plexus-io         ; for plexus-archiver runtime
           java-plexus-archiver   ; for ArchiverManager injection
           java-plexus-xml))      ; for maven-resources-plugin
    (home-page "https://www.graalvm.org/python/")
    (synopsis "Maven plugin for GraalPy Python embedding")
    (description "The graalpy-maven-plugin handles Python package management
and resource preparation for Maven-based GraalPy Java polyglot applications.
It provides goals for installing pip packages, locking dependencies, and
generating Virtual File System resources for embedded Python code.")
    (license upl1.0)))
