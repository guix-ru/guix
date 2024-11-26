(define-module (gnu packages dotnet)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages base)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages instrumentation)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages image)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (ice-9 match))

(define-public treecc
  (package
    (name "treecc")
    (version "0.3.10")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://download.savannah.gnu.org/releases/dotgnu-pnet/treecc-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1rzgnspg2xccdq3qsx0vi3j28h4qkrzbrjnhzvnny34fjfk217ay"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/dotgnu")
    (synopsis "Tree Compiler-Compiler")
    (description "The treecc program is designed to assist in the development
of compilers and other language-based tools.  It manages the generation of
code to handle abstract syntax trees and operations upon the trees.")
    (license license:gpl2+)))

;; Several improvements occurred past the 0.8.0 release that make it
;; easier to bootstrap mono.
(define-public pnet-git
  (let ((commit "3baf94734d8dc3fdabba68a8891e67a43ed6c4bd")
        (version "0.8.0")
        (revision "0"))
    (package
      (name "pnet-git")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/dotgnu-pnet/pnet.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0vznvrgz8l0mpib1rz5v3clr7cn570vyp80f7f1jvzivnc1imzn6"))
                (modules '((guix build utils)))
                (snippet
                 #~(begin
                     (for-each delete-file-recursively '("libffi" "libgc"))
                     (for-each delete-file
                               (append
                                 (filter file-exists?
                                         '("compile"
                                           "configure"
                                           "config.guess"
                                           "config.sub"
                                           "depcomp"
                                           "install-sh"
                                           "ltconfig"
                                           "ltcf-c.sh"
                                           "ltmain.sh"))
                                 (find-files "." "Makefile(\\.in)?$")
                                 (find-files "." "_(grammar|scanner)\\.(c|h)$")))
                     ;; Fix to not require bundled dependencies
                     (substitute* "configure.in"
                       (("GCLIBS='.*libgc.a'") "GCLIBS='-lgc'")
                       ;; AC_SEARCH_LIBJIT checks hardcoded header locations
                       (("search_libjit=true")
                        (string-append "search_libjit=false\n"
                                       "JIT_LIBS=-ljit")))
                     (substitute* "Makefile.am"
                       (("OPT_SUBDIRS \\+= lib.*") ""))
                     (substitute* "support/hb_gc.c"
                       (("#include .*/libgc/include/gc.h.")
                        "#include <gc.h>")
                       (("#include .*/libgc/include/gc_typed.h.")
                        "#include <gc/gc_typed.h>"))
                     (substitute* (list "codegen/Makefile.am"
                                        "cscc/bf/Makefile.am"
                                        "cscc/csharp/Makefile.am"
                                        "cscc/c/Makefile.am"
                                        "cscc/java/Makefile.am")
                       ;; Generated files aren't prerequisites
                       (("TREECC_OUTPUT =.*") ""))
                     (substitute* "cscc/csharp/cs_grammar.y"
                       (("YYLEX") "yylex()"))
                     (substitute* "cscc/common/cc_main.h"
                       (("CCPreProc CCPreProcessorStream;" all)
                        (string-append "extern " all)))
                     (substitute* "csdoc/scanner.c"
                       (("int\ttoken;" all)
                        (string-append "extern " all)))
                     (substitute* "doc/cvmdoc.py"
                       (("python1.5")
                        "python"))
                     (substitute* "profiles/full"
                       ;; If this is left unmodified, it causes a segfault in
                       ;; pnetlib's tests.  Unrollers are somewhat
                       ;; architecture-specific anyway, and it will fall back
                       ;; to using GNU C's labels-as-values feature (it can be
                       ;; made to further fall back to fully
                       ;; standards-portable interpreter implementations).
                       (("IL_CONFIG_UNROLL=y")
                        "IL_CONFIG_UNROLL=n"))))
                (patches (search-patches "pnet-newer-libgc-fix.patch"
                                         "pnet-newer-texinfo-fix.patch"
                                         "pnet-fix-line-number-info.patch"
                                         "pnet-fix-off-by-one.patch"))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf
             automake
             bison
             flex
             libatomic-ops
             libtool
             python-minimal-wrapper
             texinfo
             treecc))
      (inputs
       (cons* libffi
              libgc
              (if (supported-package? libjit)
                  (list libjit)
                  '())))
      (arguments
       (append (if (this-package-input "libjit")
                   (list #:configure-flags #~(list "--with-jit"))
                   '())
               (list #:make-flags
                     #~(list "CFLAGS=-O2 -g -Wno-pointer-to-int-cast"))))
      (native-search-paths
       (list (search-path-specification
              (variable "CSCC_LIB_PATH")
              (files (list "lib/cscc/lib")))))
      (home-page "http://www.gnu.org/software/dotgnu/html2.0/pnet.html")
      (synopsis "Compiler for the C# programming language")
      (description
       "The goal of this project is to build a suite of free software tools
to build and execute .NET applications, including a C# compiler,
assembler, disassembler, and runtime engine.")
      (license license:gpl2+))))

(define-public pnetlib-git
  (let ((version "0.8.0")
        (commit "c3c12b8b0c65f5482d03d6a4865f7670e98baf4c")
        (revision "0"))
    (package
      (name "pnetlib-git")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url
                       "https://git.savannah.gnu.org/git/dotgnu-pnet/pnetlib.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "04dikki3lr3m1cacirld90rpi95656b2y2mc5rkycb7s0yfdz1nk"))
                (modules '((guix build utils)))
                (snippet
                 #~(begin
                     (for-each delete-file
                               (append (filter file-exists?
                                               '("configure"
                                                 "config.guess"
                                                 "config.sub"
                                                 "install-sh"
                                                 "ltmain.sh"))
                                       (find-files "." "Makefile(\\.in)?$")))
                     (substitute* (find-files "tests" "^Makefile\\.am$")
                       (("TESTS_ENVIRONMENT.*")
                        (string-append
                         "LOG_COMPILER = $(SHELL)\n"
                         "AM_LOG_FLAGS = $(top_builddir)/tools/run_test.sh"
                         " $(top_builddir)")))
                     (substitute* "tools/run_test.sh.in"
                       (("en_US") "en_US.utf8"))
                     (substitute* "tools/wrapper.sh.in"
                       (("exec .LN_S clrwrap ..1.")
                        (string-append
                         "echo '#!@SHELL@' >> $1\n"
                         "echo exec $CLRWRAP"
                         " $(dirname $(dirname $1))"
                         "/lib/cscc/lib/$(basename $1).exe >> $1\n"
                         "chmod +x $1")))))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:make-flags #~(list "CFLAGS=-O2 -g -Wno-pointer-to-int-cast")
        #:tests? (and (not (%current-target-system))
                      (not (target-aarch64?)))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'disable-x11-tests
              (lambda _
                (substitute* "tests/Makefile.am"
                  ;; This actually always fails, for a number of
                  ;; reasons:
                  ;; 1. We have no libx11 present, nor do we have an X display
                  ;;    present.  This will cause libXsharpSupport.so to be
                  ;;    built with only shims that fail at runtime.
                  ;; 2. No mechanism is provided for
                  ;;    tests/System.Windows.Forms/TestForms.dll to find
                  ;;    libXsharpSupport.so, which seems to sit at
                  ;;    Xsharp/.libs/libXsharpSupport.so.
                  ;; With a libjit pnet,
                  ;; System.Drawing.Toolkit.ToolkitHandler.CreateDefaultToolkit
                  ;; throws ArgumentNullException when invoking Assembly.Load,
                  ;; while a cvm pnet instead succeeds temporarily, but then
                  ;; fails when invoking
                  ;; System.Drawing.Toolkit.DrawingToolkit..ctor.  For some
                  ;; reason this results in csunit passing the former and
                  ;; failing the latter.
                  (("System\\.Windows\\.Forms") "")))))))
      (native-inputs
       (list autoconf automake libtool treecc))
      (inputs
       (list pnet-git))
      (home-page "http://www.gnu.org/software/dotgnu/html2.0/pnet.html")
      (synopsis "Libraries for the C# programming language")
      (description
       "DotGNU Portable.NET Library contains an implementation of the C# library,
for use with .NET-capable runtime engines and applications.")
      (license license:gpl2+))))
