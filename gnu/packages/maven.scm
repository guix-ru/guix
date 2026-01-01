;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018-2022 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define-module (gnu packages maven)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
  #:use-module (gnu packages java-compression)
  #:use-module (gnu packages java-xml)
  #:use-module (gnu packages maven-parent-pom)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages web)
  #:use-module (ice-9 match))

(define-public maven-resolver-api
  (package
    (name "maven-resolver-api")
    (version "1.9.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/apache/maven-resolver")
                    (commit (string-append "maven-resolver-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0s18vivvapmrk407syrc8ib2qpmp01i3k46h6gqp7961n1p9wzlq"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-resolver-api.jar"
       #:source-dir "maven-resolver-api/src/main/java"
       #:test-dir "maven-resolver-api/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "maven-resolver-api/pom.xml")))))
    (native-inputs
     `(("java-asm-8" ,java-asm-8)
       ("java-cglib" ,java-cglib)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-junit" ,java-junit)
       ("java-mockito-1" ,java-mockito-1)
       ("java-objenesis" ,java-objenesis)))
    (propagated-inputs
     (list maven-resolver-parent-pom))
    (home-page "https://github.com/apache/maven-resolver")
    (synopsis "Maven repository system API")
    (description "This package contains the API for the maven repository system.")
    (license license:asl2.0)))

(define maven-resolver-parent-pom
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-parent-pom")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (add-before 'install 'fix-pom
           (lambda _
             (substitute* "pom.xml"
               (("<classifier>no_aop</classifier>") ""))
             #t))
         (replace 'install
           (install-pom-file "pom.xml")))))
    (propagated-inputs (list maven-parent-pom-37))))

(define-public maven-resolver-spi
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-spi")
    (arguments
     `(#:jar-name "maven-resolver-spi.jar"
       #:source-dir "maven-resolver-spi/src/main/java"
       #:test-dir "maven-resolver-spi/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "maven-resolver-spi/pom.xml")))))
    (propagated-inputs
     (list maven-resolver-api))
    (synopsis "Maven repository system SPI")
    (description "This package contains the service provider interface (SPI)
for repository system implementations and repository connectors.")))

(define-public maven-resolver-test-util
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-test-util")
    (arguments
     `(#:jar-name "maven-resolver-test-util.jar"
       #:source-dir "maven-resolver-test-util/src/main/java"
       #:test-dir "maven-resolver-test-util/src/test"
       #:jdk ,icedtea-8))
    (inputs
     (list maven-resolver-api maven-resolver-spi))
    (synopsis "Utility classes for testing the maven repository system")
    (description "This package contains a collection of utility classes to
ease testing of the repository system.")))

(define-public maven-resolver-util
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-util")
    (arguments
     `(#:jar-name "maven-resolver-util.jar"
       #:source-dir "maven-resolver-util/src/main/java"
       #:test-dir "maven-resolver-util/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "maven-resolver-util/pom.xml")))))
    (propagated-inputs
     (list maven-resolver-api))
    (native-inputs
     (list java-junit java-hamcrest-all maven-resolver-test-util))
    (synopsis "Utility classes for the maven repository system")
    (description "This package contains a collection of utility classes to
ease usage of the repository system.")))

(define-public maven-resolver-connector-basic
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-connector-basic")
    (arguments
     `(#:jar-name "maven-resolver-connector-basic.jar"
       #:source-dir "maven-resolver-connector-basic/src/main/java"
       #:test-dir "maven-resolver-connector-basic/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sisu
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda _
                 (display "org.eclipse.aether.connector.basic.BasicRepositoryConnectorFactory\n")))
             #t))
         (replace 'install
           (install-from-pom "maven-resolver-connector-basic/pom.xml")))))
    (propagated-inputs
     (list maven-resolver-api maven-resolver-spi maven-resolver-util
           java-slf4j-api))
    (native-inputs
     (list java-javax-inject java-junit maven-resolver-test-util))
    (synopsis "Maven repository connector implementation")
    (description "This package contains a repository connector implementation
for repositories using URI-based layouts.")))

(define-public maven-resolver-named-locks
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-named-locks")
    (arguments
     `(#:jar-name "maven-resolver-named-locks.jar"
       #:source-dir "maven-resolver-named-locks/src/main/java"
       #:test-dir "maven-resolver-named-locks/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sisu
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda _
                 (display "org.eclipse.aether.named.providers.FileLockNamedLockFactory
org.eclipse.aether.named.providers.LocalReadWriteLockNamedLockFactory
org.eclipse.aether.named.providers.LocalSemaphoreNamedLockFactory
org.eclipse.aether.named.providers.NoopNamedLockFactory\n")))))
         (replace 'install
           (install-from-pom "maven-resolver-named-locks/pom.xml")))))
    (propagated-inputs
      (list java-slf4j-api))
    (native-inputs
      (list java-javax-inject java-junit java-hamcrest-all))
    (synopsis "Maven artifact resolver named locks")
    (description "This package contains a synchronization utility implementation
using named locks.")))

(define-public maven-resolver-impl
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-impl")
    (arguments
     `(#:jar-name "maven-resolver-impl.jar"
       #:source-dir "maven-resolver-impl/src/main/java"
       #:test-dir "maven-resolver-impl/src/test"
       #:tests? #f; require more recent hamcrest
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sisu
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda _
                 ;; Build this list by looking for files containing "@Named"
                 (display "org.eclipse.aether.internal.impl.DefaultArtifactResolver
org.eclipse.aether.internal.impl.DefaultChecksumPolicyProvider
org.eclipse.aether.internal.impl.DefaultDeployer
org.eclipse.aether.internal.impl.DefaultFileProcessor
org.eclipse.aether.internal.impl.DefaultInstaller
org.eclipse.aether.internal.impl.DefaultLocalPathComposer
org.eclipse.aether.internal.impl.DefaultLocalPathPrefixComposerFactory
org.eclipse.aether.internal.impl.DefaultLocalRepositoryProvider
org.eclipse.aether.internal.impl.DefaultMetadataResolver
org.eclipse.aether.internal.impl.DefaultOfflineController
org.eclipse.aether.internal.impl.DefaultRemoteRepositoryManager
org.eclipse.aether.internal.impl.DefaultRepositoryConnectorProvider
org.eclipse.aether.internal.impl.DefaultRepositoryEventDispatcher
org.eclipse.aether.internal.impl.DefaultRepositoryLayoutProvider
org.eclipse.aether.internal.impl.DefaultRepositorySystem
org.eclipse.aether.internal.impl.DefaultRepositorySystemLifecycle
org.eclipse.aether.internal.impl.DefaultTrackingFileManager
org.eclipse.aether.internal.impl.DefaultTransporterProvider
org.eclipse.aether.internal.impl.DefaultUpdateCheckManager
org.eclipse.aether.internal.impl.DefaultUpdatePolicyAnalyzer
org.eclipse.aether.internal.impl.EnhancedLocalRepositoryManagerFactory
org.eclipse.aether.internal.impl.LoggerFactoryProvider
org.eclipse.aether.internal.impl.Maven2RepositoryLayoutFactory
org.eclipse.aether.internal.impl.SimpleLocalRepositoryManagerFactory
org.eclipse.aether.internal.impl.checksum.DefaultChecksumAlgorithmFactorySelector
org.eclipse.aether.internal.impl.checksum.Md5ChecksumAlgorithmFactory
org.eclipse.aether.internal.impl.checksum.Sha1ChecksumAlgorithmFactory
org.eclipse.aether.internal.impl.checksum.Sha256ChecksumAlgorithmFactory
org.eclipse.aether.internal.impl.checksum.Sha512ChecksumAlgorithmFactory
org.eclipse.aether.internal.impl.checksum.SparseDirectoryTrustedChecksumsSource
org.eclipse.aether.internal.impl.checksum.SummaryFileTrustedChecksumsSource
org.eclipse.aether.internal.impl.checksum.TrustedToProvidedChecksumsSourceAdapter
org.eclipse.aether.internal.impl.collect.DefaultDependencyCollector
org.eclipse.aether.internal.impl.collect.bf.BfDependencyCollector
org.eclipse.aether.internal.impl.collect.df.DfDependencyCollector
org.eclipse.aether.internal.impl.filter.DefaultRemoteRepositoryFilterManager
org.eclipse.aether.internal.impl.filter.GroupIdRemoteRepositoryFilterSource
org.eclipse.aether.internal.impl.filter.PrefixesRemoteRepositoryFilterSource
org.eclipse.aether.internal.impl.resolution.TrustedChecksumsArtifactResolverPostProcessor
org.eclipse.aether.internal.impl.slf4j.Slf4jLoggerFactory
org.eclipse.aether.internal.impl.synccontext.DefaultSyncContextFactory
org.eclipse.aether.internal.impl.synccontext.legacy.DefaultSyncContextFactory
org.eclipse.aether.internal.impl.synccontext.named.NamedLockFactoryAdapterFactoryImpl
org.eclipse.aether.internal.impl.synccontext.named.providers.DiscriminatingNameMapperProvider
org.eclipse.aether.internal.impl.synccontext.named.providers.FileGAVNameMapperProvider
org.eclipse.aether.internal.impl.synccontext.named.providers.FileHashingGAVNameMapperProvider
org.eclipse.aether.internal.impl.synccontext.named.providers.GAVNameMapperProvider
org.eclipse.aether.internal.impl.synccontext.named.providers.StaticNameMapperProvider\n")))))
         (replace 'install
           (install-from-pom "maven-resolver-impl/pom.xml")))))
    (propagated-inputs
     (list maven-resolver-api
           maven-resolver-named-locks
           maven-resolver-spi
           maven-resolver-util
           java-commons-lang3
           java-eclipse-sisu-inject
           java-javax-inject
           java-guice
           java-slf4j-api
           maven-resolver-parent-pom))
    (native-inputs
     (list java-hamcrest-all java-junit java-mockito-1 maven-resolver-test-util))))

(define-public maven-resolver-transport-wagon
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-transport-wagon")
    (arguments
     `(#:jar-name "maven-resolver-transport-wagon.jar"
       #:source-dir "maven-resolver-transport-wagon/src/main/java"
       #:test-dir "maven-resolver-transport-wagon/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sisu
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda _
                 (display "org.eclipse.aether.internal.transport.wagon.PlexusWagonConfigurator
org.eclipse.aether.internal.transport.wagon.PlexusWagonProvider
org.eclipse.aether.transport.wagon.WagonTransporterFactory"))))))))
    (inputs
     `(("maven-resolver-api" ,maven-resolver-api)
       ("maven-resolver-spi" ,maven-resolver-spi)
       ("maven-resolver-util" ,maven-resolver-util)
       ("java-javax-inject" ,java-javax-inject)
       ("mavne-wagon-provider-api" ,maven-wagon-provider-api)
       ("java-plexus-component-annotation" ,java-plexus-component-annotations-1.7)
       ("java-plexus-classworld" ,java-plexus-classworlds)
       ("java-plexus-plexus-util" ,java-plexus-utils)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)))
    (native-inputs
     (list java-junit
           java-hamcrest-core
           maven-resolver-test-util
           java-guava
           java-cglib
           java-aopalliance
           java-guice))
    (synopsis "Transport implementation for Maven")
    (description "This package contains a transport implementation based on
Maven Wagon, for use in Maven.")))

(define-public maven-resolver-transport-file
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-transport-file")
    (arguments
     `(#:jar-name "maven-resolver-transport-file.jar"
       #:source-dir "maven-resolver-transport-file/src/main/java"
       #:test-dir "maven-resolver-transport-file/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sisu
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda _
                 (display "org.eclipse.aether.transport.file.FileTransporterFactory\n"))))))))
    (inputs
     (list java-eclipse-sisu-inject
           java-eclipse-sisu-plexus
           java-javax-inject
           java-plexus-classworlds
           java-plexus-component-annotations
           java-plexus-utils
           java-slf4j-api
           maven-resolver-api
           maven-resolver-spi
           maven-resolver-util
           maven-wagon-provider-api))
    (native-inputs
     (list java-aopalliance
           java-cglib
           java-guava
           java-guice
           java-hamcrest-core
           java-junit
           maven-resolver-test-util))
    (synopsis "Transport implementation for Maven")
    (description "This package contains a transport implementation based on
files, for use in Maven.")))

(define-public maven-resolver-transport-http
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-transport-http")
    (arguments
     `(#:jar-name "maven-resolver-transport-http.jar"
       #:source-dir "maven-resolver-transport-http/src/main/java"
       #:test-dir "maven-resolver-transport-http/src/test"
       #:jdk ,icedtea-8
       ;; Tests all fail because
       ;; org.eclipse.aether.transport.http.SslSocketFactory is not available.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sisu
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda _
                 (display "org.eclipse.aether.transport.http.HttpTransporterFactory
org.eclipse.aether.transport.http.Nexus2ChecksumExtractor
org.eclipse.aether.transport.http.XChecksumChecksumExtractor\n"))))))))
    (inputs
     (list java-eclipse-sisu-inject
           java-eclipse-sisu-plexus
           java-javax-inject
           java-plexus-classworlds
           java-plexus-component-annotations
           java-plexus-utils
           java-slf4j-api
           maven-resolver-api
           maven-resolver-spi
           maven-resolver-util
           maven-wagon-provider-api))
    (propagated-inputs
     (list java-httpcomponents-httpclient
           java-httpcomponents-httpcore))
    (native-inputs
     (list java-aopalliance
           java-cglib
           java-eclipse-aether-api
           java-eclipse-jetty-http
           java-eclipse-jetty-io
           java-eclipse-jetty-server
           java-eclipse-jetty-servlet
           java-eclipse-jetty-util
           java-guava
           java-guice
           java-hamcrest-core
           java-javaee-servletapi
           java-junit
           maven-resolver-test-util))
    (synopsis "Transport implementation for Maven")
    (description "This package contains a transport implementation based on
HTTP, for use in Maven.")))

;; aether is the parent project that was forked into maven-resolver.  It used
;; to be used with older versions of Maven, and is still required for some
;; plugins and their dependencies.  This version is required for the plugins,
;; even though there are newer versions of this project.
(define-public java-sonatype-aether-api
  (package
    (name "java-sonatype-aether-api")
    (version "1.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/sonatype/sonatype-aether")
                     (commit (string-append "aether-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wn9fv91n40bvlwbzy0dmh0xqibxl2mpzpnbibhqss3c0zlr1ccq"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "aether-api.jar"
       #:source-dir "aether-api/src/main/java"
       #:test-dir "aether-api/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'install-parent (install-pom-file "pom.xml"))
         (replace 'install (install-from-pom "aether-api/pom.xml")))))
    (propagated-inputs
     (list java-sonatype-forge-parent-pom-6))
    (native-inputs (list java-junit))
    (home-page "https://github.com/sonatype/sonatype-aether")
    (synopsis "Maven repository system API")
    (description "This package contains the API for the maven repository system.")
    (license license:asl2.0)))

(define-public java-sonatype-aether-spi
  (package
    (inherit java-sonatype-aether-api)
    (name "java-sonatype-aether-spi")
    (arguments
     `(#:jar-name "aether-spi.jar"
       #:source-dir "aether-spi/src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-from-pom "aether-spi/pom.xml")))))
    (propagated-inputs
     (list java-sonatype-aether-api))
    (synopsis "Maven repository system SPI")
    (description "This package contains the service provider interface (SPI)
for repository system implementations and repository connectors.")))

(define-public java-sonatype-aether-test-util
  (package
    (inherit java-sonatype-aether-api)
    (name "java-sonatype-aether-test-util")
    (arguments
     `(#:jar-name "java-sonatype-aether-test-util.jar"
       #:source-dir "aether-test-util/src/main/java"
       #:test-dir "aether-test-util/src/test"))
    (inputs
     (list java-sonatype-aether-api java-sonatype-aether-spi))
    (synopsis "Utility classes for testing the maven repository system")
    (description "This package contains a collection of utility classes to
ease testing of the repository system.")))

(define-public java-sonatype-aether-util
  (package
    (inherit java-sonatype-aether-api)
    (name "java-sonatype-aether-util")
    (arguments
     `(#:jar-name "aether-util.jar"
       #:source-dir "aether-util/src/main/java"
       #:test-dir "aether-util/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-from-pom "aether-util/pom.xml")))))
    (propagated-inputs
     (list java-sonatype-aether-api))
    (native-inputs
     (list java-junit java-sonatype-aether-test-util))
    (synopsis "Utility classes for the maven repository system")
    (description "This package contains a collection of utility classes to
ease usage of the repository system.")))

(define-public java-sonatype-aether-impl
  (package
    (inherit java-sonatype-aether-api)
    (name "java-sonatype-aether-impl")
    (arguments
     `(#:jar-name "aether-impl.jar"
       #:source-dir "aether-impl/src/main/java"
       #:test-dir "aether-impl/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'fix-pom
           (lambda _
             (substitute* "aether-impl/pom.xml"
               (("org.sonatype.sisu") "org.codehaus.plexus")
               (("sisu-inject-plexus") "plexus-container-default"))
             #t))
         (add-after 'build 'generate-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "src/main/java"
                     "--output" "build/classes/META-INF/plexus/components.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes/META-INF")
             #t))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t))
         (replace 'install (install-from-pom "aether-impl/pom.xml")))))
    (propagated-inputs
     (list java-sonatype-aether-api
           java-sonatype-aether-spi
           java-sonatype-aether-util
           java-plexus-component-annotations
           java-plexus-container-default
           java-slf4j-api))
    (native-inputs
     (list java-junit java-plexus-component-metadata
           java-sonatype-aether-test-util))))

;; This slightly newer version is also required by some plugins
(define-public java-sonatype-aether-api-1.13
  (package
    (name "java-sonatype-aether-api")
    (version "1.13.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/sonatype/sonatype-aether")
                     (commit (string-append "aether-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1yl34dqhm6ykb7h63gkssyrdxv3dsa3n5b8d8cvy8rh4qsm6p2yb"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "aether-api.jar"
       #:source-dir "aether-api/src/main/java"
       #:test-dir "aether-api/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'install-parent (install-pom-file "pom.xml"))
         (replace 'install (install-from-pom "aether-api/pom.xml")))))
    (propagated-inputs
     `(("java-sonatype-forge-parent-pom" ,java-sonatype-forge-parent-pom-10)))
    (native-inputs `(("java-junit" ,java-junit)))
    (home-page "https://github.com/sonatype/sonatype-aether")
    (synopsis "Maven repository system API")
    (description "This package contains the API for the maven repository system.")
    (license license:asl2.0)))

(define-public java-sonatype-aether-spi-1.13
  (package
    (inherit java-sonatype-aether-api-1.13)
    (name "java-sonatype-aether-spi")
    (arguments
     `(#:jar-name "aether-spi.jar"
       #:source-dir "aether-spi/src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-from-pom "aether-spi/pom.xml")))))
    (propagated-inputs
     `(("java-sonatype-aether-api" ,java-sonatype-aether-api-1.13)))
    (synopsis "Maven repository system SPI")
    (description "This package contains the service provider interface (SPI)
for repository system implementations and repository connectors.")))

(define-public java-sonatype-aether-test-util-1.13
  (package
    (inherit java-sonatype-aether-api-1.13)
    (name "java-sonatype-aether-test-util")
    (arguments
     `(#:jar-name "java-sonatype-aether-test-util.jar"
       #:source-dir "aether-test-util/src/main/java"
       #:test-dir "aether-test-util/src/test"))
    (inputs
     `(("java-sonatype-aether-api" ,java-sonatype-aether-api-1.13)
       ("java-sonatype-aether-spi" ,java-sonatype-aether-spi-1.13)))
    (synopsis "Utility classes for testing the maven repository system")
    (description "This package contains a collection of utility classes to
ease testing of the repository system.")))

(define-public java-sonatype-aether-util-1.13
  (package
    (inherit java-sonatype-aether-api-1.13)
    (name "java-sonatype-aether-util")
    (arguments
     `(#:jar-name "aether-util.jar"
       #:source-dir "aether-util/src/main/java"
       #:test-dir "aether-util/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-from-pom "aether-util/pom.xml")))))
    (propagated-inputs
     `(("java-sonatype-aether-api" ,java-sonatype-aether-api-1.13)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-sonatype-aether-test-util" ,java-sonatype-aether-test-util-1.13)))
    (synopsis "Utility classes for the maven repository system")
    (description "This package contains a collection of utility classes to
ease usage of the repository system.")))

;; Again, this old version is required by some maven plugins
(define-public java-eclipse-aether-api
  (package
    (name "java-eclipse-aether-api")
    (version "1.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/eclipse/aether-core")
                     (commit "aether-1.0.2.v20150114")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14d336nn0kh5ddf23j37va3hd8gaai19llrpxhf4bcc7g7sgdqxs"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "aether-api.jar"
       #:source-dir "aether-api/src/main/java"
       #:test-dir "aether-api/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'install-parent (install-pom-file "pom.xml"))
         (replace 'install (install-from-pom "aether-api/pom.xml")))))
    (native-inputs (list java-junit))
    (home-page "https://github.com/sonatype/sonatype-aether")
    (synopsis "Maven repository system API")
    (description "This package contains the API for the maven repository system.")
    (license license:asl2.0)))

(define-public java-eclipse-aether-spi
  (package
    (inherit java-eclipse-aether-api)
    (name "java-eclipse-aether-spi")
    (arguments
     `(#:jar-name "aether-spi.jar"
       #:source-dir "aether-spi/src/main/java"
       #:test-dir "aether-spi/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-from-pom "aether-spi/pom.xml")))))
    (propagated-inputs
     (list java-eclipse-aether-api))
    (synopsis "Maven repository system SPI")
    (description "This package contains the service provider interface (SPI)
for repository system implementations and repository connectors.")))

(define-public java-eclipse-aether-test-util
  (package
    (inherit java-eclipse-aether-api)
    (name "java-eclipse-aether-test-util")
    (arguments
     `(#:jar-name "aether-test-util.jar"
       #:source-dir "aether-test-util/src/main/java"
       #:test-dir "aether-test-util/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-from-pom "aether-util/pom.xml")))))
    (propagated-inputs
     (list java-eclipse-aether-api java-eclipse-aether-spi))
    (synopsis "Utility classes for testing the maven repository system")
    (description "This package contains a collection of utility classes to
ease testing of the repository system.")))

(define-public java-eclipse-aether-util
  (package
    (inherit java-eclipse-aether-api)
    (name "java-eclipse-aether-util")
    (arguments
     `(#:jar-name "aether-util.jar"
       #:source-dir "aether-util/src/main/java"
       #:test-dir "aether-util/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-from-pom "aether-util/pom.xml")))))
    (propagated-inputs
     (list java-eclipse-aether-api))
    (native-inputs
     (list java-eclipse-aether-test-util java-junit))
    (synopsis "Utility classes for the maven repository system")
    (description "This package contains a collection of utility classes to
ease usage of the repository system.")))

(define-public java-eclipse-aether-impl
  (package
    (inherit java-eclipse-aether-api)
    (name "java-eclipse-aether-impl")
    (arguments
     `(#:jar-name "aether-impl.jar"
       #:jdk ,openjdk11
       #:source-dir "aether-impl/src/main/java"
       #:test-dir "aether-impl/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-from-pom "aether-impl/pom.xml")))))
    (propagated-inputs
     (list java-eclipse-aether-api
           java-eclipse-aether-spi
           java-eclipse-aether-util
           java-javax-inject
           java-eclipse-sisu-inject
           java-guice
           java-slf4j-api))
    (native-inputs
     (list java-eclipse-aether-test-util java-junit))))

(define-public maven-shared-utils
  (package
    (name "maven-shared-utils")
    (version "3.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/shared/"
                                  "maven-shared-utils-" version "-source-release.zip"))
              (sha256
               (base32
                "1h42ilhpgkn2cqc83lj3q9bcj6r2l4kkx7g69p55pssmwwxz3k2w"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-shared-utils.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-/bin/sh-invocation
           (lambda _
             (substitute* (find-files "src" ".*.java$")
               (("/bin/sh") (which "sh")))
             #t))
         (add-before 'check 'remove-cyclic-dep
           (lambda _
             (delete-file
               "src/test/java/org/apache/maven/shared/utils/introspection/ReflectionValueExtractorTest.java")
             #t))
         (add-before 'check 'set-test-env
           (lambda _
             (setenv "TEST_SHARED_ENV" "TestValue")))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     `(("java-jansi" ,java-jansi)
       ("java-commons-io" ,java-commons-io)
       ("java-jsr305" ,java-jsr305)
       ("java-plexus-container-default" ,java-plexus-container-default)
       ("maven-parent-pom-30" ,maven-parent-pom-30)))
    (native-inputs
     (list unzip java-junit java-hamcrest-all java-commons-lang3 java-commons-text))
    (home-page "https://maven.apache.org/shared/maven-shared-utils/")
    (synopsis "Plexus-util replacement for maven")
    (description "This project aims to be a functional replacement for
plexus-utils in Maven.  It is not a 100% API compatible replacement but a
replacement with improvements.")
    (license license:asl2.0)))

(define-public maven-plugin-annotations
  (package
    (name "maven-plugin-annotations")
    (version "3.15.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/"
                                  "plugin-tools/maven-plugin-tools-" version
                                  "-source-release.zip"))
              (sha256 (base32 "0w7k7x3w9x269z219664swh57m7l330b0mxii0az5c21dkqs2awr"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-plugin-annotations.jar"
       #:source-dir "maven-plugin-annotations/src/main/java"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "maven-plugin-annotations/pom.xml")))))
    (propagated-inputs
     (list maven-artifact maven-plugin-tools-parent-pom))
    (native-inputs
     (list unzip))
    (home-page "https://maven.apache.org/plugin-tools/maven-plugin-annotations/")
    (synopsis "Java 5 annotations to use in Mojos")
    (description "This package contains Java 5 annotations for use in Mojos.")
    (license license:asl2.0)))

(define-public maven-plugin-tools-parent-pom
  (package
    (inherit maven-plugin-annotations)
    (name "maven-plugin-tools-parent-pom")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (add-before 'install 'fix-dependency-versions
           (lambda _
             ;; Update dependency versions to match what we have in Guix.
             ;; Maven's plugin classloader resolves dependencies from
             ;; the pom.xml, and will fail in offline mode if versions
             ;; don't match the m2 repository.
             (substitute* "pom.xml"
               ;; junit-bom
               (("5\\.11\\.3")
                ,(package-version junit-bom-5.11))
               ;; plexus-xml: 3.0.1 -> 3.0.2
               (("<plexusXmlVersion>3\\.0\\.1</plexusXmlVersion>")
                ,(string-append "<plexusXmlVersion>"
                                (package-version java-plexus-xml)
                                "</plexusXmlVersion>"))
               ;; plexus-archiver: 4.10.0 -> 4.10.4
               (("<version>4\\.10\\.0</version>")
                ,(string-append "<version>"
                                (package-version java-plexus-archiver)
                                "</version>"))
               ;; velocity-engine-core: 2.4 -> 2.4.1
               (("<version>2\\.4</version>")
                ,(string-append "<version>"
                                (package-version java-velocity-engine-core)
                                "</version>"))
               ;; plexus-velocity: 2.2.0 -> 2.0
               ;; NOTE: Must come before qdox substitution to avoid conflict
               (("<version>2\\.2\\.0</version>")
                ,(string-append "<version>"
                                (package-version java-plexus-velocity)
                                "</version>"))
               ;; qdox: 2.1.0 -> 2.2.0 (java-qdox-2)
               (("<version>2\\.1\\.0</version>")
                ,(string-append "<version>"
                                (package-version java-qdox-2)
                                "</version>"))
               ;; jsoup: 1.18.1 -> 1.15.3
               (("<version>1\\.18\\.1</version>")
                ,(string-append "<version>"
                                (package-version java-jsoup)
                                "</version>")))))
         (replace 'install
           (install-pom-file "pom.xml")))))
    (propagated-inputs
     (list maven-parent-pom-43))))

(define-public maven-wagon-provider-api
  (package
    (name "maven-wagon-provider-api")
    (version "3.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/wagon/"
                                  "wagon-" version "-source-release.zip"))
              (sha256
               (base32
                "1rnviw0yr4g5902fb8pkd1gyvci4bz7hndjvhkqmnkj7ay0y6mf0"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-wagon-provider-api.jar"
       #:source-dir "wagon-provider-api/src/main/java"
       #:test-dir "wagon-provider-api/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "wagon-provider-api/pom.xml")))))
    (propagated-inputs
     `(("java-plexus-utils" ,java-plexus-utils)
       ("maven-wagon-parent-pom" ,maven-wagon-parent-pom)))
    (native-inputs
     (list unzip java-junit java-easymock))
    (home-page "https://maven.apache.org/wagon")
    (synopsis "Transport abstraction for Maven")
    (description "Maven Wagon is a transport abstraction that is used in Maven's
artifact and repository handling code.")
    (license license:asl2.0)))

(define maven-wagon-parent-pom
  (package
    (inherit maven-wagon-provider-api)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (install-pom-file "pom.xml")))))
    (propagated-inputs
     `(("maven-parent-pom-33" ,maven-parent-pom-33)))
    (native-inputs
     `(("unzip" ,unzip)))))

(define-public maven-wagon-provider-test
  (package
    (inherit maven-wagon-provider-api)
    (name "maven-wagon-provider-test")
    (arguments
     `(#:jar-name "maven-wagon-provider-test.jar"
       #:source-dir "wagon-provider-test/src/main/java"
       #:tests? #f; no tests
       #:jdk ,icedtea-8))
    (inputs
     `(("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-container-default" ,java-plexus-container-default)
       ("java-eclipse-jetty-http-9.2" ,java-eclipse-jetty-http-9.2)
       ("java-eclipse-jetty-util-9.2" ,java-eclipse-jetty-util-9.2)
       ("java-eclipse-jetty-security-9.2" ,java-eclipse-jetty-security-9.2)
       ("java-eclipse-jetty-server-9.2" ,java-eclipse-jetty-server-9.2)
       ("java-eclipse-jetty-servlet-9.2" ,java-eclipse-jetty-servlet-9.2)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-javaee-servletapi" ,java-javaee-servletapi)
       ("maven-wagon-provider-api" ,maven-wagon-provider-api)))
    (synopsis "Test classes from maven-wagon")
    (description "Maven Wagon is a transport abstraction that is used in Maven's
artifact and repository handling code.  This package contains common test
classes used in multiple maven-wagon components.")))

(define-public maven-wagon-file
  (package
    (inherit maven-wagon-provider-api)
    (name "maven-wagon-file")
    (arguments
     `(#:jar-name "maven-wagon-file.jar"
       #:source-dir "wagon-providers/wagon-file/src/main/java"
       #:test-dir "wagon-providers/wagon-file/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-paths
           (lambda _
             ;; Tests assume they are run by maven, which copies test resources
             ;; to target.  Our ant-build-system does the same, but with the
             ;; build directory.
             (substitute* "wagon-providers/wagon-file/src/test/java/org/apache/maven/wagon/providers/file/FileWagonTest.java"
               (("target") "build"))
             #t))
         (add-after 'build 'generate-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "wagon-providers/wagon-file/src/main/java"
                     "--output" "build/classes/META-INF/plexus/components.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes/META-INF")
             #t))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t)))))
    (inputs
     (list java-plexus-utils maven-wagon-provider-api))
    (native-inputs
     `(("maven-wagon-provider-test" ,maven-wagon-provider-test)
       ("java-plexus-component-metadata" ,java-plexus-component-metadata-1.7)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations-1.7)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-guava" ,java-guava)
       ("java-guice" ,java-guice)
       ("java-javax-inject" ,java-javax-inject)
       ("java-cglib" ,java-cglib)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-cli" ,java-plexus-cli)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-plugin-annotations" ,maven-plugin-annotations)
       ("maven-core" ,maven-core)
       ("maven-model" ,maven-model)
       ("java-commons-cli" ,java-commons-cli)
       ("java-qdox" ,java-qdox)
       ("java-jdom2" ,java-jdom2)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ,@(package-native-inputs maven-wagon-provider-api)))
    (synopsis "Wagon provider that gets and puts artifacts using the file system")
    (description "Maven Wagon is a transport abstraction that is used in Maven's
artifact and repository handling code.  It uses providers, that are tools to
manage artifacts and deployment.  This package contains the file provider which
gets and puts artifacts using the file system.")))

(define-public maven-wagon-tck-http
  (package
    (inherit maven-wagon-provider-api)
    (name "maven-wagon-tck-http")
    (arguments
     `(#:jar-name "maven-wagon-tck-http.jar"
       #:source-dir "wagon-tcks/wagon-tck-http/src/main/java"
       #:tests? #f; no tests
       #:jdk ,icedtea-8))
    (inputs
     `(("java-plexus-util" ,java-plexus-utils)
       ("maven-wagon-provider-api" ,maven-wagon-provider-api)
       ("java-javaee-servletapi" ,java-javaee-servletapi)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-commons-codec" ,java-commons-codec)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-eclipse-jetty-util-9.2" ,java-eclipse-jetty-util-9.2)
       ("java-eclipse-jetty-webapp-9.2" ,java-eclipse-jetty-webapp-9.2)
       ("java-eclipse-jetty-security-9.2" ,java-eclipse-jetty-security-9.2)
       ("java-eclipse-jetty-server-9.2" ,java-eclipse-jetty-server-9.2)
       ("java-eclipse-jetty-servlet-9.2" ,java-eclipse-jetty-servlet-9.2)))
    (synopsis "Wagon HTTP Test Compatibility Kit")
    (description "Maven Wagon is a transport abstraction that is used in Maven's
artifact and repository handling code.  This package contains the HTTP
Test Compatibility Kit.")))

(define-public maven-wagon-http-shared
  (package
    (inherit maven-wagon-provider-api)
    (name "maven-wagon-http-shared")
    (arguments
     `(#:jar-name "maven-wagon-http-shared.jar"
       #:source-dir "wagon-providers/wagon-http-shared/src/main/java"
       #:test-dir "wagon-providers/wagon-http-shared/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'generate-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "wagon-providers/wagon-http-shared/src/main/java"
                     "--output" "build/classes/META-INF/plexus/components.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes/META-INF")
             #t))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t)))))
    (inputs
     (list java-plexus-utils
           java-httpcomponents-httpclient
           java-httpcomponents-httpcore
           java-commons-io
           java-jsoup
           maven-wagon-provider-api))
    (native-inputs
     `(("maven-wagon-provider-test" ,maven-wagon-provider-test)
       ("java-plexus-component-metadata" ,java-plexus-component-metadata-1.7)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations-1.7)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-guava" ,java-guava)
       ("java-guice" ,java-guice)
       ("java-javax-inject" ,java-javax-inject)
       ("java-cglib" ,java-cglib)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-cli" ,java-plexus-cli)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-plugin-annotations" ,maven-plugin-annotations)
       ("maven-core" ,maven-core)
       ("maven-model" ,maven-model)
       ("java-commons-cli" ,java-commons-cli)
       ("java-qdox" ,java-qdox)
       ("java-jdom2" ,java-jdom2)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ,@(package-native-inputs maven-wagon-provider-api)))
    (synopsis "Shared Library for wagon providers supporting HTTP")
    (description "Maven Wagon is a transport abstraction that is used in Maven's
artifact and repository handling code.  It uses providers, that are tools to
manage artifacts and deployment.  This package contains a shared library for
wagon providers supporting HTTP.")))

(define-public maven-wagon-http
  (package
    (inherit maven-wagon-provider-api)
    (name "maven-wagon-http")
    (arguments
     `(#:jar-name "maven-wagon-http.jar"
       #:source-dir "wagon-providers/wagon-http/src/main/java"
       #:test-dir "wagon-providers/wagon-http/src/test"
       #:test-exclude (list
                        "**/Abstract*.java"
                        ;; FIXME: javax.net.ssl.SSLHandshakeException:
                        ;; sun.security.validator.ValidatorException:
                        ;; PKIX path building failed:
                        ;; sun.security.provider.certpath.SunCertPathBuilderException:
                        ;; unable to find valid certification path to requested target
                        "**/HttpsWagonPreemptiveTest.java"
                        "**/HttpsWagonTest.java"
                        ;; Timeout
                        "**/HugeFileDownloadTest.java"
                        ;; Injection errors
                        "**/TckTest.java")
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (install-file "wagon-providers/wagon-http/src/main/resources/META-INF/plexus/components.xml"
                           "build/classes/META-INF/plexus")
             #t))
         (add-before 'check 'fix-resource-path
           (lambda _
             (substitute* '("wagon-providers/wagon-http/src/test/java/org/apache/maven/wagon/providers/http/HttpsWagonPreemptiveTest.java"
                            "wagon-providers/wagon-http/src/test/java/org/apache/maven/wagon/providers/http/HttpsWagonTest.java")
               (("src/test") "wagon-providers/wagon-http/src/test"))
             #t)))))
    (inputs
     (list java-plexus-utils
           java-httpcomponents-httpclient
           java-httpcomponents-httpcore
           maven-wagon-http-shared
           maven-wagon-tck-http
           maven-wagon-provider-api))
    (native-inputs
     `(("maven-wagon-provider-test" ,maven-wagon-provider-test)
       ("java-plexus-component-metadata" ,java-plexus-component-metadata-1.7)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations-1.7)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-container-default" ,java-plexus-container-default)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-guava" ,java-guava)
       ("java-guice" ,java-guice)
       ("java-inject" ,java-javax-inject)
       ("java-cglib" ,java-cglib)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-cli" ,java-plexus-cli)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-plugin-annotations" ,maven-plugin-annotations)
       ("maven-slf4j-provider" ,maven-slf4j-provider)
       ("maven-core" ,maven-core)
       ("maven-model" ,maven-model)
       ("java-commons-cli" ,java-commons-cli)
       ("java-qdox" ,java-qdox)
       ("java-jdom2" ,java-jdom2)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-javaee-servletapi" ,java-javaee-servletapi)
       ("java-eclipse-jetty-util-9.2" ,java-eclipse-jetty-util-9.2)
       ("java-eclipse-jetty-io-9.2" ,java-eclipse-jetty-io-9.2)
       ("java-eclipse-jetty-http-9.2" ,java-eclipse-jetty-http-9.2)
       ("java-eclipse-jetty-server-9.2" ,java-eclipse-jetty-server-9.2)
       ("java-eclipse-jetty-servlet-9.2" ,java-eclipse-jetty-servlet-9.2)
       ("java-eclipse-jetty-security-9.2" ,java-eclipse-jetty-security-9.2)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-commons-codec" ,java-commons-codec)
       ("java-commons-io" ,java-commons-io)
       ("java-jsoup" ,java-jsoup)
       ,@(package-native-inputs maven-wagon-provider-api)))
    (synopsis "Wagon provider that gets and puts artifacts through HTTP(S)")
    (description "Maven Wagon is a transport abstraction that is used in Maven's
artifact and repository handling code.  It uses providers, that are tools to
manage artifacts and deployment.  This package contains a Wagon provider that
gets and puts artifacts through HTTP(S) using Apache HttpClient-4.x.")))

(define maven-pom
  (package
    (name "maven-pom")
    (version "3.9.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/"
                                  "maven-3/" version "/source/"
                                  "apache-maven-" version "-src.tar.gz"))
              (sha256 (base32 "0b4blz63q75pdfg2jlxmrp8h5306a1r4v6h11impwfdcsi4c894a"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file (find-files "." "\\.jar$"))
                  #t))
              (patches
                (search-patches "maven-generate-component-xml.patch"
                                "maven-generate-javax-inject-named.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (add-before 'install 'fix-dependencies
           (lambda _
             (substitute* "pom.xml"
               (("classWorldsVersion>.*")
                (string-append
                  "classWorldsVersion>"
                  ,(package-version java-plexus-classworlds)
                  "</classWorldsVersion>\n"))
               (("commonsCliVersion>.*")
                (string-append
                  "commonsCliVersion>"
                  ,(package-version java-commons-cli)
                  "</commonsCliVersion>\n"))
               (("commonsLangVersion>.*")
                (string-append
                  "commonsLangVersion>"
                  ,(package-version java-commons-lang3)
                  "</commonsLangVersion>\n"))
               (("plexusUtilsVersion>.*")
                (string-append
                  "plexusUtilsVersion>"
                  ,(package-version java-plexus-utils)
                  "</plexusUtilsVersion>\n"))
               (("plexusInterpolationVersion>.*")
                (string-append
                  "plexusInterpolationVersion>"
                  ,(package-version java-plexus-interpolation)
                  "</plexusInterpolationVersion>\n"))
               (("guiceVersion>.*")
                (string-append
                  "guiceVersion>"
                  ,(package-version java-guice)
                  "</guiceVersion>\n"))
               (("sisuInjectVersion>.*")
                (string-append
                  "sisuInjectVersion>"
                  ,(package-version java-eclipse-sisu-inject)
                  "</sisuInjectVersion>\n"))
               (("securityDispatcherVersion>.*")
                (string-append
                  "securityDispatcherVersion>"
                  ,(package-version java-plexus-sec-dispatcher)
                  "</securityDispatcherVersion>\n"))
               (("cipherVersion>.*")
                (string-append
                  "cipherVersion>"
                  ,(package-version java-plexus-cipher)
                  "</cipherVersion>\n"))
               (("slf4jVersion>.*")
                (string-append
                  "slf4jVersion>"
                  ,(package-version java-slf4j-api)
                  "</slf4jVersion>\n"))
               (("<classifier>no_aop</classifier>") ""))
             #t))
         (replace 'install
           (install-pom-file "pom.xml")))))
    (propagated-inputs
     (list maven-parent-pom-39))
    (home-page "https://maven.apache.org/")
    (synopsis "Build system")
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains the Maven pom file, used by all maven components.")
    (license license:asl2.0)))

(define-public maven-artifact
  (package
    (inherit maven-pom)
    (name "maven-artifact")
    (arguments
     `(#:jar-name "maven-artifact.jar"
       #:source-dir "maven-artifact/src/main/java"
       #:test-dir "maven-artifact/src/test"
       #:main-class "org.apache.maven.artifact.versioning.ComparableVersion"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "maven-artifact/pom.xml")))))
    (propagated-inputs
     (list java-plexus-utils java-commons-lang3 maven-pom))
    (native-inputs
     (list java-junit))
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains the Maven Artifact classes, providing the
@code{Artifact} interface, with its @code{DefaultArtifact} implementation.  The
jar file is executable and provides a little tool to display how Maven parses
and compares versions:")))

(define-public maven-model
  (package
    (inherit maven-artifact)
    (name "maven-model")
    (arguments
     `(#:jar-name "maven-model.jar"
       #:source-dir "maven-model/src/main/java"
       #:test-dir "maven-model/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java" "org.codehaus.modello.ModelloCli"
                       file mode "maven-model/src/main/java" version
                       "false" "true"))
             (let ((file "maven-model/src/main/mdo/maven.mdo"))
               (modello-single-mode file "4.0.0" "java")
               (modello-single-mode file "4.0.0" "xpp3-reader")
               (modello-single-mode file "4.0.0" "xpp3-extended-reader")
               (modello-single-mode file "4.0.0" "xpp3-writer")
               (modello-single-mode file "4.0.0" "xpp3-extended-writer"))
             #t))
         (replace 'install (install-from-pom "maven-model/pom.xml")))))
    (propagated-inputs
     (list java-commons-lang3 java-plexus-utils maven-pom))
    (native-inputs
     (list java-modello-core
           ;; for modello:
           java-eclipse-sisu-plexus
           java-plexus-component-annotations
           java-guice
           java-cglib
           java-eclipse-sisu-inject
           java-javax-inject
           java-plexus-classworlds
           java-guava
           java-geronimo-xbean-reflect
           java-plexus-build-api
           ;; modello plugins:
           java-modello-plugins-java
           java-modello-plugins-xml
           java-modello-plugins-xpp3
           ;; for tests
           java-junit))
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains the model for Maven @dfn{POM} (Project Object Model),
so really just plain Java objects.")))

(define-public maven-builder-support
  (package
    (inherit maven-artifact)
    (name "maven-builder-support")
    (arguments
     `(#:jar-name "maven-builder-support.jar"
       #:source-dir "maven-builder-support/src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "maven-builder-support/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-paths
           (lambda _
             (with-directory-excursion "maven-builder-support/src/test/java"
               (substitute*
                 '("org/apache/maven/building/FileSourceTest.java"
                   "org/apache/maven/building/UrlSourceTest.java")
                 (("target/test-classes") "maven-builder-support/src/test/resources")))
             #t))
         (replace 'install (install-from-pom "maven-builder-support/pom.xml")))))
    (propagated-inputs
     (list maven-pom))
    (native-inputs
     (list java-junit))
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains a support library for descriptor builders (model,
setting, toolchains)")))

(define-public maven-settings
  (package
    (inherit maven-artifact)
    (name "maven-settings")
    (arguments
     `(#:jar-name "maven-settings.jar"
       #:source-dir "maven-settings/src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java" "org.codehaus.modello.ModelloCli"
                       file mode "maven-settings/src/main/java" version
                       "false" "true"))
             (let ((file "maven-settings/src/main/mdo/settings.mdo"))
               (modello-single-mode file "1.2.0" "java")
               (modello-single-mode file "1.2.0" "xpp3-reader")
               (modello-single-mode file "1.2.0" "xpp3-writer"))
             #t))
         (replace 'install (install-from-pom "maven-settings/pom.xml")))))
    (propagated-inputs
     (list java-plexus-utils maven-pom))
    (native-inputs
     (list java-modello-core
           ;; for modello:
           ;("container" ,java-plexus-container-default)
           java-eclipse-sisu-plexus
           java-plexus-component-annotations
           java-guice
           java-cglib
           java-eclipse-sisu-inject
           java-javax-inject
           java-plexus-classworlds
           java-plexus-utils
           java-guava
           java-geronimo-xbean-reflect
           java-plexus-build-api
           ;; modello plugins:
           java-modello-plugins-java
           java-modello-plugins-xml
           java-modello-plugins-xpp3))
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains strictly the model for Maven settings, that is
simply plain java objects.")))

(define-public maven-settings-builder
  (package
    (inherit maven-artifact)
    (name "maven-settings-builder")
    (arguments
     `(#:jar-name "maven-settings-builder.jar"
       #:source-dir "maven-settings-builder/src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "maven-settings-builder/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sisu-named
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (chmod "sisu.sh" #o755)
             (invoke "./sisu.sh" "maven-settings-builder/src/main/java"
                     "build/classes/META-INF/sisu/javax.inject.Named")))
         (replace 'install (install-from-pom "maven-settings-builder/pom.xml")))))
    (propagated-inputs
     (list java-plexus-utils
           java-plexus-interpolation
           java-plexus-sec-dispatcher
           maven-builder-support
           maven-settings
           maven-pom))
    (native-inputs
     (list java-junit java-javax-inject java-plexus-component-annotations))
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains the effective model builder, with profile activation,
inheritance, interpolation, @dots{}")))

(define-public maven-model-builder
  (package
    (inherit maven-artifact)
    (name "maven-model-builder")
    (arguments
     `(#:jar-name "maven-model-builder.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "src/test"
       ;; Tests require powermock - skip for now
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             ;; Required for tests that rely on the package's default
             ;; locations, that reference ${basedir}/src/test.
             (chdir "maven-model-builder")))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources"
                               "build/classes")
             #t))
         (add-before 'build 'generate-sisu-named
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (chmod "../sisu.sh" #o755)
             (invoke "../sisu.sh" "src/main/java"
                     "build/classes/META-INF/sisu/javax.inject.Named")))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list java-plexus-interpolation
           java-plexus-utils
           maven-artifact
           maven-builder-support
           maven-model
           maven-pom))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-guava" ,java-guava)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-powermock-reflect" ,java-powermock-reflect)
       ("java-objenesis" ,java-objenesis)
       ("guice" ,java-guice)
       ("java-cglib" ,java-cglib)
       ("sisu-inject" ,java-eclipse-sisu-inject)
       ("javax-inject" ,java-javax-inject)
       ("java-xmlunit" ,java-xmlunit)
       ("java-xmlunit-matchers" ,java-xmlunit-matchers)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("classworlds" ,java-plexus-classworlds)))
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains the effective model builder, with profile activation,
inheritance, interpolation, @dots{}")))

(define-public maven-repository-metadata
  (package
    (inherit maven-artifact)
    (name "maven-repository-metadata")
    (arguments
     `(#:jar-name "maven-repository-metadata.jar"
       #:source-dir "maven-repository-metadata/src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java" "org.codehaus.modello.ModelloCli"
                       file mode "maven-repository-metadata/src/main/java" version
                       "false" "true"))
             (let ((file "maven-repository-metadata/src/main/mdo/metadata.mdo"))
               (modello-single-mode file "1.1.0" "java")
               (modello-single-mode file "1.1.0" "xpp3-reader")
               (modello-single-mode file "1.1.0" "xpp3-writer"))
             #t))
         (replace 'install
           (install-from-pom "maven-repository-metadata/pom.xml")))))
    (propagated-inputs
     (list java-plexus-utils maven-pom))
    (native-inputs
     `(("modello" ,java-modello-core)
       ;; for modello:
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-guice" ,java-guice)
       ("java-cglib" ,java-cglib)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-javax-inject" ,java-javax-inject)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-guava" ,java-guava)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-plexus-build-api" ,java-plexus-build-api)
       ;; modello plugins:
       ("java-modello-plugins-java" ,java-modello-plugins-java)
       ("java-modello-plugins-xml" ,java-modello-plugins-xml)
       ("java-modello-plugins-xpp3" ,java-modello-plugins-xpp3)))
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains strictly the model for Maven Repository Metadata,
so really just plain objects.")))

(define-public maven-resolver-provider
  (package
    (inherit maven-artifact)
    (name "maven-resolver-provider")
    (arguments
     `(#:jar-name "maven-resolver-provider.jar"
       #:source-dir "maven-resolver-provider/src/main/java"
       #:test-dir "maven-resolver-provider/src/test"
       #:jdk ,icedtea-8
       #:tests? #f; dependency loop on maven-core (@Component RepositorySystem)
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sisu-named
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (chmod "./sisu.sh" #o755)
             (invoke "./sisu.sh" "maven-resolver-provider/src/main/java"
                     "build/classes/META-INF/sisu/javax.inject.Named")
             #t))
         (replace 'install
           (install-from-pom "maven-resolver-provider/pom.xml")))))
    (propagated-inputs
     (list maven-model
           maven-model-builder
           maven-resolver-spi
           maven-resolver-api
           maven-resolver-impl
           maven-resolver-util
           maven-builder-support
           maven-repository-metadata
           java-plexus-utils
           java-plexus-component-annotations
           java-guice
           java-javax-inject))))

(define-public maven-plugin-api
  (package
    (inherit maven-artifact)
    (name "maven-plugin-api")
    (arguments
     `(#:jar-name "maven-plugin-api.jar"
       #:source-dir "maven-plugin-api/src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "maven-plugin-api/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java" "org.codehaus.modello.ModelloCli"
                       file mode "maven-plugin-api/src/main/java" version
                       "false" "true"))
             (let ((file "maven-plugin-api/src/main/mdo/lifecycle.mdo"))
               (modello-single-mode file "1.0.0" "java")
               (modello-single-mode file "1.0.0" "xpp3-reader")
               (modello-single-mode file "1.0.0" "xpp3-writer"))
             #t))
         (replace 'install
           (install-from-pom "maven-plugin-api/pom.xml")))))
    (propagated-inputs
     `(("maven-artifact" ,maven-artifact)
       ("maven-model" ,maven-model)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("guice" ,java-guice)
       ("java-cglib" ,java-cglib)
       ("sisu-inject" ,java-eclipse-sisu-inject)
       ("javax-inject" ,java-javax-inject)
       ("utils" ,java-plexus-utils)))
    (native-inputs
     `(("modello" ,java-modello-core)
       ;; for modello:
       ("classworlds" ,java-plexus-classworlds)
       ("guava" ,java-guava)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("build-api" ,java-plexus-build-api)
       ;; modello plugins:
       ("java" ,java-modello-plugins-java)
       ("xml" ,java-modello-plugins-xml)
       ("xpp3" ,java-modello-plugins-xpp3)
       ;; for tests
       ("java-junit" ,java-junit)))
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains strictly the API for plugins -- composed of goals
implemented by Mojos -- development.

A plugin is described in a @file{META-INF/maven/plugin.xml} plugin descriptor,
generally generated from plugin sources using maven-plugin-plugin.")))

(define-public maven-core-bootstrap
  (hidden-package
    (package
      (inherit maven-artifact)
      (name "maven-core")
      (arguments
       `(#:jar-name "maven-core.jar"
         #:source-dir "src/main/java"
         #:jdk ,icedtea-8
         ;; Tests need maven-compat, which requires maven-core
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'chdir
             (lambda _
               ;; Required for generating components.xml in maven-core
               (chdir "maven-core")
               #t))
           (add-before 'build 'copy-resources
             (lambda _
               (mkdir-p "build/classes/")
               (copy-recursively "src/main/resources" "build/classes")
               #t))
           (add-after 'copy-resources 'fill-properties
             (lambda _
               ;; This file controls the output of some mvn subcommands, such as
               ;; mvn -version.
               (substitute* "build/classes/org/apache/maven/messages/build.properties"
                 (("\\$\\{buildNumber\\}") "guix_build")
                 (("\\$\\{timestamp\\}") "0")
                 (("\\$\\{project.version\\}") ,(package-version maven-artifact))
                 (("\\$\\{distributionId\\}") "apache-maven")
                 (("\\$\\{distributionShortName\\}") "Maven")
                 (("\\$\\{distributionName\\}") "Apache Maven"))
               #t))
           (add-before 'build 'generate-sisu-named
             (lambda _
               (mkdir-p "build/classes/META-INF/sisu")
               (chmod "../sisu.sh" #o755)
               (invoke "../sisu.sh" "src/main/java"
                       "build/classes/META-INF/sisu/javax.inject.Named")
               #t))
           (add-before 'build 'generate-models
             (lambda* (#:key inputs #:allow-other-keys)
               (define (modello-single-mode file version mode)
                 (invoke "java" "org.codehaus.modello.ModelloCli"
                         file mode "src/main/java" version
                         "false" "true"))
               (let ((file "src/main/mdo/toolchains.mdo"))
                 (modello-single-mode file "1.1.0" "java")
                 (modello-single-mode file "1.1.0" "xpp3-reader")
                 (modello-single-mode file "1.1.0" "xpp3-writer"))
               #t))
           (add-before 'install 'fix-pom
             (lambda _
               (substitute* "pom.xml"
                 (("<classifier>no_aop</classifier>") ""))
               #t))
           (replace 'install
             (install-from-pom "pom.xml")))))
      (propagated-inputs
       `(("maven-artifact" ,maven-artifact)
         ("maven-resolver-provider" ,maven-resolver-provider)
         ("maven-builder-support" ,maven-builder-support)
         ("maven-model" ,maven-model)
         ("maven-model-builder" ,maven-model-builder)
         ("maven-settings" ,maven-settings)
         ("maven-settings-builder" ,maven-settings-builder)
         ("maven-plugin-api" ,maven-plugin-api)
         ("maven-repository-metadata" ,maven-repository-metadata)
         ("maven-shared-utils" ,maven-shared-utils)
         ("java-plexus-component-annotations" ,java-plexus-component-annotations-1.7)
         ("java-plexus-utils" ,java-plexus-utils)
         ("java-commons-lang3" ,java-commons-lang3)
         ("java-guava" ,java-guava)
         ("java-guice" ,java-guice)
         ("maven-resolver-api" ,maven-resolver-api)
         ("maven-resolver-spi" ,maven-resolver-spi)
         ("maven-resolver-util" ,maven-resolver-util)
         ("maven-resolver-impl" ,maven-resolver-impl)
         ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
         ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
         ("java-javax-inject" ,java-javax-inject)
         ("java-plexus-classworld" ,java-plexus-classworlds)))
      (native-inputs
       `(("java-modello-core" ,java-modello-core)
         ("java-cglib" ,java-cglib)
         ("java-plexus-classworlds" ,java-plexus-classworlds)
         ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
         ("java-plexus-build-api" ,java-plexus-build-api)
         ("java-modello-plugins-java" ,java-modello-plugins-java)
         ("java-modello-plugins-xml" ,java-modello-plugins-xml)
         ("java-modello-plugins-xpp3" ,java-modello-plugins-xpp3)
         ;; tests
         ("java-junit" ,java-junit)
         ("java-mockito-1" ,java-mockito-1)
         ("java-commons-jxpath" ,java-commons-jxpath)))
      (description "Apache Maven is a software project management and comprehension
tool.  This package contains the maven core classes managing the whole build
process."))))

(define-public maven-core
  (package
    (inherit maven-core-bootstrap)
    (arguments
      (substitute-keyword-arguments (package-arguments maven-core-bootstrap)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-before 'build 'modify-metainf
              (lambda _
                (substitute* "build.xml"
                  (("message=\"\"")
                   (string-append "message=\"Implementation-Version: "
                                  (package-version maven) "\n\"")))
                #t))
            (add-before 'build 'add-maven-files
              (lambda _
                (mkdir-p "build/classes/META-INF/maven/org.apache.maven/maven-core")
                (copy-file "pom.xml"
                           "build/classes/META-INF/maven/org.apache.maven/maven-core/pom.xml")
                (with-output-to-file "build/classes/META-INF/maven/org.apache.maven/maven-core/pom.properties"
                  (lambda _
                    (format #t "version=~a~%
groupId=org.apache.maven~%
artifactId=maven-core" ,(package-version maven-core-bootstrap))))
                #t))
            (add-after 'build 'generate-metadata
              (lambda _
                (define (components file)
                  (let ((sxml (with-input-from-file file
                                (lambda _ (xml->sxml (current-input-port)
                                                     #:trim-whitespace? #t)))))
                    ;; Select the list of <component>s inside the <component-set>
                    ;; and <components>.
                    ((@ (ice-9 match) match) sxml
                     (('*TOP*
                       ('*PI* foo ...)
                       ('component-set
                        ('components x ...))) x))))
                (use-modules (sxml simple))
                (delete-file "build/classes/META-INF/plexus/components.xml")
                (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                        "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                        "--source" "build/classes/META-INF/plexus"
                        "--output" "build/classes/META-INF/plexus/components.t.xml"
                        "--classes" "build/classes"
                        "--descriptors" "build/classes")
                ;; Now we merge all other components from hand-written xml
                (let ((generated-xml (components "build/classes/META-INF/plexus/components.t.xml"))
                      (components-xml (components "src/main/resources/META-INF/plexus/components.xml"))
                      (default-bindings-xml (components "src/main/resources/META-INF/plexus/default-bindings.xml"))
                      (artifact-handlers-xml (components "src/main/resources/META-INF/plexus/artifact-handlers.xml")))
                  (with-output-to-file "build/classes/META-INF/plexus/components.xml"
                    (lambda _
                      (display "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
                      (sxml->xml
                        `(component-set
                           (components
                             ,@(append generated-xml components-xml
                                       default-bindings-xml
                                       artifact-handlers-xml)))))))
                #t))
            (add-after 'generate-metadata 'fix-plugin-versions
              (lambda _
                ;; This file controls the default plugins used by Maven.  Ensure
                ;; we use the versions we have packaged by default
                (substitute* '("build/classes/META-INF/plexus/default-bindings.xml"
                               "build/classes/META-INF/plexus/components.xml")
                  (("maven-install-plugin:[0-9.]+")
                   (string-append "maven-install-plugin:"
                                  ,(package-version maven-install-plugin)))
                  (("maven-resources-plugin:[0-9.]+")
                   (string-append "maven-resources-plugin:"
                                  ,(package-version maven-resources-plugin)))
                  (("maven-compiler-plugin:[0-9.]+")
                   (string-append "maven-compiler-plugin:"
                                  ,(package-version maven-compiler-plugin)))
                  ;; Surefire version includes milestone suffix like -M8
                  (("maven-surefire-plugin:[^:]+")
                   (string-append "maven-surefire-plugin:"
                                  ,(package-version maven-surefire-plugin)))
                  (("maven-jar-plugin:[0-9.]+")
                   (string-append "maven-jar-plugin:"
                                  ,(package-version maven-jar-plugin))))))
            (add-after 'fix-plugin-versions 'rebuild
              (lambda _
                (invoke "ant" "jar")
                #t))))))
    (native-inputs
     `(("java-plexus-component-metadata" ,java-plexus-component-metadata-1.7)
       ("java-commons-cli" ,java-commons-cli)
       ("java-plexus-cli" ,java-plexus-cli)
       ("java-jdom2" ,java-jdom2)
       ("java-qdox" ,java-qdox)
       ("maven-core-boot" ,maven-core-bootstrap)
       ,@(package-native-inputs maven-core-bootstrap)))
    ;; Add plexus-archiver to propagated-inputs so it ends up in plexus.core
    ;; ClassRealm.  Without this, Sisu cannot discover DefaultArchiverManager
    ;; (which uses @Named) because it only scans plexus.core, not plugin realms.
    (propagated-inputs
     (modify-inputs (package-propagated-inputs maven-core-bootstrap)
       (append java-plexus-archiver)))))

(define-public maven-slf4j-provider
  (package
    (inherit maven-artifact)
    (name "maven-slf4j-provider")
    (arguments
     `(#:jar-name "maven-slf4j-provider.jar"
       #:source-dir "maven-slf4j-provider/src/main/java"
       #:tests? #f ;no tests
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'unpack-slf4j
                    (lambda* (#:key inputs #:allow-other-keys)
                      (mkdir-p "generated-sources")
                      (copy-recursively (assoc-ref inputs
                                                   "java-slf4j-simple-source")
                                        "generated-sources")
                      (with-directory-excursion "generated-sources"
                        (for-each delete-file
                                  (find-files "." "StaticLoggerBinder.java")))
                      (for-each (lambda (simple)
                                  (for-each (lambda (java)
                                              (copy-file java
                                                         (string-append
                                                          "maven-slf4j-provider/src/main/java/org/slf4j/impl/"
                                                          (basename java))))
                                            (find-files (string-append simple
                                                         "/src/main/java/")
                                                        "\\.java$")))
                                (find-files "generated-sources" "slf4j-simple"
                                            #:directories? #t))))
                  (replace 'install
                    (install-from-pom "maven-slf4j-provider/pom.xml")))))
    (inputs `(("java-slf4j-api" ,java-slf4j-api)
              ("java-slf4j-simple-source" ,(package-source java-slf4j-simple))
              ("maven-shared-utils" ,maven-shared-utils)))
    (native-inputs (list unzip))))

(define-public maven-embedder
  (package
    (inherit maven-artifact)
    (name "maven-embedder")
    (arguments
     `(#:jar-name "maven-embedder.jar"
       #:source-dir "maven-embedder/src/main/java"
       #:test-dir "maven-embedder/src/test"
       #:test-exclude (list "**/MavenCliTest.java")
       #:tests? #f; require junit 4.13
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sisu-named
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (chmod "sisu.sh" #o755)
             (invoke "./sisu.sh" "maven-embedder/src/main/java"
                     "build/classes/META-INF/sisu/javax.inject.Named")
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes/")
             (copy-recursively "maven-embedder/src/main/resources" "build/classes")))
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java" "org.codehaus.modello.ModelloCli"
                       file mode "maven-embedder/src/main/java" version
                       "false" "true"))
             (let ((file "maven-embedder/src/main/mdo/core-extensions.mdo"))
               (modello-single-mode file "1.1.0" "java")
               (modello-single-mode file "1.1.0" "xpp3-reader")
               (modello-single-mode file "1.1.0" "xpp3-writer"))
             #t))
         (add-before 'check 'fix-test-paths
           (lambda _
             (substitute* "maven-embedder/src/test/java/org/apache/maven/cli/CLIManagerDocumentationTest.java"
               (("target/test-classes") "build/test-classes"))
             #t))
         (add-before 'check 'fix-test-compilation
           (lambda _
             ;; Tests are in the java/ subdir. Other subdirectories contain
             ;; additional test plugins, with duplicate classes, so we can't
             ;; compile them. Also, they are meant to be built with maven, to
             ;; test its build process.
             (substitute* "build.xml"
               (("srcdir=\"maven-embedder/src/test\"")
                "srcdir=\"maven-embedder/src/test/java\""))
             #t))
         (add-before 'check 'disable-failing-test
           (lambda _
             (delete-file "maven-embedder/src/test/java/org/apache/maven/cli/event/ExecutionEventLoggerTest.java")))
         (add-before 'install 'fix-pom
           (lambda _
             (substitute* "maven-embedder/pom.xml"
               (("jsr250-api") "javax.annotation-api"))))
         (replace 'install
           (install-from-pom "maven-embedder/pom.xml")))))
    (propagated-inputs
     `(("maven-core" ,maven-core)
       ("maven-artifact" ,maven-artifact)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-builder-support" ,maven-builder-support)
       ("maven-model" ,maven-model)
       ("maven-model-builder" ,maven-model-builder)
       ("maven-settings" ,maven-settings)
       ("maven-settings-builder" ,maven-settings-builder)
       ("maven-shared-utils" ,maven-shared-utils)
       ("maven-slf4j-provider" ,maven-slf4j-provider)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-plexus-util" ,java-plexus-utils)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-cipher" ,java-plexus-cipher)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-sec-dispatcher" ,java-plexus-sec-dispatcher)
       ("maven-resolevr-util" ,maven-resolver-util)
       ("maven-resolevr-api" ,maven-resolver-api)
       ("java-logback-core" ,java-logback-core)
       ("java-logback-classic" ,java-logback-classic)
       ("java-commons-cli" ,java-commons-cli)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang3" ,java-commons-lang3)
       ("java-guava" ,java-guava)
       ("java-guice" ,java-guice)
       ("java-javax-inject" ,java-javax-inject)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-jsr250" ,java-jsr250)))
    (native-inputs
     `(("java-asm-8" ,java-asm-8)
       ("java-modello-core" ,java-modello-core)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-plexus-build-api" ,java-plexus-build-api)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-cglib" ,java-cglib)
       ("java-modello-plugins-java" ,java-modello-plugins-java)
       ("java-modello-plugins-xml" ,java-modello-plugins-xml)
       ("java-modello-plugins-xpp3" ,java-modello-plugins-xpp3)
       ;; tests
       ("java-junit" ,java-junit)
       ("java-objenesis" ,java-objenesis)
       ("java-mockito-1" ,java-mockito-1)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains a Maven embeddable component, with CLI and
logging support.")))

(define-public maven-compat
  (package
    (inherit maven-artifact)
    (name "maven-compat")
    (arguments
     `(#:jar-name "maven-compat.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         ;; Tests assume we're in this directory
         (add-before 'configure 'chdir
           (lambda _
             (chdir "maven-compat")
             #t))
         (add-before 'build 'recreate-removed-jar
           (lambda _
             (with-output-to-file "src/test/repository-system/maven-core-2.1.0.jar"
               (const #t))
             (with-directory-excursion "src/test/resources"
               (with-output-to-file "artifact-install/artifact-1.0.jar"
                 (lambda _
                   (format #t "dummy~%")))
               (for-each
                 (lambda (file)
                   (with-output-to-file file
                     (lambda _
                       (format #t "foo~%"))))
                 '("local-repo/maven-test/jars/maven-test-a-1.0.jar"
                   "local-repo/maven-test/jars/maven-test-c-1.0.jar"
                   "local-repo/maven-test/jars/maven-test-d-1.0.jar"
                   "inheritance-repo/t04/maven-test/jars/t04-a-1.0.jar"
                   "inheritance-repo/t04/maven-test/jars/t04-b-1.0.jar"
                   "inheritance-repo/t04/maven-test/jars/t04-b-2.0.jar"
                   "inheritance-repo/t04/maven-test/jars/t04-c-1.0.jar"
                   "inheritance-repo/t04/maven-test/jars/t04-c-2.0.jar"
                   "inheritance-repo/t05/maven-test/jars/t05-a-1.0.jar"
                   "inheritance-repo/t05/maven-test/jars/t05-a-2.0.jar"
                   "inheritance-repo/t05/maven-test/jars/t05-b-1.0.jar"
                   "inheritance-repo/t05/maven-test/jars/t05-b-1.1.jar"
                   "inheritance-repo/t05/maven-test/jars/t05-b-2.0.jar"
                   "inheritance-repo/t05/maven-test/jars/t05-c-1.0.jar"
                   "inheritance-repo/t05/maven-test/jars/t05-d-1.0.jar"
                   "inheritance-repo/t05/maven-test/jars/t05-d-1.1.jar"
                   "inheritance-repo/t05/maven-test/jars/t05-d-1.2.jar"
                   "inheritance-repo/t06/maven-test/jars/t06-a-1.0.jar"
                   "inheritance-repo/t06/maven-test/jars/t06-b-1.0.jar"
                   "inheritance-repo/t06/maven-test/jars/t06-b-1.1.jar"
                   "inheritance-repo/t06/maven-test/jars/t06-c-1.0.jar"
                   "inheritance-repo/t06/maven-test/jars/t06-d-1.0.jar"
                   "inheritance-repo/t06/maven-test/jars/t06-d-1.1.jar"
                   "inheritance-repo/t06/maven-test/jars/t06-d-1.2.jar"
                   "inheritance-repo/t07/maven-test/jars/t07-a-1.0.jar"
                   "inheritance-repo/t07/maven-test/jars/t07-b-1.0.jar"
                   "inheritance-repo/t07/maven-test/jars/t07-b-1.1.jar"
                   "inheritance-repo/t07/maven-test/jars/t07-c-1.0.jar"
                   "inheritance-repo/t07/maven-test/jars/t07-d-1.0.jar"
                   "inheritance-repo/t07/maven-test/jars/t07-d-1.1.jar"
                   "inheritance-repo/t07/maven-test/jars/t07-d-1.2.jar"
                   "inheritance-repo/t08/maven-test/jars/t08-a-1.0.jar"
                   "inheritance-repo/t08/maven-test/jars/t08-b-1.0.jar"
                   "inheritance-repo/t08/maven-test/jars/t08-b-1.1.jar"
                   "inheritance-repo/t08/maven-test/jars/t08-c-1.0.jar"
                   "inheritance-repo/t08/maven-test/jars/t08-d-1.0.jar"
                   "inheritance-repo/t08/maven-test/jars/t08-d-1.1.jar"
                   "inheritance-repo/t08/maven-test/jars/t08-d-1.2.jar"
                   "inheritance-repo/t09/maven-test/jars/t09-a-1.0.jar"
                   "inheritance-repo/t09/maven-test/jars/t09-b-1.0.jar"
                   "inheritance-repo/t09/maven-test/jars/t09-c-1.0.jar"
                   "inheritance-repo/t09/maven-test/jars/t09-d-1.0.jar"
                   "inheritance-repo/t10/maven-test/jars/t10-a-1.0.jar"
                   "inheritance-repo/t10/maven-test/jars/t10-b-1.0.jar"
                   "inheritance-repo/t10/maven-test/jars/t10-c-1.0.jar"))
               (with-directory-excursion "local-repo/snapshot-test/jars"
                 (for-each
                   (lambda (file)
                     (with-output-to-file file
                       (lambda _
                         ;; No end-of-line
                         (format #t "local"))))
                   '("maven-snapshot-e-1.0-SNAPSHOT.jar"
                     "maven-snapshot-b-1.0-SNAPSHOT.jar"
                     "maven-snapshot-a-1.0-SNAPSHOT.jar"))))
             (for-each
               (lambda (letter)
                 (with-directory-excursion
                   (string-append "src/test/remote-repo/org/apache/maven/its/"
                                  letter "/0.1")
                   (let ((dir (string-append "META-INF/maven/org.apache.maven.its/"
                                             letter)))
                     (mkdir-p dir)
                     (copy-file (string-append letter "-0.1.pom")
                                (string-append dir "/pom.xml"))
                     (with-output-to-file (string-append dir "/pom.properties")
                       (lambda _
                         (format #t "version=0.1~%")
                         (format #t "groupId=org.apache.maven.its")
                         (format #t (string-append "artifactId=" letter))))
                     (with-output-to-file "META-INF/MANIFEST.MF"
                       (lambda _
                         (format #t "Manifest-Version: 1.0~%"))))
                     (invoke "jar" "cmf" "META-INF/MANIFEST.MF"
                             (string-append letter "-0.1.jar") "META-INF")))
               '("a" "b"))
             #t))
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java" "org.codehaus.modello.ModelloCli"
                       file mode "src/main/java" version
                       "false" "true"))
             (let ((file "src/main/mdo/profiles.mdo"))
               (modello-single-mode file "1.0.0" "java")
               (modello-single-mode file "1.0.0" "xpp3-reader")
               (modello-single-mode file "1.0.0" "xpp3-writer"))
             (let ((file "src/main/mdo/paramdoc.mdo"))
               (modello-single-mode file "1.0.0" "java")
               (modello-single-mode file "1.0.0" "xpp3-reader")
               (modello-single-mode file "1.0.0" "xpp3-writer"))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes/")
             (copy-recursively "src/main/resources" "build/classes")))
         (add-after 'build 'generate-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "src/main/java"
                     "--output" "build/classes/META-INF/plexus/components.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes/META-INF")
             #t))
         (add-before 'check 'build-tests
          (lambda _
            (invoke "ant" "compile-tests")
            #t))
         (add-after 'build-tests 'generate-test-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH")
                                                 ":build/classes"
                                                 ":build/test-classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "src/test/java"
                     "--output" "build/test-classes/META-INF/plexus/components.xml"
                     "--classes" "build/test-classes"
                     "--descriptors" "build/test-classes/META-INF")
             #t))
         (add-before 'check 'disable-failing-test
           (lambda _
             (delete-file "src/test/java/org/apache/maven/profiles/manager/DefaultProfileManagerTest.java")))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list maven-artifact
           maven-repository-metadata
           maven-builder-support
           maven-model
           maven-model-builder
           maven-settings
           maven-settings-builder
           maven-core
           maven-wagon-provider-api
           maven-wagon-file
           maven-resolver-api
           maven-resolver-util
           maven-resolver-spi
           java-plexus-interpolation))
    (native-inputs
     `(("java-modello-core" ,java-modello-core)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations-1.7)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-plexus-build-api" ,java-plexus-build-api)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-exclispe-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-javax-inject" ,java-javax-inject)
       ("java-guice" ,java-guice)
       ("java-guava" ,java-guava)
       ("java-cglib" ,java-cglib)
       ("java-modello-plugins-java" ,java-modello-plugins-java)
       ("java-modello-plugins-xml" ,java-modello-plugins-xml)
       ("java-modello-plugins-xpp3" ,java-modello-plugins-xpp3)
       ;; metadata
       ("java-plexus-component-metadata" ,java-plexus-component-metadata-1.7)
       ("java-commons-cli" ,java-commons-cli)
       ("java-plexus-cli" ,java-plexus-cli)
       ("java-jdom2" ,java-jdom2)
       ("maven-plugin-api" ,maven-plugin-api)
       ("java-qdox" ,java-qdox)
       ;; tests
       ("java-plexus-cipher" ,java-plexus-cipher)
       ("java-plexus-sec-dispatcher" ,java-plexus-sec-dispatcher)
       ("java-jsr250" ,java-jsr250)
       ("java-cdi-api" ,java-cdi-api)
       ("java-junit" ,java-junit)
       ("java-mockito-1" ,java-mockito-1)
       ("java-objenesis" ,java-objenesis)
       ("maven-resolver-impl" ,maven-resolver-impl)
       ("maven-resolver-connector-basic" ,maven-resolver-connector-basic)
       ("maven-resolver-transport-wagon" ,maven-resolver-transport-wagon)
       ("java-commons-lang3" ,java-commons-lang3)
       ("java-aop" ,java-aopalliance)
       ("maven-resolver-provider" ,maven-resolver-provider)
       ("maven-slf4j-provider" ,maven-slf4j-provider)
       ("java-slf4j-api" ,java-slf4j-api)
       ,@(package-inputs java-slf4j-api)))
    (description "Apache Maven is a software project management and comprehension
tool.  This package contains Maven2 classes maintained as compatibility
layer for plugins that need to keep Maven2 compatibility.")))

(define-public maven
  (package
    (inherit maven-artifact)
    (name "maven")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Recreate the configuration for the loader
             (with-output-to-file "apache-maven/src/bin/m2.conf"
               (lambda _
                 (format #t "main is org.apache.maven.cli.MavenCli from plexus.core~%")
                 (format #t "~%")
                 (format #t "set maven.conf default ${maven.home}/conf~%")
                 (format #t "~%")
                 (format #t "[plexus.core]~%")
                 (format #t "load       ${maven.conf}/logging~%")
                 (format #t "optionally ${maven.home}/lib/ext/*.jar~%")
                 ;; Reference every jar so plexus-classworlds can find them.
                 (for-each
                   (lambda (dependency)
                     (for-each
                       (lambda (file)
                         (format #t "load       ~a~%" file))
                       (find-files (assoc-ref inputs dependency) ".*.jar$")))
                   '("maven-artifact" "maven-embedder" "maven-core" "maven-compat"
                     "maven-builder-support" "maven-model" "maven-model-builder"
                     "maven-settings" "maven-settings-builder" "maven-plugin-api"
                     "maven-repository-metadata" "maven-shared-utils" "maven-resolver-api"
                     "maven-resolver-spi" "maven-resolver-util" "maven-resolver-impl"
                     "maven-resolver-connector-basic" "maven-resolver-named-locks"
                     "maven-resolver-provider"
                     "maven-resolver-transport-wagon" "maven-slf4j-provider"
                     "maven-wagon-provider-api"
                     "maven-wagon-file" "maven-wagon-http" "java-commons-logging-minimal"
                     "java-httpcomponents-httpclient" "java-httpcomponents-httpcore"
                     "maven-wagon-http-shared" "maven-wagon-tck-http"
                     "java-eclipse-sisu-plexus" "java-guice" "java-aopalliance"
                     "java-cglib" "java-asm" "java-eclipse-sisu-inject"
                     "java-javax-inject" "java-plexus-component-annotations"
                     "java-plexus-utils" "java-plexus-interpolation"
                     "java-plexus-sec-dispatcher" "java-plexus-cipher" "java-guava"
                     "java-guava-futures-failureaccess" "java-jansi"
                     "java-jsr250" "java-cdi-api" "java-commons-cli"
                     "java-commons-io" "java-commons-lang3" "java-slf4j-api"
                     ;; plexus-archiver is needed for sisu to discover DefaultArchiverManager
                     ;; which is required by maven-plugin-plugin for the descriptor goal.
                     "java-plexus-archiver" "java-plexus-io" "java-commons-compress"))))
             (substitute* "apache-maven/src/bin/mvn"
               (("cygwin=false;")
                (string-append
                  "CLASSPATH="
                  ;; plexus-classworlds is the launcher
                  (car (find-files
                         (assoc-ref inputs "java-plexus-classworlds")
                         ".*.jar"))
                  ;; plexus-archiver and dependencies must be on the Java classpath
                  ;; (not just in m2.conf) because Sisu discovers @Named components
                  ;; by scanning the classloader hierarchy.  Plugin realms have
                  ;; AppClassLoader as parent (not plexus.core), so components in
                  ;; plexus.core are invisible to Sisu unless also on the classpath.
                  ":"
                  (car (find-files
                         (assoc-ref inputs "java-plexus-archiver")
                         "plexus-archiver.*\\.jar$"))
                  ":"
                  (car (find-files
                         (assoc-ref inputs "java-plexus-io")
                         "plexus-io.*\\.jar$"))
                  ":"
                  (car (find-files
                         (assoc-ref inputs "java-commons-compress")
                         "commons-compress.*\\.jar$"))
                  "\ncygwin=false;"))
               (("-classpath.*") "-classpath ${CLASSPATH} \\\n"))
             #t))
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/"))
                   (conf (string-append (assoc-ref outputs "out") "/conf/")))
               (mkdir-p (string-append (assoc-ref outputs "out") "/lib"))
               (for-each (lambda (file)
                           (install-file (string-append "apache-maven/src/bin/" file)
                                         bin)
                           (chmod (string-append bin file) #o755))
                '("mvn" "mvnDebug" "mvnyjp"))
               (install-file "apache-maven/src/bin/m2.conf" bin)
               (copy-recursively "apache-maven/src/conf" conf))
             #t)))))
    (inputs
     (list java-plexus-classworlds
           maven-artifact
           maven-embedder
           maven-core
           maven-compat
           maven-builder-support
           maven-model
           maven-model-builder
           maven-settings
           maven-settings-builder
           maven-plugin-api
           maven-repository-metadata
           maven-shared-utils
           maven-resolver-api
           maven-resolver-spi
           maven-resolver-util
           maven-resolver-named-locks
           maven-resolver-impl
           maven-resolver-connector-basic
           maven-resolver-provider
           maven-resolver-transport-wagon
           maven-slf4j-provider
           maven-wagon-provider-api
           maven-wagon-file
           maven-wagon-http
           java-commons-logging-minimal
           java-httpcomponents-httpclient
           java-httpcomponents-httpcore
           maven-wagon-http-shared
           maven-wagon-tck-http
           java-eclipse-sisu-plexus-0.9
           java-guice-5
           java-aopalliance
           java-cglib
           java-asm-9
           java-eclipse-sisu-inject-0.9
           java-javax-inject
           java-plexus-component-annotations
           java-plexus-utils
           java-plexus-interpolation
           java-plexus-sec-dispatcher
           java-plexus-cipher
           java-guava
           java-guava-futures-failureaccess
           java-jansi
           java-jsr250
           java-cdi-api
           java-commons-cli
           java-commons-io
           java-commons-lang3
           java-slf4j-api
           ;; plexus-archiver is needed for sisu to discover DefaultArchiverManager
           ;; which is required by maven-plugin-plugin for the descriptor goal.
           java-plexus-archiver
           java-plexus-io
           java-commons-compress))
    (propagated-inputs
     (list coreutils which))
    (description "Apache Maven is a software project management and comprehension
tool.  Based on the concept of a project object model: builds, dependency
management, documentation creation, site publication, and distribution
publication are all controlled from the @file{pom.xml} declarative file.  Maven
can be extended by plugins to utilise a number of other development tools for
reporting or the build process.")))

;; Many plugins require maven 3.0 as a dependency.
(define maven-3.0-pom
  (package
    (inherit maven-pom)
    (version "3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/apache/maven")
                     (commit (string-append "maven-" version))))
              (file-name (git-file-name "maven" version))
              (sha256
               (base32
                "06jdwxx9w24shhv3kca80rlrikynn7kdqcrwg59lv2b7adpllwnh"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file (find-files "." "\\.jar$"))
                  (for-each (lambda (file) (chmod file #o644))
                            (find-files "." "."))
                  #t))
              (patches
                (search-patches "maven-generate-component-xml.patch"
                                "maven-generate-javax-inject-named.patch"))))
    (propagated-inputs
     `(("maven-parent-pom-15" ,maven-parent-pom-15)))))

(define-public maven-3.0-artifact
  (package
    (inherit maven-artifact)
    (version (package-version maven-3.0-pom))
    (source (package-source maven-3.0-pom))
    (propagated-inputs
      (map
        (lambda (input)
          (if (equal? (car input) "maven-pom")
              `("maven-pom" ,maven-3.0-pom)
              input))
        (package-propagated-inputs maven-artifact)))))

(define-public maven-3.0-model
  (package
    (inherit maven-model)
    (version (package-version maven-3.0-pom))
    (source (package-source maven-3.0-pom))
    (propagated-inputs
      (map
        (lambda (input)
          (if (equal? (car input) "maven-pom")
              `("maven-pom" ,maven-3.0-pom)
              input))
        (package-propagated-inputs maven-artifact)))
    (arguments
     `(#:jar-name "maven-model.jar"
       #:source-dir "maven-model/src/main/java"
       #:test-dir "maven-model/src/test"
       #:modules
       ((guix build ant-build-system)
        (guix build java-utils)
        (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'use-newer-model
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The model has almost not changed, but the newer version is
             ;; needed to prevent an error in the newer modello we have
             (let ((source (assoc-ref inputs "maven-source"))
                   (dir (mkdtemp "maven-source-XXXXXXXX")))
               (with-directory-excursion dir
                 (invoke "tar" "xf" source)
                 (copy-file (car (find-files "." "maven.mdo"))
                            "../maven-model/src/main/mdo/maven.mdo")))
             #t))
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java" "org.codehaus.modello.ModelloCli"
                       file mode "maven-model/src/main/java" version
                       "false" "true" "UTF-8"))
             (let ((file "maven-model/src/main/mdo/maven.mdo"))
               (modello-single-mode file "4.0.0" "java")
               (modello-single-mode file "4.0.0" "xpp3-reader")
               (modello-single-mode file "4.0.0" "xpp3-writer")
               (modello-single-mode file "4.0.0" "xpp3-extended-reader"))
             #t))
         (replace 'install
           (install-from-pom "maven-model/pom.xml")))))
    (inputs
      `(("maven-source" ,(package-source maven-pom))
        ,@(package-inputs maven-model)))))

(define-public maven-3.0-settings
  (package
    (inherit maven-settings)
    (version (package-version maven-3.0-pom))
    (source (package-source maven-3.0-pom))
    (propagated-inputs
      (map
        (lambda (input)
          (if (equal? (car input) "maven-pom")
              `("maven-pom" ,maven-3.0-pom)
              input))
        (package-propagated-inputs maven-settings)))))

(define-public maven-3.0-settings-builder
  (package
    (inherit maven-settings-builder)
    (version (package-version maven-3.0-pom))
    (source (package-source maven-3.0-pom))
    (arguments
      (substitute-keyword-arguments (package-arguments maven-settings-builder)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-before 'build 'generate-components.xml
              (lambda _
                (mkdir-p "build/classes/META-INF/plexus")
                (chmod "components.sh" #o755)
                (invoke "./components.sh" "maven-settings-builder/src/main/java"
                        "build/classes/META-INF/plexus/components.xml")))))))
    (propagated-inputs
     `(("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ,@(filter
           (lambda (a) a)
           (map
             (lambda (input)
               (match (car input)
                 ("maven-pom" `("maven-pom" ,maven-3.0-pom))
                 ("maven-settings" `("maven-settings" ,maven-3.0-settings))
                 ("maven-builder-support" #f)
                 ("java-plexus-sec-dispatcher"
                  `("java-plexus-sec-dispatcher" ,java-plexus-sec-dispatcher-1.4))
                 (_ input)))
             (package-propagated-inputs maven-settings-builder)))))))

(define-public maven-3.0-model-builder
  (package
    (inherit maven-model-builder)
    (version (package-version maven-3.0-pom))
    (source (package-source maven-3.0-pom))
    (arguments
      (substitute-keyword-arguments (package-arguments maven-model-builder)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-before 'build 'generate-components.xml
              (lambda _
                (mkdir-p "build/classes/META-INF/plexus")
                (chmod "../components.sh" #o755)
                (invoke "../components.sh" "src/main/java"
                        "build/classes/META-INF/plexus/components.xml")))
            (add-before 'check 'remove-failing-test
              (lambda _
                (delete-file "src/test/java/org/apache/maven/model/interpolation/StringSearchModelInterpolatorTest.java")))))))
    (propagated-inputs
     `(("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ,@(filter
           (lambda (a) a)
           (map
             (lambda (input)
               (match (car input)
                 ("maven-pom" `("maven-pom" ,maven-3.0-pom))
                 ("maven-model" `("maven-model" ,maven-3.0-model))
                 ("maven-artifact" `("maven-artifact" ,maven-3.0-artifact))
                 ("maven-builder-support" #f)
                 (_ input)))
             (package-propagated-inputs maven-model-builder)))))))

(define-public maven-3.0-plugin-api
  (package
    (inherit maven-plugin-api)
    (version (package-version maven-3.0-pom))
    (source (package-source maven-3.0-pom))
    (arguments
      (substitute-keyword-arguments (package-arguments maven-plugin-api)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-before 'install 'fix-pom
              (lambda _
                (substitute* "maven-plugin-api/pom.xml"
                  (("org.sonatype.sisu") "org.codehaus.plexus")
                  (("sisu-inject-plexus") "plexus-container-default"))
                #t))))))
    (propagated-inputs
      (map
        (lambda (input)
          (match (car input)
            ("maven-pom" `("maven-pom" ,maven-3.0-pom))
            ("maven-artifact" `("maven-artifact" ,maven-3.0-artifact))
            ("maven-model" `("maven-model" ,maven-3.0-model))
            (_ input)))
        (package-propagated-inputs maven-model-builder)))
    (native-inputs
     (modify-inputs (package-native-inputs maven-plugin-api)
       (prepend java-plexus-container-default)))))

(define-public maven-3.0-repository-metadata
  (package
    (inherit maven-repository-metadata)
    (version (package-version maven-3.0-pom))
    (source (package-source maven-3.0-pom))
    (propagated-inputs
      (map
        (lambda (input)
          (if (equal? (car input) "maven-pom")
              `("maven-pom" ,maven-3.0-pom)
              input))
        (package-propagated-inputs maven-repository-metadata)))))

(define-public maven-3.0-aether-provider
  (package
    (inherit maven-3.0-pom)
    (name "maven-aether-provider")
    (arguments
     `(#:jar-name "maven-aether-provider.jar"
       #:source-dir "maven-aether-provider/src/main/java"
       #:tests? #f; no tests in 3.0
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'generate-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "src/main/java"
                     "--output" "build/classes/META-INF/plexus/components.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes/META-INF")
             #t))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t))
         (replace 'install
           (install-from-pom "maven-aether-provider/pom.xml")))))
    (propagated-inputs
     `(("maven-model" ,maven-3.0-model)
       ("maven-model-builder" ,maven-3.0-model-builder)
       ("maven-repository-metadata" ,maven-3.0-repository-metadata)
       ("java-sonatype-aether-api" ,java-sonatype-aether-api)
       ("java-sonatype-aether-spi" ,java-sonatype-aether-spi)
       ("java-sonatype-aether-impl" ,java-sonatype-aether-impl)
       ("java-plexus-component-annotation" ,java-plexus-component-annotations)
       ("java-plexus-utils" ,java-plexus-utils)
       ("maven-pom" ,maven-3.0-pom)))
    (native-inputs
     (list java-plexus-component-metadata))))

(define-public maven-3.0-core
  (package
    (inherit maven-core)
    (version (package-version maven-3.0-pom))
    (source (package-source maven-3.0-pom))
    (arguments
     `(#:jar-name "maven-core.jar"
       #:source-dir "src/main/java"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             ;; Required for generating components.xml in maven-core
             (chdir "maven-core")
             #t))
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java" "org.codehaus.modello.ModelloCli"
                       file mode "src/main/java" version
                       "false" "true" "UTF-8"))
             (let ((file "src/main/mdo/toolchains.mdo"))
               (modello-single-mode file "1.0.0" "java")
               (modello-single-mode file "1.0.0" "xpp3-reader")
               (modello-single-mode file "1.0.0" "xpp3-writer"))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes/")
             (copy-recursively "src/main/resources" "build/classes")
             #t))
         (add-after 'build 'generate-metadata
           (lambda _
             (define (components file)
               (let ((sxml (with-input-from-file file
                             (lambda _ (xml->sxml (current-input-port)
                                                  #:trim-whitespace? #t)))))
                 ;; Select the list of <component>s inside the <component-set>
                 ;; and <components>.
                 ((@ (ice-9 match) match) sxml
                  (('*TOP*
                    ('*PI* foo ...)
                    ('component-set
                     ('components x ...))) x))))
             (use-modules (sxml simple))
             (delete-file "build/classes/META-INF/plexus/components.xml")
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "build/classes/META-INF/plexus"
                     "--output" "build/classes/META-INF/plexus/components.t.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes")
             ;; Now we merge all other components from hand-written xml
             (let ((generated-xml (components "build/classes/META-INF/plexus/components.t.xml"))
                   (components-xml (components "src/main/resources/META-INF/plexus/components.xml"))
                   (artifact-handlers-xml (components "src/main/resources/META-INF/plexus/artifact-handlers.xml")))
               (with-output-to-file "build/classes/META-INF/plexus/components.xml"
                 (lambda _
                   (display "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
                   (sxml->xml
                     `(component-set
                        (components
                          ,@(append generated-xml components-xml
                                    artifact-handlers-xml)))))))
             #t))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t))
         (add-before 'install 'fix-pom
           (lambda _
             (substitute* "pom.xml"
               (("org.sonatype.sisu") "org.codehaus.plexus")
               (("sisu-inject-plexus") "plexus-container-default"))
             #t))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list maven-3.0-model
           maven-3.0-settings
           maven-3.0-settings-builder
           maven-3.0-repository-metadata
           maven-3.0-artifact
           maven-3.0-model-builder
           maven-3.0-aether-provider
           java-sonatype-aether-impl
           java-sonatype-aether-api
           java-sonatype-aether-util
           java-plexus-interpolation
           java-plexus-utils
           java-plexus-classworlds
           java-plexus-component-annotations
           java-plexus-container-default
           java-plexus-sec-dispatcher-1.4
           maven-3.0-pom))))

(define-public maven-3.0-compat
  (package
    (inherit maven-compat)
    (version (package-version maven-3.0-pom))
    (source (package-source maven-3.0-pom))
    (arguments
     `(#:tests? #f ;require an old version of java-easymock
       ,@(substitute-keyword-arguments (package-arguments maven-compat)
          ((#:phases phases)
           `(modify-phases ,phases
              (add-before 'install 'fix-pom
                (lambda _
                  (substitute* "pom.xml"
                    (("org.sonatype.sisu") "org.codehaus.plexus")
                    (("sisu-inject-plexus") "plexus-container-default"))
                  #t))
              (delete 'build-tests))))))
    (propagated-inputs
     (list maven-3.0-model
           maven-3.0-model-builder
           maven-3.0-settings
           maven-3.0-settings-builder
           maven-3.0-artifact
           maven-3.0-core
           maven-3.0-aether-provider
           maven-3.0-repository-metadata
           java-sonatype-aether-api
           java-sonatype-aether-util
           java-sonatype-aether-impl
           java-plexus-utils
           java-plexus-interpolation
           java-eclipse-sisu-plexus
           java-plexus-component-annotations
           java-plexus-container-default
           maven-wagon-provider-api
           maven-3.0-pom))))

(define-public maven-shared-utils-3.0
  (package
    (inherit maven-shared-utils)
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/shared/"
                                  "maven-shared-utils-" version "-source-release.zip"))
              (sha256
               (base32
                "0qm8y85kip2hyhnhlkqgj0rhmf83z07s7l7gzsfl5dzl3kvp8nal"))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs maven-shared-utils)
       (prepend maven-3.0-core maven-components-parent-pom-21)))))

(define-public maven-shared-utils-3.1
  (package
    (inherit maven-shared-utils)
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/shared/"
                                  "maven-shared-utils-" version "-source-release.zip"))
              (sha256
               (base32
                "0vfaas4g09ch0agrd1dcxcmhdd3w971ssvfr9mx9gi2lp5nv8w66"))))))

(define-public maven-shared-io
  (package
    (name "maven-shared-io")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/shared/"
                                  "maven-shared-io-" version
                                  "-source-release.zip"))
              (sha256
               (base32
                "0hsyll8gg581802xhs4achdz8fpmfz7y02abx9s4mb8bc6yfh229"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-shared-io.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes/")
             (copy-recursively "src/test/resources" "build/test-classes/")
             #t))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list maven-3.0-artifact
           maven-3.0-compat
           maven-3.0-plugin-api
           maven-shared-utils
           maven-wagon-provider-api
           java-plexus-utils
           maven-components-parent-pom-22))
    (native-inputs
     (list unzip java-junit java-easymock))
    (home-page "https://maven.apache.org/shared/maven-dependency-tree")
    (synopsis "Tree-based API for resolution of Maven project dependencies")
    (description "This package provides a tree-based API for resolution of
Maven project dependencies.")
    (license license:asl2.0)))

(define-public maven-file-management
  (package
    (name "maven-file-management")
    (version "3.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/apache/maven-file-management")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kq3dgyflfacymb7szfhz33jq34css81k3yiz5kciaxhpljzfl1x"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-file-management.jar"
       #:source-dir "src/main/java"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes/")
             #t))
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java"
                       "org.codehaus.modello.ModelloCli"
                       file mode "src/main/java" version
                       "false" "true"))
             (let ((file "src/main/mdo/fileset.mdo"))
               (modello-single-mode file "1.1.0" "java")
               (modello-single-mode file "1.1.0" "xpp3-reader")
               (modello-single-mode file "1.1.0" "xpp3-writer"))
             #t))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list java-commons-io
           java-plexus-utils
           java-slf4j-api))
    (native-inputs
     `(("java-modello-core" ,java-modello-core)
       ;; modello plugins:
       ("java-modellop-plugins-java" ,java-modello-plugins-java)
       ("java-modellop-plugins-xpp3" ,java-modello-plugins-xpp3)))
    (home-page "https://maven.apache.org/shared/file-management")
    (synopsis "API to collect files from a project")
    (description "This package provides an API to collect files from a project
for creating archives.")
    (license license:asl2.0)))

(define-public maven-archiver
  (package
    (name "maven-archiver")
    (version "3.6.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/apache/maven-archiver")
                    (commit (string-append "maven-archiver-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0kbibjspmc31wdl2pajpq06xvr7f81lzgrzvn5dwxfb67yzg8c5s"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-archiver.jar"
       #:source-dir "src/main/java"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list java-commons-io
           maven-artifact
           maven-core
           maven-model
           maven-shared-utils
           java-plexus-archiver
           java-plexus-interpolation
           java-plexus-utils
           java-plexus-xml))
    (home-page "https://maven.apache.org/shared/maven-archiver")
    (synopsis "Handle manifest and archive creation in Maven plugins")
    (description "This package provides classes for creating archives in
Maven plugins.")
    (license license:asl2.0)))

(define-public maven-dependency-tree
  (package
    (name "maven-dependency-tree")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/shared/"
                                  "maven-dependency-tree-" version
                                  "-source-release.zip"))
              (sha256
               (base32
                "1vhcd3lmbyy8q61c37sqgbllqj4ypkxm344l6pb05mkchlyk5dy5"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-dependency-tree.jar"
       #:source-dir "src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list maven-3.0-core java-plexus-component-annotations
           maven-parent-pom-34))
    (inputs
     (list java-sonatype-aether-api-1.13 java-sonatype-aether-util-1.13
           java-eclipse-aether-api java-eclipse-aether-util))
    (native-inputs
     (list unzip java-junit))
    (home-page "https://maven.apache.org/shared/maven-dependency-tree")
    (synopsis "Tree-based API for resolution of Maven project dependencies")
    (description "This package provides a tree-based API for resolution of
Maven project dependencies.")
    (license license:asl2.0)))

(define-public maven-common-artifact-filters
  (package
    (name "maven-common-artifact-filters")
    (version "3.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/shared/"
                                  "maven-common-artifact-filters-" version
                                  "-source-release.zip"))
              (sha256
               (base32
                "0ab7c82pc1cyh575jfywn372zy8a8kfblm217rzqdmqjhswlf1mp"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-common-artifact-filters.jar"
       #:source-dir "src/main/java"
       #:tests? #f; require maven-plugin-testing-harness, which requires maven 3.2.
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list maven-artifact
           maven-model
           maven-core
           maven-plugin-api
           maven-shared-utils
           maven-parent-pom-41
           java-eclipse-sisu-plexus
           java-eclipse-aether-api
           java-eclipse-aether-util))
    (inputs
     (list maven-resolver-api maven-resolver-util))
    (native-inputs
     (list unzip))
   (home-page "https://maven.apache.org/shared/maven-dependency-tree")
    (synopsis "Tree-based API for resolution of Maven project dependencies")
    (description "This package provides a tree-based API for resolution of
Maven project dependencies.")
    (license license:asl2.0)))

(define-public maven-common-artifact-filters-3.1.0
  (package
    (inherit maven-common-artifact-filters)
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/shared/"
                                  "maven-common-artifact-filters-" version
                                  "-source-release.zip"))
              (sha256
               (base32
                "1cl1qk4r0gp62bjzfm7lml9raz1my2kd4yf0ci0lnfsn0h5qivnb"))))
    (arguments
      (substitute-keyword-arguments (package-arguments maven-common-artifact-filters)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'build 'remove-sisu
             (lambda _
               (substitute* "pom.xml"
                 (("sisu-inject-plexus") "maven-plugin-api")
                 (("org.sonatype.sisu") "org.apache.maven"))))))))
    ;; 3.1.0 uses Sonatype aether, not Eclipse aether
    ;; Use maven-3.0 components to avoid conflicts with packages using maven-3.0-core
    (propagated-inputs
     (list maven-3.0-artifact
           maven-3.0-model
           maven-3.0-core
           maven-3.0-plugin-api
           maven-shared-utils-3.0
           maven-components-parent-pom-21
           java-sonatype-aether-api
           java-sonatype-aether-util))))

(define-public maven-enforcer-api
  (package
    (name "maven-enforcer-api")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/enforcer/"
                                  "enforcer-" version "-source-release.zip"))
              (sha256
               (base32
                "1479yp58jv788xc1jc2sbdxpajlbvwlk60639vd2h4s8r6x7naqh"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-enforcer-api.jar"
       #:source-dir "enforcer-api/src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "enforcer-api/pom.xml")))))
    (propagated-inputs
     (list maven-plugin-api java-plexus-container-default java-jsr305
           maven-enforcer-parent-pom))
    (native-inputs
     (list unzip))
    (home-page "https://maven.apache.org/shared/maven-dependency-tree")
    (synopsis "Tree-based API for resolution of Maven project dependencies")
    (description "This package provides a tree-based API for resolution of
Maven project dependencies.")
    (license license:asl2.0)))

(define maven-enforcer-parent-pom
  (package
    (inherit maven-enforcer-api)
    (name "maven-enforcer-parent-pom")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (add-before 'install 'fix-pom-versions
           (lambda _
             (substitute* "pom.xml"
               (("<maven.version>.*</maven.version>")
                ,(string-append "<maven.version>" (package-version maven)
                                "</maven.version>"))
               (("2.11.0") ,(package-version java-commons-io))
               (("3.12.0") ,(package-version java-commons-lang3))
               (("1.6.1") ,(package-version maven-resolver-util))
               (("1.15") ,(package-version java-commons-codec)))))
         (replace 'install
           (install-pom-file "pom.xml")))))
    (propagated-inputs
     `(("maven-parent-pom" ,maven-parent-pom-30)))))

(define-public maven-enforcer-rules
  (package
    (inherit maven-enforcer-api)
    (name "maven-enforcer-rules")
    (arguments
     `(#:tests? #f; requires maven-plugin-testing-harness
       #:jar-name "maven-enforcer-rules.jar"
       #:source-dir "enforcer-rules/src/main/java"
       #:test-dir "enforcer-rules/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "enforcer-rules/pom.xml")))))
    (propagated-inputs
     (list maven-artifact
           maven-plugin-api
           maven-core
           maven-common-artifact-filters
           java-commons-codec
           java-commons-lang3
           maven-enforcer-api
           maven-resolver-util
           java-bsh
           maven-dependency-tree
           maven-3.0-compat
           maven-enforcer-parent-pom))))

(define-public maven-enforcer-plugin
  (package
    (inherit maven-enforcer-api)
    (name "maven-enforcer-plugin")
    (arguments
     `(#:tests? #f
       #:jar-name "maven-enforcer-plugin.jar"
       #:source-dir "maven-enforcer-plugin/src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-plugin.xml
           (generate-plugin.xml "maven-enforcer-plugin/pom.xml"
             "enforcer"
             "maven-enforcer-plugin/src/main/java/org/apache/maven/plugins/enforcer/"
             (list
               (list "DisplayInfoMojo.java")
               (list "EnforceMojo.java"))))
         (replace 'install
           (install-from-pom "maven-enforcer-plugin/pom.xml")))))
    (propagated-inputs
     (list maven-artifact
           maven-plugin-api
           maven-core
           java-plexus-utils
           maven-enforcer-api
           maven-enforcer-rules
           maven-plugin-annotations
           maven-enforcer-parent-pom))))

(define-public maven-sisu-plugin
  (package
    (name "maven-sisu-plugin")
    (version "0.3.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/eclipse/sisu.mojos/")
                    (commit (string-append "releases/" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00hb7v6hz8jg0mgkj3cl0nmqz49za4k2a0rbjr4gdhy2m7f34sq3"))))
    (build-system ant-build-system)
    (arguments
     (list #:jar-name "maven-sisu-plugin.jar"
           #:source-dir "src"
           #:tests? #f ;no tests
           #:phases #~(modify-phases %standard-phases
                        (add-before 'build 'generate-plugin.xml
                          (generate-plugin.xml "pom.xml" "sisu"
                           "src/main/java/org/eclipse/sisu/mojos/"
                           (list (list "IndexMojo.java")
                                 (list "MainIndexMojo.java")
                                 (list "TestIndexMojo.java"))))
                        (replace 'install
                          (install-from-pom "pom.xml")))))
    (propagated-inputs (list java-sonatype-oss-parent-pom-9))
    (inputs (list maven-artifact
                  maven-plugin-api
                  maven-plugin-annotations
                  maven-core
                  maven-common-artifact-filters
                  java-slf4j-nop
                  java-eclipse-sisu-inject
                  java-plexus-utils
                  java-plexus-build-api
                  java-slf4j-api))
    (home-page "https://www.eclipse.org/sisu/")
    (synopsis "Maven plugin that generates annotation indexes for Sisu")
    (description "Maven plugin that generates annotation indexes for Sisu to
avoid classpath scanning at runtime.")
    (license license:epl1.0)))

(define-public maven-artifact-transfer
  (package
    (name "maven-artifact-transfer")
    (version "0.13.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/shared/"
                                  "maven-artifact-transfer-" version
                                  "-source-release.zip"))
              (sha256
               (base32
                "0xl7lkksljacrhmvwf924zb6h0h5zw9494jaz9cz4hll0lrhlpz6"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; require mockito 2
       #:jar-name "maven-artifact-transfer.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'generate-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "src/main/java"
                     "--output" "build/classes/META-INF/plexus/components.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes/META-INF")
             #t))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list java-commons-codec
           maven-3.0-artifact
           maven-3.0-core
           maven-common-artifact-filters-3.1.0
           java-plexus-component-annotations
           java-plexus-utils
           java-slf4j-api
           java-plexus-classworlds
           java-sonatype-aether-api
           java-eclipse-aether-api
           java-eclipse-aether-util
           java-eclipse-aether-impl))
    (native-inputs
     (list unzip java-plexus-component-metadata))
    (home-page "https://maven.apache.org/shared/maven-artifact-transfer")
    (synopsis "API to install, deploy and resolve artifacts in Maven")
    (description "This package contains an API to install, deploy and resolve
artifacts in Maven 3.")
    (license license:asl2.0)))

(define-public maven-install-plugin
  (package
    (name "maven-install-plugin")
    (version "3.0.0-M1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/plugins/"
                                  "maven-install-plugin-" version
                                  "-source-release.zip"))
              (sha256
               (base32
                "1l9iydxririrair0i5sk2iypn9wspzbb666lc0ddg20yyr8w39dm"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; require maven-plugin-testing-harness
       #:jar-name "maven-install-plugin.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-pom
           (lambda _
             (substitute* "pom.xml"
               (("maven-project") "maven-core")
               (("maven-artifact-manager") "maven-artifact")
               (("2.0.6") "3.0"))
             #t))
         (add-before 'build 'generate-plugin.xml
           (generate-plugin.xml "pom.xml"
             "install"
             "src/main/java/org/apache/maven/plugins/install"
             (list
               (list "AbstractInstallMojo.java" "InstallFileMojo.java")
               (list "AbstractInstallMojo.java" "InstallMojo.java"))))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     `(("maven-artifact" ,maven-artifact)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-compat" ,maven-compat)
       ("maven-artifact-transfer" ,maven-artifact-transfer)
       ("maven-plugins-pom-23" ,maven-plugins-pom-23)
       ("java-plexus-digest" ,java-plexus-digest)))
    (inputs
     (list maven-plugin-annotations java-slf4j-api))
    (native-inputs
     (list unzip))
    (home-page "https://maven.apache.org/plugin/maven-install-plugin")
    (synopsis "Maven's install plugin")
    (description "The Install Plugin is used during the install phase to add
artifact(s) to the local repository.  The Install Plugin uses the information
in the POM (groupId, artifactId, version) to determine the proper location for
the artifact within the local repository.

The local repository is the local cache where all artifacts needed for the
build are stored.  By default, it is located within the user's home directory
(@file{~/.m2/repository}) but the location can be configured in
@file{~/.m2/settings.xml} using the @code{<localRepository>} element.")
    (license license:asl2.0)))

(define-public maven-filtering
  (package
    (name "maven-filtering")
    (version "3.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/"
                                  "shared/maven-filtering-" version
                                  "-source-release.zip"))
              (sha256
               (base32
                "09wrdhchnszd2l6h4z30ra0bv1a19qyjgac9z8zf1pn0m4nw05yz"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-filtering.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       ;; this test comes from plexus-build-api, not this package
       #:test-exclude (list "**/IncrementalResourceFilteringTest.java"
                            "**/Abstract*.java")
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'generate-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "src/main/java"
                     "--output" "build/classes/META-INF/plexus/components.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes/META-INF")
             #t))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t))
         (add-before 'check 'decompress-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((build-api-source (assoc-ref inputs "java-plexus-build-api-origin"))
                    (classes (string-append build-api-source "/src/test/java")))
               (copy-recursively classes "src/test/"))
             #t))
         (add-before 'check 'fix-directory
           (lambda _
             (substitute* (find-files "src/test" ".*.java$")
               (("target/test-classes/") "build/test-classes/"))))
         (add-before 'check 'copy-test-resources
           (lambda _
             (copy-recursively "src/test/resources" "build/test-classes/")
             #t))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list maven-3.0-core
           maven-shared-utils
           java-plexus-utils-3.2.1
           java-plexus-interpolation
           java-plexus-build-api
           maven-parent-pom-30))
    (inputs
     (list java-jsr305))
    (native-inputs
     `(("unzip" ,unzip)
       ("java-assertj" ,java-assertj)
       ("java-junit" ,java-junit)
       ("java-mockito" ,java-mockito-1)
       ("java-objenesis" ,java-objenesis)
       ("java-plexus-component-metadata" ,java-plexus-component-metadata)
       ("java-plexus-build-api-origin" ,(package-source java-plexus-build-api))))
    (home-page "https://maven.apache.org/shared/maven-filtering")
    (synopsis "Shared component for all plugins that needs to filter resources")
    (description "This component provides an API to filter resources in Maven
projects.")
    (license license:asl2.0)))

(define-public maven-resources-plugin
  (package
    (name "maven-resources-plugin")
    (version "3.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/apache/maven-resources-plugin")
                     (commit (string-append  "maven-resources-plugin-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "090k5j2y7ak54czfjjg3v7pdmdlgd96fbs91d1fd3vslm9zzndg8"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-resources-plugin.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:tests? #f; test depends on maven-plugin-test-harness
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-plugin.xml
           (generate-plugin.xml "pom.xml" "resources"
             "src/main/java/org/apache/maven/plugins/resources"
             (list
               (list "ResourcesMojo.java" "CopyResourcesMojo.java")
               (list "ResourcesMojo.java")
               (list "ResourcesMojo.java" "TestResourcesMojo.java"))))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list maven-plugin-api
           maven-core
           java-plexus-utils
           maven-filtering
           java-plexus-interpolation
           maven-parent-pom-31))
    (inputs
     (list maven-plugin-annotations java-commons-io))
    (native-inputs
     (list java-plexus-component-metadata))
    (home-page "https://maven.apache.org/plugins/maven-resources-plugin")
    (synopsis "Maven plugin to collect and install resources")
    (description "The Resources Plugin handles the copying of project resources
to the output directory.  There are two different kinds of resources: main
resources and test resources.  The difference is that the main resources are
the resources associated to the main source code while the test resources are
associated to the test source code.

Thus, this allows the separation of resources for the main source code and its
unit tests.")
    (license license:asl2.0)))

(define-public maven-shared-incremental
  (package
    (name "maven-shared-incremental")
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/shared/"
                                  "maven-shared-incremental-" version
                                  "-source-release.zip"))
              (sha256
               (base32
                "03n4nfswyg9ahkz2zx4skcr3ghs01zh95g9js51hc75mfqx9b976"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "shared-incremental.java"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'change-parent
           (lambda _
             (substitute* "pom.xml"
               (("19") "30"))
             #t))
         (add-before 'build 'fix-pom
           (lambda _
             (substitute* "pom.xml"
               (("plexus-component-api") "plexus-component-annotations"))
             #t))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list maven-plugin-api maven-core maven-shared-utils
           java-plexus-component-annotations maven-parent-pom-30))
    (native-inputs
     (list unzip))
    (home-page "https://maven.apache.org/shared/maven-shared-incremental")
    (synopsis "Maven Incremental Build support utilities")
    (description "This package contains various utility classes and plexus
components for supporting incremental build functionality in maven plugins.")
    (license license:asl2.0)))

(define-public maven-compiler-plugin
  (package
    (name "maven-compiler-plugin")
    (version "3.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/apache/maven-compiler-plugin")
                     (commit (string-append "maven-compiler-plugin-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fq4m1rihbj0r2fs68n0544mv23pp2snpf1vips22n30xhi891d7"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-compiler-plugin.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:jdk ,openjdk11
       #:tests? #f; test depends on maven-plugin-test-harness
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-plugin.xml
           (generate-plugin.xml "pom.xml"
             "compiler"
             "src/main/java/org/apache/maven/plugin/compiler"
             (list
               (list "AbstractCompilerMojo.java" "CompilerMojo.java")
               (list "AbstractCompilerMojo.java" "TestCompilerMojo.java"))))
         (add-after 'generate-plugin.xml 'fix-plugin.xml
           (lambda _
             (substitute* "build/classes/META-INF/maven/plugin.xml"
               ;; These are defined in AbstractCompilerMojo.java, but not
               ;; parsed correctly in the previous phase
               (("DEFAULT_TARGET") "1.6")
               (("DEFAULT_SOURCE") "1.6"))
             #t))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list maven-plugin-api
           maven-artifact
           maven-core
           maven-shared-utils
           maven-shared-incremental
           java-plexus-java-1
           java-plexus-compiler-api
           java-plexus-compiler-manager
           java-plexus-compiler-javac
           maven-parent-pom-33))
    (inputs
     (list maven-plugin-annotations java-commons-io))
    (home-page "https://maven.apache.org/plugins/maven-compiler-plugin")
    (synopsis "Compiler plugin for Maven")
    (description "The Compiler Plugin is used to compile the sources of your
project.  Since 3.0, the default compiler is @code{javax.tools.JavaCompiler}
(if you are using java 1.6) and is used to compile Java sources.  If you want
to force the plugin using javac, you must configure the plugin option
@code{forceJavacCompilerUse}.

Also note that at present the default source setting is 1.6 and the default
target setting is 1.6, independently of the JDK you run Maven with.  You are
highly encouraged to change these defaults by setting source and target as
described in Setting the -source and -target of the Java Compiler.

Other compilers than javac can be used and work has already started on
AspectJ, .NET, and C#.")
    (license license:asl2.0)))

(define-public java-surefire-logger-api
  (package
    (name "java-surefire-logger-api")
    (version "3.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/maven/surefire/"
                                  "surefire-" version "-source-release.zip"))
              (sha256
               (base32
                "1ifw0ml0b1cycjpjsfyjy5420y4xgbssms72cvbpxny94dd7y8hs"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-surefire-logger-api.jar"
       #:source-dir "surefire-logger-api/src/main/java"
       #:tests? #f; require mockito 2
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "surefire-logger-api/pom.xml")))))
    (propagated-inputs
     (list java-surefire-parent-pom))
    (native-inputs
     (list unzip))
    (home-page "https://maven.apache.org/surefire/surefire-logger-api")
    (synopsis "Interfaces and Utilities related only to internal SureFire Logger API")
    (description "This package contains interfaces and utilities that are
internal to the SureFire Logger API.  It is designed to have no dependency.")
    (license license:asl2.0)))

;; JUnit 5 BOM (Bill of Materials) - a pom-only package for dependency management.
(define (make-junit-bom version hash)
  (package
    (name "junit-bom")
    (version version)
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://repo1.maven.org/maven2/org/junit/junit-bom/"
                    version "/junit-bom-" version ".pom"))
              (sha256 (base32 hash))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key source outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (pom-dir (string-append out "/lib/m2/org/junit/junit-bom/"
                                            ,version)))
               (mkdir-p pom-dir)
               (copy-file source (string-append pom-dir "/junit-bom-" ,version ".pom"))))))))
    (home-page "https://junit.org/junit5/")
    (synopsis "JUnit 5 Bill of Materials")
    (description "This package provides a Maven BOM (Bill of Materials) for
JUnit 5, used for dependency version management.")
    (license license:epl2.0)))

(define-public junit-bom
  (make-junit-bom "5.9.3" "1mrf6kf0y6bic98n30w8s9gnxykgzv5j8mx1bh745wkjkv6jj0sd"))

(define-public junit-bom-5.10
  (make-junit-bom "5.10.3" "1g5syqjzm1pigqzsr4ya85b198z1293x69cjffw4r615qi27v4qh"))

(define-public junit-bom-5.10.2
  (make-junit-bom "5.10.2" "0s4r7bjcs9072a9gm2d92hy9zzib55icqn76zw655xmhlh2dk78n"))

(define-public junit-bom-5.12
  (make-junit-bom "5.12.1" "1w1b3i8a4ci9qj4xyf3x6xdgb80jj1zh4qwjmmyq3p7d5g3np0kw"))

(define-public java-surefire-parent-pom
  (package
    (inherit java-surefire-logger-api)
    (name "java-surefire-parent-pom")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (add-before 'install 'fix-pom-dependency-versions
           (lambda _
             (substitute* "pom.xml"
               (("1.11") ,(package-version java-commons-codec)))
             (substitute* "pom.xml"
               (("mavenVersion>.*")
                (string-append
                  "mavenVersion>"
                  ,(package-version maven-pom)
                  "</mavenVersion>\n"))
               (("commonsLang3Version>.*")
                (string-append
                  "commonsLang3Version>"
                  ,(package-version java-commons-lang3)
                  "</commonsLang3Version>\n"))
               (("commonsCompress>.*")
                (string-append
                  "commonsCompress>"
                  ,(package-version java-commons-compress)
                  "</commonsCompress>\n"))
               (("commonsIoVersion>.*")
                (string-append
                  "commonsIoVersion>"
                  ,(package-version java-commons-io)
                  "</commonsIoVersion>\n"))
               (("0.11.0") ,(package-version maven-artifact-transfer))
               (("1.0.3") ,(package-version java-plexus-java))
               (("3.1.1") ,(package-version maven-common-artifact-filters)))
             #t))
         (add-after 'install 'install-providers
           (install-pom-file "surefire-providers/pom.xml"))
         (replace 'install
           (install-pom-file "pom.xml")))))
    (propagated-inputs
     (list maven-parent-pom-41 junit-bom)))) ; FIXME: wtf does junit-bom do here???

(define-public java-surefire-api
  (package
    (inherit java-surefire-logger-api)
    (name "java-surefire-api")
    (arguments
     `(#:tests? #f
       #:jar-name "java-surefire-api.jar"
       #:source-dir "surefire-api/src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes")
             (copy-recursively "surefire-api/src/main/resources" "build/classes")
             #t))
         (add-before 'build 'prepare-shade
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "build/classes")
             (mkdir-p "build/shaded-deps")
             (let ((jarjar (car (find-files (assoc-ref inputs "java-jarjar") ".*.jar$"))))
               ;; Shade maven-shared-utils
               (for-each
                 (lambda (jar-file)
                   (let ((out-jar "build/shaded-deps/shared-utils-shaded.jar"))
                     (with-output-to-file "build/rules-utils"
                       (lambda _
                         (format #t "rule org.apache.maven.shared.utils.** org.apache.maven.surefire.shared.utils.@1~%")))
                     (invoke "java" "-jar" jarjar "process" "build/rules-utils" jar-file out-jar)))
                 (find-files (assoc-ref inputs "maven-shared-utils") ".*.jar$"))
               ;; Shade commons-lang3
               (for-each
                 (lambda (jar-file)
                   (let ((out-jar "build/shaded-deps/lang3-shaded.jar"))
                     (with-output-to-file "build/rules-lang3"
                       (lambda _
                         (format #t "rule org.apache.commons.lang3.** org.apache.maven.surefire.shared.lang3.@1~%")))
                     (invoke "java" "-jar" jarjar "process" "build/rules-lang3" jar-file out-jar)))
                 (find-files (assoc-ref inputs "java-commons-lang3") ".*.jar$"))
               ;; Shade commons-codec
               (for-each
                 (lambda (jar-file)
                   (let ((out-jar "build/shaded-deps/codec-shaded.jar"))
                     (with-output-to-file "build/rules-codec"
                       (lambda _
                         (format #t "rule org.apache.commons.codec.** org.apache.maven.surefire.shared.codec.@1~%")))
                     (invoke "java" "-jar" jarjar "process" "build/rules-codec" jar-file out-jar)))
                 (find-files (assoc-ref inputs "java-commons-codec") ".*.jar$")))
             ;; Extract shaded jars
             (with-directory-excursion "build/classes"
               (for-each
                 (lambda (jar-file)
                   (invoke "jar" "xf" jar-file)
                   (when (file-exists? "META-INF")
                     (delete-file-recursively "META-INF")))
                 (find-files "../shaded-deps" ".*.jar$")))
             #t))
         (add-after 'build 'shade
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((jarjar
                   (car (find-files (assoc-ref inputs "java-jarjar") ".*.jar$")))
                   (injar "java-surefire-api.jar")
                   (outjar "java-surefire-api-shaded.jar"))
               (with-directory-excursion "build/jar"
                 (with-output-to-file "rules"
                   (lambda _
                     (format #t (string-append
                                  "rule "
                                  "org.apache.maven.shared.utils.** "
                                  "org.apache.maven.surefire.shared.utils.@1~%"))
                     (format #t (string-append
                                  "rule "
                                  "org.apache.commons.codec.** "
                                  "org.apache.maven.surefire.shared.codec.@1~%"))
                     (format #t (string-append
                                  "rule "
                                  "org.apache.commons.lang3.** "
                                  "org.apache.maven.surefire.shared.lang3.@1~%"))))
                 (invoke "java" "-jar" jarjar "process" "rules" injar outjar)
                 (delete-file injar)
                 (rename-file outjar injar)))
             #t))
         (add-before 'install 'remove-shared-utils-dep
           (lambda _
             (use-modules (guix build maven pom) (sxml simple) (srfi srfi-1))
             (define fix-maven-xml (@@ (guix build maven pom) fix-maven-xml))
             (define dep-tag (string->symbol "http://maven.apache.org/POM/4.0.0:dependency"))
             (define deps-tag (string->symbol "http://maven.apache.org/POM/4.0.0:dependencies"))
             (define artifactid-tag (string->symbol "http://maven.apache.org/POM/4.0.0:artifactId"))
             (define (is-shared-utils? dep)
               (and (pair? dep)
                    (eq? (car dep) dep-tag)
                    (any (lambda (part)
                           (and (pair? part)
                                (eq? (car part) artifactid-tag)
                                (member "surefire-shared-utils" part)))
                         (cdr dep))))
             (define (filter-deps sxml)
               (cond
                ((not (pair? sxml)) sxml)
                ((eq? (car sxml) deps-tag)
                 (cons deps-tag
                       (filter (lambda (d) (not (is-shared-utils? d)))
                               (cdr sxml))))
                (else
                 (cons (filter-deps (car sxml))
                       (filter-deps (cdr sxml))))))
             (let* ((pom-file "surefire-api/pom.xml")
                    (pom (get-pom pom-file))
                    (fixed (filter-deps pom)))
               (with-output-to-file pom-file
                 (lambda ()
                   (sxml->xml (fix-maven-xml fixed)))))
             #t))
         (replace 'install
           (install-from-pom "surefire-api/pom.xml")))))
    (propagated-inputs
     (list java-surefire-logger-api java-commons-codec java-commons-lang3
           java-surefire-parent-pom maven-shared-utils))
    (inputs
     (list java-jsr305))
    (native-inputs
     (list unzip java-jarjar))
    (synopsis "Maven SureFire API")
    (description "This package contains the API to use Maven SureFire.")))

;; Upstream uses maven-shade-plugin to do shading at build time, but Guix
;; builds are offline so Maven plugins cannot run.  We simulate the shading
;; using jarjar to relocate classes from the dependencies.
(define-public java-surefire-shared-utils
  (package
    (inherit java-surefire-logger-api)
    (name "java-surefire-shared-utils")
    (arguments
     `(#:jar-name "java-surefire-shared-utils.jar"
       #:source-dir "surefire-shared-utils/src/main/java"
       #:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'create-source-dir
           (lambda _
             ;; This module has no source - it's just shaded dependencies
             (mkdir-p "surefire-shared-utils/src/main/java")
             #t))
         (add-before 'build 'prepare-shade
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "build/classes")
             ;; Extract all jars to shade into build/classes
             (for-each
               (lambda (input-name pattern)
                 (let ((jar (car (find-files (assoc-ref inputs input-name) pattern))))
                   (with-directory-excursion "build/classes"
                     (invoke "jar" "xf" jar)
                     (when (file-exists? "META-INF")
                       (delete-file-recursively "META-INF")))))
               '("maven-shared-utils" "java-commons-io" "java-commons-lang3" "java-commons-compress")
               '("maven-shared-utils.*\\.jar$" "commons-io.*\\.jar$" "commons-lang3.*\\.jar$" "commons-compress.*\\.jar$"))
             #t))
         (add-after 'build 'shade
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((jarjar (car (find-files (assoc-ref inputs "java-jarjar") ".*.jar$")))
                   (injar "build/jar/java-surefire-shared-utils.jar")
                   (outjar "build/jar/java-surefire-shared-utils-shaded.jar"))
               (with-output-to-file "rules"
                 (lambda _
                   (format #t "rule org.apache.maven.shared.utils.** org.apache.maven.surefire.shared.utils.@1~%")
                   (format #t "rule org.apache.commons.io.** org.apache.maven.surefire.shared.io.@1~%")
                   (format #t "rule org.apache.commons.lang3.** org.apache.maven.surefire.shared.lang3.@1~%")
                   (format #t "rule org.apache.commons.compress.** org.apache.maven.surefire.shared.compress.@1~%")))
               (invoke "java" "-jar" jarjar "process" "rules" injar outjar)
               (delete-file injar)
               (rename-file outjar injar))
             #t))
         (replace 'install
           (install-from-pom "surefire-shared-utils/pom.xml")))))
    (propagated-inputs
     (list maven-shared-utils java-commons-io java-commons-lang3
           java-commons-compress java-surefire-parent-pom))
    (native-inputs
     (list unzip java-jarjar))
    (synopsis "Shaded utilities for Maven SureFire")
    (description "This package contains relocated Java packages of
maven-shared-utils and several Apache Commons utilities used by SureFire.")))

(define-public java-surefire-extensions-spi
  (package
    (inherit java-surefire-logger-api)
    (name "java-surefire-extensions-spi")
    (arguments
     `(#:tests? #f
       #:jar-name "java-surefire-extensions-spi.jar"
       #:source-dir "surefire-extensions-spi/src/main/java"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "surefire-extensions-spi/pom.xml")))))
    (propagated-inputs
     (list java-surefire-api java-surefire-parent-pom))
    (inputs
     (list java-jsr305))
    (synopsis "Maven SureFire Extensions SPI")
    (description "This package contains the Service Provider Interface for
Maven SureFire extensions.")))

(define-public java-surefire-booter
  (package
    (inherit java-surefire-logger-api)
    (name "java-surefire-booter")
    (arguments
     `(#:tests? #f ; requires mockito 2
       #:jar-name "java-surefire-booter.jar"
       #:source-dir "surefire-booter/src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-/bin/sh
           (lambda _
             (substitute* "surefire-booter/src/main/java/org/apache/maven/surefire/booter/PpidChecker.java"
               (("/bin/sh") (which "sh")))
             #t))
         (add-before 'build 'add-service-provider-config
           (lambda _
             ;; The ServiceLoader config file must be included in the jar for
             ;; ForkedBooter to find MasterProcessChannelProcessorFactory implementations.
             (mkdir-p "build/classes/META-INF/services")
             (copy-file "surefire-booter/src/main/resources/META-INF/services/org.apache.maven.surefire.spi.MasterProcessChannelProcessorFactory"
                        "build/classes/META-INF/services/org.apache.maven.surefire.spi.MasterProcessChannelProcessorFactory")))
         (replace 'install
           (install-from-pom "surefire-booter/pom.xml")))))
    (propagated-inputs
     (list java-surefire-api java-surefire-extensions-api
           java-surefire-extensions-spi java-surefire-shared-utils
           java-surefire-parent-pom))
    (inputs
     (list java-jsr305))
    (native-inputs
     (list unzip))
    (synopsis "API and Facilities used by forked tests running in JVM sub-process")
    (description "SureFire runs tests inside a forked JVM subprocess.  This
package contains an API and facilities used inside that forked JVM.")))

(define-public java-surefire-extensions-api
  (package
    (inherit java-surefire-logger-api)
    (name "java-surefire-extensions-api")
    (arguments
     `(#:tests? #f ; requires mockito 2
       #:jar-name "java-surefire-extensions-api.jar"
       #:source-dir "surefire-extensions-api/src/main/java"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "surefire-extensions-api/pom.xml")))))
    (propagated-inputs
     (list java-surefire-api java-surefire-shared-utils java-surefire-parent-pom))
    (inputs
     (list java-plexus-component-annotations))
    (synopsis "Extension API for Maven SureFire")
    (description "Surefire is a test framework project.  This is the aggregator
POM in Apache Maven Surefire project.")))

(define-public java-surefire-common-java5
  (package
    (inherit java-surefire-logger-api)
    (name "java-surefire-common-java5")
    (arguments
     `(#:jar-name "java-surefire-common-java5.jar"
       #:source-dir "surefire-providers/common-java5/src/main/java"
       #:test-dir "surefire-providers/common-java5/src/test"
       #:test-exclude (list
                        ;; Abstract class
                        "**/PojoStackTraceWriterTest.java"
                        ;; Fails
                        "**/SmartStackTraceParserTest.java")
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "surefire-providers/common-java5/pom.xml")))))
    (propagated-inputs
     (list java-surefire-shared-utils java-surefire-api
           java-surefire-parent-pom))
    (native-inputs
     (list unzip java-junit java-assertj))
    (synopsis "Common java5 facilities for Maven SureFire")
    (description "This package contains shared Java 5 code for all providers.")))

(define-public java-surefire-common-junit3
  (package
    (inherit java-surefire-logger-api)
    (name "java-surefire-common-junit3")
    (arguments
     `(#:jar-name "java-surefire-common-junit3.jar"
       #:source-dir "surefire-providers/common-junit3/src/main/java"
       #:test-dir "surefire-providers/common-junit3/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "surefire-providers/common-junit3/pom.xml")))))
    (propagated-inputs
     (list java-junit java-surefire-api java-surefire-parent-pom))
    (native-inputs
     (list unzip java-junit java-fest-assert))
    (synopsis "Shared JUnit3 provider code for Maven SureFire")
    (description "This package contains shared code for all JUnit providers.")))

(define-public java-surefire-common-junit4
  (package
    (inherit java-surefire-logger-api)
    (name "java-surefire-common-junit4")
    (arguments
     `(#:jar-name "java-surefire-common-junit4.jar"
       #:source-dir "surefire-providers/common-junit4/src/main/java"
       #:tests? #f; tests require junit 4.0
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "surefire-providers/common-junit4/pom.xml")))))
    (propagated-inputs
     (list java-junit
           java-surefire-api
           java-surefire-common-java5
           java-surefire-common-junit3
           maven-shared-utils-3.1
           java-surefire-parent-pom))
    (synopsis "Shared JUnit4 provider code for Maven SureFire")
    (description "This package contains shared code for all JUnit providers,
starting from JUnit 4.")))

(define-public java-surefire-junit4
  (package
    (inherit java-surefire-logger-api)
    (name "java-surefire-junit4")
    (arguments
     `(;#:tests? #f
       #:jar-name "java-surefire-junit4.jar"
       #:source-dir "surefire-providers/surefire-junit4/src/main/java"
       #:test-dir "surefire-providers/surefire-junit4/src/test"
       #:modules ((guix build ant-build-system)
                  (guix build utils)
                  (guix build java-utils)
                  (sxml simple))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'regenerate-own-pom
           ;; Surefire struggles resolving artifacts because of this pom
           ;; file, resulting in a NullPointerException when collecting
           ;; Artifacts (and a "Failure detected." message from
           ;; DefaultArtifactResolver).  Replace the pom file with a much
           ;; simpler one.  Everything is shaded anyway (as used to be the
           ;; case in 2.22), so there will not be missing dependencies.
           (generate-pom.xml
             "surefire-providers/surefire-junit4/pom.xml"
             "org.apache.maven.surefire" "surefire-junit4"
             ,(package-version java-surefire-common-java5)
             #:name "Surefire JUnit4"))
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes")
             (copy-recursively "surefire-providers/surefire-junit4/src/main/resources"
                               "build/classes")
             #t))
         (add-before 'build 'prepare-shade
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "build/classes")
             (with-directory-excursion "build/classes"
               (for-each
                 (lambda (input)
                   (for-each
                     (lambda (jar-file)
                       (invoke "jar" "xf" jar-file)
                       (delete-file-recursively "META-INF"))
                     (find-files (assoc-ref inputs input) ".*.jar$")))
                 '("maven-shared-utils" "java-surefire-common-java5"
                   "java-surefire-common-junit3" "java-surefire-common-junit4"
                   "java-surefire-api")))
             #t))
         (add-after 'build 'shade
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((jarjar
                   (car (find-files (assoc-ref inputs "java-jarjar") ".*.jar$")))
                   (injar "java-surefire-junit4.jar")
                   (outjar "java-surefire-junit4-shaded.jar"))
               (with-directory-excursion "build/jar"
                 (with-output-to-file "rules"
                   (lambda _
                     (format #t (string-append
                                  "rule "
                                  "org.apache.maven.shared.utils.** "
                                  "org.apache.maven.surefire.shade."
                                  "org.apache.maven.shared.utils.@1~%"))))
                 (invoke "java" "-jar" jarjar "process" "rules" injar outjar)
                 (delete-file injar)
                 (rename-file outjar injar)))
             #t))
         (replace 'install
           (install-from-pom "surefire-providers/surefire-junit4/pom.xml")))))
    (propagated-inputs
     (list java-junit java-surefire-parent-pom))
    (inputs
     (list java-surefire-common-junit4 java-surefire-common-junit3
           java-surefire-common-java5 java-surefire-api))
    (native-inputs
     (list java-jarjar unzip java-junit java-hamcrest-all
           java-fest-assert))
    (synopsis "SureFire JUnit 4.0+ runner")
    (description "This package contains the runner for tests run on a forked
JVM, using JUnit 4.0 or later.")))

(define-public maven-surefire-common
  (package
    (inherit java-surefire-logger-api)
    (name "maven-surefire-common")
    (arguments
     `(#:tests? #f; require mockito 2
       #:jar-name "maven-surefire-common.jar"
       #:source-dir "maven-surefire-common/src/main/java"
       #:jdk ,openjdk11
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'prepare-shade
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Shade dependencies BEFORE compilation so that the source code
             ;; can import from org.apache.maven.surefire.shared.*
             (mkdir-p "build/classes")
             (mkdir-p "build/shaded-deps")
             (let ((jarjar (car (find-files (assoc-ref inputs "java-jarjar")
                                            ".*.jar$"))))
               ;; Shade maven-shared-utils
               (for-each
                (lambda (jar-file)
                  (let ((out-jar "build/shaded-deps/shared-utils-shaded.jar"))
                    (with-output-to-file "build/rules-utils"
                      (lambda _
                        (format #t "rule org.apache.maven.shared.utils.** org.apache.maven.surefire.shared.utils.@1~%")))
                    (invoke "java" "-jar" jarjar "process" "build/rules-utils"
                            jar-file out-jar)))
                (find-files (assoc-ref inputs "maven-shared-utils") ".*.jar$"))
               ;; Shade commons-io
               (for-each
                (lambda (jar-file)
                  (let ((out-jar "build/shaded-deps/io-shaded.jar"))
                    (with-output-to-file "build/rules-io"
                      (lambda _
                        (format #t "rule org.apache.commons.io.** org.apache.maven.surefire.shared.io.@1~%")))
                    (invoke "java" "-jar" jarjar "process" "build/rules-io"
                            jar-file out-jar)))
                (find-files (assoc-ref inputs "java-commons-io") ".*.jar$"))
               ;; Shade commons-lang3
               (for-each
                (lambda (jar-file)
                  (let ((out-jar "build/shaded-deps/lang3-shaded.jar"))
                    (with-output-to-file "build/rules-lang3"
                      (lambda _
                        (format #t "rule org.apache.commons.lang3.** org.apache.maven.surefire.shared.lang3.@1~%")))
                    (invoke "java" "-jar" jarjar "process" "build/rules-lang3"
                            jar-file out-jar)))
                (find-files (assoc-ref inputs "java-commons-lang3") ".*.jar$"))
               ;; Shade commons-compress
               (for-each
                (lambda (jar-file)
                  (let ((out-jar "build/shaded-deps/compress-shaded.jar"))
                    (with-output-to-file "build/rules-compress"
                      (lambda _
                        (format #t "rule org.apache.commons.compress.** org.apache.maven.surefire.shared.compress.@1~%")))
                    (invoke "java" "-jar" jarjar "process" "build/rules-compress"
                            jar-file out-jar)))
                (find-files (assoc-ref inputs "java-commons-compress") ".*.jar$")))
             ;; Extract shaded jars and non-shaded artifacts-filter
             (with-directory-excursion "build/classes"
               (for-each
                (lambda (jar-file)
                  (invoke "jar" "xf" jar-file)
                  (when (file-exists? "META-INF")
                    (delete-file-recursively "META-INF")))
                (find-files "../shaded-deps" ".*.jar$"))
               ;; Also extract maven-common-artifact-filters (not shaded)
               (for-each
                (lambda (jar-file)
                  (invoke "jar" "xf" jar-file)
                  (when (file-exists? "META-INF")
                    (delete-file-recursively "META-INF")))
                (find-files (assoc-ref inputs "maven-common-artifact-filters")
                            ".*.jar$")))
             #t))
         (add-after 'build 'generate-plexus-components
           (lambda _
             ;; Create Plexus components.xml for @Component annotated classes.
             ;; We do this manually because PlexusMetadataGeneratorCli has issues
             ;; with our isolated classpath setup.
             (mkdir-p "build/classes/META-INF/plexus")
             (with-output-to-file "build/classes/META-INF/plexus/components.xml"
               (lambda ()
                 (display "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<component-set>
  <components>
    <component>
      <role>org.apache.maven.surefire.providerapi.ProviderDetector</role>
      <implementation>org.apache.maven.surefire.providerapi.ProviderDetector</implementation>
      <requirements>
        <requirement>
          <role>org.apache.maven.surefire.providerapi.ServiceLoader</role>
          <field-name>serviceLoader</field-name>
        </requirement>
      </requirements>
    </component>
    <component>
      <role>org.apache.maven.surefire.providerapi.ServiceLoader</role>
      <implementation>org.apache.maven.surefire.providerapi.ServiceLoader</implementation>
    </component>
  </components>
</component-set>
")))
             ;; Create Sisu index for @Named components
             (mkdir-p "build/classes/META-INF/sisu")
             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda ()
                 (display "org.apache.maven.plugin.surefire.SurefireDependencyResolver\n")))
             (invoke "ant" "jar")))
         (add-after 'generate-plexus-components 'shade
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Shade the output jar as well for consistency
             (let ((jarjar (car (find-files (assoc-ref inputs "java-jarjar")
                                            ".*.jar$")))
                   (injar "maven-surefire-common.jar")
                   (outjar "maven-surefire-common-shaded.jar"))
               (with-directory-excursion "build/jar"
                 (with-output-to-file "rules"
                   (lambda _
                     (format #t "rule org.apache.maven.shared.utils.** org.apache.maven.surefire.shared.utils.@1~%")
                     (format #t "rule org.apache.commons.io.** org.apache.maven.surefire.shared.io.@1~%")
                     (format #t "rule org.apache.commons.lang3.** org.apache.maven.surefire.shared.lang3.@1~%")
                     (format #t "rule org.apache.commons.compress.** org.apache.maven.surefire.shared.compress.@1~%")))
                 (invoke "java" "-jar" jarjar "process" "rules" injar outjar)
                 (delete-file injar)
                 (rename-file outjar injar)))
             #t))
         (add-before 'install 'fix-pom
           (lambda _
             (substitute* "maven-surefire-common/pom.xml"
               (("maven-toolchain") "maven-core"))
             #t))
         (add-before 'install 'remove-shared-utils-dep
           (lambda _
             (use-modules (guix build maven pom) (sxml simple) (srfi srfi-1))
             (define fix-maven-xml (@@ (guix build maven pom) fix-maven-xml))
             (define dep-tag
               (string->symbol "http://maven.apache.org/POM/4.0.0:dependency"))
             (define deps-tag
               (string->symbol "http://maven.apache.org/POM/4.0.0:dependencies"))
             (define artifactid-tag
               (string->symbol "http://maven.apache.org/POM/4.0.0:artifactId"))
             (define (is-shared-utils? dep)
               (and (pair? dep)
                    (eq? (car dep) dep-tag)
                    (any (lambda (part)
                           (and (pair? part)
                                (eq? (car part) artifactid-tag)
                                (member "surefire-shared-utils" part)))
                         (cdr dep))))
             (define (filter-deps sxml)
               (cond
                ((not (pair? sxml)) sxml)
                ((eq? (car sxml) deps-tag)
                 (cons deps-tag
                       (filter (lambda (d) (not (is-shared-utils? d)))
                               (cdr sxml))))
                (else
                 (cons (filter-deps (car sxml))
                       (filter-deps (cdr sxml))))))
             (let* ((pom-file "maven-surefire-common/pom.xml")
                    (pom (get-pom pom-file))
                    (fixed (filter-deps pom)))
               (with-output-to-file pom-file
                 (lambda ()
                   (sxml->xml (fix-maven-xml fixed)))))
             #t))
         (replace 'install
           (install-from-pom "maven-surefire-common/pom.xml")))))
    (propagated-inputs
     (list java-surefire-api
           java-surefire-extensions-api
           java-surefire-booter
           maven-core
           maven-plugin-annotations
           maven-common-artifact-filters
           maven-artifact-transfer
           java-plexus-java-1
           java-jansi
           java-commons-io
           java-commons-lang3
           java-commons-compress
           maven-shared-utils
           java-slf4j-simple
           java-surefire-parent-pom))
    (inputs
     (list java-jsr305))
    (native-inputs
     `(("unzip" ,unzip)
       ("java-jarjar" ,java-jarjar)
       ("java-plexus-component-metadata" ,java-plexus-component-metadata)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ;; For isolated classpath in generate-plexus-components phase
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-cli" ,java-plexus-cli)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-plexus-container-default" ,java-plexus-container-default)
       ("java-qdox" ,java-qdox)
       ("java-jdom" ,java-jdom)
       ("java-jdom2" ,java-jdom2)
       ("java-asm" ,java-asm-9)
       ("java-commons-cli" ,java-commons-cli)
       ("java-guava" ,java-guava)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)))
    (synopsis "API used in Surefire and Failsafe MOJO")
    (description "This package contains an API used in SureFire and Failsafe
MOJO.")))

(define-public maven-surefire-plugin
  (package
    (inherit java-surefire-logger-api)
    (name "maven-surefire-plugin")
    (arguments
     `(#:jar-name "maven-surefire-plugin.jar"
       #:source-dir "maven-surefire-plugin/src/main/java"
       #:jdk ,openjdk11
       #:tests? #f; test depends on maven-plugin-test-harness
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-line-endings
           (lambda _
             ;; Source files have CRLF line endings which break the Guix Maven Java parser.
             (for-each
               (lambda (file)
                 (substitute* file
                  (("\r\n")
                   "\n")))
               (find-files "." "\\.java$"))))
         (add-before 'build 'generate-plugin.xml
           (generate-plugin.xml "maven-surefire-plugin/pom.xml"
             "surefire"
             "."
             (list
               (list
                 "maven-surefire-common/src/main/java/org/apache/maven/plugin/surefire/AbstractSurefireMojo.java"
                 "maven-surefire-plugin/src/main/java/org/apache/maven/plugin/surefire/SurefireMojo.java"))))
         (add-after 'generate-plugin.xml 'fix-plugin.xml
           (lambda _
             ;; The generate-plugin.xml phase doesn't fully qualify class names
             ;; for classes in the same package (no import statement).
             ;; Fix the role references to use fully qualified names.
             (substitute* "build/classes/META-INF/maven/plugin.xml"
               (("<role>SurefireDependencyResolver</role>")
                "<role>org.apache.maven.plugin.surefire.SurefireDependencyResolver</role>"))))
         (replace 'install
           (install-from-pom "maven-surefire-plugin/pom.xml")))))
    (propagated-inputs
     (list maven-surefire-common maven-core))
    (native-inputs
     (list maven-plugin-annotations unzip))
    (synopsis "SureFire Maven plugin that runs tests")
    (description "The Surefire Plugin is used during the test phase of the
build lifecycle to execute the unit tests of an application.  It generates
reports in two different file formats, plain text and xml.")))

(define-public maven-jar-plugin
  (package
    (name "maven-jar-plugin")
    ;; Note: 3.5.0 uses javax.inject.* annotations (JSR-330) for Sisu
    ;; injection, which works correctly with Maven's plugin classloader.
    ;; Earlier 3.4.x versions used org.apache.maven.api.di.* annotations
    ;; which required Maven 4's api realm and failed to load.
    (version "3.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/apache/maven-jar-plugin")
                     (commit (string-append "maven-jar-plugin-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02bhpsb7mr81z2q1dvc9dl653mji60p7zqkcj3wg1ylhxxmdifc1"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-jar-plugin.jar"
       #:source-dir "src/main/java"
       #:tests? #f; test depends on maven-plugin-test-harness
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-plugin.xml
           (generate-plugin.xml "pom.xml"
             "jar"
             "src/main/java/org/apache/maven/plugins/jar"
             (list
               (list "AbstractJarMojo.java" "JarMojo.java")
               (list "AbstractJarMojo.java" "TestJarMojo.java"))))
         (add-before 'build 'generate-sisu-index
           ;; Generate META-INF/sisu/javax.inject.Named for ToolchainsJdkSpecification
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (call-with-output-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda (port)
                 (display "org.apache.maven.plugins.jar.ToolchainsJdkSpecification\n"
                          port)))))
         (add-before 'install 'add-transitive-dependencies
           ;; maven-archiver requires plexus-interpolation at runtime, and
           ;; plexus-archiver requires plexus-io at runtime.  Maven's plugin
           ;; classloader only loads direct dependencies, so we need to add
           ;; these transitive dependencies explicitly.
           (lambda _
             (substitute* "pom.xml"
               (("</dependencies>")
                "<dependency>
      <groupId>org.codehaus.plexus</groupId>
      <artifactId>plexus-interpolation</artifactId>
      <version>1.27</version>
    </dependency>
    <dependency>
      <groupId>org.codehaus.plexus</groupId>
      <artifactId>plexus-io</artifactId>
      <version>3.6.0</version>
    </dependency>
    <dependency>
      <groupId>org.apache.commons</groupId>
      <artifactId>commons-compress</artifactId>
      <version>1.28.0</version>
    </dependency>
    <dependency>
      <groupId>org.codehaus.plexus</groupId>
      <artifactId>plexus-xml</artifactId>
      <version>3.0.2</version>
    </dependency>
  </dependencies>"))))
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list maven-archiver
           maven-artifact
           maven-core
           maven-plugin-api
           maven-file-management
           maven-shared-utils
           java-plexus-archiver
           java-plexus-io  ; Required by plexus-archiver at runtime
           java-commons-compress  ; Required by plexus-archiver at runtime
           java-plexus-xml  ; Required by plexus-utils at runtime
           java-plexus-interpolation  ; Required by maven-archiver at runtime
           java-plexus-utils
           java-javax-inject
           java-slf4j-api
           java-commons-io))
    (inputs
     (list maven-plugin-annotations))
    (home-page "https://maven.apache.org/plugins/maven-jar-plugin")
    (synopsis "Jar builder plugin for Maven")
    (description "This plugin provides the capability to build jars.  If you
would like to sign jars please use the Maven Jarsigner Plugin instead.")
    (license license:asl2.0)))

(define-public maven-doxia-sink-api
  (package
    (name "maven-doxia-sink-api")
    (version "2.0.0-M2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitbox.apache.org/repos/asf/maven-doxia.git")
                    (commit (string-append "doxia-" version))))
              (file-name (git-file-name "doxia" version))
              (sha256
               (base32
                "0jx96lg0hgjsrm8mynhac4hwh2hmgiwjpwpx2k03yr14040zcr48"))))
    (build-system ant-build-system)
    (propagated-inputs
     (list maven-doxia-parent-pom))
    (arguments
     `(#:jar-name "doxia-sink-api.jar"
       #:source-dir "doxia-sink-api/src/main/java"
       #:tests? #f ; no tests
       #:phases (modify-phases %standard-phases
                  (replace 'install
                    (install-from-pom "doxia-sink-api/pom.xml")))))
    (home-page "https://maven.apache.org/doxia/index.html")
    (synopsis "Generic markup language interface")
    (description
     "The @code{Sink} interface is a generic markup language
interface provided as a Java API.  It contains several methods that
encapsulate common text syntax.  A start tag is denoted by @code{xxxx()}
method and a end of tag by @code{xxxx_()} method.")
    (license license:asl2.0)))

(define maven-doxia-parent-pom
  (package
    (inherit maven-doxia-sink-api)
    (name "maven-doxia-parent-pom")
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (delete 'build)
                  (replace 'install
                    (install-pom-file "pom.xml")))))
    (propagated-inputs
     (list maven-parent-pom-34))
    (synopsis "Content generation framework")
    (description "@samp{Doxia} is a content generation framework that provides
powerful techniques for generating static and dynamic content, supporting a
variety of markup languages.")))

(define-public maven-doxia-core
  (package
    (inherit maven-doxia-sink-api)
    (name "maven-doxia-core")
    (arguments
     `(#:jar-name "doxia-core.jar"
       #:source-dir "doxia-core/src/main/java"
       #:test-dir "doxia-core/src/test/java"
       #:tests? #f ; tests require JUnit5
       #:phases (modify-phases %standard-phases
                  (replace 'install
                    (install-from-pom "doxia-core/pom.xml")))))
    (propagated-inputs (list maven-doxia-parent-pom
                             maven-doxia-sink-api
                             java-slf4j-api
                             java-javax-inject
                             java-plexus-utils
                             java-eclipse-sisu-plexus
                             java-commons-text))
    (synopsis "Doxia core classes and interfaces")
    (description
     "Doxia is a content generation framework that provides powerful
techniques for generating static and dynamic content, supporting a variety of
markup languages.

This package contains Doxia core classes and interfaces.")))

(define-public maven-reporting-api
  (package
    (name "maven-reporting-api")
    (version "4.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitbox.apache.org/repos/asf/maven-reporting-api.git")
                    (commit (string-append "maven-reporting-api-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0477sx16p6grspdfbcgjizf6cm1vs3lnr36m4jkygjm416h0b3yz"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-reporting-api.jar"
       #:source-dir "src/main/java"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list maven-doxia-sink-api))
    (home-page "https://maven.apache.org/shared/maven-reporting-api/")
    (synopsis "Maven Reporting API")
    (description "API for Maven report generation.")
    (license license:asl2.0)))

(define-public wagon-provider-api
  (package
    (name "wagon-provider-api")
    (version "3.5.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitbox.apache.org/repos/asf/maven-wagon.git")
                    (commit (string-append "wagon-" version))))
              (file-name (git-file-name "maven-wagon" version))
              (sha256
               (base32 "134wqkyvjv3h7n052ghd1yy1c3zlp0fv0xlvblxbi352fcqnzx9b"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "wagon-provider-api.jar"
       #:source-dir "wagon-provider-api/src/main/java"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "wagon-provider-api/pom.xml")))))
    (propagated-inputs
     (list java-plexus-utils))
    (home-page "https://maven.apache.org/wagon/")
    (synopsis "Maven Wagon Provider API")
    (description "Transport abstraction for Maven artifact and repository handling.")
    (license license:asl2.0)))

;;; Velocity packages - needed for maven-plugin-tools

;; TODO: This package manually invokes jjtree and javacc because I was too lazy
;; to do it right by packaging java-javacc-maven-plugin and using maven-build-system.
(define velocity-engine-parent-pom
  (package
    (name "velocity-engine-parent-pom")
    (version "2.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/apache/velocity-engine")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1b6vk9wlny7qkspgapqd8x38ipm04lsi1i1bnkzikmi3zp5pl42n"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (add-before 'install 'remove-parent-reference
           (lambda _
             ;; Remove the parent pom reference to avoid needing
             ;; velocity-master:7 -> apache:33 parent chain.
             ;; Use sed to remove the multiline <parent>...</parent> block,
             ;; then add the missing groupId (which was inherited from parent).
             (invoke "sed" "-i" "/<parent>/,/<\\/parent>/d" "pom.xml")
             ;; Add groupId after modelVersion since it was inherited from parent
             (substitute* "pom.xml"
               (("<modelVersion>4.0.0</modelVersion>")
                "<modelVersion>4.0.0</modelVersion>\n    <groupId>org.apache.velocity</groupId>"))))
         (replace 'install
           (install-pom-file "pom.xml")))))
    (native-inputs
     (list sed))
    (home-page "https://velocity.apache.org/")
    (synopsis "Apache Velocity Engine parent POM")
    (description "Parent POM for Apache Velocity Engine modules.")
    (license license:asl2.0)))

(define-public java-velocity-engine-core
  (package
    (name "java-velocity-engine-core")
    (version "2.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/apache/velocity-engine")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1b6vk9wlny7qkspgapqd8x38ipm04lsi1i1bnkzikmi3zp5pl42n"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "velocity-engine-core.jar"
       #:source-dir "velocity-engine-core/src/main/java"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-parser
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((javacc (search-input-file inputs "/bin/javacc"))
                   (jjtree (search-input-file inputs "/bin/jjtree"))
                   (parser-dir "velocity-engine-core/src/main/parser")
                   (gen-dir "velocity-engine-core/src/main/java/org/apache/velocity/runtime/parser"))
               ;; Substitute Maven template variables in Parser.jjt
               (substitute* (string-append parser-dir "/Parser.jjt")
                 (("\\$\\{parser\\.package\\}")
                  "org.apache.velocity.runtime.parser")
                 (("\\$\\{parser\\.basename\\}")
                  "Standard")
                 (("\\$\\{parser\\.char\\.asterisk\\}")
                  "*")
                 (("\\$\\{parser\\.char\\.at\\}")
                  "@")
                 (("\\$\\{parser\\.char\\.dollar\\}")
                  "$")
                 (("\\$\\{parser\\.char\\.hash\\}")
                  "#"))
               ;; Create output directory
               (mkdir-p gen-dir)
               (mkdir-p (string-append gen-dir "/node"))
               ;; Run jjtree to generate Parser.jj (javacc 7.x uses -OPT=val syntax)
               (invoke jjtree
                       (string-append "-OUTPUT_DIRECTORY=" gen-dir)
                       "-NODE_PACKAGE=org.apache.velocity.runtime.parser.node"
                       "-NODE_USES_PARSER=true"
                       "-MULTI=true"
                       "-BUILD_NODE_FILES=false"
                       (string-append parser-dir "/Parser.jjt"))
               ;; Run javacc on the generated Parser.jj
               (invoke javacc
                       (string-append "-OUTPUT_DIRECTORY=" gen-dir)
                       "-STATIC=false"
                       "-TOKEN_MANAGER_USES_PARSER=true"
                       (string-append gen-dir "/Parser.jj"))
               ;; Delete generated Node.java - we have a handwritten one
               (delete-file (string-append gen-dir "/Node.java"))
               ;; Move files that have node package to node directory
               (let ((node-dir (string-append gen-dir "/node")))
                 (for-each (lambda (f)
                             (rename-file (string-append gen-dir "/" f)
                                          (string-append node-dir "/" f)))
                           '("StandardParserTreeConstants.java"
                             "JJTStandardParserState.java"
                             "StandardParserDefaultVisitor.java"))))))
         (add-before 'build 'generate-version-file
           (lambda _
             (let ((template "velocity-engine-core/src/main/java-templates/org/apache/velocity/runtime/VelocityEngineVersion.java")
                   (target "velocity-engine-core/src/main/java/org/apache/velocity/runtime/VelocityEngineVersion.java"))
               (copy-file template target)
               (substitute* target
                 (("\\$\\{project\\.version\\}") ,version)))))
         (replace 'install
           (install-from-pom "velocity-engine-core/pom.xml")))))
    (propagated-inputs
     (list velocity-engine-parent-pom
           java-commons-lang3
           java-slf4j-api))
    (native-inputs
     (list javacc))
    (home-page "https://velocity.apache.org/")
    (synopsis "Apache Velocity template engine core")
    (description "Apache Velocity is a Java-based template engine.")
    (license license:asl2.0)))

(define-public java-plexus-velocity
  (package
    (name "java-plexus-velocity")
    (version "2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/codehaus-plexus/plexus-velocity")
                    (commit (string-append "plexus-velocity-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1vw9k6z7d503x6gscc6px7nwcpl9r6sqy25fiaam9pm4qs4bmrv0"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "plexus-velocity.jar"
       #:source-dir "src/main/java"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "pom.xml")))))
    (propagated-inputs
     (list java-velocity-engine-core
           java-plexus-container-default
           java-plexus-utils
           plexus-components-pom-6.6))
    (home-page "https://codehaus-plexus.github.io/plexus-velocity/")
    (synopsis "Plexus Velocity component")
    (description "Plexus component for Apache Velocity template engine.")
    (license license:asl2.0)))

;;; Maven Plugin Tools 3.15.1 - needed for building Maven plugins

(define-public maven-plugin-tools-api
  (package
    (inherit maven-plugin-annotations)
    (name "maven-plugin-tools-api")
    (arguments
     `(#:jar-name "maven-plugin-tools-api.jar"
       #:source-dir "maven-plugin-tools-api/src/main/java"
       #:jdk ,openjdk11
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'generate-sisu-index
           (lambda _
             ;; Generate Sisu index for @Named components
             (mkdir-p "build/classes/META-INF/sisu")
             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda ()
                 (display "org.apache.maven.tools.plugin.scanner.DefaultMojoScanner\n")))
             (invoke "ant" "jar")))
         (replace 'install
           (install-from-pom "maven-plugin-tools-api/pom.xml")))))
    (propagated-inputs
     (list maven-plugin-tools-parent-pom
           maven-core
           maven-model
           maven-plugin-api
           maven-artifact
           maven-reporting-api
           maven-wagon-provider-api
           java-httpcomponents-httpclient
           java-httpcomponents-httpcore
           java-javax-inject
           java-plexus-classworlds
           java-plexus-java
           java-plexus-utils
           java-plexus-xml
           java-slf4j-api))
    (synopsis "Maven Plugin Tools API")
    (description "API for extracting plugin descriptors from Maven plugins.")))

(define-public maven-plugin-tools-generators
  (package
    (inherit maven-plugin-annotations)
    (name "maven-plugin-tools-generators")
    (arguments
     `(#:jar-name "maven-plugin-tools-generators.jar"
       #:source-dir "maven-plugin-tools-generators/src/main/java"
       #:jdk ,openjdk11
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "maven-plugin-tools-generators/pom.xml")))))
    (propagated-inputs
     (list maven-plugin-tools-parent-pom
           maven-plugin-tools-api
           maven-plugin-api
           java-jsoup
           java-jtidy
           java-plexus-utils
           java-plexus-xml
           java-plexus-velocity
           java-velocity-engine-core))
    (synopsis "Maven Plugin Tools Generators")
    (description "Generators for Maven plugin descriptors.")))

(define-public maven-plugin-tools-java
  (package
    (inherit maven-plugin-annotations)
    (name "maven-plugin-tools-java")
    (arguments
     `(#:jar-name "maven-plugin-tools-java.jar"
       #:source-dir "maven-plugin-tools-java/src/main/java"
       #:jdk ,openjdk11
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-from-pom "maven-plugin-tools-java/pom.xml")))))
    (propagated-inputs
     (list maven-plugin-tools-parent-pom
           maven-plugin-tools-api
           java-qdox-2))
    (synopsis "Maven Plugin Tools Java Extractor")
    (description "Extracts plugin descriptors from Java source.")))

(define-public maven-plugin-tools-annotations
  (package
    (inherit maven-plugin-annotations)
    (name "maven-plugin-tools-annotations")
    (arguments
     `(#:jar-name "maven-plugin-tools-annotations.jar"
       #:source-dir "maven-plugin-tools-annotations/src/main/java"
       #:jdk ,openjdk11
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'generate-sisu-index
           (lambda _
             ;; Generate Sisu index for @Named components
             (mkdir-p "build/classes/META-INF/sisu")
             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda ()
                 (display "org.apache.maven.tools.plugin.extractor.annotations.JavaAnnotationsMojoDescriptorExtractor
org.apache.maven.tools.plugin.extractor.annotations.converter.JavadocBlockTagsToXhtmlConverter
org.apache.maven.tools.plugin.extractor.annotations.scanner.DefaultMojoAnnotationsScanner
org.apache.maven.tools.plugin.extractor.annotations.converter.JavadocInlineTagsToXhtmlConverter
org.apache.maven.tools.plugin.extractor.annotations.converter.tag.block.SeeTagConverter
org.apache.maven.tools.plugin.extractor.annotations.converter.tag.inline.CodeTagConverter
org.apache.maven.tools.plugin.extractor.annotations.converter.tag.inline.LinkTagToHtmlConverter
org.apache.maven.tools.plugin.extractor.annotations.converter.tag.inline.DocRootTagConverter
org.apache.maven.tools.plugin.extractor.annotations.converter.tag.inline.LinkPlainTagToHtmlConverter
org.apache.maven.tools.plugin.extractor.annotations.converter.tag.inline.ValueTagConverter
org.apache.maven.tools.plugin.extractor.annotations.converter.tag.inline.LiteralTagToHtmlConverter
")))
             (invoke "ant" "jar")))
         (replace 'install
           (install-from-pom "maven-plugin-tools-annotations/pom.xml")))))
    (propagated-inputs
     (list maven-plugin-tools-parent-pom
           maven-plugin-tools-api
           maven-plugin-annotations
           java-asm-9
           java-asm-util-9
           java-plexus-archiver
           java-jsoup))
    (synopsis "Maven Plugin Tools Annotations Extractor")
    (description "Extracts plugin descriptors from Java annotations.")))

(define-public maven-plugin-plugin
  (package
    (inherit maven-plugin-annotations)
    (name "maven-plugin-plugin")
    (arguments
     `(#:jar-name "maven-plugin-plugin.jar"
       #:source-dir "maven-plugin-plugin/src/main/java"
       #:jdk ,openjdk11
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-deprecated-dependencies
           (lambda _
             ;; Remove deprecated optional dependencies that don't exist
             ;; in maven-plugin-tools 3.15.1 (they are now in maven-script/).
             ;; These are marked optional but Maven still tries to resolve them.
             ;; Use perl for multiline matching since sed doesn't handle it well.
             (invoke "perl" "-i" "-0pe"
                     "s|<dependency>\\s*<groupId>org.apache.maven.plugin-tools</groupId>\\s*<artifactId>maven-plugin-tools-ant</artifactId>.*?</dependency>||gs"
                     "maven-plugin-plugin/pom.xml")
             (invoke "perl" "-i" "-0pe"
                     "s|<dependency>\\s*<groupId>org.apache.maven.plugin-tools</groupId>\\s*<artifactId>maven-plugin-tools-beanshell</artifactId>.*?</dependency>||gs"
                     "maven-plugin-plugin/pom.xml")
             ;; Change sisu-plexus scope from provided to compile so it gets
             ;; included in the plugin's runtime classpath.  Without this,
             ;; PlexusBindingModule can't discover components like ArchiverManager
             ;; from plexus-archiver because it only scans jars in plexus.core.
             (invoke "perl" "-i" "-0pe"
                     "s|(<artifactId>org.eclipse.sisu.plexus</artifactId>\\s*)<scope>provided</scope>|\\1<scope>compile</scope>|gs"
                     "maven-plugin-plugin/pom.xml")))
         (add-before 'build 'generate-plugin.xml
           (generate-plugin.xml "maven-plugin-plugin/pom.xml"
             "plugin"
             "maven-plugin-plugin/src/main/java/org/apache/maven/plugin/plugin"
             (list
               (list "AbstractGeneratorMojo.java" "DescriptorGeneratorMojo.java")
               (list "AbstractGeneratorMojo.java" "HelpGeneratorMojo.java")
               (list "metadata/AddPluginArtifactMetadataMojo.java"))))
         (replace 'install
           (install-from-pom "maven-plugin-plugin/pom.xml")))))
    (native-inputs
     (list perl unzip))
    (propagated-inputs
     (list maven-plugin-tools-parent-pom
           maven-core
           maven-plugin-api
           maven-plugin-tools-api
           maven-plugin-tools-generators
           maven-plugin-tools-java
           maven-plugin-tools-annotations
           maven-plugin-annotations
           java-plexus-build-api
           java-plexus-archiver
           ;; Use sisu 0.9 and guice 5.1 for compatibility with maven-parent-43
           ;; which declares sisu 0.9.0.M3.  The older sisu 0.3.5 (from maven-core)
           ;; uses Guice 4.1 which is incompatible with the newer Sisu.
           java-eclipse-sisu-inject-0.9
           java-eclipse-sisu-plexus-0.9
           java-guice-5
           java-plexus-io))  ; needed by plexus-archiver at runtime
    (synopsis "Maven Plugin Plugin")
    (description "Plugin for generating Maven plugin descriptors.")))
