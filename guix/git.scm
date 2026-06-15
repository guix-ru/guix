;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2023 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2023, 2025, 2026 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2026 Sergio Pastor Pérez <sergio.pastorperez@gmail.com>
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

(define-module (guix git)
  #:use-module (git)
  #:use-module (guix i18n)
  #:use-module (guix base32)
  #:use-module (guix cache)
  #:autoload   (gcrypt hash) (sha256)
  #:use-module ((guix build utils)
                #:select (mkdir-p delete-file-recursively invoke/quiet))
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix records)
  #:autoload   (guix build syscalls) (terminal-string-width)
  #:use-module (guix gexp)
  #:autoload   (guix git-download)
  (git-reference-url git-reference-commit git-reference-recursive?)
  #:autoload   (guix config) (%git)
  #:use-module (guix sets)
  #:use-module ((guix diagnostics)
                #:select (leave warning info formatted-message))
  #:use-module (guix progress)
  #:autoload   (guix swh) (swh-download commit-id?)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (%repository-cache-directory
            honor-system-x509-certificates!

            url-cache-directory
            with-repository
            with-git-error-handling
            false-if-git-not-found
            repository-info
            update-cached-checkout
            url+commit->name
            ensure-reference-available
            latest-repository-commit
            commit-difference
            commit-relation
            commit-descendant?
            commit-id?
            commit-short-id
            tag->commit

            remote-refs

            git-checkout
            git-checkout?
            git-checkout-url
            git-checkout-branch
            git-checkout-commit
            git-checkout-recursive?

            git-reference->git-checkout))

(define %legacy-repository-cache-directory
  ;; Directory containing cached checkouts prior to resorting to blob
  ;; filtering.  This directory would only contain full checkouts.
  (string-append (cache-directory #:ensure? #f) "/checkouts"))

(define %repository-cache-directory
  ;; Directory used for cached checkouts.  The "-v2" suffix was introduced
  ;; when switching to blob-filtered checkouts, which the former
  ;; Guile-Git/libgit2 implementation cannot handle.
  (make-parameter (string-append (cache-directory #:ensure? #f)
                                 "/checkouts-v2")))

(define (honor-system-x509-certificates!)
  "Use the system's X.509 certificates for Git checkouts over HTTPS.  Honor
the 'SSL_CERT_FILE' and 'SSL_CERT_DIR' environment variables."
  ;; On distros such as CentOS 7, /etc/ssl/certs contains only a couple of
  ;; files (instead of all the certificates) among which "ca-bundle.crt".  On
  ;; other distros /etc/ssl/certs usually contains the whole set of
  ;; certificates along with "ca-certificates.crt".  Try to choose the right
  ;; one.
  (let ((file      (letrec-syntax ((choose
                                    (syntax-rules ()
                                      ((_ file rest ...)
                                       (let ((f file))
                                         (if (and f (file-exists? f))
                                             f
                                             (choose rest ...))))
                                      ((_)
                                       #f))))
                     (choose (getenv "SSL_CERT_FILE")
                             "/etc/ssl/certs/ca-certificates.crt"
                             "/etc/ssl/certs/ca-bundle.crt")))
        (directory (or (getenv "SSL_CERT_DIR") "/etc/ssl/certs")))
    (and (or file
             (and=> (stat directory #f)
                    (lambda (st)
                      (> (stat:nlink st) 2))))
         (begin
           (set-tls-certificate-locations! directory file)
           #t))))

(define %certificates-initialized?
  ;; Whether 'honor-system-x509-certificates!' has already been called.
  #f)

(define-syntax-rule (with-libgit2 thunk ...)
  (begin
    ;; XXX: The right thing to do would be to call (libgit2-shutdown) here,
    ;; but pointer finalizers used in guile-git may be called after shutdown,
    ;; resulting in a segfault. Hence, let's skip shutdown call for now.
    (libgit2-init!)
    (unless %certificates-initialized?
      (honor-system-x509-certificates!)
      (set! %certificates-initialized? #t))
    thunk ...))

(define* (url-cache-directory url
                              #:optional (cache-directory
                                          (%repository-cache-directory))
                              #:key recursive?)
  "Return the directory associated to URL in %repository-cache-directory."
  (string-append
   cache-directory "/"
   (bytevector->base32-string
    (sha256 (string->utf8 (if recursive?
                              (string-append "R:" url)
                              url))))))

(define (show-progress progress)
  "Display a progress bar as we fetch Git code.  PROGRESS is an
<indexer-progress> record from (git)."
  (define total
    (indexer-progress-total-objects progress))

  (define-values (done label)
    (if (< (indexer-progress-received-objects progress) total)
        (values (indexer-progress-received-objects progress)
                (G_ "receiving objects"))
        (values (indexer-progress-indexed-objects progress)
                (G_ "indexing objects"))))

  (define %
    (* 100. (/ done total)))

  ;; TODO: Both should be handled & exposed by the PROGRESS-BAR API instead.
  (define width
    (max (- (current-terminal-columns)
            (terminal-string-width label) 7)
         3))

  (define grain
    (match (quotient total (max 100 (* 8 width))) ; assume 1/8 glyph resolution
      (0 1)
      (x x)))

  (when (and (< % 100) (zero? (modulo done grain)))
    (erase-current-line (current-error-port))
    (format (current-error-port) "~a ~3,d% ~a"
              label (inexact->exact (round %))
              (progress-bar % width))
    (force-output (current-error-port)))

  (when (= % 100.)
    ;; We're done, erase the line.
    (erase-current-line (current-error-port))
    (force-output (current-error-port)))

  ;; Return true to indicate that we should go on.
  #t)


;;;
;;; Git CLI output parser.
;;;

(define (read-progress port)
  "Parse and print Git CLI progress from PORT."

  ;; Characters that delimit a progress line from the Git CLI output.
  (define line-delimiter "\r\n")

  ;; Regex to match progress lines from the Git CLI output.
  (define progress-regex
    (make-regexp "^(remote:[ \t]*)?([^:]+):[ \t]+([0-9]{1,3})%"))

  (define (line->progress line)
    (and=> (regexp-exec progress-regex line)
           (lambda (rx-match)
             (cons (string-trim-both (match:substring rx-match 2))
                   (string->number (match:substring rx-match 3))))))

  (define (translate-progress-label label)
    (match label
      ("Counting objects"    (G_ "counting objects"))
      ("Compressing objects" (G_ "compressing objects"))
      ("Receiving objects"   (G_ "receiving objects"))
      ("Resolving deltas"    (G_ "resolving deltas"))
      ("Updating files"      (G_ "updating files"))
      (else (string-downcase label)))) ;no translation.

  (define (render progress)
    (match-let* (((english-label . percent) progress)
                 (port (current-error-port))
                 (width (max (- (current-terminal-columns)
                                (terminal-string-width english-label) 7)
                             3))
                 (bar (progress-bar percent width)))
      (erase-current-line port)
      (format port "~a ~3,d% ~a"
              (translate-progress-label english-label)
              percent bar)
      (force-output port)))

  (let ((line (read-delimited line-delimiter port)))
    (unless (eof-object? line)
      (and=> (line->progress line)
             render)
      (read-progress port))))

(define-condition-type &git-cli-error &error
  git-cli-error?
  (program      git-cli-error-program)
  (arguments    git-cli-error-arguments)
  (exit-status  git-cli-error-exit-status)
  (term-signal  git-cli-error-term-signal)
  (stop-signal  git-cli-error-stop-signal))

(define (invoke-git . args)
  "Invoke %git with ARGS while parsing output into progress bars."
  (match-let* (((input . output) (pipe))
               ;; Git prints progress to STDERR, STDOUT is used for messages
               ;; that we do not want to show when using this wrapper.
               ;; Unfortunately, Guile does not provide a platform agnostic
               ;; '%null-device' variable.
               (null-device (open-output-file "/dev/null"))
               (args* `(,%git
                        "-c" "color.ui=false" ;ensure color is disabled.
                        ,@args))
               (pid (spawn %git args*
                           #:output null-device
                           #:error output
                           ;; Set and easy to parse locale.
                           #:environment (cons "LC_ALL=C" (environ)))))
    (close-port output)
    (read-progress input)
    (match-let* (((id . status) (waitpid pid)))
      (unless (zero? status)
        (raise (condition
                (&git-cli-error
                 (program %git)
                 (arguments args*)
                 (exit-status (status:exit-val status))
                 (term-signal (status:term-sig status))
                 (stop-signal (status:stop-sig status))))))
      #t)))


(define* (make-default-fetch-options #:key (verify-certificate? #t))
  "Return the default fetch options.  VERIFY-CERTIFICATE? determines whether
to verify X.509 host certificates."
  (define (warn-for-invalid-certificate host valid?)
    (unless valid?
      (warning (G_ "ignoring invalid certificate for '~a'~%") host)))

  (let* ((auth-method (%make-auth-ssh-agent))
         (options
          (make-fetch-options auth-method
                              ;; Guile-Git doesn't distinguish between these.
                              #:proxy-url (or (getenv "http_proxy")
                                              (getenv "https_proxy"))
                              #:transfer-progress
                              (and (isatty? (current-error-port))
                                   show-progress))))
    ;; When VERIFY-CERTIFICATE? is true, keep the default libgit2 behavior,
    ;; which is to raise an exception upon invalid certificates.
    (unless verify-certificate?
      (let ((callbacks (fetch-options-remote-callbacks options)))
        (set-remote-callbacks-certificate-check! callbacks
                                                 warn-for-invalid-certificate)))
    options))

(define (set-git-timeouts connection-timeout read-timeout)
  "Instruct Guile-Git to honor the given CONNECTION-TIMEOUT and READ-TIMEOUT
when talking to remote Git servers.

If one of them is #f, the corresponding default setting is kept unchanged."
  (when connection-timeout
    (set-server-connection-timeout! connection-timeout))
  (when read-timeout
    (set-server-timeout! read-timeout)))

(define* (clone* url directory
                 #:key
                 (full? #t)
                 (verify-certificate? #t))
  "Clone git repository at URL into DIRECTORY.  Upon failure,
make sure no empty directory is left behind.

If FULL? is true, perform a full checkout, containing all the branches, file
contents, and history."
  (with-throw-handler #t
    (lambda ()
      (mkdir-p directory)

      (let* ((repository
              (if full?
                  (clone url directory
                         (make-clone-options
                          #:fetch-options (make-default-fetch-options
                                           #:verify-certificate?
                                           verify-certificate?)))
                  (begin
                    ;; XXX: As of libgit2 1.9.7 (used by Guile-Git), there's
                    ;; no support for fetch filters. A big Git repo (such as
                    ;; Guix) takes a lot of bandwidth and CPU time to clone,
                    ;; this is one of the main culprits of the percived
                    ;; slowness of 'guix pull'.
                    (invoke-git "clone" "--progress"
                                "--filter=blob:none"
                                url directory)
                    ;; Return a libgit2 object
                    (repository-open directory)))))
        repository))
    (lambda _
      (false-if-exception (rmdir directory)))))

(define (url+commit->name url sha1)
  "Return the string \"<REPO-NAME>-<SHA1:7>\" where REPO-NAME is the name of
the git repository, extracted from URL and SHA1:7 the seven first digits
of SHA1 string."
  (string-append
   (string-replace-substring
    (last (string-split url #\/)) ".git" "")
   "-" (string-take sha1 7)))

(define (commit-id? str)
  "Return true if STR is likely a Git commit ID, false otherwise---e.g., if it
is a tag name.  This is based on a simple heuristic so use with care!"
  (and (or (= (string-length str) 40)   ;SHA-1
           (= (string-length str) 64))  ;SHA-256
       (string-every char-set:hex-digit str)))

(define commit-short-id
  (compose (cut string-take <> 7) oid->string commit-id))

(define (tag->commit repository tag)
  "Resolve TAG in REPOSITORY and return the corresponding object, usually a
commit."
  (let* ((oid (reference-name->oid repository
                                   (string-append "refs/tags/" tag)))
         (obj (object-lookup repository oid)))
    ;; OID may designate an "annotated tag" object or a "commit" object.
    ;; Return the commit object in both cases.
    (if (= OBJ-TAG (object-type obj))
        (object-lookup repository
                       (tag-target-id (tag-lookup repository oid)))
        obj)))

(define (resolve-reference repository ref)
  "Resolve the branch, commit or tag specified by REF, and return the
corresponding Git object."
  (let resolve ((ref ref))
    (match ref
      (('branch . branch)
       (let ((oid (reference-target
                   (branch-lookup repository branch BRANCH-REMOTE))))
         (object-lookup repository oid)))
      (('symref . (? string? symref))
       (let* ((oid (reference-name->oid repository symref))
              (obj (object-lookup repository oid)))
         ;; For consistency with the 'tag case, if OBJ is a tag, return the
         ;; commit object it points to.
         (if (= OBJ-TAG (object-type obj))
             (object-lookup repository
                            (tag-target-id (tag-lookup repository oid)))
             obj)))
      (('symref . (_ . commit))
       ;; It handles channel files automatically generated by Cuirass, where
       ;; the branch follows Pull-Request Git namespace as refs/pull/1/head,
       ;; and one commit is specifically pinned on that symref branch.
       (resolve `(commit . ,commit)))
      (('commit . commit)
       (let ((len (string-length commit)))
         ;; 'object-lookup-prefix' appeared in Guile-Git in Mar. 2018, so we
         ;; can't be sure it's available.  Furthermore, 'string->oid' used to
         ;; read out-of-bounds when passed a string shorter than 40 chars,
         ;; which is why we delay calls to it below.
         (if (< len 40)
             (object-lookup-prefix repository (string->oid commit) len)
             (object-lookup repository (string->oid commit)))))
      (('tag-or-commit . str)
       (cond ((and (string-contains str "-g")
                   (match (string-split str #\-)
                     ((version ... revision g+commit)
                      (if (and (> (string-length g+commit) 4)
                               (string-every char-set:digit revision)
                               (string-every char-set:hex-digit
                                             (string-drop g+commit 1)))
                          ;; Looks like a 'git describe' style ID, like
                          ;; v1.3.0-7-gaa34d4d28d.
                          (string-drop g+commit 1)
                          #f))
                     (_ #f)))
              => (lambda (commit) (resolve `(commit . ,commit))))
             ((or (> (string-length str) 40)
                  (not (string-every char-set:hex-digit str)))
              (resolve `(tag . ,str)))      ;definitely a tag
             (else
              (catch 'git-error
                (lambda ()
                  (resolve `(tag . ,str)))
                (lambda _
                  ;; There's no such tag, so it must be a commit ID.
                  (resolve `(commit . ,str)))))))
      (('tag    . tag)
       (tag->commit repository tag)))))

(define (delete-untracked-files repository)
  "Delete untracked files from the work directory of REPOSITORY."
  (let ((workdir (repository-working-directory repository))
        (status (status-list-new repository
                                 (make-status-options
                                  STATUS-SHOW-WORKDIR-ONLY
                                  (logior
                                   STATUS-FLAG-INCLUDE-UNTRACKED
                                   STATUS-FLAG-INCLUDE-IGNORED)))))
    (for-each (lambda (entry)
                (let ((status (status-entry-status entry)))
                  (when (or (memq 'wt-new status)
                            (memq 'ignored status))
                    (let* ((diff (status-entry-index-to-workdir entry))
                           (new  (diff-delta-new-file diff)))
                      (delete-file-recursively
                       (in-vicinity workdir (diff-file-path new)))))))
              (status-list->status-entries status))))

(define (switch-to-ref repository ref)
  "Switch to REPOSITORY's branch, commit or tag specified by REF.  Return the
OID (roughly the commit hash) corresponding to REF."
  (define obj
    (resolve-reference repository ref))

  ;; XXX: since we are cloning using Git filters though the shell we need to
  ;; use Git from the shell. This is because 'libgit2' does not support lazily
  ;; fetching Git blobs, so we need to use the CLI to fetch those missing
  ;; blobs for us.
  (reset* repository obj RESET_HARD)

  ;; There might still be untracked files in REPOSITORY due to an interrupted
  ;; checkout for example; delete them.
  (delete-untracked-files repository)

  (object-id obj))

(define (call-with-repository directory proc)
  (let ((repository #f))
   (dynamic-wind
     (lambda ()
       (set! repository (repository-open directory)))
     (lambda ()
       (proc repository))
     (lambda ()
       (repository-close! repository)))))

(define-syntax-rule (with-repository directory repository exp ...)
  "Open the repository at DIRECTORY and bind REPOSITORY to it within the
dynamic extent of EXP."
  (call-with-repository directory
                        (lambda (repository) exp ...)))

(define (report-git-error error)
  "Report the given Guile-Git error."
  (leave (G_ "Git error: ~a~%") (git-error-message error)))

(define-syntax-rule (with-git-error-handling body ...)
  (catch 'git-error
    (lambda ()
      body ...)
    (lambda (key err)
      (report-git-error err))))

(define (repository-info directory)
  "Open the Git repository in DIRECTORY or one of its parent and return three
values: the working directory of that repository, its checked out commit ID,
and its checked out reference (such as a branch name).  Return #f (three
values) if DIRECTORY does not hold a readable Git repository."
  (catch 'git-error
    (lambda ()
      (with-repository (repository-discover directory) repository
        (let* ((head   (repository-head repository))
               (commit (oid->string (reference-target head))))
          (values (repository-working-directory repository)
                  commit
                  (reference-shorthand head)))))
    (lambda _
      (values #f #f #f))))

(define* (update-submodules repository
                            #:key (log-port (current-error-port))
                            (fetch-options #f))
  "Update the submodules of REPOSITORY, a Git repository object."
  (for-each (lambda (name)
              (let ((submodule (submodule-lookup repository name)))
                (format log-port (G_ "updating submodule '~a'...~%")
                        name)
                (submodule-update submodule
                                  #:fetch-options fetch-options)

                ;; Recurse in SUBMODULE.
                (let ((directory (string-append
                                  (repository-working-directory repository)
                                  (submodule-path submodule))))
                  ;; libgit2, unlike git, does not discard invalid submodules
                  ;; that lack a path, and instead uses the module name as the
                  ;; path in this case, which would break `with-repository`
                  ;; below (see:
                  ;; <https://github.com/libgit2/libgit2/issues/7194>).
                  (when (file-exists? directory)
                    (with-repository directory repository
                      (update-submodules repository
                                         #:fetch-options fetch-options
                                         #:log-port log-port))))))
            (repository-submodules repository)))

(define-syntax-rule (false-if-git-not-found exp)
  "Evaluate EXP, returning #false if a GIT_ENOTFOUND error is raised."
  (catch 'git-error
    (lambda ()
      exp)
    (lambda (key error . rest)
      (if (= GIT_ENOTFOUND (git-error-code error))
          #f
          (apply throw key error rest)))))

(define (reference-available? repository ref)
  "Return true if REF, a reference such as '(commit . \"cabba9e\"), is
definitely available in REPOSITORY, false otherwise."
  (match ref
    (('commit . (? commit-id? commit))
     (let ((oid (string->oid commit)))
       (false-if-git-not-found
        (->bool (commit-lookup repository oid)))))
    ((or ('tag . str)
         ('tag-or-commit . str))
     (false-if-git-not-found
      (->bool (resolve-reference repository ref))))
    (('symref . (_ . (? commit-id? commit)))
     (reference-available? repository `(commit . ,commit)))
    (_
     ;; For the others REF as branch or symref, the REF cannot be available
     #f)))

(define (clone-from-swh url tag-or-commit output)
  "Attempt to clone TAG-OR-COMMIT (a string), which originates from URL, using
a copy archived at Software Heritage."
  (call-with-temporary-directory
   (lambda (bare)
     (and (swh-download url tag-or-commit bare
                        #:archive-type 'git-bare)
          (let ((repository (clone* bare output #:full? #t)))
            (remote-set-url! repository "origin" url)
            repository)))))

(define* (clone/swh-fallback url ref cache-directory
                             #:key
                             (full? #t)
                             (verify-certificate? #t))
  "Like 'clone', but fallback to Software Heritage if the repository cannot be
found at URL.

If FULL? is true, perform a full checkout, containing all the branches, file
contents, and history."
  (define (inaccessible-url-error? err)
    (let ((class (git-error-class err))
          (code  (git-error-code err)))
      (or (= class GITERR_HTTP)                   ;404 or similar
          (= class GITERR_NET))))                 ;unknown host, etc.
  ;; Catching all exceptions since need to catch both '&git-error' from
  ;; Guile-Git and '&git-cli-error' from the 'invoke-git' procedure.
  (catch #t
    (lambda ()
      (clone* url cache-directory
              #:full? full?
              #:verify-certificate? verify-certificate?))
    (lambda (key err)
      (if (and (git-cli-error? err)
               (git-cli-error-exit-status err))
          (warning (G_ "command~{ ~s~} failed with status ~a~%")
                   (cons (git-cli-error-program err)
                         (git-cli-error-arguments err))
                   (git-cli-error-exit-status err))
          (leave (G_ "command~{ ~s~} terminated due to signal signal ~a~%")
                 (cons (git-cli-error-program err)
                       (git-cli-error-arguments err))
                 (or (git-cli-error-term-signal err)
                     (git-cli-error-stop-signal err))))
      (match ref
        (((or 'commit 'tag-or-commit) . commit)
         (if (or (git-cli-error? err) ;from 'invoke-git'.
                 (inaccessible-url-error? err))
             (or (clone-from-swh url commit cache-directory)
                 (begin
                   (warning (G_ "revision ~a of ~a \
could not be fetched from Software Heritage~%")
                            commit url)
                   (throw key err)))
             (throw key err)))
        (_ (if (git-cli-error? err)
               (leave (G_ "failed to clone from '~a'~%") url)
               (throw key err)))))))

(define cached-checkout-expiration
  ;; Return the expiration time procedure for a cached checkout.
  ;; TODO: Honor $GUIX_GIT_CACHE_EXPIRATION.

  ;; Use the mtime rather than the atime to cope with file systems mounted
  ;; with 'noatime'.
  (file-expiration-time (* 90 24 3600) stat:mtime))

(define %checkout-cache-cleanup-period
  ;; Period for the removal of expired cached checkouts.
  (* 5 24 3600))

(define (delete-checkout directory)
  "Delete DIRECTORY recursively, in an atomic fashion."
  (let ((trashed (string-append directory ".trashed")))
    (rename-file directory trashed)
    (delete-file-recursively trashed)))

(define (packs-in-git-repository directory)
  "Return the number of pack files under DIRECTORY, a Git checkout."
  (catch 'system-error
    (lambda ()
      (let ((directory (opendir (in-vicinity directory ".git/objects/pack"))))
        (let loop ((count 0))
          (match (readdir directory)
            ((? eof-object?)
             (closedir directory)
             count)
            (str
             (loop (if (string-suffix? ".pack" str)
                       (+ 1 count)
                       count)))))))
    (const 0)))

(define (maybe-run-git-gc directory)
  "Run 'git gc' in DIRECTORY if needed."
  ;; XXX: As of libgit2 1.3.x (used by Guile-Git), there's no support for GC.
  ;; Each time a checkout is pulled, a new pack is created, which eventually
  ;; takes up a lot of space (lots of small, poorly-compressed packs).  As a
  ;; workaround, shell out to 'git gc' when the number of packs in a
  ;; repository has become "too large", potentially wasting a lot of space.
  ;; See <https://issues.guix.gnu.org/65720>.
  (when (> (packs-in-git-repository directory) 25)
    (info (G_ "compressing cached Git repository at '~a'...~%")
          directory)
    (invoke/quiet %git "-C" directory "gc")))

(define* (update-cached-checkout url
                                 #:key
                                 (connection-timeout 30000)
                                 (read-timeout 45000)
                                 (ref '())
                                 recursive?
                                 (full? #f)
                                 (files-with-history '())
                                 (check-out? #t)
                                 starting-commit
                                 (log-port (%make-void-port "w"))
                                 (verify-certificate? #t)
                                 (cache-directory
                                  (url-cache-directory
                                   url (%repository-cache-directory)
                                   #:recursive? recursive?)))
  "Update the cached checkout of URL to REF in CACHE-DIRECTORY.  Return three
values: the cache directory name, and the SHA1 commit (a string) corresponding
to REF, and the relation of STARTING-COMMIT relative to the new commit (if
provided) as returned by 'commit-relation'.

REF is pair whose key is [branch | commit | symref | tag | tag-or-commit ] and value
the associated data: [<branch name> | <sha1> | <tag name> | <string>].
If REF is the empty list, the remote HEAD is used.

When RECURSIVE? is true, check out submodules as well, if any.

When CHECK-OUT? is true, reset the cached working tree to REF; otherwise leave
it unchanged.

When VERIFY-CERTIFICATE? is true, raise an error when encountering an invalid
X.509 host certificate; otherwise, warn about the problem and keep going.

Wait for up to CONNECTION-TIMEOUT milliseconds when establishing connection to
the remote server, and for up to READ-TIMEOUT milliseconds when reading from
it.  When zero, use the system defaults for these timeouts; when false, leave
current settings unchanged.  Warning: These timeout values are ignored unless
FULL? is true.

If FULL? is true, perform a full checkout, containing all the branches, file
contents, and history.

FILES-WITH-HISTORY is a list of patterns that controls which file's blobs must
be pressent accross the checkout history."
  (define (cache-entries directory)
    (filter-map (match-lambda
                  ((or "." "..")
                   #f)
                  (file
                   (string-append directory "/" file)))
                (or (scandir directory) '())))

  (define canonical-ref
    ;; We used to require callers to specify "origin/" for each branch, which
    ;; made little sense since the cache should be transparent to them.  So
    ;; here we append "origin/" if it's missing and otherwise keep it.
    (match ref
      (() '(symref . "refs/remotes/origin/HEAD"))
      (('branch . branch)
       `(branch . ,(if (string-prefix? "origin/" branch)
                       branch
                       (string-append "origin/" branch))))
      (_ ref)))

  (define symref?
    (match-lambda
      (('symref . _) #t)
      (_ #f)))

  (define ref->refspecs
    (match-lambda
      ((or ('symref . (? string? symref))
           ('symref . (symref . _)))
       (list (string-append "+" symref ":" symref)))
       (_ '())))

  (with-libgit2
   (set-git-timeouts connection-timeout read-timeout)
   (let* ((cache-exists? (openable-repository? cache-directory))
          (repository    (if cache-exists?
                             (repository-open cache-directory)
                             (clone/swh-fallback url ref cache-directory
                                                 #:full? full?
                                                 #:verify-certificate?
                                                 verify-certificate?)))
          (config (repository-config repository)))
     ;; Override 'core.autocrlf' as set in ~/.gitconfig to ensure files are
     ;; left unchanged when cloning and pulling.
     (set-config-string config "core.autocrlf" "input")

     ;; When using symrefs, fetch remote again even if it has been cloned just
     ;; before as the requested reference are not fetched when cloning.
     (when (and (or cache-exists? (symref? ref))
                (not (reference-available? repository ref)))
       (remote-fetch (remote-lookup repository "origin")
                     #:fetch-options (make-default-fetch-options
                                      #:verify-certificate?
                                      verify-certificate?)
                     ;; Build refspecs from symbolic references so they are
                     ;; created locally and updated if necessary.
                     #:refspecs (ref->refspecs ref)))
     ;; Note: call 'commit-relation' from here because it's more efficient
     ;; than letting users re-open the checkout later on.
     (let* ((oid      (if check-out?
                          (switch-to-ref repository canonical-ref)
                          (object-id
                           (resolve-reference repository canonical-ref))))
            (new      (and starting-commit
                           (commit-lookup repository oid)))
            (old      (and starting-commit
                           (false-if-git-not-found
                            (commit-lookup repository
                                           (string->oid starting-commit)))))
            (relation (and starting-commit
                           (if old
                               (commit-relation old new)
                               'unrelated))))

       (when recursive?
         (update-submodules repository #:log-port log-port
                            #:fetch-options
                            (make-default-fetch-options
                             #:verify-certificate?
                             verify-certificate?)))

       ;; XXX: Now that 'switch-to-ref' moved HEAD, retrieve all blobs
       ;; corresponding to the sparse files reachable from any of the current
       ;; branch commits.  Each of the FILES-WITH-HISTORY in each commit is
       ;; required for the authentication of said commit during the call to
       ;; 'authenticate-repository'.  'with-sparse-checkout' makes a sparse
       ;; repository, only within the context of its body, the repository tree
       ;; will go back to normal outside of the body.  This is required since
       ;; 'build-aux/build-self.scm' will be needed for computing the Guix
       ;; derivation later on.
       (with-sparse-checkout repository files-with-history
         #:cone? #f
         (backfill repository))

       ;; Reclaim file descriptors and memory mappings associated with
       ;; REPOSITORY as soon as possible.
       (repository-close! repository)

       ;; Update CACHE-DIRECTORY's mtime to so the cache logic sees it.
       (match (gettimeofday)
         ((seconds . microseconds)
          (let ((nanoseconds (* 1000 microseconds)))
            (utime cache-directory
                   seconds seconds
                   nanoseconds nanoseconds))))

       ;; Run 'git gc' if needed.
       (maybe-run-git-gc cache-directory)

       ;; When CACHE-DIRECTORY is a sub-directory of the default cache
       ;; directory, remove expired checkouts that are next to it.
       (let ((parent (dirname cache-directory)))
         (when (string=? parent (%repository-cache-directory))
           (maybe-remove-expired-cache-entries parent cache-entries
                                               #:entry-expiration
                                               cached-checkout-expiration
                                               #:delete-entry delete-checkout
                                               #:cleanup-period
                                               %checkout-cache-cleanup-period)))

       ;; Blob filtering sets up a new cache directory but the old cache
       ;; directory still needs to be cleaned up.  Remove after 2027-12-01.
       (maybe-remove-expired-cache-entries %legacy-repository-cache-directory
                                           cache-entries
                                           #:entry-expiration
                                           cached-checkout-expiration
                                           #:delete-entry delete-checkout
                                           #:cleanup-period
                                           (* 2 %checkout-cache-cleanup-period))

       (values cache-directory (oid->string oid) relation)))))

;; XXX: Guile git is not able to lazily fetch blobs filtered through a clone
;; using '--filter=blob:none', 'guix pull' filters those blobs to speedup the
;; process.
(define (ensure-reference-available repository reference)
  "Ensure all blobs in the tree of REFERENCE, a string such as
\"origin/master\", are present in REPOSITORY."
  (invoke-git "-C" (repository-working-directory repository)
              "backfill" "--max-count=1" "--no-sparse" reference))

(define* (sparse-set repository patterns #:key (cone? #t))
  "Enable and configure REPOSITORY as a sparse checkout described by the list
of PATTERNS.

When CONE? is #t, each pattern in PATTERNS specifies files or directories,
including the contents of those directories and their descendants.  Otherwise,
each pattern uses Git's ignore-style syntax to select files and directories."
  ;; XXX: The usage of '--no-cone' is discouraged since it comes with a few
  ;; drawbacks, however, for the performance improvements of this pull request
  ;; this is required. Although the authors advise against using it, the
  ;; feature will not be removed as exposed here:
  ;; https://lore.kernel.org/git/CABPp-BEJoMcQSF4EyO0VJJVDY5CZtSEgzyK3_2vOMLSBv4HLeQ@mail.gmail.com
  (let ((workdir (repository-working-directory repository))
        (type (if cone? "--cone" "--no-cone")))
    (apply invoke-git "-C" workdir
           "sparse-checkout" "set" type patterns)))

(define (sparse-disable repository)
  "Disable sparse checkout in REPOSITORY."
  (invoke-git "-C" (repository-working-directory repository)
              "sparse-checkout" "disable"))

(define* (call-with-sparse-checkout repository patterns proc
                                    #:key (cone? #t))
  "Call PROC in REPOSITORY configured with a sparse checkout following
list of PATTERNS."
  (catch #t
    (lambda ()
      (sparse-set repository patterns #:cone? cone?)
      (proc repository)
      (sparse-disable repository))
    (lambda args
      (sparse-disable repository)
      (apply throw args))))

(define-syntax with-sparse-checkout
  (syntax-rules ()
    "Open the REPOSITORY at DIRECTORY and configure a sparse checkout
following list of PATTERNS within the dynamic extent of EXP.  The optional
keyword argument CONE? controls how the patterns are processed."
    ((_ repository patterns #:cone? cone? exp ...)
     (call-with-sparse-checkout
      repository patterns
      (lambda (repository) exp ...)
      #:cone? cone?))
    ((_ repository patterns exp ...)
     (call-with-sparse-checkout
      repository patterns
      (lambda (repository) exp ...)))))

(define (backfill repository)
  "Download missing objects in REPOSITORY head."
  (invoke-git "-C" (repository-working-directory repository)
              "backfill"))

(define (reset* repository obj type)
  "Reset REPOSITORY HEAD to point to OBJ ensuring blobs are present.
TYPE controls the reset behavior."
  (let* ((workdir (repository-working-directory repository))
         (commit (object->commit obj))
         (id (commit-id commit))
         (type* (cond
                 ((eq? type RESET_SOFT) "--soft")
                 ((eq? type RESET_MIXED) "--mixed")
                 ((eq? type RESET_HARD) "--hard"))))
    ;; Note: Shelling out to 'git' here because Guile-Git/libgit2 cannot
    ;; lazily fetch missing blobs from a sparse checkout.
    (invoke-git "-C" workdir "reset" type* (oid->string id))))

(define* (latest-repository-commit store url
                                   #:key
                                   recursive?
                                   (log-port (%make-void-port "w"))
                                   (verify-certificate? #t)
                                   (cache-directory
                                    (%repository-cache-directory))
                                   (ref '()))
  "Return two values: the content of the git repository at URL copied into a
store directory and the sha1 of the top level commit in this directory.  The
reference to be checkout, once the repository is fetched, is specified by REF.
REF is pair whose key is [branch | commit | tag] and value the associated
data, respectively [<branch name> | <sha1> | <tag name>].  If REF is the empty
list, the remote HEAD is used.

When RECURSIVE? is true, check out submodules as well, if any.

When VERIFY-CERTIFICATE? is true, raise an error when encountering an invalid
X.509 host certificate; otherwise, warn about the problem and keep going.

Git repositories are kept in the cache directory specified by
%repository-cache-directory parameter.

Log progress and checkout info to LOG-PORT."
  (define (dot-git? file stat)
    (and (string=? (basename file) ".git")
         (or (eq? 'directory (stat:type stat))

             ;; Submodule checkouts end up with a '.git' regular file that
             ;; contains metadata about where their actual '.git' directory
             ;; lives.
             (and recursive?
                  (eq? 'regular (stat:type stat))))))

  (format log-port "updating checkout of '~a'...~%" url)
  (let*-values
      (((checkout commit _)
        (update-cached-checkout url
                                #:recursive? recursive?
                                #:ref ref
                                #:cache-directory
                                (url-cache-directory url cache-directory
                                                     #:recursive?
                                                     recursive?)
                                #:verify-certificate? verify-certificate?
                                #:log-port log-port))
       ((name)
        (url+commit->name url commit)))
    (format log-port "retrieved commit ~a~%" commit)
    (values (add-to-store store name #t "sha256" checkout
                          #:select? (negate dot-git?))
            commit)))

(define (print-git-error port key args default-printer)
  (match args
    (((? git-error? error) . _)
     (format port (G_ "Git error: ~a~%")
             (git-error-message error)))))

(set-exception-printer! 'git-error print-git-error)


;;;
;;; Commit difference.
;;;

(define* (commit-closure commit #:optional (visited (setq))) ;to remove
  "Return the closure of COMMIT as a set.  Skip commits contained in VISITED,
a set, and adjoin VISITED to the result."
  (let loop ((commits (list commit))
             (visited visited))
    (match commits
      (()
       visited)
      ((head . tail)
       (if (set-contains? visited head)
           (loop tail visited)
           (loop (append (commit-parents head) tail)
                 (set-insert head visited)))))))

(define* (commit-difference new old #:optional (excluded '()))
  "Return the list of commits between NEW and OLD, where OLD is assumed to be
an ancestor of NEW.  Exclude all the commits listed in EXCLUDED along with
their ancestors.

Essentially, this computes the set difference between the closure of NEW and
that of OLD."
  (let loop ((commits (list new))
             (result '())
             (visited (fold commit-closure
                            (setq)
                            (cons old excluded))))
    (match commits
      (()
       (reverse result))
      ((head . tail)
       (if (set-contains? visited head)
           (loop tail result visited)
           (loop (append (commit-parents head) tail)
                 (cons head result)
                 (set-insert head visited)))))))

(define (commit-relation old new)
  "Return a symbol denoting the relation between OLD and NEW, two commit
objects: 'ancestor (meaning that OLD is an ancestor of NEW), 'descendant, or
'unrelated, or 'self (OLD and NEW are the same commit)."
  (let ((repository (commit-owner old))
        (old (commit-id old))
        (new (commit-id new)))
    (cond ((graph-descendant? repository new old)
           'ancestor)
          ((oid=? old new)
           'self)
          ((graph-descendant? repository old new)
           'descendant)
          (else 'unrelated))))

(define (commit-descendant? new old)
  "Return true if NEW is the descendant of one of OLD, a list of commits."
  (let ((repository (commit-owner new))
        (new (commit-id new)))
    (any (lambda (old)
           (let ((old (commit-id old)))
             (or (graph-descendant? repository new old)
                 (oid=? old new))))
         old)))


;;
;;; Remote operations.
;;;

(define* (remote-refs url #:key tags?)
  "Return the list of references advertised at Git repository URL.  If TAGS?
is true, limit to only refs/tags."
  (define (ref? ref)
    ;; Like `git ls-remote --refs', only show actual references.
    (and (string-prefix? "refs/" ref)
         (not (string-suffix? "^{}" ref))))

  (define (tag? ref)
    (string-prefix? "refs/tags/" ref))

  (define (include? ref)
    (and (ref? ref)
         (or (not tags?) (tag? ref))))

  (define (remote-head->ref remote)
    (let ((name (remote-head-name remote)))
      (and (include? name)
           name)))

  (with-libgit2
   (call-with-temporary-directory
    (lambda (cache-directory)
      (let* ((repository (repository-init cache-directory))
             ;; Create an in-memory remote so we don't touch disk.
             (remote (remote-create-anonymous repository url)))
        (remote-connect remote)

        (let* ((remote-heads (remote-ls remote))
               (refs (filter-map remote-head->ref remote-heads)))
          ;; Wait until we're finished with the repository before closing it.
          (remote-disconnect remote)
          (repository-close! repository)
          refs))))))


;;;
;;; Checkouts.
;;;

;; Representation of the "latest" checkout of a branch or a specific commit.
(define-record-type* <git-checkout>
  git-checkout make-git-checkout
  git-checkout?
  (url     git-checkout-url)
  (branch  git-checkout-branch (default #f))
  (commit  git-checkout-commit (default #f))      ;#f | tag | commit
  (recursive? git-checkout-recursive? (default #f)))

(define (git-reference->git-checkout reference)
  "Convert the <git-reference> REFERENCE to an equivalent <git-checkout>."
  (git-checkout
   (url (git-reference-url reference))
   (commit (git-reference-commit reference))
   (recursive? (git-reference-recursive? reference))))

(define* (latest-repository-commit* url #:key ref recursive? log-port)
  ;; Monadic variant of 'latest-repository-commit'.
  (lambda (store)
    ;; The caller--e.g., (guix scripts build)--may not handle 'git-error' so
    ;; translate it into '&message' conditions that we know will be properly
    ;; handled.
    (catch 'git-error
      (lambda ()
        (values (latest-repository-commit store url
                                          #:ref ref
                                          #:recursive? recursive?
                                          #:log-port log-port)
                store))
      (lambda (key error . _)
        (raise (condition
                (&message
                 (message
                  (match ref
                    (('commit . commit)
                     (format #f (G_ "cannot fetch commit ~a from ~a: ~a")
                             commit url (git-error-message error)))
                    (('branch . branch)
                     (format #f (G_ "cannot fetch branch '~a' from ~a: ~a")
                             branch url (git-error-message error)))
                    (_
                     (format #f (G_ "Git failure while fetching ~a: ~a")
                             url (git-error-message error))))))))))))

(define-gexp-compiler (git-checkout-compiler (checkout <git-checkout>)
                                             system target)
  ;; "Compile" CHECKOUT by updating the local checkout and adding it to the
  ;; store.
  (match checkout
    (($ <git-checkout> url branch commit recursive?)
     (latest-repository-commit* url
                                #:ref (cond (commit
                                             `(tag-or-commit . ,commit))
                                            (branch
                                             `(branch . ,branch))
                                            (else '()))
                                #:recursive? recursive?
                                #:log-port (current-error-port)))))

;; Local Variables:
;; eval: (put 'with-repository 'scheme-indent-function 2)
;; End:
