;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2017 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2017, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2019 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2023 Thomas Ieong <th.ieong@free.fr>
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

(define-module (gnu tests mail)
  #:use-module (gnu tests)
  #:use-module (gnu packages mail)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services getmail)
  #:use-module (gnu services mail)
  #:use-module (gnu services networking)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (ice-9 ftw)
  #:export (%test-opensmtpd
            %test-exim
            %test-dovecot
            %test-getmail
            %test-rspamd))

(define %opensmtpd-os
  (simple-operating-system
   (service dhcpcd-service-type)
   (service opensmtpd-service-type
            (opensmtpd-configuration
             (config-file
              (plain-file "smtpd.conf" "
listen on 0.0.0.0
action inbound mbox
match from any for local action inbound
"))))))

(define (run-opensmtpd-test)
  "Return a test of an OS running OpenSMTPD service."
  (define vm
    (virtual-machine
     (operating-system (marionette-operating-system
                        %opensmtpd-os
                        #:imported-modules '((gnu services herd))))
     (port-forwardings '((1025 . 25)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (rnrs base)
                       (srfi srfi-64)
                       (ice-9 rdelim)
                       (ice-9 regex)
                       (gnu build marionette))

          (define marionette
            (make-marionette '(#$vm)))

          (define (read-reply-code port)
            "Read a SMTP reply from PORT and return its reply code."
            (let* ((line      (read-line port))
                   (mo        (string-match "([0-9]+)([ -]).*" line))
                   (code      (string->number (match:substring mo 1)))
                   (finished? (string= " " (match:substring mo 2))))
              (if finished?
                  code
                  (read-reply-code port))))

          (test-runner-current (system-test-runner #$output))
          (test-begin "opensmptd")

          (test-assert "service is running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'smtpd))
             marionette))

          (test-assert "mbox is empty"
            (marionette-eval
             '(and (file-exists? "/var/mail")
                   (not (file-exists? "/var/mail/root")))
             marionette))

          (test-eq "accept an email"
            #t
            (let* ((smtp (socket AF_INET SOCK_STREAM 0))
                   (addr (make-socket-address AF_INET INADDR_LOOPBACK 1025)))
              (connect smtp addr)
              ;; Be greeted.
              (read-reply-code smtp)              ;220
              ;; Greet the server.
              (write-line "EHLO somehost" smtp)
              (read-reply-code smtp)              ;250
              ;; Set sender email.
              (write-line "MAIL FROM: <someone>" smtp)
              (read-reply-code smtp)              ;250
              ;; Set recipient email.
              (write-line "RCPT TO: <root>" smtp)
              (read-reply-code smtp)              ;250
              ;; Send message.
              (write-line "DATA" smtp)
              (read-reply-code smtp)              ;354
              (write-line "Subject: Hello" smtp)
              (newline smtp)
              (write-line "Nice to meet you!" smtp)
              (write-line "." smtp)
              (read-reply-code smtp)              ;250
              ;; Say goodbye.
              (write-line "QUIT" smtp)
              (read-reply-code smtp)              ;221
              (close smtp)
              #t))

          (test-assert "mail arrived"
            (marionette-eval
             '(begin
                (use-modules (ice-9 popen)
                             (ice-9 rdelim))

                (define (queue-empty?)
                  (let* ((pipe (open-pipe* OPEN_READ
                                           #$(file-append opensmtpd
                                                          "/sbin/smtpctl")
                                           "show" "queue"))
                         (line (read-line pipe)))
                    (close-pipe pipe)
                    (eof-object? line)))

                (let wait ((n 20))
                  (cond ((queue-empty?)
                         (file-exists? "/var/mail/root"))
                        ((zero? n)
                         (error "root mailbox didn't show up"))
                        (else
                         (sleep 1) (wait (- n 1))))))
             marionette))

          (test-end))))

  (gexp->derivation "opensmtpd-test" test))

(define %test-opensmtpd
  (system-test
   (name "opensmtpd")
   (description "Send an email to a running OpenSMTPD server.")
   (value (run-opensmtpd-test))))


(define %exim-os
  (simple-operating-system
   (service dhcpcd-service-type)
   (service mail-aliases-service-type '())
   (service exim-service-type
            (exim-configuration
             (config-file
              (plain-file "exim.conf" "
primary_hostname = komputilo
domainlist local_domains = @
domainlist relay_to_domains =
hostlist   relay_from_hosts = localhost

never_users =

acl_smtp_rcpt = acl_check_rcpt
acl_smtp_data = acl_check_data

begin acl

acl_check_rcpt:
  accept
acl_check_data:
  accept
"))))))

(define (run-exim-test)
  "Return a test of an OS running an Exim service."
  (define vm
    (virtual-machine
     (operating-system (marionette-operating-system
                        %exim-os
                        #:imported-modules '((gnu services herd))))
     (port-forwardings '((1025 . 25)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (rnrs base)
                       (srfi srfi-64)
                       (ice-9 ftw)
                       (ice-9 rdelim)
                       (ice-9 regex)
                       (gnu build marionette))

          (define marionette
            (make-marionette '(#$vm)))

          (define (read-reply-code port)
            "Read a SMTP reply from PORT and return its reply code."
            (let* ((line      (read-line port))
                   (mo        (string-match "([0-9]+)([ -]).*" line))
                   (code      (string->number (match:substring mo 1)))
                   (finished? (string= " " (match:substring mo 2))))
              (if finished?
                  code
                  (read-reply-code port))))

          (define smtp (socket AF_INET SOCK_STREAM 0))
          (define addr (make-socket-address AF_INET INADDR_LOOPBACK 1025))

          (test-runner-current (system-test-runner #$output))
          (test-begin "exim")

          (test-assert "service is running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'exim))
             marionette))

          (sleep 1) ;; give the service time to start talking

          (connect smtp addr)
          ;; Be greeted.
          (test-eq "greeting received"
            220 (read-reply-code smtp))
          ;; Greet the server.
          (write-line "EHLO somehost" smtp)
          (test-eq "greeting successful"
            250 (read-reply-code smtp))
          ;; Set sender email.
          (write-line "MAIL FROM: test@example.com" smtp)
          (test-eq "sender set"
            250 (read-reply-code smtp))           ;250
          ;; Set recipient email.
          (write-line "RCPT TO: root@komputilo" smtp)
          (test-eq "recipient set"
            250 (read-reply-code smtp))           ;250
          ;; Send message.
          (write-line "DATA" smtp)
          (test-eq "data begun"
            354 (read-reply-code smtp))           ;354
          (write-line "Subject: Hello" smtp)
          (newline smtp)
          (write-line "Nice to meet you!" smtp)
          (write-line "." smtp)
          (test-eq "message sent"
            250 (read-reply-code smtp))           ;250
          ;; Say goodbye.
          (write-line "QUIT" smtp)
          (test-eq "quit successful"
            221 (read-reply-code smtp))           ;221
          (close smtp)

          (test-eq "the email is received"
            1
            (marionette-eval
             '(begin
                (use-modules (ice-9 ftw))
                (length (scandir "/var/spool/exim/msglog"
                                 (lambda (x) (not (string-prefix? "." x))))))
             marionette))

          (test-end))))

  (gexp->derivation "exim-test" test))

(define %test-exim
  (system-test
   (name "exim")
   (description "Send an email to a running an Exim server.")
   (value (run-exim-test))))

(define %dovecot-os
  (simple-operating-system
   (service dhcpcd-service-type)
   (service dovecot-service-type
            (dovecot-configuration
             (extensions (list dovecot-pigeonhole))
             (disable-plaintext-auth? #f)
             ;; Required for sieve!
             (postmaster-address "postmaster@komputilo")
             (ssl? "no")
             (auth-mechanisms '("anonymous"))
             (auth-anonymous-username "alice")
             (protocols
              (list (protocol-configuration
                     (name "imap")
                     (mail-plugins '("$mail_plugins" "imap_sieve"))
                     (imap-metadata? #t))))

             (plugin-configuration
              (plugin-configuration
               (entries (list
                         (cons 'sieve-global "/tmp")
                         (cons 'sieve-extensions "+editheader")

                         (cons 'imapsieve-mailbox1-name "*")
                         (cons 'imapsieve-mailbox1-causes "APPEND")
                         ;; Run the script *before* the user scripts
                         (cons 'imapsieve-mailbox1-before "file:/tmp/main.sieve")
                         ;; We want to automatically remove original email
                         (cons 'imapsieve-expunge-discarded "yes")

                         (cons 'sieve-trace-debug "yes")
                         (cons 'sieve-trace-dir "/tmp")
                         (cons 'sieve-trace-level "tests")
                         (cons 'sieve-plugins "sieve_imapsieve")
                         ;; You cannot run scripts anywhere you want
                         ;; Sieve allows you to only run scripts under
                         ;; sieve_pipe_bin_dir.
                         (cons 'sieve-pipe-bin-dir "/tmp")))))

             (mail-location
              (string-append "maildir:~/Maildir"
                             ":INBOX=~/Maildir/INBOX"
                             ":LAYOUT=fs"))))))

(define (run-dovecot-test)
  "Return a test of an OS running Dovecot service."
  (define vm
    (virtual-machine
     (operating-system (marionette-operating-system
                        %dovecot-os
                        #:imported-modules '((gnu services herd))))
     (port-forwardings '((8143 . 143)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (ice-9 iconv)
                       (ice-9 rdelim)
                       (rnrs base)
                       (rnrs bytevectors)
                       (srfi srfi-64))

          (define marionette
            (make-marionette '(#$vm)))

          (define* (message-length message #:key (encoding "iso-8859-1"))
            (bytevector-length (string->bytevector message encoding)))

          (define message "From: test@example.com\n\
Subject: Hello Nice to meet you!")

          (define sieve-script
            "require \"editheader\";\n
addheader \"X-Sieve-Filtered\" \"Guix\";
")
          ;; Install our sieve script
          (marionette-eval
           `(begin
              (with-output-to-file "/tmp/main.sieve"
                (lambda ()
                  (display ,sieve-script))))
           marionette)

          (test-runner-current (system-test-runner #$output))
          (test-begin "dovecot")

          ;; Wait for dovecot to be up and running.
          (test-assert "dovecot running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'dovecot))
             marionette))

          ;; Check Dovecot service's PID.
          (test-assert "service process id"
            (let ((pid
                   (number->string (wait-for-file "/var/run/dovecot/master.pid"
                                                  marionette))))
              (marionette-eval `(file-exists? (string-append "/proc/" ,pid))
                               marionette)))

          (test-assert "accept an email"
            (let ((imap (socket AF_INET SOCK_STREAM 0))
                  (addr (make-socket-address AF_INET INADDR_LOOPBACK 8143)))
              (connect imap addr)
              ;; Be greeted.
              (read-line imap) ;OK
              ;; Authenticate
              (write-line "a AUTHENTICATE ANONYMOUS" imap)
              (read-line imap) ;+
              (write-line "c2lyaGM=" imap)
              (read-line imap) ;OK
              ;; Create a TESTBOX mailbox
              (write-line "a CREATE TESTBOX" imap)
              (read-line imap) ;OK
              ;; Select mailbox. This is required so that dovecot did
              ;; synchronization correctly.
              (write-line "a SELECT TESTBOX" imap)
              ;; ("* FLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft)\r")
              ;; ("* OK [PERMANENTFLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft \\*)] Flags permitted.\r")
              ;; ("* 1 EXISTS\r")
              ;; ("* 1 RECENT\r")
              ;; ("* OK [UNSEEN 1] First unseen.\r")
              ;; ("* OK [UIDVALIDITY 1732177859] UIDs valid\r")
              ;; ("* OK [UIDNEXT 3] Predicted next UID\r")
              (for-each (lambda (n)
                          (read-line imap))
                        (iota 7))
              ;; Append a message to a TESTBOX mailbox
              (write-line (format #f "a APPEND TESTBOX {~a}"
                                  (number->string (message-length message)))
                          imap)
              (read-line imap) ;+
              (write-line message imap)
              (read-line imap) ;OK
              ;; Logout
              (write-line "a LOGOUT" imap)
              (close imap)
              #t))

          (test-equal "mail arrived"
            (string-join (list "X-Sieve-Filtered: Guix" message) "\n")
            (marionette-eval
             '(begin
                (use-modules (ice-9 ftw)
                             (ice-9 match)
                             (rnrs io ports))
                ;; XXX: We expect a new email in /cur directory
                (let ((TESTBOX/cur "/home/alice/Maildir/TESTBOX/cur/"))
                  (match (scandir TESTBOX/cur)
                    (("." ".." message-file)
                     (call-with-input-file
                         (string-append TESTBOX/cur message-file)
                       get-string-all)))))
             marionette))

          (test-end))))

  (gexp->derivation "dovecot-test" test))

(define %test-dovecot
  (system-test
   (name "dovecot")
   (description "Connect to a running Dovecot server.")
   (value (run-dovecot-test))))

(define %getmail-os
  (operating-system
    (inherit (simple-operating-system))

    ;; Set a password for the user account; the test needs it.
    (users (cons (user-account
                  (name "alice")
                  (password (crypt "testpass" "$6$abc"))
                  (comment "Bob's sister")
                  (group "users")
                  (supplementary-groups '("wheel" "audio" "video")))
                 %base-user-accounts))

    (services (cons* (service dhcpcd-service-type)
                     (service dovecot-service-type
                              (dovecot-configuration
                               (disable-plaintext-auth? #f)
                               (ssl? "no")
                               (auth-mechanisms '("anonymous" "plain"))
                               (auth-anonymous-username "alice")
                               (mail-location
                                (string-append "maildir:~/Maildir"
                                               ":INBOX=~/Maildir/INBOX"
                                               ":LAYOUT=fs"))))
                     (service getmail-service-type
                              (list
                               (getmail-configuration
                                (name 'test)
                                (user "alice")
                                (directory "/var/lib/getmail/alice")
                                (idle '("TESTBOX"))
                                (rcfile
                                 (getmail-configuration-file
                                  (retriever
                                   (getmail-retriever-configuration
                                    (type "SimpleIMAPRetriever")
                                    (server "localhost")
                                    (username "alice")
                                    (port 143)
                                    (extra-parameters
                                     '((password . "testpass")
                                       (mailboxes . ("TESTBOX"))))))
                                  (destination
                                   (getmail-destination-configuration
                                    (type "Maildir")
                                    (path "/home/alice/TestMaildir/")))
                                  (options
                                   (getmail-options-configuration
                                    (read-all #f))))))))
                     %base-services))))

(define (run-getmail-test)
  "Return a test of an OS running Getmail service."
  (define vm
    (virtual-machine
     (operating-system (marionette-operating-system
                        %getmail-os
                        #:imported-modules '((gnu services herd))))
     (port-forwardings '((8143 . 143)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (ice-9 iconv)
                       (ice-9 rdelim)
                       (rnrs base)
                       (rnrs bytevectors)
                       (srfi srfi-64))

          (define marionette
            (make-marionette '(#$vm)))

          (define* (message-length message #:key (encoding "iso-8859-1"))
            (bytevector-length (string->bytevector message encoding)))

          (define message "From: test@example.com\n\
Subject: Hello Nice to meet you!")

          (test-runner-current (system-test-runner #$output))
          (test-begin "getmail")

          ;; Wait for dovecot to be up and running.
          (test-assert "dovecot running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'dovecot))
             marionette))

          ;; Wait for getmail to be up and running.
          (test-assert "getmail-test running"
            (marionette-eval
             '(let* ((pw (getpw "alice"))
                     (uid (passwd:uid pw))
                     (gid (passwd:gid pw)))
                (use-modules (gnu services herd))

                (for-each
                 (lambda (dir)
                   (mkdir dir)
                   (chown dir uid gid))
                 '("/home/alice/TestMaildir"
                   "/home/alice/TestMaildir/cur"
                   "/home/alice/TestMaildir/new"
                   "/home/alice/TestMaildir/tmp"
                   "/home/alice/TestMaildir/TESTBOX"
                   "/home/alice/TestMaildir/TESTBOX/cur"
                   "/home/alice/TestMaildir/TESTBOX/new"
                   "/home/alice/TestMaildir/TESTBOX/tmp"))

                (start-service 'getmail-test))
             marionette))

          ;; Check Dovecot service's PID.
          (test-assert "service process id"
            (let ((pid
                   (number->string (wait-for-file "/var/run/dovecot/master.pid"
                                                  marionette))))
              (marionette-eval `(file-exists? (string-append "/proc/" ,pid))
                               marionette)))

          (test-assert "accept an email"
            (let ((imap (socket AF_INET SOCK_STREAM 0))
                  (addr (make-socket-address AF_INET INADDR_LOOPBACK 8143)))
              (connect imap addr)
              ;; Be greeted.
              (read-line imap) ;OK
              ;; Authenticate
              (write-line "a AUTHENTICATE ANONYMOUS" imap)
              (read-line imap) ;+
              (write-line "c2lyaGM=" imap)
              (read-line imap) ;OK
              ;; Create a TESTBOX mailbox
              (write-line "a CREATE TESTBOX" imap)
              (read-line imap) ;OK
              ;; Append a message to a TESTBOX mailbox
              (write-line (format #f "a APPEND TESTBOX {~a}"
                                  (number->string (message-length message)))
                          imap)
              (read-line imap) ;+
              (write-line message imap)
              (read-line imap) ;OK
              ;; Logout
              (write-line "a LOGOUT" imap)
              (close imap)
              #t))

          (sleep 1)

          (test-assert "mail arrived"
            (string-contains
             (marionette-eval
              '(begin
                 (use-modules (ice-9 ftw)
                              (ice-9 match)
                              (rnrs io ports))

                 (let ((TESTBOX/new "/home/alice/TestMaildir/new/"))
                   (match (scandir TESTBOX/new)
                     (("." ".." message-file)
                      (call-with-input-file
                          (string-append TESTBOX/new message-file)
                        get-string-all)))))
              marionette)
             message))

          (test-end))))

  (gexp->derivation "getmail-test" test))

(define %test-getmail
  (system-test
   (name "getmail")
   (description "Connect to a running Getmail server.")
   (value (run-getmail-test))))

(define %rspamd-os
  (simple-operating-system
   (service rspamd-service-type)))

(define (run-rspamd-test)
  "Return a test of an OS running Rspamd service."

  (define vm
    (virtual-machine
      (marionette-operating-system
        %rspamd-os
        #:imported-modules '((gnu services herd)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-64)
                       (gnu build marionette))

          (define marionette
            (make-marionette '(#$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "rspamd")

          (test-assert "service is running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'rspamd))
             marionette))

          (test-assert "rspamd socket ready"
            (wait-for-unix-socket
             "/var/lib/rspamd/rspamd.sock"
             marionette))

          (test-assert "rspamd log file"
            (wait-for-file "/var/log/rspamd/rspamd.log" marionette))

          ;; Check that we can access the web ui

          (test-equal "http-get"
            200
            (marionette-eval
              '(begin
                 (use-modules (web client)
                              (web response))
                 ;; HEAD returns 500 internal server error, so use GET even though
                 ;; only the headers are relevant
                 (response-code (http-get "http://localhost:11334")))
              marionette))

          (test-end))))

  (gexp->derivation "rspamd-test" test))

(define %test-rspamd
  (system-test
   (name "rspamd")
   (description "Basic rspamd service test.")
   (value (run-rspamd-test))))
