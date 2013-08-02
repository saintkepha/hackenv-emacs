;;;_ + Gnus customization file

(setq gnus-agent nil) ;;; never use the agent!

;; gnus mail, news, etc. directories
(setq gnus-directory "~/Gnus")
(setq gnus-kill-files-directory "~/Gnus/scores")
(setq message-directory "~/Gnus/mail") ; where messages are tracked
(setq gnus-article-save-directory "~/Gnus/saved")
(setq gnus-dribble-directory "~/.gnus-dribble") ;
(setq gnus-cache-directory "~/Gnus/cache")

;; caching configuration
(setq gnus-use-cache t)
(setq gnus-cache-active-file "~/Gnus/cache/active")
(setq gnus-cacheable-groups ".*"
      gnus-uncacheable-groups "^nnmbox")
(setq gnus-cache-enter-articles '(ticked dormant unread))

;; primary news source
(setq gnus-select-method '(nnnil ""))

;; list other mail/news accounts here
(setq gnus-secondary-select-methods
	  '((nntp "localhost")
        (nnimap "corenova" ;; my corenova e-mail acct
				(nnimap-address "mail.corenova.com")
				(nnimap-stream ssl)
				(nnimap-nov-is-evil t))
		(nnimap "clearpath"
				(nnimap-user "plee")
				(nnimap-address "SPESMAL01")
				(nnimap-stream network)  ; gssapi, kerberos4, ssl, network,...
				(nnimap-nov-is-evil t))))

;; my nnimap split rules (combined with BBDB!)
(setq nnimap-split-inbox '("INBOX"))
(setq nnimap-split-predicate "UNDELETED")
(setq nnimap-split-crosspost nil)

;;; run splitting only for "corenova" mail account!
(setq nnimap-split-rule '(("corenova" ("INBOX" bbdb/gnus-split-method)))
      bbdb/gnus-split-nomatch-function 'nnimap-split-fancy
      bbdb/gnus-split-myaddr-regexp gnus-ignored-from-addresses
      nnimap-split-fancy
      `(|
        ("X-BeenThere" "partner-list@.*" "INBOX.list.partners")
        ("X-BeenThere" "encfs-users@.*" "INBOX.list.encfs")
        ("X-BeenThere" "gardeners-reports@.*" "INBOX.list.gardeners-reports")
        ("X-BeenThere" "maemo-developers@.*" "INBOX.list.maemo-developers")
        (from "dreamhost" "INBOX.dreamhost")
        (from "americanexpress\\|ingdirect\\|bankofamerica\\|citibank\\|discovercard" "INBOX.finance")
		("X-Spam-Flag" "YES" "INBOX.spam")
        (any "elixar" "INBOX.elixar")
        (any ,gnus-ignored-from-addresses "INBOX.misc")
        "INBOX.spam"))

;;; my posting styles -- pretty cool how this works, check:
;;; http://www.gnus.org/manual/gnus_139.html
(setq gnus-posting-styles
      '((".*" ; default for all
         (signature-file "~/.signature")
         (organization "Corenova, LLC")
         ("X-Home-Page" "http://www.corenova.com"))
        ("INBOX.baby"
         (name "Your Love")
         (to "My Sweetheart <pinktiara@gmail.com>")
         (signature "Love, Everlasting."))
        ("INBOX.friends\\|INBOX.list.partners"
         (signature "For trusted eyes only!"))
        ("INBOX.list.gardeners-reports"
         (signature-file "~/.signature-lisper"))
		("clearpath.*" ; for WORK
		 (from "Peter K. Lee <plee@clearpathnet.com>")
		 (signature-file "~/.signature-clearpath")
		 (organization "ClearPath Networks, Inc.")
		 ("X-Home-Page" "http://www.clearpathnet.com"))
        ("gmane.lisp.cl-gardeners"
         (to "Tending the Lisp Garden <gardeners@lispniks.com>")
         (signature-file "~/.signature-lisper")
         (newsgroups nil))
        ("gmane.emacs.wiki.general"
         (to "Emacs Wiki Discussion List <emacs-wiki-discuss@nongnu.org>")
         (signature-file "~/.signature-lisper")
         (newsgroups nil))))

;; gnus archiving sent messages (just store it locally)
(setq gnus-message-archive-method
	  '(nnfolder "archive"
        (nnfolder-inhibit-expiry t)
        (nnfolder-get-new-mail nil)))

(setq gnus-message-archive-group
	  '(("clearpath.*" "nnimap+clearpath:Sent Items")
		(if (message-news-p)
			(concat "news-" (format-time-string "%Y" (current-time)))
		  (concat "mail-" (format-time-string "%Y-%m" (current-time)) ) )))

;; default SMTP configuration
(setq smtpmail-default-smtp-server "mail.corenova.com")
(setq smtpmail-smtp-server smtpmail-default-smtp-server)
(setq smtpmail-smtp-service 587)
(setq smtpmail-local-domain "corenova.com")
(setq smtpmail-auth-credentials 
	  '(("mail.corenova.com" 587 "saint" nil)))
(setq smtpmail-debug-info t)

;; dynamically select my SMTP server based on from: address
(add-hook 'message-send-hook 'fs-change-smtp)

;; based on something I found at my.gnus.org
(defun fs-change-smtp ()
  "Change the SMTP server according to the current from line."
  (save-excursion
    (let ((from
           (save-restriction
             (message-narrow-to-headers)
             (message-fetch-field "from"))))
      (message "From is `%s', setting `smtpmail-smtp-server' to `%s'"
               from
               (cond
                ((string-match "saint@elixar\\.com\\|saint@elixar\\.net" from)
                 ;; use mail.elixar.com relay server
                 (message "Using mail.elixar.com")
				 (setq user-full-name "Peter K. Lee"
					   user-mail-address "saint@elixar.com")
                 (setq smtpmail-sendto-domain "elixar.com"
                       smtpmail-local-domain "elixar.com"
					   smtpmail-smtp-service 587
					   smtpmail-smtp-server "mail.elixar.com"))
				
                ((string-match "saint@corenova\\.com" from)
                 ;; use mail.corenova.com relay server
                 (message "Using mail.corenova.com")
				 (add-to-list 'smtpmail-auth-credentials '("mail.corenova.com" 587 "saint" nil))
				 (setq user-full-name "Peter K. Lee"
					   user-mail-address "saint@corenova.com")
                 (setq smtpmail-sendto-domain "corenova.com"
                       smtpmail-local-domain "corenova.com"
					   smtpmail-smtp-service 587
					   smtpmail-smtp-server "mail.corenova.com"))
				
				((string-match "plee@clearpathnet\\.com" from)
				 (message "Using SPESMAL01")
				 (setq user-full-name "Peter K. Lee"
					   user-mail-address "plee@clearpathnet.com")
				 (add-to-list 'smtpmail-auth-credentials '("SPESMAL01" 25 "plee" nil))
				 (setq smtpmail-sendto-domain "clearpathnet.com"
					   smtpmail-local-domain  "cleapathnet.com"
					   smtpmail-smtp-service 25
					   smtpmail-smtp-server "SPESMAL01"))
                (t
				 ;; use localhost tunnel
				 (message "Using built-in tunnel")
				 (setq smtpmail-smtp-service 25
					   smtpmail-smtp-server "localhost")))))))

 ;; message variables
(setq message-default-headers "From: Peter K. Lee <saint@corenova.com>")
(setq message-from-style (quote (angles)))
(setq message-interactive t)
(setq message-mail-alias-type nil)
(setq message-send-mail-partially-limit nil)
(setq mm-text-html-renderer (quote w3m))   ; '(mm-text-html-renderer (quote html2text))
(setq mm-discouraged-alternatives (quote ("application/msword" "text/richtext" "text/html")))

 ;; gnus variables
(setq gnus-use-undo t)
(setq gnus-ignored-from-addresses "saint@corenova\\.com\\|plee@clearpathnet\\.com")
(setq gnus-asynchronous t)
(setq gnus-novice-user nil)
(setq gnus-use-trees nil)
(setq gnus-use-adaptive-scoring (quote (line)))
(setq gnus-topic-display-empty-topics nil)
(setq gnus-summary-mark-below -100)
(setq gnus-summary-expunge-below -100)
(setq gnus-score-expiry-days 90)
(setq gnus-score-default-duration (quote p))
(setq gnus-save-newsrc-file nil)
(setq gnus-save-killed-list nil)
(setq gnus-read-newsrc-file nil)
(setq gnus-ignored-mime-types (quote ("application/x-pkcs7-signature" "application/ms-tnef" "text/x-vcard")))
(setq gnus-group-default-list-level 4)
(setq gnus-gcc-mark-as-read t)
(setq gnus-extra-headers (quote (To)))
(setq gnus-default-article-saver (quote gnus-summary-write-to-file))
(setq gnus-default-adaptive-score-alist (quote ((gnus-dormant-mark (from 20) (subject 100)) (gnus-ticked-mark (subject 30)) (gnus-read-mark (subject 30)) (gnus-del-mark (subject -150)) (gnus-catchup-mark (subject -50)) (gnus-killed-mark (subject -500)) (gnus-expirable-mark (from -1000) (subject -1000)))))
(setq gnus-article-date-lapsed-new-header t)
(setq gnus-always-read-dribble-file t)
(setq gnus-suppress-duplicates t)

(let ((header-list
       '("^Organization:"
         "^Newsgroups:"
         "^From:"
         "^Subject:"
         "^Summary:"
         "^Keywords:"
         "^\\(Followup\\|Reply\\)-To:"
         "^\\(X-\\|Apparently-\\)?To:"
         "^\\(X-\\)?[BFG]?Cc:"
         "^Date:"
         "^User-Agent:"
         "^X-\\(Mailer\\|Newsreader\\):"
         "^X-Sent:")))
  (setq gnus-visible-headers header-list)
  (setq gnus-sorted-header-list header-list))

;; Gnus and Mail functions & hooks

(setq send-mail-function          'smtpmail-send-it) ; for mail
(setq message-send-mail-function  'smtpmail-send-it) ; for gnus

(setq gnus-score-find-score-files-function (quote (gnus-score-find-hierarchical)))
 ;; gnus hooks, methods, functions
(setq gnus-subscribe-newsgroup-method     (quote gnus-subscribe-topics))
(setq gnus-article-sort-functions         (quote ((not gnus-article-sort-by-date))))
(setq gnus-thread-sort-functions          (quote (gnus-thread-sort-by-number gnus-thread-sort-by-subject (not gnus-thread-sort-by-date) gnus-thread-sort-by-total-score)))
(setq gnus-split-methods                  (quote ((gnus-article-archive-name) (gnus-article-nndoc-name))))
(setq gnus-sort-gathered-threads-function (quote gnus-thread-sort-by-date))
(setq gnus-simplify-subject-functions     (quote (gnus-simplify-subject-fuzzy)))
(setq gnus-group-sort-function            (quote gnus-group-sort-by-method))
(setq gnus-generate-tree-function         (quote gnus-generate-horizontal-tree))
(setq check-mail-summary-function         (quote check-mail-box-summary))

(setq message-mode-hook  (quote (flyspell-mode auto-fill-mode footnote-mode)))

(add-hook 'message-setup-hook                  'gnus-fix-gcc-header)
(add-hook 'message-sent-hook                   'gnus-score-followup-article)
(add-hook 'gnus-started-hook                   `(lambda nil (run-hooks 'gnus-after-getting-new-news-hook)))
(add-hook 'gnus-select-group-hook              'gnus-group-set-timestamp)
(add-hook 'gnus-summary-prepare-exit-hook      (lambda ()
                                                 (my-gnus-summary-expire-articles) (gnus-summary-expire-articles)))
(add-hook 'gnus-summary-exit-hook 'gnus-summary-bubble-group)

(add-hook 'gnus-suspend-gnus-hook              'gnus-group-save-newsrc)
(add-hook 'gnus-group-mode-hook                'gnus-topic-mode)
(add-hook 'gnus-after-getting-new-news-hook    (lambda ()
                                                 (gnus-score-groups) (gnus-group-list-groups) (gnus-group-save-newsrc)
                                                 (gnus-display-time-event-handler) (gnus-fixup-nnimap-unread-after-getting-new-news)))

;; switch around the g with M-g
(eval-after-load "gnus-topic"
  '(progn
    (define-key gnus-topic-mode-map (kbd "g") 'gnus-topic-get-new-news-this-topic)
    (define-key gnus-topic-mode-map (kbd "M-g") 'gnus-group-get-new-news)))

;; for really cool BBDB mail expansion based on the mail-alias field
(add-hook 'message-setup-hook 'bbdb-define-all-aliases)

;;; Gnus Citation
;; Trivial Cite
(autoload 'trivial-cite "tc" t t)

(setq message-cite-function 'trivial-cite)
(add-hook 'mail-citation-hook 'trivial-cite)

(setq tc-remove-signature "^\\(-- \\|--\\|--- \\|---\\|___ \\|___\\|\\.\\.\\.\\)$")
(setq tc-debug-level       1
      tc-cleanup-cited-marks-p t
      tc-fill-long-lines   't
      tc-make-attribution 'saint/tc-attribution)

(defun saint/tc-attribution ()
  "Generate an attribution."
  (let ((date (cdr (assoc "date" tc-strings-list)))
        (name (or (cdr (assoc "real-name" tc-strings-list))
                  (cdr (assoc "email-addr" tc-strings-list)))))
    (concat ",----[ " name " wrote on " date " ]\n")))

;;;
;; Gnus Layout - Window Configuration

(setq gnus-use-full-window t) ;;; not using full window is peril indeed :(

;; Determine layout when viewing server
(gnus-add-configuration
 '(server (vertical 1.0
           (group 1.0)
           (server 0.25 point)
           (if gnus-carpal '(server-carpal 2)))))

;; Determine layout when viewing group
(gnus-add-configuration
 '(group (vertical 1.0
          (group 1.0 point))))

;; Determine layout when viewing summary
(gnus-add-configuration
 '(summary (vertical 1.0
            (group 0.25)
            (summary 1.0 point))))

;; Determine layout when viewing article
(gnus-add-configuration
 '(article (vertical 1.0
            (group 0.25)
            (summary 0.25 point)
            (if gnus-carpal (summary-carpal 4))
            (article 1.0))))

;; New message composition takes place in a different frame
;; no... that leaves a frame laying around that's inconvenient in terminal

;; (gnus-add-configuration
;;  '(message (frame 1.0
;;             (if (not (buffer-live-p gnus-summary-buffer))
;;                 (car (cdr (assoc 'group gnus-buffer-configuration)))
;;                 (car (cdr (assoc 'summary gnus-buffer-configuration))))
;;             (vertical ((user-position . t) (top . 1) (left . 1)
;;                        (name . "Message"))
;;              (message 1.0 point)))))

;;;_ Display Configuration

(load "gnus-dynamic-subject") ; replace %s with %us;

(setq gnus-parameters
	  '(("INBOX.*"
         (gnus-show-threads t))
		("INBOX\\.Sent"
		 (gnus-show-threads nil))
		("drafts"
		 (gnus-show-threads nil))
		))

;; (defface gnus-folder-groups-face
;;   '((t (:foreground "DarkSeaGreen1" :bold t))) "all folders', esp. nnfolders' group face")
;; (defface gnus-mail-groups-face
;;   '((t (:foreground "DarkSeaGreen4" :bold t))) "all mails', esp. nnmails' group face")
;; (defface gnus-news-groups-face
;;   '((t (:foreground "Green4" :bold t))) "all news', esp. nntp's group face")

(setq gnus-signature-separator (quote ("^-- $" "^-- *$" "^_____+$")))

;; (setq gnus-tree-minimize-window nil)
(setq gnus-treat-strip-trailing-blank-lines t)
(setq gnus-treat-strip-multiple-blank-lines t)
(setq gnus-treat-strip-leading-blank-lines t)
(setq gnus-treat-strip-cr t)
(setq gnus-treat-date-lapsed (quote head))

;; Gnus thread parameters
(setq gnus-show-threads t
	  gnus-thread-indent-level 2
      gnus-thread-hide-subtree nil
	  gnus-thread-ignore-subject t
      gnus-summary-display-arrow nil
      gnus-summary-display-while-building t)

 ;; gnus threading characters
(when (null standard-display-table)
  (setq standard-display-table (make-display-table)))

(dolist (c '((?\207 . ?q) (?\216 . ?x) (?\212 . ?t) (?\203 . ?m)))
  (aset standard-display-table (car c)
        (vector (create-glyph (concat "\e(0" "\e[35m" ; magenta
                                      (char-to-string (cdr c))
                                      "\e[0m" "\e(B")))))

;; (setq gnus-sum-thread-tree-vertical "\216"
;;       gnus-sum-thread-tree-root ""
;;       gnus-sum-thread-tree-false-root ""
;;       gnus-sum-thread-tree-indent " "
;;       gnus-sum-thread-tree-single-indent ""
;;       gnus-sum-thread-tree-leaf-with-other "\212\207>"
;;       gnus-sum-thread-tree-single-leaf "\203\207>")

(setq
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-single-indent ""
 gnus-sum-thread-tree-vertical "|"
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-leaf-with-other "|-> "
 gnus-sum-thread-tree-single-leaf "`-> ")

(copy-face 'default 'my-from-string)
(set-face-foreground 'my-from-string "yellow")
(setq gnus-face-1 'my-from-string)

(copy-face 'default 'my-flag-string)
(set-face-foreground 'my-flag-string "red")
(setq gnus-face-3 'my-flag-string)

(setq gnus-group-line-format "%M%S%m%P%4I,%4T,%5R;%6N: %(%G%) %-75=%l%o %ud %L%p%B\n"
      gnus-topic-line-format "%i[ %(%{%n%}%) - %A/%g/%l ] %v\n"
	  gnus-summary-line-format "%6L @ %-5k  %&user-date;  %2{|%}%(%3{%R%}%1{ %-25,25ua %}%3{%*%U%}%)%2{|%}%2{ %}%u&thread;%2{%ut %}%N:%(%us%)\n"
;;	  gnus-summary-line-format "%6V.%-5i %6L @ %-5k  %&user-date;  %2{|%}%1{%R%z %-25,25ua %*%U%}%2{|%} %2{ %}%3{%u&thread;%}%2{%ut%} %N:%(%S%)\n"
)

(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "--- %k:%M")
        ((+ (gnus-seconds-today) 86400) . "yes %k:%M")
        (604800 . "%a %k:%M")
        ((gnus-seconds-month) . "%a %m/%d")
        ((gnus-seconds-year) . "%b %d   ")
        (t . "%b %d %y")))

;; turn on egocentric/superego mode
;(require 'superego)
;(add-hook 'gnus-summary-prepare-hook 'superego-mode)
;(add-hook 'gnus-article-prepare-hook 'superego-mode)

;; automatic mail scan without manual effort.
;;
;; level-specified group scanner.
(defun gnus-demon-scan-mail-or-news-and-update (level)
  "Scan for new mail, updating the *Group* buffer."
  (let ((win (current-window-configuration)))
	(unwind-protect
		(save-window-excursion
		  (save-excursion
			(when (gnus-alive-p)
			  (save-excursion
				(set-buffer gnus-group-buffer)
				(gnus-group-get-new-news level)))))
	  (set-window-configuration win))))
;;
;; level 2: only mail groups are scanned.
(defun gnus-demon-scan-mail-and-update ()
  "Scan for new mail, updating the *Group* buffer."
  (gnus-demon-scan-mail-or-news-and-update 2))
;(gnus-demon-add-handler 'gnus-demon-scan-mail-and-update 15 t)
;;
;; level 3: mail and local news groups are scanned.
(defun gnus-demon-scan-news-and-update ()
  "Scan for new mail, updating the *Group* buffer."
  (gnus-demon-scan-mail-or-news-and-update 3))
;(gnus-demon-add-handler 'gnus-demon-scan-news-and-update 30 t)

;;(gnus-demon-init) ; ack, this automatic stuff fires at the worst times

;;
;; MIME configuration
(add-to-list 'mm-inline-media-tests '
			 ("application/msword" mm-inline-text identity))
(add-to-list 'mm-automatic-external-display "application/msword")
(add-to-list 'mm-automatic-display "application/msword")
; uncomment for displaying-inline w/o mime button
;(add-to-list 'mm-attachment-override-types  "application/msword")

; if mailcap does not work
(defun mm-inline-msword (handle)
  "Return foo bar"
  (let (text)
    (with-temp-buffer
      (mm-insert-part handle)
      (call-process-region (point-min) (point-max) "antiword" t t nil "-")
      (setq text (buffer-string)))
    (mm-insert-inline handle text))) 
;; (add-to-list 'mm-inlined-types "application/msword")
;; (add-to-list 'mm-inline-media-tests
;;                  '("application/msword" mm-inline-msword identity))

; when the sucker provided the wrong mimetype, use the file extension
(add-to-list 'mm-inlined-types "application/octet-stream")
(add-to-list 'mm-inline-media-tests
         '("application/octet-stream" mm-inline-msword
           (lambda (handle)
			 (let ((name (mail-content-type-get (mm-handle-disposition handle) 'filename)))
			   (and name (equal ".doc" (substring name -4 nil)))
			   ))))

;;
(defun gnus-user-format-function-thread (dummy)
  (propertize gnus-tmp-thread-tree-header-string 'gnus-face t))

;;;_ + Expire articles with expiry mark immediately

(defun my-gnus-summary-expire-articles ()
  "Expire all articles that are marked as expirable in the current group."
  (interactive)
  (when (and (not gnus-group-is-exiting-without-update-p)
	     (gnus-check-backend-function
	      'request-expire-articles gnus-newsgroup-name))
    ;; This backend supports expiry.
    (let* ((expirable (setq gnus-newsgroup-expirable
			    (sort gnus-newsgroup-expirable '<)))
	   (nnmail-expiry-target
	    (or (gnus-group-find-parameter gnus-newsgroup-name
					   'expiry-target)
		nnmail-expiry-target))
	   es)
      (when expirable
	;; There are expirable articles in this group, so we run them
	;; through the expiry process.
	(gnus-message 6 "Expiring articles...")
	(unless (gnus-check-group gnus-newsgroup-name)
	  (error "Can't open server for %s" gnus-newsgroup-name))
	;; The list of articles that weren't expired is returned.
	(save-excursion
	  (let ((nnmail-expiry-wait-function nil)
		(nnmail-expiry-wait 'immediate))
	    (setq es (gnus-request-expire-articles expirable
						   gnus-newsgroup-name)))
	  (setq gnus-newsgroup-expirable es)
	  ;; We go through the old list of expirable, and mark all
	  ;; really expired articles as nonexistent.
	  (unless (eq es expirable) ;If nothing was expired, we don't mark.
	    (let ((gnus-use-cache nil))
	      (dolist (article expirable)
		(when (and (not (memq article es))
			   (gnus-data-find article))
		  (gnus-summary-mark-article article gnus-canceled-mark)
		  (run-hook-with-args 'gnus-summary-article-expire-hook
				      'delete
				      (gnus-data-header
				       (assoc article (gnus-data-list nil)))
				      gnus-newsgroup-name
				      nil
				      nil))))))
	(gnus-message 6 "Expiring articles...done")))))

;;;_ + Scoring

(defun gnus-score-groups ()
  (interactive)
  (save-excursion
    (let (info newsrc group entry)
      (setq newsrc (cdr gnus-newsrc-alist))
      (while (setq info (pop newsrc))
	(setq group (gnus-info-group info)
	      entry (gnus-gethash group gnus-newsrc-hashtb))
	(when (and
	       (<= (gnus-info-level info) gnus-level-subscribed)
	       (and (car entry)
		    (or (eq (car entry) t)
			(not (zerop (car entry)))))
	       (and (string-match "^nnml:list" group)
		    (not (string-match
			  "^nnml:list\\.\\(unsolicited\\|spam\\)" group))))
	  (ignore-errors
	    (gnus-summary-read-group group nil t))
	  (when (and gnus-summary-buffer
		     (buffer-live-p gnus-summary-buffer)
		     (eq (current-buffer) (get-buffer gnus-summary-buffer)))
	    (gnus-summary-exit)))))))


;;; USER FORMAT FUNCTIONS

(defun gnus-user-format-function-Z (header)
  (let ((to (cdr (assq 'To (mail-header-extra header))))
	(newsgroups (cdr (assq 'Newsgroups (mail-header-extra header))))
	(mail-parse-charset gnus-newsgroup-charset)
	(mail-parse-ignored-charsets
	 (save-excursion
	   (set-buffer gnus-summary-buffer)
	   gnus-newsgroup-ignored-charsets)))
    (cond
     ((and to gnus-ignored-from-addresses
	   (string-match gnus-ignored-from-addresses
			 (mail-header-from header)))
      (concat "-> "
	      (or (car (funcall gnus-extract-address-components
				(funcall
				 gnus-decode-encoded-word-function to)))
		  (funcall gnus-decode-encoded-word-function to))))
     ((and newsgroups gnus-ignored-from-addresses
	   (string-match gnus-ignored-from-addresses
			 (mail-header-from header)))
      (concat "=> " newsgroups))
     (t
      (let* ((from (mail-header-from header))
	     (data (condition-case nil
		       (mail-extract-address-components from)
		     (error nil)))
	     (name (car data))
	     (net (car (cdr data))))
	(if name
	    name 
	  net))))))

(defsubst dot-gnus-tos (time)
  "Convert TIME to a floating point number."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (car (cdr (cdr time))) 0) 1000000.0)))

(defun gnus-user-format-function-S (header)
  "Return how much time it's been since something was sent."
  (condition-case err
      (let ((date (mail-header-date header)))
		(if (> (length date) 0)
			(let* ((then (dot-gnus-tos
						  (apply 'encode-time (parse-time-string date))))
				   (now (dot-gnus-tos (current-time)))
				   (diff (- now then)))
			  (cond ((>= diff (* 86400.0 7.0 52.0))
					 (if (>= diff (* 86400.0 7.0 52.0 10.0))
						 (format "%3dY" (floor (/ diff (* 86400.0 7.0 52.0))))
					   (format "%3.1fY" (/ diff (* 86400.0 7.0 52.0)))))
					((>= diff (* 86400.0 30.0))
					 (if (>= diff (* 86400.0 30.0 10.0))
						 (format "%3dM" (floor (/ diff (* 86400.0 30.0))))
					   (format "%3.1fM" (/ diff (* 86400.0 30.0)))))
					((>= diff (* 86400.0 7.0))
					 (if (>= diff (* 86400.0 7.0 10.0))
						 (format "%3dw" (floor (/ diff (* 86400.0 7.0))))
					   (format "%3.1fw" (/ diff (* 86400.0 7.0)))))
					((>= diff 86400.0)
					 (if (>= diff (* 86400.0 10.0))
						 (format "%3dd" (floor (/ diff 86400.0)))
					   (format "%3.1fd" (/ diff 86400.0))))
					((>= diff 3600.0)
					 (if (>= diff (* 3600.0 10.0))
						 (format "%3dh" (floor (/ diff 3600.0)))
					   (format "%3.1fh" (/ diff 3600.0))))
					((>= diff 60.0)
					 (if (>= diff (* 60.0 10.0))
						 (format "%3dm" (floor (/ diff 60.0)))
					   (format "%3.1fm" (/ diff 60.0))))
					(t
					 (format "%3ds" (floor diff)))))))
    (error "    ")))

(eval-when-compile
  (defvar thread)
  (defvar gnus-tmp-level))

(defun gnus-user-format-function-t (header)
  (let ((tcount (gnus-summary-number-of-articles-in-thread
		 (and (boundp 'thread) (car thread)) gnus-tmp-level)))
    (if (> tcount 1)
	(number-to-string tcount)
      " ")))

(defun gnus-user-format-function-T (dummy)
  "Tell Gnus to not put fancy highlighting for thread"
  (propertize gnus-tmp-thread-tree-header-string 'gnus-face t))

(defun gnus-user-format-function-d (ignore)
  "new Timestamp format"
  (setq gnus-tmp-timestamp
        (gnus-group-timestamp gnus-tmp-group))
  (format-time-string "%a, %d.%m.%Y %H:%M:%S, %W" gnus-tmp-timestamp))

(defun ff-add-al (email str) "Returns str if emails does contains only
one address, and str + \"& al.\" if email contains more than one
address"
  (and str
	   (if (string-match "," (gnus-mail-strip-quoted-names email)) (concat str " & al.") str)))

(defun ff-explicit-name (email)
  "Returns a string identity for the first address in email. The
identity is got in bbdb if possible or from the address itself
with mail-extract-address-components. The sufix \"& al.\" is
added if there are more than one address."
  (if email
      (let* ((data (condition-case () (mail-extract-address-components email) (error nil)))
			 (name (car data))
			 (net (car (cdr data))))
    
		(or (and data 
				 (or (and bbdb/gnus-summary-prefer-bbdb-data bbdb/gnus-summary-prefer-real-names
						  (let* ((record (bbdb-search-simple name (if (and net bbdb-canonicalize-net-hook)
                                                                      (bbdb-canonicalize-address net)
                                                                      net))))
							(and record bbdb/gnus-summary-prefer-real-names
                                 (ff-add-al email (bbdb-record-name record)))))
					 (and data
                          (concat "((" (ff-add-al email (or name net email)) "))"))))
			"**undefined**")
		)
    nil)
  )

(defun gnus-user-format-function-a (header)
  (let* ((from (gnus-header-from header)))
    (if (string-match gnus-ignored-from-addresses from)
		(let ((recipient (gnus-extra-header 'To)))
		  ;; no recipient, looks like into a newsgroup
		  (if (string= recipient "") (ff-explicit-name from)
              ;; There is an undisclosed-recipients, looks like posted
              ;; in a newsgroups and cc: to ourself
              (if (string= recipient "undisclosed-recipients:;") (concat "to: " (gnus-extra-header 'Newsgroups))
                  ;; There is a disclosed recipient, show its name
                  (concat "to: " (ff-explicit-name recipient)))))
        ;; The sender is not me, show who it is
        (ff-explicit-name from))))

;;;_ + Smart Gcc: header generation

(defun gnus-fix-gcc-header ()
  "Called to fix any problems with the Gcc header."
  (let ((gcc (gnus-fetch-field "Gcc")))
    (when gcc
      (when (string-match "\\`nndoc:\\(.+?\\)-[0-9]+\\'" gcc)
		(setq gcc (match-string 1 gcc))
		(message-remove-header "Gcc")
		(message-add-header (format "Gcc: %s" gcc)))
      (cond
	  ((string-match "\\`nnml:list\\." gcc)
	   (let* ((split-addr
			   (function
				(lambda (field)
				  (let ((value (gnus-fetch-field field)))
					(if value
						(mapcar 'downcase
								(split-string
								 (gnus-mail-strip-quoted-names value)
								 "\\s-*,\\s-*")))))))
			  (addrs (append (funcall split-addr "To")
							 (funcall split-addr "Cc")))
			  (to-addr (or (gnus-group-get-parameter gcc 'to-address)
						   (gnus-group-get-parameter gcc 'to-list)))
			  (list-addr (and to-addr (downcase to-addr))))
		 (when (and list-addr (member list-addr addrs))
		   (message-remove-header "Gcc")
		   (message-add-header "FCC: ~/Gnus/listposts"))))))))

;;;_ + Archive groups

(defcustom gnus-archive-groups nil
  "*A list of regexp->group pairs, for compounding archive groups."
  :type '(repeat (cons regexp (string :tag "Group")))
  :group 'nnmail)

;; (defun gnus-determine-archive-group (group)
;;   (let* (lookup
;; 	 (group
;; 	  (cond
;; 	   ((string-match "^nntp.*:\\(.*\\)" group)
;; 	    (setq lookup t)
;; 	    (match-string 1 group))
;; 	   ((or (null group)
;; 		(string= group ""))
;; 	    "nnfolder:sent")
;; 	   ((string-match "^\\([^:.]+\\.[^:]+\\)" group)
;; 	    (setq lookup t)
;; 	    (match-string 1 group))
;; 	   (t group)))
;; 	 (table gnus-archive-groups))
;;     (if lookup
;;       (while table
;; 	(if (string-match (caar table) group)
;; 	    (setq group (cdar table)
;; 		  table nil)
;; 	  (setq table (cdr table)))))
;;     group))

;;; keybindings

;;;_ + mml

(eval-after-load "mml"
  '(define-key mml-mode-map [(control ?c) (control ?m) ?w]
     'muse-message-markup))

;;;_ + Cleanup all Gnus buffers on exit

(defun exit-gnus-on-exit ()
  (if (and (fboundp 'gnus-group-exit)
	   (gnus-alive-p))
      (with-current-buffer (get-buffer "*Group*")
	(let (gnus-interactive-exit)
	  (gnus-group-exit)))))

(add-hook 'kill-emacs-hook 'exit-gnus-on-exit)

;;;_ + RSS integration



;; The following code may be useful to open an nnrss url directly from the summary
;; buffer.

;; (require 'browse-url)
                                                          
;; (defun browse-nnrss-url( arg )
;;   (interactive "p")
;;   (let ((url (assq nnrss-url-field
;;                    (mail-header-extra
;;                     (gnus-data-header
;;                      (assq (gnus-summary-article-number)
;;                            gnus-newsgroup-data))))))
;;     (if url
;;         (progn
;;           (browse-url (cdr url))
;;           (gnus-summary-mark-as-read-forward 1))
;;       (gnus-summary-scroll-up arg))))

;; (eval-after-load "gnus"
;;   #'(define-key gnus-summary-mode-map (kbd "<RET>") 'browse-nnrss-url))
;; ;;(define-key gnus-summary-mode-map (kbd "<RET>") 'browse-nnrss-url)
;; (add-to-list 'nnmail-extra-headers nnrss-url-field)
