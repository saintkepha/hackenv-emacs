;; -*- emacs-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BBDB setup  (Info-goto-node "(bbdb)Top")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "bbdb-2.35/lisp" emacs-packages-dir))

(require 'bbdb-autoloads)
(require 'bbdb)
(require 'bbdb-com)
(require 'bbdb-hooks)

(setq bbdb-file (expand-file-name "bbdb" emacs-home-dir))

(bbdb-initialize 'gnus 'sendmail 'message 'sc)

;;;_+ Basic configuration

(setq bbdb-use-pop-up t) ;;; put 'horiz if want side-by-side

;; Sometimes people have work and personal e-mail addresses.
(setq bbdb-complete-name-allow-cycling t)
;; Ignore subnets
(setq bbdb-canonicalize-redundant-nets-p t)
;; Always save
(setq bbdb-offer-save 'auto)
;; My screen is not that wide, so I need information displayed on multiple lines
(setq bbdb-display-layout 'full-multi-line)
;; The window should be as small as possible, though.
(setq bbdb-pop-up-target-lines 7)

;(setq bbdb-electric-p t)
(setq bbdb/gnus-summary-mark-known-posters t)
(setq bbdb/gnus-summary-known-poster-mark "^")

(setq bbdb-default-area-code nil)
(setq bbdb-default-country "USA")

;;; use BBDB to auto create notes
(add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)

(setq saint/bbdb-auto-create-list '("partner-list@" "gardeners-reports@"))

(setq bbdb-ignore-most-messages-alist `(("To" . ,(regexp-opt saint/bbdb-auto-create-list))
                                        ("Cc" . ,(regexp-opt saint/bbdb-auto-create-list)))

      bbdb-ignore-some-messages-alist '(("From" . "gmane\\.org"))

      bbdb-auto-notes-alist '(("Organization" (".*" company 0))
                              ("User-Agent" (".*" e-mailer 0))
                              ("X-Mailer" (".*" e-mailer 0))
                              ("X-Newsreader" (".*" e-mailer 0))
                              ("Subject" (".*" last-received 0 t)))
 
      bbdb/mail-auto-create-p 'bbdb-ignore-most-messages-hook
      bbdb/news-auto-create-p nil)

;;; Custom bbdb record updating behavior
(setq gnus-groups-bbdb-update-records-mode-alist
      '(("^nntp.*:gmane\\." . 'annotating)
        ("^nn.*:INBOX\\.spam" . 'searching)))

(setq gnus-groups-bbdb-always-add-addresses-alist
      '(("^nntp.*:gmane\\." . nil)
        ("^nn.*:INBOX\\.friends" . t)
        ("^nn.*:INBOX\\.list\\." . 'ask)))

(defun regexp-assoc (key list)
  "Return first item of LIST with a car that matches KEY"
  (let ((l list)
        res)
    (while (and l (not res))
      (if (string-match (caar l)
                        key)
          (setq res (car l)))
      (setq l (cdr l)))
    res))

(defadvice bbdb/gnus-update-records (around saint activate preactivate)
  "Dynamically set bbdb update behavior based on group.  This
  allows us to bypass certain groups (i.e. gmane) that mangles
  net addresses and prevent bbdb from asking us whether to add
  that address to our existing record."
  (let* ((current-group (when (and (boundp 'gnus-summary-buffer)
                                   (buffer-live-p gnus-summary-buffer))
                          (with-current-buffer gnus-summary-buffer
                            gnus-newsgroup-name)))
         (bbdb/gnus-update-records-mode
          (or (when (boundp 'gnus-groups-bbdb-update-records-mode-alist)
                (assoc-default
                 current-group gnus-groups-bbdb-update-records-mode-alist #'string-match))
              bbdb/gnus-update-records-mode))
         (bbdb-always-add-addresses
          (if (boundp 'gnus-groups-bbdb-always-add-addresses-alist)
              (let ((res (regexp-assoc current-group gnus-groups-bbdb-always-add-addresses-alist)))
                (if res (cdr res)
                    bbdb-always-add-addresses))
              bbdb-always-add-addresses)))
    ad-do-it))

(put 'subjects 'field-separator "\n")
(put 'notes 'field-separator "; ")

;; I love my Gnus
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'message-setup-hook 'bbdb-define-all-aliases)
(add-hook 'bbdb-change-hook 'bbdb-timestamp-hook)
(add-hook 'bbdb-create-hook 'bbdb-creation-date-hook)
