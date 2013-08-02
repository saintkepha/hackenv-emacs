;; -*- Mode: Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personal Organizer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst organizer-dir (expand-file-name "~/Organizer")
  "Directory where all personal organization related files go")

(defvar organizer-keymap (make-sparse-keymap
                         "Organizer (c)alendar, (d|D)iary, (r)emember (n)ote, (a)genda")
  "Keymap used to globally access calendar, diary, remember related functions")

(define-key mode-specific-map (kbd "o") organizer-keymap)
(define-key organizer-keymap [??] 'describe-prefix-bindings)

(define-key organizer-keymap [?d] 'diary) ;; show fancy diary
(define-key organizer-keymap [?D] 'show-all-diary-entries) ;; show raw diary
(define-key organizer-keymap [?c] 'calendar) ;; show calendar

(define-key organizer-keymap [?r] 'remember-in-org)
(define-key organizer-keymap [?R] 'remember-region)
(define-key organizer-keymap [?b] 'remember-buffer)
(define-key organizer-keymap [?u] 'remember-url)
(define-key organizer-keymap [?l] 'remember-location)

(define-key organizer-keymap [?a] 'org-agenda)
(define-key organizer-keymap [?g] 'org-goto-org-file)
(define-key organizer-keymap [?s] 'org-store-link)

;;;;;;;;;;;;;;;;;
;; Calendar    ;;
;;;;;;;;;;;;;;;;;

(require 'calendar)

(setq mark-holidays-in-calendar t
      mark-diary-entries-in-calendar t)

;;;;;;;;;;;;;;
;; Diary    ;;
;;;;;;;;;;;;;;

(setq diary-file (expand-file-name "diary" organizer-dir))

(setq view-diary-entries-initially t
      number-of-dary-entries 7
      diary-schedule-interval-time 60
      diary-schedule-expand-grid t
      diary-default-schedule-start-time 1000
      diary-default-schedule-stop-time  1800
      diary-schedule-start-time 1000
      diary-schedule-stop-time 1800)

(add-hook 'diary-display-hook 'fancy-diary-display)

(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
(add-hook 'list-diary-entries-hook 'include-other-diary-files)
(add-hook 'list-diary-entries-hook 'sort-diary-entries t)
(add-hook 'mark-diary-entries-hook 'mark-included-diary-files)

;;;;;;;;;;;;;;;;;
;; Org mode    ;;
;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "~/Archives/Attic/org@arch"))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(autoload 'org-diary "org" "Diary entries from Org mode")
(autoload 'org-goto-org-file "org" "Jump to a page in Organizer")

(setq org-directory organizer-dir)
(setq org-default-notes-file (expand-file-name "Notes.org" org-directory))
(setq org-agenda-include-diary t)
(setq org-CUA-compatible t)             ; my term does not process S-...
(setq org-calendar-show-agenda t)       ; show agenda with calendar

(add-hook 'org-mode-hook (lambda () 
                           (turn-on-auto-fill)
						   (dolist (org-file (file-expand-wildcards (expand-file-name "*.org" org-directory)))
							 (org-add-file org-file))
                           (define-key org-mode-map (kbd "C-c h") 'hide-sublevels)
                           (define-key org-mode-map (kbd "C-c H") 'show-all)
						   (define-key org-mode-map (kbd "RET") 'org-return)))

;;; hmm not good to have it bound to RET...
(defun org-newline-or-follow-link (&optional in-emacs)
  (interactive "P")
  (condition-case err (org-open-at-point in-emacs)
	(error (if (string= "No link found" (cadr err))
			   (newline)
			 (error (cadr err))))))

;;;;;;;;;;;;;;;;;
;; Remember    ;;
;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "~/Archives/remember--sacha"))

(require 'remember)
(require 'remember-bbdb)
;; (require 'remember-bibl)
(require 'remember-diary)

(setq remember-use-bbdb t
      remember-save-after-remembering t)

(add-hook 'remember-mode-hook #'(lambda nil
                                  (muse-mode)
                                  (use-local-map remember-mode-map)
                                  (setq major-mode 'remember-mode
                                        mode-name "Remember")
                                  (turn-on-auto-fill)))

(defun remember-in-org (&optional prefix)
  "Called to dynamically remember stuff in org context"
  (interactive)
  (setq remember-handler-functions '(org-remember-handler))
  (setq remember-annotation-functions '(org-remember-annotation))
  (remember prefix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timeclock Buffer usage    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'timeclock-buffer)
;; (timeclock-buffer-activate t)

