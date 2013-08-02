;; -*- Mode: Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar find-keymap (make-sparse-keymap
					 "Find (g)rep, (s)earch, (f)ile at point, (d|D)ired, (t|T)ag")
  "Keymap used to globally access find related functions")

(define-key find-keymap [??] 'describe-prefix-bindings)

(define-key find-keymap [?f] 'find-file-at-point)
(define-key find-keymap [?d] 'find-dired)
(define-key find-keymap [?D] 'find-grep-dired)
(define-key find-keymap [?s] 'grep-find)
(define-key find-keymap [?g] 'grep)
(define-key find-keymap [?t] 'find-tag)
(define-key find-keymap [?T] 'tags-search)
(define-key find-keymap [?l] 'find-library)

(define-key mode-specific-map [?f] find-keymap)

(define-key global-map (kbd "C-x d")   'dired)
(define-key global-map (kbd "C-x C-d") 'dired)
(define-key global-map (kbd "C-x C-b") 'ibuffer)

;;; XXX - look into igrep

;; (define-key find-keymap [?g] 'igrep)
;; (define-key find-keymap [?G] 'igrep-find)

;;;;;;;;;;;;;;;;;;
;; Bookmarks    ;;
;;;;;;;;;;;;;;;;;;

(setq bookmark-default-file (expand-file-name "bookmarks.el" emacs-home-dir)
	  bookmark-save-flag 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;_* File & Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ido)
(require 'iswitchb)
(require 'iswitchb-highlight)

(ido-mode t)                            ; ido is good :)
(iswitchb-mode 1)
(setq iswitchb-case t) ; turn off case-sensitivity

(add-hook 'iswitchb-make-buflist-hook 'saint/iswitchb-summaries-to-end)

(defun saint/iswitchb-summaries-to-end ()
  "Move the summaries to the end of the list.
This is my personal function which can be hooked on to
`iswitchb-make-buflist-hook'.  Any buffer matching the regexps
`^*' or `Summary' or `output\*$'are put to the end of the list."
  (let ((summaries (delq nil
						 (mapcar
						  (lambda (x)
							(if (string-match "^*Help\\|Apropos\\|Tree\\|Messages\\|Completions\\|Directory\\|tramp\\|output\\*$" x)
								x))
						  iswitchb-temp-buflist))))
    (iswitchb-to-end summaries)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Archives with Dired	  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load "tar-mode"
  '(progn
	 (define-key tar-mode-map (kbd "U") 'tar-untar-buffer)))

(defadvice tar-untar-buffer (before saint (directory) activate)
  "Allow untar to select a DIRECTORY."
  (interactive "Duntar destination:")
  (cd directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load crypt, which is a package for automatically decoding and reencoding
;;; files by various methods - for example, you can visit a .Z or .gz file,
;;; edit it, and have it automatically re-compressed when you save it again.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'crypt++)

;; (setq crypt-encryption-type 'gpg
;;       crypt-confirm-password t	   ; make sure new passwords are correct
;;       crypt-encryption-file-extension "\\(Secure\\)$\\|\\(\\.enc\\|\\.crypt\\|\\.pgp\\|\\.gpg\\)$")
;; (crypt-rebuild-tables)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;_* Utility Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Florian Weimer in <877lhs476b.fsf@deneb.cygnus.argh.org>
(defun global-change-directory (from to)
  "Change directory of all buffers with default-directory FROM to TO."
  (interactive "DGlobally change directory from: \nDTo: ")
  (let ((bufs (buffer-list))
	(from (expand-file-name from)))
    (while bufs
      (with-current-buffer (car bufs)
	(when (equal from (expand-file-name default-directory))
	  (setq default-directory to)))
      (setq bufs (cdr bufs)))))

(setq tramp-default-method "ssh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;_* Shell-Command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq-default start-command "")
;; (setq dm-term-command "xterm")

;; ;; ^M weglassen
;; (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;_* List Lines In Buffer(s)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'color-occur)
;; (require 'color-moccur)

