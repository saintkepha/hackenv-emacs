;; -*- Mode: Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is an emacs/xemacs configuration.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(or (boundp 'running-xemacs)
    (defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Path Variables    ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defconst emacs-home-dir (expand-file-name "~/Emacs/")
  "Home directory for emacs")

(defconst emacs-packages-dir (expand-file-name "packages/" emacs-home-dir)
  "Directory for user installed emacs packages")

(defconst emacs-mylisp-dir (expand-file-name "my-lisp" emacs-home-dir)
  "Contains homegrown libraries")

(defconst emacs-init-dir (expand-file-name "init/" emacs-home-dir)
  "Contains emacs initialization lisp files")

(defconst emacs-info-dir (expand-file-name "info/" emacs-home-dir)
  "Directory where user lisp package info files are installed to")

;;;;;;;;;;;;;;;;;;
;; Load path    ;;
;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path emacs-home-dir)
(add-to-list 'load-path emacs-packages-dir)
(add-to-list 'load-path emacs-mylisp-dir)

;;;;;;;;;;;;;;;;;;
;; Info path    ;;
;;;;;;;;;;;;;;;;;;

(require 'info)
(add-to-list 'Info-default-directory-list emacs-info-dir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update and load autoloads    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (load "autoload-make")

;; (let ((generated-autoload-file (concat emacs-home-dir "autoloads.el")))
;;   (unless (file-exists-p generated-autoload-file)
;;     (update-autoloads-for-lisp-dir)))

;; (load "autoloads")

;;;;;;;;;;;;;;;;;;;;;
;; Load modules    ;;
;;;;;;;;;;;;;;;;;;;;;

(dolist (name '("globals"               ; global settings
                "edit"                  ; general editing & movement
                "files"                 ; files & buffers
                "devel"                 ; general development settings
                "c"
                "vc"                    ; version control
                "lisp"                  ; lisp programming settings
                "www"                   ; interaction with the web
                "bbdb"                  ; contact database
;                "gnus"                  ; the mail beast
                "chat"                  ; emacs chat client
;                "muse"                  ; dynamic authoring
;                "organizer"             ; diary, calendar, remember, org
;                "planner"               ; project manager
                "multimedia"            ; play music...
                ))
  (let ((module-name (concat emacs-init-dir name ".el")))
	(when (file-exists-p module-name)
	  (load module-name))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Desktop restore    ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(require 'desktop)

(setq history-length 100)
(add-to-list 'desktop-globals-to-save 'file-name-history)
(add-to-list 'desktop-globals-to-save 'vc-comment-ring)
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'erc-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
