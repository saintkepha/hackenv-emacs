;; -*- emacs-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs-Internals and Essentials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file (expand-file-name "customize.el" emacs-home-dir))
(load custom-file)

(unless window-system
  (require 'my-console))

;; setup default frame
(setq initial-frame-alist 
	  '((top . 0) (left . 0)
		(font . "-*-bitstream vera sans mono-medium-r-normal-*-*-100-100-100-m-*-*-*")
		(background-mode . dark)
		(cursor-type . bar)
		))
(setq default-frame-alist initial-frame-alist)
(setq frame-title-format (list (invocation-name) ":" " (%b)"))

;; Makes things a little bit more concise.
(fset 'yes-or-no-p 'y-or-n-p)

;; use extended compund-text coding for X clipboard
(set-selection-coding-system 'compound-text-with-extensions)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Variables    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(setq max-lisp-eval-depth 4096
      enable-local-variables t
      auto-save-default t
      make-backup-files t
      backup-by-copying t
      delete-auto-save-files t
      inhibit-startup-message t
      default-major-mode 'indented-text-mode
      visible-bell nil
      display-time-24hr-format t
      dired-dwim-target t)

;;;;;;;;;;;;;;;;;;;;;
;; Global Modes    ;;
;;;;;;;;;;;;;;;;;;;;;

(dolist (item '((global-font-lock-mode 1)
                (scroll-bar-mode -1)
                (menu-bar-mode -1)
                (tool-bar-mode -1)
                (icomplete-mode 1)
                (ido-mode 1)
                (column-number-mode 1)
                (line-number-mode 1)
                (blink-cursor-mode -1)
                (auto-compression-mode 1)
                (display-time)
                (show-paren-mode t)
                (auto-image-file-mode 1)))
  (when (fboundp (car item))
    (apply (car item) (cdr item))))

;;;;;;;;;;;;;
;; User    ;;
;;;;;;;;;;;;;

(setq user-full-name    "Peter K. Lee"
      user-mail-address "saint@corenova.com")

;;;;;;;;;;;;;;;;;;;
;; Mode setup    ;;
;;;;;;;;;;;;;;;;;;;

(dolist (spec '(("[A-Z]+$" . text-mode)
		("\\.el$" . emacs-lisp-mode)
		("\\.asd$" . lisp-mode)
		("\\.log$" . text-mode)
		("\\.py$" . python-mode)
		("\\.inputrc$" . sh-mode)
		("\\.js$" . java-mode)
		("\\.sql$" . sql-mode)
		("ChangeLog$" . change-log-mode)))
    (add-to-list 'auto-mode-alist spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default global keybindings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map (kbd "C-h a") 'apropos)
(define-key global-map (kbd "C-h A") 'apropos-command)
(define-key global-map (kbd "C-h M") 'woman)

(define-key global-map (kbd "C-z")     'advertised-undo)
(define-key global-map (kbd "C-x C-u") 'advertised-undo)
(define-key global-map (kbd "C-M-z")   'suspend-frame)

(define-key global-map (kbd "C-x C-c")   nil)
;; (define-key global-map (kbd "C-x M-c")   'server-save-buffers-kill-display)
;; (define-key global-map (kbd "C-x C-M-c") 'server-save-buffers-kill-display)
(define-key global-map (kbd "C-x M-c")   'save-buffers-kill-emacs)
(define-key global-map (kbd "C-x C-M-c") 'save-buffers-kill-emacs)

(define-key global-map (kbd "C-x 5 n") 'saint/make-new-frame)
(define-key global-map (kbd "C-x 5 g") 'select-frame-by-name)
(define-key global-map (kbd "C-x 5 s") 'set-frame-name)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Functions    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(defun saint/make-new-frame (frame-name)
  "create a new frame and ask for name"
  (interactive "sNew frame name: ")
  (make-frame-command)
  (set-frame-name frame-name))

(defun insert-timestamp ()
  "Insert the date and time into the current buffer at the
  current location."
  (interactive)
  (insert (format-time-string "%e %b %Y %H:%M:%S")))

(defun insert-date ()
  "Insert the date at point."
  (interactive)
   (insert (format-time-string "%Y-%m-%d")))
