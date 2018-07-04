;; -*- Mode: Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Development initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check http://www.sugarshark.de/elisp/init/devel.el and update as necessary    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;_* Compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'compile)
;; (require 'hl-compile)

(setq-default compile-command "make ")
(setq compile-auto-highlight t)
(setq compilation-scroll-output t)
(setq compilation-window-height 16)

(global-set-key [(f10)] 'next-error) ;; C-x `
(global-set-key [(control f10)] 'previous-error)

(add-hook 'compilation-mode-hook #'(lambda nil
									 (set (make-local-variable 'truncate-partial-width-windows) nil)
									 (setq truncate-lines nil)))

(define-key global-map (kbd "C-x c") 'compile)

;;;;;;;;;;;;;;
;; Cedet    ;;
;;;;;;;;;;;;;;

;; with emacs24 semantic is bundled in
;(load-file (expand-file-name "cedet-1.0pre6/common/cedet.el" emacs-packages-dir))
;(require 'semantic-gcc)
;(semantic-load-enable-minimum-features)
;(semantic-load-enable-code-helpers)
;(semantic-load-enable-gaudy-code-helpers)
;; (semantic-load-enable-excessive-code-helpers)
;; (semantic-load-enable-semantic-debugging-helpers)
(semantic-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Code Browser	 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; with emacs24 ecb is bundled in
;(add-to-list 'load-path (expand-file-name "ecb-2.32" emacs-packages-dir))
;;(require 'ecb-autoloads)

(global-ede-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;_* Changelog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [(control x) (control a)] 
				(lambda () 
				  (interactive) 
				  (add-change-log-entry nil (expand-file-name "ChangeLog") t nil)))

;; NXML included by default in emacs-23
; (load "~/Emacs/packages/nxml-mode-20041004/rng-auto.el")
(add-to-list 'auto-mode-alist
			 (cons (concat "\\." (regexp-opt '("xml" "xsl" "rng" "xhtml" "html") t) "\\'")
				   'nxml-mode))

;;;;;;;;;;;;
;; CSS	  ;;
;;;;;;;;;;;;

(autoload 'css-mode "css-mode" "Cascading Style Sheet mode")
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(eval-after-load "css-mode"
  '(progn
	 (setq cssm-indent-function 'cssm-c-style-indenter)
	 (setq cssm-indent-level 4)))

;;;;;;;;;;;;
;; PHP	  ;;
;;;;;;;;;;;;

(autoload 'php-mode "php-mode")
(add-to-list 'auto-mode-alist
			 (cons (concat "\\." (regexp-opt '("php" "php3" "php4" "phtml") t) "\\'")
				   'php-mode))

;;;;;;;;;;;;;;;;;;;
;; Javascript	 ;;
;;;;;;;;;;;;;;;;;;;

;; (require 'generic-x)
(when (locate-library "javascript")
  (autoload 'javascript-mode "javascript" nil t)
  (add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
  (setq javascript-indent-level 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;_* ASCII Table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ascii-table nil t)

(defun saint/ascii-table ()
  "Display an ASCII table in a small buffer"
  (interactive)
  (let ((i 32)
	(buffer (get-buffer "*ascii-table*")))
    (if buffer
	nil
      (setq buffer (create-file-buffer "*ascii-table*"))
      (set-buffer buffer)
      (while (< i 128)
	(insert (concat " " (char-to-string i)))
	(setq i (+ i 1)))
      (beginning-of-buffer)
      (fill-paragraph t)
      )
    (switch-to-buffer-other-window buffer)
    (shrink-window-if-larger-than-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;_* RFCView
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq get-rfc-local-rfc-directory (expand-file-name "~/doc/rfc/")
      get-rfc-open-in-new-frame nil)
 
(autoload 'rfcview-mode "rfcview" nil t)
(setq auto-mode-alist (cons '("/rfc[0-9]+\\.txt" . rfcview-mode)
			    auto-mode-alist))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;_* ASCII Table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ascii-table nil t)

(defun saint/ascii-table ()
  "Display an ASCII table in a small buffer"
  (interactive)
  (let ((i 32)
	(buffer (get-buffer "*ascii-table*")))
    (if buffer
	nil
      (setq buffer (create-file-buffer "*ascii-table*"))
      (set-buffer buffer)
      (while (< i 128)
	(insert (concat " " (char-to-string i)))
	(setq i (+ i 1)))
      (beginning-of-buffer)
      (fill-paragraph t)
      )
    (switch-to-buffer-other-window buffer)
    (shrink-window-if-larger-than-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;_* RFCView
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq get-rfc-local-rfc-directory (expand-file-name "~/doc/rfc/")
      get-rfc-open-in-new-frame nil)
 
(autoload 'rfcview-mode "rfcview" nil t)
(setq auto-mode-alist (cons '("/rfc[0-9]+\\.txt" . rfcview-mode)
			    auto-mode-alist))

(add-to-list 'load-path (expand-file-name "coffee-mode" emacs-packages-dir))
(require 'coffee-mode)
(defun coffee-custom ()
    "coffee-mode-hook"
	;; Emacs key binding
	(define-key coffee-mode-map (kbd "M-r") 'coffee-compile-buffer))

(add-hook 'coffee-mode-hook 'coffee-custom)

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist
			 (cons (concat "\\." (regexp-opt '("js") t) "\\'")
				   'js2-mode))

(load-file (expand-file-name "json-mode.el" emacs-packages-dir))
(add-to-list 'auto-mode-alist
			 (cons (concat "\\." (regexp-opt '("json") t) "\\'")
				   'json-mode))

(load-file (expand-file-name "yang-mode.el" emacs-packages-dir))
(defun my-yang-mode-hook ()
  "Configuration for YANG Mode. Add this to `yang-mode-hook'."
  (progn
		(c-set-style "BSD")
		(setq indent-tabs-mode nil)
		(setq c-basic-offset 2)
		(setq font-lock-maximum-decoration t)
		(font-lock-mode t)))

(add-hook 'yang-mode-hook 'my-yang-mode-hook)


(load-file (expand-file-name "yaml-mode.el" emacs-packages-dir))
