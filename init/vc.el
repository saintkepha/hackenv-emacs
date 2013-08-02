;; -*- emacs-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;_* Version Control & Backup initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq vc-command-messages nil)

;;;;;;;;;;;;;
;; Xtla    ;;
;;;;;;;;;;;;;

;; (add-to-list 'load-path (expand-file-name "~/Archives/xtla--main/lisp"))

;; (require 'xtla-autoloads)
;; (setq tla-arch-branch 'tla)   ; (change to baz once upgrade to bazaar)
;; (setq tla-switch-to-buffer-mode 'single-window) ; enough of these annoying popup buffers!
;; (setq tla-bookmarks-align 30)           ; some names are long...
;; (eval-after-load "xtla"
;;   `(if (boundp 'tla-changes-mode-map)
;;        (define-key tla-changes-mode-map "a" 'tla-add-log-entry)))

;;;;;;;;;;;;;;
;; Darcs    ;;
;;;;;;;;;;;;;;

;; (require 'darcsum)
;; (add-to-list 'vc-handled-backends 'DARCS)


;;;;;;;;;;;;;;;;
;; Subversion ;;
;;;;;;;;;;;;;;;;

(require 'psvn)
