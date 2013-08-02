;; -*- Mode: Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edit initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq case-fold-search t
      comment-multi-line t
      comment-style 'extra-line
      completion-ignore-case t
      cursor-in-non-selected-windows nil
      default-tab-width 4
      diff-switches "-u"
      even-window-heights nil
      font-lock-auto-fontify t
      font-lock-use-colors t
      font-lock-use-fonts nil
      indent-tabs-mode nil
      mouse-yank-at-point t
      next-line-add-newlines nil
      query-replace-highlight t
      read-file-name-completion-ignore-case t
      require-final-newline t
      resize-mini-windows nil
      resize-minibuffer-mode t ; make sure all contents of minibuffer is visible
      search-highlight t
      show-paren-delay 0
      show-paren-style 'parenthesis
      transient-mark-mode t
      truncate-lines t
      truncate-partial-width-windows t
      x-stretch-cursor nil)

(add-hook 'text-mode-hook #'(lambda nil
                              (turn-on-auto-fill)
                              (setq truncate-lines nil)))

;; note: C-x C-a calls add-changelog-entry!

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable narrowing    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;;;;;;;;;;;;;
;; Shell    ;;
;;;;;;;;;;;;;;
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;(require 'multi-term)

;;;;;;;;;;;;;;
;; Pager    ;;
;;;;;;;;;;;;;;
(require 'pager)
(define-key global-map (kbd "C-v")        'pager-page-down)
(define-key global-map [pgdown]           'pager-page-down)
(define-key global-map (kbd "M-v")        'pager-page-up)
(define-key global-map [pgup]             'pager-page-up)
;; (define-key global-map (kbd "ESC <up>")   'pager-row-up)
;; (define-key global-map (kbd "ESC <down>") 'pager-row-down)

;;;;;;;;;;;;;;;;;;;
;; Completion    ;;
;;;;;;;;;;;;;;;;;;;
(require 'hippie-exp)

;;;;;;;;;;;;;;;;;;;;;;
;; Generic Modes    ;;
;;;;;;;;;;;;;;;;;;;;;;
(setq generic-define-unix-modes t
      generic-define-mswindows-modes t)
;; (require 'generic-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windowing / Movement    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defadvice split-window-vertically (around saint first (arg) activate)
;;   "Make the other window contain the most recent buffer."
;;   (let ((has-arg arg))
;; 	(when has-arg
;; 	  (ad-set-arg 0 nil))
;; 	ad-do-it
;; 	(unless has-arg
;; 	  (set-window-buffer (next-window) (other-buffer)))))

;; (defadvice split-window-horizontally (around saint first (arg) activate)
;;   "Make the other window contain the most recent buffer."
;;   (let ((has-arg arg))
;; 	(when has-arg
;; 	  (ad-set-arg 0 nil))
;; 	ad-do-it
;; 	(when has-arg
;; 	  (set-window-buffer (next-window) (other-buffer)))))

(define-key global-map (kbd "C-x g") 'goto-line)
(define-key global-map (kbd "M-n")   'forward-paragraph)
(define-key global-map (kbd "M-p")   'backward-paragraph)

(winner-mode 1)                 ; allow window configuration to change

(defvar windmove-keymap (make-sparse-keymap
						 "Window move (up/down/left/right) arrow keys")
  "Keymap used to move between windows")
(define-key windmove-keymap (kbd "<up>")    'windmove-up)
(define-key windmove-keymap (kbd "<down>")  'windmove-down)
(define-key windmove-keymap (kbd "<left>")  'windmove-left)
(define-key windmove-keymap (kbd "<right>") 'windmove-right)
;;(define-key windmove-keymap [t] 'windmove-mode)

(define-minor-mode windmove-mode
  "Toggle window moving mode.
With no argument htis command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When windmove mode is enabled, the keypad \(left, right, up,
down\) moves the cursor between windows.

\\{windmove-keymap}"
  nil "-Windmove" windmove-keymap :global t)

(define-key mode-specific-map (kbd "w") 'windmove-mode)

;;;;;;;;;;;;;;;;;
;; Boxquote    ;;
;;;;;;;;;;;;;;;;;
(require 'boxquote)
(autoload 'rs-info-insert-current-node "rs-info"
  "Insert reference to current Info node using STYPE in buffer." t nil)
(autoload 'rs-info-boxquote "rs-info"
  "Yank text (from an info node), box it and use current info node as title."
  t nil)
(autoload 'rs-info-reload "rs-info" "Reload current info node." t nil)
(autoload 'rs-info-insert-node-for-variable "rs-info"
  "Insert a custom style info node for the top level form at point." t nil)
(defalias 'boxquote-info 'rs-info-boxquote)

;;;;;;;;;;;;;;;
;; Ispell	 ;;
;;;;;;;;;;;;;;;

(defvar ispell-keymap (make-sparse-keymap
					   "Spelling (w)ord, (r)egion, (b)uffer, (m)essage, (c)hange dictionary, (f)lyspell")
  "Keymap used to globally access spell checking functions")

(define-key ispell-keymap (kbd "TAB") 'ispell-complete-word)
(define-key ispell-keymap (kbd "w")   'ispell-word)
(define-key ispell-keymap (kbd "r")   'ispell-region)
(define-key ispell-keymap (kbd "b")   'ispell-buffer)
(define-key ispell-keymap (kbd "m")   'ispell-message)
(define-key ispell-keymap (kbd "c")   'ispell-change-dictionary)
(define-key ispell-keymap (kbd "s")   'ispell-comments-and-strings)
(define-key ispell-keymap (kbd "f")   'flyspell-mode)
(define-key ispell-keymap (kbd "?")   'describe-prefix-bindings)

(define-key mode-specific-map (kbd "s") ispell-keymap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various Keybindings for Editing    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'comment-box "newcomment" "Make a comment box" t nil)

(define-key global-map (kbd "C-c C-c") 'comment-region)
(define-key global-map (kbd "C-c C-b") 'comment-box)

(define-key global-map (kbd "C-x f")   'fill-paragraph)
(define-key global-map (kbd "C-x F")   'fill-region)

(define-key global-map (kbd "C-c u")   'capitalize-word)
(define-key global-map (kbd "C-c d")   'downcase-word)

(define-key global-map (kbd "C-x %")   'query-replace-regexp)

(define-key global-map (kbd "C-x C-z") 'repeat)
