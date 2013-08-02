;; -*- Mode: Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (Emacs) and Lisp programming settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'parenface)
(require 'mic-paren)

(show-paren-mode nil) ;;; turn off default

;;; parenface stuff
(when (> (length (defined-colors)) 16)
  (set-face-foreground 'paren-face "brightblack"))

(setq paren-match-face    '(:foreground "magenta" :weight bold))
(setq paren-no-match-face '(:foreground "black" :background "red"))
(setf paren-priority 'close)

(paren-activate)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paredit settings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'paredit)

(add-hook 'paredit-terminal-mode-hook
          #'(lambda nil
              (define-key paredit-mode-map (kbd "C-c '") 'paredit-splice-sexp)
              (define-key paredit-mode-map (kbd "C-c <") 'paredit-splice-sexp-killing-backward)
              (define-key paredit-mode-map (kbd "C-c >") 'paredit-splice-sexp-killing-forward)
              (define-key paredit-mode-map (kbd "C-c {") 'paredit-backward-barf-sexp)
              (define-key paredit-mode-map (kbd "C-c }") 'paredit-forward-barf-sexp)
              (define-key paredit-mode-map (kbd "C-c (") 'paredit-backward-slurp-sexp)
              (define-key paredit-mode-map (kbd "C-c )") 'paredit-forward-slurp-sexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inferior Lisp & Slime    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "slime-2009-03-28" emacs-packages-dir))

;; (require 'hl-sexp)
(require 'outline)
(require 'slime)

(slime-setup)

(setq inferior-lisp-program "sbcl"
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      common-lisp-hyperspec-root "file:///Users/saint/Lisp/HyperSpec/"
      slime-startup-animation nil
      slime-enable-evaluate-in-emacs t)

(setq lisp-indent-function 'lisp-indent-function)

(defmacro defslime-start (name lisp)
  `(defun ,name ()
     (interactive)
     (slime ,lisp)))

(defslime-start sbcl "sbcl")

(add-hook 'lisp-mode-hook #'(lambda nil
                              (enable-paredit-mode)
                              ;; (hl-sexp-mode 1)
                              (set (make-local-variable 'lisp-indent-function)
                                   'common-lisp-indent-function)))

(add-hook 'emacs-lisp-mode-hook #'(lambda nil
                                    (enable-paredit-mode)))

(add-hook 'slime-repl-mode-hook #'(lambda nil
									(enable-paredit-mode)
									(define-key slime-repl-mode-map (kbd "<return>")     'slime-repl-return)
									(define-key slime-repl-mode-map (kbd "C-c <return>") 'slime-repl-closing-return)
									(define-key slime-repl-mode-map (kbd "C-c <up>")     'slime-repl-previous-input)
									(define-key slime-repl-mode-map (kbd "C-c <down>")   'slime-repl-next-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp Editing Functions    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun oa:end-of-defun (&optional arg)
  "Move to end of defun leaving point directly after the closing paren."
  (interactive "p")
  (forward-char 1)
  (end-of-defun arg)
  (backward-char 1))

(defun oa:reformat-defun ()
  "Reformat trailing parentheses Lisp-stylishly and reindent toplevel form."
  (interactive)
  (save-excursion
    (oa:end-of-defun)
    (slime-close-all-sexp)
    (slime-reindent-defun)))

(defun oa:backward-up-list-or-backward-sexp ()
  "Move point one list up or one sexp backwards if at toplevel. "
  (interactive)
  (condition-case c
      (backward-up-list)
    (scan-error (backward-sexp))))

(defun oa:down-list-or-forward-sexp ()
  "Move point one list down or one sexp forward if at lowest level. "
  (interactive)
  (condition-case c
      (down-list)
    (scan-error (progn (backward-up-list) (forward-sexp) (oa:down-list-or-forward-sexp)))))

(defun oa:mark-list (&optional arg)
  "Repeatedly select ever larger balanced expressions around the cursor.
Once you have such an expression marked, you can expand to the end of
the following expression with \\[mark-sexp] and to the beginning of the
previous with \\[backward-sexp]."
  (interactive "p")
  (condition-case c
      (progn
	(backward-up-list arg)
	(let ((end (save-excursion (forward-sexp) (point))))
	  (push-mark end nil t)))
    (scan-error (mark-sexp))))

(defun oa:close-all-sexp-and-reindent (&optional region)
  "Balance parentheses of open s-expressions at point.
Insert enough right parentheses to balance unmatched left parentheses.
Delete extra left parentheses.  Reformat trailing parentheses 
Lisp-stylishly."
  (interactive)
  (slime-close-all-sexp region)
  (newline-and-indent)
  (slime-reindent-defun))

(defun oa:move-past-close-and-reindent ()
  "Move past the closing paren and reindent. "
  (interactive)
  (condition-case c
      (move-past-close-and-reindent)
    (error (oa:close-all-sexp-and-reindent))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Lisp Keybindings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-lisp-key (keyspec command)
  `(progn
     ,@(mapcar (lambda (map)
		 `(define-key ,map ,keyspec ,command))
	       '(slime-mode-map lisp-mode-map emacs-lisp-mode-map))))

(define-lisp-key (kbd "M-n")     'mic-paren-forward-sexp)
(define-lisp-key (kbd "M-p")     'mic-paren-backward-sexp)
(define-lisp-key (kbd "M-a")     'beginning-of-defun)
(define-lisp-key (kbd "M-e")     'oa:end-of-defun)
(define-lisp-key (kbd "M-t")     'transpose-sexp)
(define-lisp-key (kbd "M-h")     'oa:mark-list)
(define-lisp-key (kbd "M-d")     'kill-sexp)
(define-lisp-key (kbd "M-k")     'kill-sexp)
(define-lisp-key (kbd "M-u")     'oa:backward-up-list-or-backward-sexp)
(define-lisp-key (kbd "M-l")     'oa:down-list-or-forward-sexp)
(define-lisp-key (kbd "M-q")     'oa:reformat-defun)
(define-lisp-key (kbd "M-DEL")   'backward-kill-sexp)
(define-lisp-key (kbd "C-M-l")   'down-list)
(define-lisp-key (kbd "C-M-u")   'backward-up-list)

(define-lisp-key (kbd "C-c TAB") 'lisp-complete-symbol)
(define-lisp-key (kbd "C-c RET") 'oa:move-past-close-and-reindent)

(define-lisp-key (kbd "C-h F") 'find-function-at-point)
(define-lisp-key (kbd "C-h V") 'find-variable-at-point)
(define-lisp-key (kbd "C-h l") 'find-library)
(define-lisp-key (kbd "C-h L") 'view-lossage)
(define-lisp-key (kbd "C-h !") 'describe-foo-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slime & Lisp Keybindings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key slime-mode-map (kbd "C-c C-d C-d")       'slime-describe-symbol)
(define-key slime-mode-map (kbd "TAB")               'slime-indent-and-complete-symbol)
(define-key slime-mode-map (kbd "C-c TAB")           'slime-complete-form)

(define-key lisp-mode-map  (kbd "C-c TAB")           'slime-complete-form)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp Keybindings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun describe-foo-at-point ()
  "Show the documentation of the Elisp function and variable near point.
This checks in turn:

-- for a function name where point is
-- for a variable name where point is
-- for a surrounding function call
"
  (interactive)
  (let (sym)
    ;; sigh, function-at-point is too clever.  we want only the first half.
    (cond ((setq sym (ignore-errors
		       (with-syntax-table emacs-lisp-mode-syntax-table
			 (save-excursion
			   (or (not (zerop (skip-syntax-backward "_w")))
			       (eq (char-syntax (char-after (point))) ?w)
			       (eq (char-syntax (char-after (point))) ?_)
			       (forward-sexp -1))
			   (skip-chars-forward "`'")
			   (let ((obj (read (current-buffer))))
			     (and (symbolp obj) (fboundp obj) obj))))))
	   (describe-function sym))
	  ((setq sym (variable-at-point)) (describe-variable sym))
	  ;; now let it operate fully -- i.e. also check the
	  ;; surrounding sexp for a function call.
	  ((setq sym (function-at-point)) (describe-function sym)))))

;; (define-key emacs-lisp-mode-map [(tab)]
;;   (make-region-indent-completion-function (lisp-complete-symbol)))
