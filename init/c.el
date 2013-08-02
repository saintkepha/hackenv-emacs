;; -*- Mode: Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C programming environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst personal-c-style
  '((c-basic-offset             . 4)
    (c-tab-always-indent        . t)
    (c-comment-only-line-offset . 0)
	(c-auto-newline             . nil)
	(c-indent-level             . 0)
	(case-fold-search           . nil)
	(tab-width                  . 4)
	(indent-tabs-mode           . nil)
    ;; for symbols also see doc of `c-offsets-alist'
    (c-hanging-braces-alist 
     . ((brace-list-intro)
		(brace-list-open)
		(brace-list-close)
		(brace-entry-open)
		(extern-lang-open after)))
	;; (defun-open . ole-newline-after-opening-brace)
	;; (defun-close . ole-newline-before-closing-brace)
	;; (inline-open . ole-newline-after-opening-brace)
	;; (inline-close . ole-newline-before-closing-brace)
	;; (class-open . ole-newline-after-opening-brace)
	;; (class-close . ole-newline-before-and-after-closing-brace)
	;; (substatement-open . ole-newline-after-opening-brace)
	;; (substatement-close . ole-newline-before-and-after-closing-brace)
	;; (block-open . ole-newline-after-opening-brace)
	;; (block-close . ole-newline-before-and-after-closing-brace)
	;; (inexpr-class-open . ole-newline-after-opening-brace)
	;; (inexpr-class-close . ole-newline-before-closing-brace)))
		
    (c-hanging-colons-alist 
     . ((member-init-intro before)
		(inher-intro)
		;; (case-label after)
		(label after)
		(access-label after)))
    (c-cleanup-list 
     . (scope-operator
		brace-else-brace
		brace-elseif-brace
		brace-catch-brace
		list-close-comma
		defun-close-semi))
    (c-offsets-alist 
     . (
		(arglist-intro . +)
		;; (arglist-cont-nonempty . (ole-c-lineup-arglist-operators ++))
		(arglist-close . c-lineup-arglist)
		(func-decl-cont . +)
		(inher-cont . c-lineup-java-inher)
		(inexpr-class . +)
		;; (topmost-intro-cont . +)
		(inline-open . 0)
		(substatement-open . 0)
		(case-label        . 2)
		(block-open        . 0)
		(knr-argdecl-intro . -)))
	(c-hanging-semi&comma-criteria 
	 . (c-semi&comma-no-newlines-at-all))
	) "My C Programming Style")

;; make style available
(c-add-style "personal" personal-c-style)

(defun personal-c-mode-hook ()
  (c-set-style "personal" personal-c-style)
  (c-set-offset 'member-init-intro '++)
  (c-toggle-hungry-state 0)
  (c-toggle-auto-state 1)
  (c-setup-paragraph-variables)
  (outline-minor-mode 1)

  (set (make-local-variable 'truncate-partial-width-windows) t)
  
  (require 'paredit)
  (define-key c-mode-map (kbd "(")    'paredit-open-list)
  (define-key c-mode-map (kbd ")")    'self-insert-command)
  (define-key c-mode-map (kbd "\"")   'paredit-doublequote)
  (define-key c-mode-map (kbd "M-\\") 'delete-horizontal-space)
  (define-key c-mode-map (kbd "C-m")  'c-context-line-break)
  (define-key c-mode-map (kbd "{")    'electric-c-leftbrace))

(add-hook 'c-mode-common-hook 'personal-c-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C mode special functions	   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c-semi&comma-no-newlines-at-all ()
  "never add a newline after a comma or a semicolon"
  'stop)

;; Suppresses newlines before non-blank lines
(defun ole-newline-after-opening-brace (syntax pos)
  "Controls newline insertion after opening braces."
  (save-excursion
    (if (and (= last-command-char ?{)
			 (zerop (forward-line 1))
			 (not (looking-at "^[ \t]*$")))
		(progn
		  (previous-line 1)
		  (end-of-line)
		  (insert " }"))
	  '(after))))

(defun ole-newline-before-closing-brace (syntax pos)
  "Controls newline insertion after and before closing brace."
  (save-excursion
    (if (and (= last-command-char ?})
	     (zerop (forward-line 0))
	     (not (looking-at "^[ \t]*$")))
	'(before)
      '())))

(defun ole-newline-before-and-after-closing-brace (syntax pos)
  "Controls newline insertion after and before closing brace."
  (save-excursion
    (let ((list '()))
      (if (= last-command-char ?})
	  (progn
	    (if (and (zerop (forward-line 0))
		     (not (looking-at "^[ \t]*$")))
		(setq list '(before)))
	    (if (and (zerop (forward-line 1))
		     (looking-at "^[ \t]*$"))
		(add-to-list 'list 'after t))))
      list)))

(defun ole-c-lineup-arglist-operators (langelem)
  "Line up lines starting with an infix operator under the open paren.
Return nil on lines that don't start with an operator, to leave those
cases to other lineup functions.  Example:

if (x < 10
    || at_limit (x,       <- c-lineup-arglist-operators
                 list)    <- c-lineup-arglist-operators returns nil
   )

Since this function doesn't do anything for lines without an infix
operator you typically want to use it together with some other lineup
settings, e.g. as follows:

\(c-set-offset 'arglist-cont '(c-lineup-arglist-operators 0))
\(c-set-offset 'arglist-cont-nonempty '(c-lineup-arglist-operators
                                        c-lineup-arglist))

Works with: arglist-cont, arglist-cont-nonempty."
  (save-excursion
    (back-to-indentation)
    (when (looking-at "[-+|&*%<>=]\\|\\(/[^/*]\\)")
      (c-lineup-arglist langelem))))

;;; add "show matching paren" feature
;;; Both methods below are from of Ashwin Ram (Ram-Ashwin@yale), who writes:
;;; Here's a nice variation on the paren balancing feature.  If the cursor is
;;; already sitting on a right paren, the cursor will just flash back to the
;;; matching open paren without inserting another paren, otherwise it'll insert
;;; a matching right paren and then flash back to the matching open paren.
;;; This is often nicer than the default, which always inserts another paren.
(setq blink-paren-hook
      '(lambda ()
	 (if (looking-at (char-to-string last-input-char))
	     (delete-char 1))
	 (blink-matching-open)))

(defun electric-c-leftbrace (arg)
  "Insert { character and correct line's indentation.
   Then insert a newline and a }, correctly indented.
   Return point to just after the {.  If c-auto-newline is non-nil,
   insert a newline and leave the cursor (and point) indented on
   the blank line between the braces."
  (interactive "P")
  (c-electric-brace (prefix-numeric-value arg))
  ;;  (electric-c-brace (prefix-numeric-value arg))
  (c-indent-line)
  (insert "\n\n" "}")
  (c-indent-line)
  (previous-line 1)
  (c-indent-line)
  (end-of-line nil)
  ;; (if c-auto-newline (newline-and-indent))
  )
