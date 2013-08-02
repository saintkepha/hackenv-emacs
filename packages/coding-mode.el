; coding.el

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

(defun open-line-and-indent (arg)
  "Invoke open-line, then c-indent-line."
  (interactive "P")
  (open-line (prefix-numeric-value arg))
  (c-indent-line)
  )

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
  (if c-auto-newline (newline-and-indent))
  )

;; make ';' insert self and indent line, but not insert newline.
(defun electric-c-semi (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg)))

;;; end modifications to c-mode

(defun electric-java-leftbrace (arg)
  "Insert { character and correct line's indentation.
   Then insert a newline and a }, correctly indented.
   Return point to just after the {.  If c-auto-newline is non-nil,
   insert a newline and leave the cursor (and point) indented on
   the blank line between the braces."
  (interactive "P")
  ;;  (electric-c-brace (prefix-numeric-value arg))
  (c-electric-brace (prefix-numeric-value arg))
  (java-indent-line)
  (insert "\n\n" "}")
  (java-indent-line)
  (previous-line 1)
  (java-indent-line)
  (end-of-line nil)
  (if java-auto-newline (newline-and-indent))
  )

;; make ';' insert self and indent line, but not insert newline.
(defun electric-java-semi (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg)))

(provide 'coding-mode)
