;; flexible-keymap.el
; by Peter K. Lee (5/31/2005)
;
;; * converted to minor mode (a better way to do it)
;; previous statement written in dvorak.
;; 
;; typing f1 f1 will start the dvorak mode.
;; it seems like this dvorak thing may just be a hoax.
;; 
;; but there seems to be a relatively strong argument for dvorak in
;; that it allows words to be typed while traveling much shorter
;; distances.
;
; ---
; inspired from original dvorak-mode.el by Matthew Weathers 
; 
; http://www.matthewweathers.com
; email address: (my two initials)@matthewweathers.com
;

(defvar saint/qwerty-keyboard-keys 
  '( "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]"
     "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "\'"
     "z" "x" "c" "v" "b" "n" "m" "," "." "/"
     "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "{" "}"
     "A" "S" "D" "F" "G" "H" "J" "K" "L" ":" "\""
     "Z" "X" "C" "V" "B" "N" "M" "<" ">" "?" 
     "-" "_" "=" "+" )
  "QWERTY keyboard layout as defined.")

(defvar saint/dvorak-keyboard-keys
  '( "\'" "," "." "p" "y" "f" "g" "c" "r" "l" "/" "="
     "a" "o" "e" "u" "i" "d" "h" "t" "n" "s" "-"
     ";" "q" "j" "k" "x" "b" "m" "w" "v" "z"
     "\"" "<" ">" "P" "Y" "F" "G" "C" "R" "L" "?" "+"
     "A" "O" "E" "U" "I" "D" "H" "T" "N" "S" "_"
     ":" "Q" "J" "K" "X" "B" "M" "W" "V" "Z"
     "[" "{" "]" "}" )
  "DVORAK keyboard layout as defined.")

(defvar saint/keymap-mode-map ()
  "custom Keyboard input mode mapping.")
(unless saint/keymap-mode-map
  (setq saint/keymap-mode-map (make-sparse-keymap)))

(defvar saint/keymap-translation-alist ()
  "translate x -> y based on keymap")

(defun saint/keymap-translation-setup (from to)
  "setup translation of key mapping FROM to TO."
  (when (and (listp from)
             (listp to))
    (setq saint/keymap-translation-alist
          (mapcar* 'cons from to))
    (mapcar (lambda (key)
              (define-key saint/keymap-mode-map key 'saint/keymap-insert))
            from)))

(defun saint/keymap-insert ()
  "Insert a key based on `keymap-translation-alist'."
  (interactive)
  (when saint/keymap-translation-alist
    (insert (cdr (assoc (char-to-string last-command-char) saint/keymap-translation-alist)))))

(provide 'flexible-keymap)

; ---

(define-key global-map (kbd "<f1> <f1>") 'saint/dvorak-minor-mode)

(define-minor-mode saint/dvorak-minor-mode
  "Minor mode used for turning on Dvorak map on a buffer"
  nil
  " Dvorak"
  saint/keymap-mode-map
  (saint/keymap-translation-setup saint/qwerty-keyboard-keys saint/dvorak-keyboard-keys)
)
