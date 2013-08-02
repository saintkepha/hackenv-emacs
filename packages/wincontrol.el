; wincontrol.el

(defvar winsize-mode-map 
  (let ((map (make-sparse-keymap)))
	(define-key map [up]    'shrink-window)
	(define-key map [down]  'enlarge-window)
	(define-key map [right] 'enlarge-window-horizontally)
	(define-key map [left]  'shrink-window-horizontally)
	map)
  "Keymap used in winsize-mode.")

(define-minor-mode winsize-mode
  "Toggle window size mode.
With no argument, this comand toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When winsize mode is enabled, the keypad 
\(left, right, up, down\) resizes the window.
\\{winsize-mode-map}"
  ;; initial value
  nil
  ;; mode-line indicator
  "-Winsize"
  ;; minor mode bindings.
  winsize-mode-map
  ;; body
  :global t)

(provide 'wincontrol)
