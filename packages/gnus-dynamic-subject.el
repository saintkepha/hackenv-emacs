;;; gnus-dynamic-subject.el --- Display subject on the first line, dynamically

;;; Time-stamp: <2005-05-14 12:37:38 bojohan>
;; <http://www.dd.chalmers.se/~bojohan/emacs/gnus-dynamic-subject.html>

;;; Commentary:
;;
;; Load this file and replace the "%s" subject spec with "%us" in
;; gnus-summary-line-format.

;;; Code:

(defvar gnus-dynamic-subject-full-width nil
  "Make `gnus-summary-same-subject' occupy the full width of the subject.
\(Padded with spaces). You probably want to use this if you have
a subject spec without an explicit width (i.e. of the form
\"%us\" rather than \"%20us\") that is not at end of line.")

(defvar gnus-dynamic-subject-use-post-command-hook (featurep 'xemacs)
  "If non-nil use `post-command-hook'.
Otherwise use `window-scroll-functions' (useless in XEmacs) .")


;; Internal variables
(defvar gnus-dynamic-subject-overlay nil)
(make-variable-buffer-local 'gnus-dynamic-subject-overlay)
(defvar gnus-dynamic-subject-spec nil)
(make-variable-buffer-local 'gnus-dynamic-subject-spec)

(defun gnus-user-format-function-s (dummy)
  (let ((subject (aref gnus-tmp-header 1)))
    (if (string= subject gnus-tmp-subject-or-nil)
	subject
      (let* ((str (if (equal gnus-dynamic-subject-spec "")
		      gnus-summary-same-subject
		    (gnus-eval-format
		     (concat "%" gnus-dynamic-subject-spec "s")
		     '((?s gnus-tmp-subject-or-nil ?s)))))
	     (string (if gnus-dynamic-subject-full-width
			 (concat
			  str
			  (make-string
			   (max (- (length subject) (length string)) 0) ?\ ))
		       str)))
	(propertize (if (string= "" string) " " string)
		    'gnus-summary-same-subject t)))))

(defun gnus-dynamic-subject (&optional window start)
  (when gnus-dynamic-subject-overlay
    (save-excursion
      (goto-char (or start (window-start)))
      (let* ((limit (line-end-position))
	     (first (text-property-not-all
		     (point) limit 'gnus-summary-same-subject nil)))
	(if (not first)
	    (delete-overlay gnus-dynamic-subject-overlay)
	  (let ((last (next-single-property-change
		       first 'gnus-summary-same-subject nil limit)))
	    (move-overlay gnus-dynamic-subject-overlay first last)
	    (overlay-put
	     gnus-dynamic-subject-overlay
	     'display
	     (if (or gnus-dynamic-subject-full-width
		     (not (equal gnus-dynamic-subject-spec "")))
		 (substring (gnus-summary-subject-string
			     (gnus-summary-article-number))
			    0 (- last first))
	       (gnus-summary-subject-string
		(gnus-summary-article-number))))))))))

(defvar gnus-dynamic-subject-window-start 0)
(make-variable-buffer-local 'gnus-dynamic-subject-window-start)

(defun gnus-dynamic-subject-update ()
  (unless (= (window-start) gnus-dynamic-subject-window-start)
    (setq gnus-dynamic-subject-window-start (window-start))
    (gnus-dynamic-subject nil gnus-dynamic-subject-window-start)))

(defun gnus-dynamic-subject-setup-1 ()
  (setq gnus-dynamic-subject-spec
	(and (string-match "%\\([^%&]*\\)us" gnus-summary-line-format)
	     (match-string 1 gnus-summary-line-format))))

(defun gnus-dynamic-subject-setup-2 ()
  (when gnus-dynamic-subject-spec
    (if (not gnus-dynamic-subject-use-post-command-hook)
	(add-hook 'window-scroll-functions 'gnus-dynamic-subject nil t)
      (add-hook 'post-command-hook 'gnus-dynamic-subject-update nil t))
    (setq gnus-dynamic-subject-overlay (make-overlay 1 1))))

(add-hook 'gnus-summary-generate-hook 'gnus-dynamic-subject-setup-1)
(add-hook 'gnus-summary-prepared-hook 'gnus-dynamic-subject-setup-2)

(defadvice gnus-summary-position-point (after summary-update-subject activate)
  (when gnus-dynamic-subject-spec
    (gnus-dynamic-subject)))

(provide 'gnus-dynamic-subject)


;; Same thing using a conditional display property.
;;
;; (defvar gnus-user-format-x-window-start 0)
;; (make-variable-buffer-local 'gnus-user-format-x-window-start)
;; (defun gnus-user-format-function-x (dummy)
;;   (let ((subject (aref gnus-tmp-header 1)))
;;     (if (string= subject gnus-tmp-subject-or-nil)
;; 	subject
;;       (propertize
;;        (format "%1s" gnus-summary-same-subject)
;;        'display
;;        `(when (progn
;; 		(when (eq (window-buffer) object)
;; 		  (setq gnus-user-format-x-window-start (window-start)))
;; 		(eq (save-excursion (goto-char buffer-position)
;; 				    (line-beginning-position))
;; 		    gnus-user-format-x-window-start))
;; 	  . ,subject)))))

;;; Some test cases
;; (setq gnus-summary-line-format "%U%R%z%I %0{%15,25us%} %1{%[%4L: %-23,23f%]%} \n")
;;
;; (setq gnus-summary-line-format ":%U%R %I %us %-60= |%4L |%-20,20f|%&user-date; \n")
;;
;; (setq gnus-summary-line-format "%ua%6N %6&user-date; %1{%[%z%} %2{%(%-18,18f%)%}%* %R%U%1{%]%} %u&thread;%0{%us%}\n")
;;
;; (setq gnus-summary-line-format "%*%U%R%z%7V.%-6i %6L=%-6k %&user-date; %[%-20,20n%] }%u&thread;%1{%us%} %N\n")

;;; gnus-dynamic-subject.el ends here
