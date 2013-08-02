;;; planner-sectionalize.el --- sectionalize areas of text

;; Copyright (C) 2005 Peter K. Lee

;; Author: Peter K. Lee <saint@ c o r e n o v a .com>
;; Keywords: planner sectionalize heading
;; Time-stamp:
;; Version: 0.2
;; X-URL: http://www.corenova.com/...

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Introduction
;; ------------

;; Wrap sections of planner files before publishing to have proper
;; sectionalization with DIV tags.

;; For instance, if the DELIM character is '*' and the buffer looks
;; like the following:

;; ,----
;; | * Tasks
;; | 
;; | my tasks...
;; | 
;; | * Description
;; | 
;; | Some description text...
;; | 
;; | ** List of links
;; | 
;; | - item1
;; | - item2
;; | ...
;; | 
;; | ** Other Description
;; | 
;; | Some other description text...
;; | 
;; | **
;; | 
;; | Final words regarding the description as listed above...
;; | 
;; | * Notes
;; | 
;; | my notes...
;; `----

;; then this module will (assuming HTML publishing) generate the
;; following markup:

;; ,----
;; | <div class="section"><h2>Tasks</h2>
;; | 
;; | my tasks...
;; | 
;; | </div>
;; | <div class="section"><h2>Description</h2>
;; | 
;; | Some description text...
;; | 
;; | <div class="section"><h3>List of links</h3>
;; | 
;; | - item1
;; | - item2
;; | ...
;; | 
;; | </div>
;; | <div class="section"><h3>Other Description</h3>
;; | 
;; | Some other description text...
;; | 
;; | </div>
;; | 
;; | Final words regarding the description as listed above...
;; | 
;; | </div>
;; | <div class="section"><h2>Notes</h2>
;; | 
;; | my notes...
;; | </div>
;; `----

;; The above is just an example output to demonstrate how the
;; nesting is done.

;; *NEW FEATURE*
;; notice the empty ** ?
;; normally, that should become an empty heading, or ignored
;; altogether as insignificant.  However, in our case, it is used to
;; denote END OF SECTION such that text following the empty heading is
;; now considered to be part of the original * section!

;; If the above statement doesn't make sense, double check the
;; generated markup to see how it played out...

;; Now you can do special CSS selectors to do things like...

;; div.section { margin-left: 5px; }
;; div.section div.section { margin-left: 15px; }

;; to simulate subsection being indented inside the parent section!

;;
;; Todo:
;; -----
;; 1. verify that this module works with emacs-wiki
;; 2. write a check inside planner-sectionalize-page to make sure
;;    we're doing HTML publishing
;;
;; History:
;; --------
;;
;; 2005-07-01 (0.1) : initial release
;; 2005-07-07 (0.2) : added better documentation

;;;

(require 'planner) 

(defvar planner-sectionalize-delimiter "*"
  "The delimiter used to sectionalize.")

(defun planner-sectionalize-page ()
  "A wrapper around `sectionalize' that calls it on the
  entire page.  Uses the `planner-sectionalize-delimiter'
  variable value.  Should not have to call directly.  Should be a
  part of before-publish-hook."
  (interactive)
  (let ((delim planner-sectionalize-delimiter))
    (save-excursion
      (goto-char (point-min))
      (sectionalize delim)
      t)))

(if (boundp 'muse-before-publish-hook)
    (add-hook 'muse-before-publish-hook 'planner-sectionalize-page))

;; uncomment following if enabling for emacs-wiki 
;;
;; someone please verify that the following works! 
;;
;; (if (boundp 'emacs-wiki-before-markup-hook)
;;     (add-hook 'emacs-wiki-before-markup-hook 'planner-sectionalize-page))

(defvar sectionalize-start-function 'sectionalize-html-start)
(defvar sectionalize-end-function   'sectionalize-html-end)

(defun sectionalize-html-start (title depth)
  "Return HTML representation of the section's start."
  (let ((tag (concat "h" (number-to-string (+ depth 1)))))
    (concat "<div class=\"section\"><" tag ">" title "</" tag ">")))

(defun sectionalize-html-end ()
  "Return HTML representation of the section's end."
  "</div>\n")

(defun sectionalize (delim &optional n)
  "A routine that envelops regions of the buffer based on areas
bound by the DELIM character.

optional parameter N is used *internally* to denote the current
recursion depth."
  (unless n (setq n 0))
  (let ((regexp (concat "^\\(\\" delim "+\\)\\s-+")))
    (while (and regexp (re-search-forward regexp nil t))
      (let ((len (length (match-string 1)))
            (title (buffer-substring (match-end 0) (point-at-eol))))
        (cond ((> len n) 
               (delete-region (match-beginning 0) (point-at-eol))
               (when (not (string= title ""))
                 (insert (funcall sectionalize-start-function title len))
                 (sectionalize delim len)
                 (insert (funcall sectionalize-end-function))))
              (t (setq regexp nil)
                 (goto-char (match-beginning 0))))))
      (if regexp (goto-char (point-max)))))

(provide 'planner-sectionalize)

;;; planner-sectionalize.el ends here
