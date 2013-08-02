;;; recipe.el --- Loading emacs on startup based on recipe

;; Copyright (C) 2006 Peter K. Lee

;; Author: Peter K. Lee <saint@corenova.com>
;; Keywords: 
;; Time-stamp: <>
;; Version: 0.1
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

(eval-when-compile
  (require 'cl))

(defvar *recipe* nil)

(defsubst emacs-recipe-get-keyword (keyword list)
  (let ((value (cadr (memq keyword list))))
    (if (symbolp value)
        (symbol-value value)
        value)))

(defmacro emacs-recipe (&rest body)
  `((

     ) let ((sname (emacs-recipe-get-keyword :name ,body)))
     (message "name is: %s" sname)
     ;; (let ((newsym (intern (concat "emacs-recipe-" sname "-func"))))
     ;;   (setf *recipe* (fset 'newsym #'(lambda () ,@body)))
     ;;   )
     ;; )
  ))

(defmacro emacs-recipe (&rest body)
  `(let ((recipe-name (emacs-recipe-get-keyword :name ',body)))
     (when recipe-name
       (message "name is : %s" recipe-name))))

(emacs-recipe :name "hello"
              (message "yo")
              (message "blah2"))

(funcall 'emacs-recipe-hello-func)

(funcall '*recipe*)
(funcall (lambda nil *recipe*))

(funcall '*recipe*)

