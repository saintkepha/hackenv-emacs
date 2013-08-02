;;; planner-auto-schedule.el --- auto schedule todo task

;; Copyright (C) 2005 Peter K. Lee

;; Author: Peter K. Lee <saint@ c o r e n o v a .com>
;; Keywords: planner auto prompt schedule next task
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
;;
;;
;; Todo:
;; -----
;;
;;
;; History:
;; --------
;;
;; 2005-05-01 (0.1) : initial release
;;
;; 2005-06-05 (0.2) : description should match *exactly* not as regex

;;;

(require 'planner)

(defadvice planner-extract-tasks-with-status (around saint activate)
  "Returns all tasks on PAGES with the specified STATUS where
    STATUS can be a STRING OR LIST."
  (let ((pages (ad-get-arg 0))
        (status (ad-get-arg 1)))
    (setq ad-return-value
          (planner-extract-tasks pages
                                 (lambda (item)
                                   (if (and status (listp status))
                                       (consp (member (planner-task-status item) status))
                                     (equal (planner-task-status item)
                                            status)))))))

(defun saint/planner-task-description-alist (tasks)
  "Returns alist of task descriptions to their respective position in the list."
  (let ((pos 0))
    (mapcar (lambda (task-info)
              (list (planner-task-description task-info) (setq pos (1+ pos))))
            tasks)))

(defun saint/planner-list-unfinished-plan-tasks (plan)
  "Returns all unfinished tasks in a given PLAN."
  (interactive
   (list (planner-read-non-date-page
          (planner-file-alist) nil)))
  ;; if plan isn't doesn't already exist, ask for page again.
  (while (null (planner-page-exists-p (planner-link-base plan) t))
    (setq plan (planner-read-non-date-page (planner-file-alist))))
  (planner-extract-tasks-with-status (list (planner-link-base plan)) '("o" "P" "_" ">")))

(defun saint/planner-prompt-next-plan-task (plan)
  "Prompt for the next task in this project.  If PLAN is
   provided, it is assumed to be a plan page.  If there are
   unfinished tasks in the PLAN, user can interactively select a
   task from the list of unfinished tasks.  However, at any time
   during the task selection, user may decide to create a new
   task by specifying a new task description.  If the selected
   task is already scheduled for today, task will simply change
   to task-in-progress.  However, if the task is scheduled for a
   different date, user will be asked whether to re-schedule the
   scheduled task for today.  If the task is not yet scheduled,
   it will automatically be scheduled for today, and marked
   in-progress.  At any time, user may press C-g to cancel.

   This routine may be called interctively at any time, to look
   for unfinished tasks.  User will be asked to provide the PLAN
   for which they wish to look under.

   Now it works with `planner-tasks-never-suppress-fixing-flag'
   set to (t)."
  (interactive
   (list (planner-read-non-date-page
          (planner-file-alist) nil)))
  ;; if plan isn't doesn't already exist, ask for page again.
  (while (null (planner-page-exists-p (planner-link-base plan) t))
    (setq plan (planner-read-non-date-page (planner-file-alist))))
  (save-window-excursion
    (save-excursion
      (if (when (planner-find-file plan)
            (planner-seek-to-first)
            (planner-current-task-info))
          (let* ((plan-page  (planner-link-base plan))
                 (task-info  (planner-current-task-info))
                 (task-list  (saint/planner-list-unfinished-plan-tasks plan-page))
                 (task-alist (saint/planner-task-description-alist task-list))
                 (history    (mapcar #'car task-alist))
                 (minibuffer-local-completion-map 
                  (let ((my-minicomp-keymap (copy-keymap minibuffer-local-completion-map)))
                    (define-key my-minicomp-keymap " " 'self-insert-command)
                    my-minicomp-keymap))
                 (task-description (completing-read
                                    (format "Next Task in %s %s: " plan-page
                                            (if (> (length task-list) 0) 
                                                (format "(+%d) [%s]" (length task-list)
                                                        (if (< (length (caar task-alist)) 25) (caar task-alist)
                                                          (concat (substring (caar task-alist) 0 25) "...")))
                                              (format "(*new*)" t)))
                                    task-alist nil nil nil 'history (caar task-alist) t))
                 (existing-task nil))
            (unless (string= task-description "")
              (setq task-info (planner-find-task-by-description plan-page task-description))
              (unless task-info         ; make a new task and search for it again
                (planner-create-task task-description (planner-today) nil plan-page)
                (setq task-info (planner-find-task-by-description plan-page task-description)))
              (unless (string= (planner-task-date task-info) (planner-today))
                (when (or (not (planner-task-date task-info))
                          (y-or-n-p 
                           (format "Selected task currently scheduled for %s, re-schedule for today?"
                                   (planner-task-date task-info))))
                      (planner-copy-or-move-task (planner-today))
                      (setq task-info (planner-find-task-by-description plan-page task-description))))
              (planner-task-in-progress)
              task-description))
        (message "There are no tasks in %s!" plan)))))

(defun planner-find-task-by-description (page desc)
  "Search the given planner PAGE for matching DESC."
  (when (and page desc
             (planner-find-file page))
    (planner-seek-to-first)
    (let ((task-info nil)
          (existing-task nil))
      (while (and (search-forward desc nil t)
                  (not existing-task))
        (beginning-of-line)
        (setq task-info (planner-current-task-info))
        (when (and task-info
                   (string= desc
                            (planner-task-description task-info)))
          (setq existing-task t)))
      task-info)))

(defcustom planner-prompt-next-plan-task t
  "Non-nil means do auto search for next plan item and allow user to
    move onto the next task seamlessly."
  :type 'boolean
  :group 'planner-tasks)

(defadvice planner-task-done (around saint activate)
  "Prompt for the next task in this project. Use C-g to cancel."
  (let ((task-info (planner-current-task-info)))
    ad-do-it
    (message "finished task %s in %s" (planner-task-description task-info)
             (planner-task-plan task-info))
    (when (and planner-prompt-next-plan-task
               task-info)
      (saint/planner-prompt-next-plan-task (planner-task-plan task-info)))))

(provide 'planner-auto-schedule)

;;; planner-auto-schedule.el ends here
