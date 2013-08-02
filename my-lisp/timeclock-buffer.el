;;; timeclock-buffer.el --- Track time spent in buffers by mode

;; Copyright (C) 2006 Peter K. Lee

;; Author: Peter K. Lee <saint@corenova.com>
;; Keywords; timeclock, buffer, mode

;; Time-stamp: <>

;; This file i free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;_ + Commentary:

;; Get day-by-day breakdown on time spent in buffers organized by MODE
;;
;; Example: 

;;;_ + History:

;; 1.0 - 2/7/2006 - birthday

;;; Code:

(defgroup timeclock-buffer nil
  "Options concerning timeclock-buffer."
  :group 'data)

(defcustom timeclock-buffer-directory "~/.timeclock"
  "*Directory where daily timeclock files are saved."
  :type 'directory
  :group 'timeclock-buffer)

(defcustom timeclock-buffer-idle-timeout 300
  "*Seconds elapsed without activity to mark as being idle.  If
  nil, do not use timer to track idle periods.  Not using the
  timer may impact accuracy of actual time spent doing things."
  :set (lambda (symbol value)
         (if (boundp 'timeclock-buffer-timer-reset)
             (timeclock-buffer-timer-reset value))
         (setq timeclock-buffer-idle-timeout value))
  :type 'integer
  :group 'timeclock-buffer)

(defcustom timeclock-buffer-track-idle t
  "*Set to enable/disable tracking of idle time.  If t, there
  will be an extra entry with IDLE that tracks time spent not
  doing anything constructive."
  :type 'boolean
  :group 'timeclock-buffer)

(defvar timeclock-buffer-list nil
  "Internally store the buffer timeclock in following format:
'((\"YYYY-MM-DD\" (MODE-NAME '((buffer-name . time)
                             (buffer-name . time)
                             ...))
                (MODE-NAME '(...)))")

(defvar timeclock-buffer-last-buffer-info nil
  "Internally store some info about the last buffer:
'(BUFFER-NAME MODE-NAME TIMESTAMP)")

(defsubst timeclock-buffer-info-name      (info) (nth 0 info))
(defsubst timeclock-buffer-info-mode      (info) (nth 1 info))
(defsubst timeclock-buffer-info-timestamp (info) (nth 2 info))

(defsubst timeclock-buffer-time-to-date (time)
  "Convert the TIME value to a textual date string."
  (format-time-string "%Y-%m-%d" time))

(defun timeclock-buffer-file-name ()
  (concat
   (when timeclock-buffer-directory
     (file-name-as-directory timeclock-buffer-directory))
   (timeclock-buffer-time-to-date (current-time))
   ".timelog"))

(defun timeclock-buffer-save ()
  "Periodically save the modes-list data to disk."
  (when (and timeclock-buffer-buffer
             (buffer-name timeclock-buffer-buffer))
    (save-excursion
      (set-buffer timeclock-buffer-buffer)
      (setq buffer-file-name (timeclock-buffer-file-name))
      (save-buffer))))

(defun timeclock-buffer-load ()
  "When there is no `timeclock-buffer-modes-list', try loading
  the latest timeclock-buffer file for today."
  
  )

(defun timeclock-buffer-by-date (date)
  (when date
    (or (assoc date timeclock-buffer-list)
        (assoc date (add-to-list 'timeclock-buffer-list (list date))))))

(defun timeclock-buffer-today ()
  (timeclock-buffer-by-date (timeclock-buffer-time-to-date (current-time))))

;;; Timer routines

(defvar timeclock-buffer-timer nil
  "Timer used to update timeclock-buffer save file.")

(defvar timeclock-buffer-is-idle nil
  "Internally used to track when emacs goes idle.")

;;;###autoload
(defun timeclock-buffer-timer-reset (interval)
  (when timeclock-buffer-timer
    (cancel-timer timeclock-buffer-timer))
  (when (> interval 0)
    (setq timeclock-buffer-timer (run-with-idle-timer interval t 'timeclock-buffer-idle-update interval))))

;;; Update routines

(defun timeclock-buffer-update-elapsed-time (info seconds)
  (when (> seconds 1)
      (let* ((buffer-name   (timeclock-buffer-info-name info))
             (mode-name     (timeclock-buffer-info-mode info))
             (today-buffers (timeclock-buffer-today))
             (mode-buffers (or (assoc mode-name today-buffers)
                               (assoc mode-name (nconc today-buffers (list (list mode-name))))))
             (buffer-entry (or (assoc buffer-name mode-buffers)
                               (assoc buffer-name (nconc mode-buffers (list (cons buffer-name nil)))))))
        (setcdr buffer-entry (+ (or (cdr buffer-entry)
                                    0.0)
                                seconds)))))

(defun timeclock-buffer-idle-update (interval)
  (setq timeclock-buffer-is-idle t)
  (when timeclock-buffer-last-buffer-info
    (let ((timeclock-in (timeclock-buffer-info-timestamp timeclock-buffer-last-buffer-info)))
      (timeclock-buffer-update-elapsed-time timeclock-buffer-last-buffer-info
                                            (- (time-to-seconds (time-since timeclock-in))
                                               interval))))
  (timeclock-buffer-clock-in nil (seconds-to-time (- (time-to-seconds (current-time))
                                                     interval))))

(defun timeclock-buffer-update ()
  (when timeclock-buffer-last-buffer-info
    (timeclock-buffer-update-elapsed-time timeclock-buffer-last-buffer-info
                                          (time-to-seconds (time-since (timeclock-buffer-info-timestamp
                                                                        timeclock-buffer-last-buffer-info))))))

(defun timeclock-buffer-clock-in (buffer &optional time)
  (setq timeclock-buffer-last-buffer-info
        (if buffer
            (list (buffer-name buffer) (buffer-local-value 'mode-name buffer) (or time (current-time)))
            (when timeclock-buffer-track-idle
              (list "*idle*" "Fundamental" (or time (current-time)))))))

(defun timeclock-buffer-track-change ()
  "Added as part of `post-command-hook' to check whenever a
  command is issued.  Ideally, it should be attached to a less
  frequently occuring hook, something related to buffer-switch or
  such."
  (interactive)
  (when (and (zerop (minibuffer-depth))
             (or timeclock-buffer-is-idle
                 (frame-or-buffer-changed-p)))
    (setq timeclock-buffer-is-idle nil)
    (timeclock-buffer-update)
    (timeclock-buffer-clock-in (current-buffer))))

;;;###autoload
(defun timeclock-buffer-activate (activate)
  "Used to toggle timeclock-buffer capability based on value of
  ACTIVATE."
  (if activate
      (add-hook 'post-command-hook 'timeclock-buffer-track-change)
      (remove-hook 'post-command-hook 'timeclock-buffer-track-change)))

(provide 'timeclock-buffer)

