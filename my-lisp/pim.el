(require 'org)
(require 'planner)

(defcustom org-update-file-behavior 'close
  "Controls behavior of org file creation and updates.
If 'close, newly-opened files are saved and closed.
If 'save, newly-opened files are saved and left open.
If nil, no actions will be taken."
  :group 'org
  :type '(choice (const :tag "Save and close opened files" 'close)
                 (const :tag "Save opened files" 'save)
                 (const :tag "Do nothing" nil)))

(defmacro with-org-update-setup (&rest body)
  (let ((live-buffers (make-symbol "live-buffers")))
    `(save-window-excursion
       (save-restriction
         (let ((,live-buffers (and (eq org-update-file-behavior
                                       'close)
                                   (buffer-list)))
               (current-buffer (current-buffer)))
           (prog1
               (let ((org-startup-with-deadline-check nil)
                     (org-startup-folded nil))
                 ,@body)
             (when org-update-file-behavior
               (org-save-buffers ,live-buffers current-buffer))))))))

;;;###autoload
(defun org-goto-org-file (file)
  "Opens ORG file in the default-directory `org-directory'."
  (interactive (list (org-get-org-file)))
  (find-file file))

(defadvice org-schedule (around saint activate)
  "Insert the SCHEDULED: string to schedule a TODO item.
A timestamp is also inserted - use \\[org-timestamp-up] and \\[org-timestamp-down]
to modify it to the correct date."
  (interactive)
  (require 'timeclock)
  (let ((org-time-was-given t))
    (let* ((start-time (org-read-date t 'to-time))
           (end-time (if org-time-was-given
                         (timeclock-seconds-to-time
                          (+ (timeclock-time-to-seconds start-time)
                             (pim-read-duration)))))
           (fmt (if org-time-was-given 
                    (cdr org-time-stamp-formats)
                  (car org-time-stamp-formats))))
      (message "%s" org-time-was-given)
      (insert org-scheduled-string " "
              (format-time-string fmt start-time)
              (format "%s" 
                      (or (if end-time
                              (concat "--" (format-time-string fmt end-time)))
                          "")))))
  (message (substitute-command-keys
            "Use \\[org-timestamp-up-day] and \\[org-timestamp-down-day] to change the date.")))

(defvar pim-read-duration-default "60"
  "The default duration of scheduled agenda items in minutes")

(defun pim-read-duration (&optional prompt)
  "Ask the user for the length of a duration in time.
User may express the duration in following:

1 (d\\w*?) 1 (h\\w*?) 1(m\\w*?)
 or
1234 (which will default to minutes)

If PROMPT is non-nil, display it as the prompt string.
returns floating-point representation"
  (interactive)
  (let ((duration (read-string (or prompt
                                   "Duration (x d|day y h|hour z m|min): ")))
        day hour min)
    (when (string-match "^\\s-*$" duration)
      (setq duration (or pim-read-duration-default "nil")))
    (cond
     ((string= "nil" duration) nil)
     ((string-match (concat "^\\(\\([0-9]+\\)\\s-*d\\w*?\\)?\\s-*"
                            "\\(\\([0-9]+\\)\\s-*h\\w*?\\)?\\s-*"
                            "\\(\\([0-9]+\\)\\s-*m\\w*?\\)?$") duration)
      (setq day  (string-to-number (or (match-string 2 duration) "0"))
            hour (string-to-number (or (match-string 4 duration) "0"))
            min  (string-to-number (or (match-string 6 duration) "0")))
      (+ (* day  86400.0)
         (* hour 3600.0)
         (* min  60.0)))
     ((string-match "^\\([0-9]+\\)$" duration)
      (* (string-to-number (match-string 1 duration)) 60.0))
     (t (error "invalid duration specification! review `pim-read-duration' for details on usage") 0.0))))

(defun org-save-buffers (&optional buffer-list skip-buffer)
  "Save all org buffers.
If BUFFER-LIST is a list of buffers, close all buffers not found
in that list. If SKIP-BUFFER is non-nil, do not save that
buffer."
  (interactive)
  (mapcar
   (lambda (buffer)
     (unless (eq buffer skip-buffer)
       (with-current-buffer buffer
         ;; Save all org buffers
         (when (pim-derived-mode-p 'org-mode)
           (when (buffer-modified-p)
             (save-buffer))
           (when (and buffer-list
                      (not (memq buffer buffer-list)))
             (kill-buffer nil))))))
   (buffer-list)))

(defconst org-paste-entry-help
"Select a destination location for the PROJECT.
UP/DOWN=headline   TAB=cycle visibility  [Q]uit   RET/<left>/<right>=Store
RET at beg-of-buf -> Append to file as level x headline
RET on headline   -> Store as sublevel entry to current headline
<left>/<right>    -> before/after current headline, same headings level")

(defun org-paste-entry-ask-maybe (entry level &optional loc reversed)
  "Insert an ENTRY into current org file in specified LOC, with
  requested LEVEL or ask user if not provided.  REVERSED places
  entry first."
  (when entry 
    (unless loc
      (setq loc (org-get-location (current-buffer) org-paste-entry-help)))
    (when loc
      (goto-char loc)
      (cond ((bobp)
             (goto-char (if reversed (point-min) (point-max))))
            ((and (org-on-heading-p nil) (not level))
             ;; Put it below this entry, at the beg/end of the subtree
             (org-back-to-heading)
             (setq level (1+ (outline-level)))
             (if reversed
                 (outline-end-of-heading)
               (outline-end-of-subtree))))
      (if (not (bolp)) (newline))
      (org-paste-subtree level entry))))

(defun org-planner-project-regexp (&optional project)
  "return the regexp for matching planner PROJECT heading.  If
  PROJECT is nil, then match any planner project."
  (concat "\\*+.*?" (or project "\\(\\w+\\)") "\\s-+(Planner)"))

(defvar current-time-string-regexp "\\(\\w+\\s-+\\w+\\s-+[0-9]+ [0-9]+:[0-9]+:[0-9]+ [0-9]+\\)"
  "Matches time string in the form of: Fri Aug 5 11:12:42 2005")

(defvar org-remember-heading-regexp (concat "\\*+.*?" current-time-string-regexp ".*?(\\(.*?\\))$")
  "Matches DATESTRING and TITLE of remember generated headings.")

(defun org-get-planner-context ()
  "Search all headings that comprise the current point's entry
  and return the first match that corresponds to a Planner
  project heading."
  (save-excursion
    (org-back-to-heading)
    (while (and (not (looking-at (org-planner-project-regexp)))
                (condition-case nil
                    (org-up-heading-all 1)
                  (error nil))) )
    (if (looking-at (org-planner-project-regexp))
        (match-string-no-properties 1))))

(defun org-goto-planner-page ()
  "If the current heading is a (Planner) project, goto the planner page."
  (interactive)
  (let ((planner-page (org-get-planner-context)))
    (if (and planner-page
           (planner-page-exists-p planner-page))
        (planner-find-file planner-page)
      (message "Not a planner project entry"))))

(defun planner-create-note-with-timestamp (&optional page)
  "Extended version of `planner-create-note' which includes
  timestamp info with link to the TODAY day page if PAGE is not a
  day page.  This routine does NOT create a cross reference to
  the day page, just a reference to the day page in general.  The
  point sits between the ANCHOR and TIMESTAMP LINK where TITLE
  should go.  If cross reference is desired, follow up on this
  call with `planner-update-note-xref'."
  (interactive)
  (planner-create-note page)
  (save-excursion
    (goto-char (planner-line-end-position))
    (insert " " (format-time-string "%H:%M" (current-time))
            (if (not (string-match planner-date-regexp (planner-page-name)))
                (concat " (" (planner-make-link (planner-today)) ")")
              ""))))

(defun planner-update-note-xref ()
  "Follow links in the current note, creating new note entries if
  the linked pages do not have the note, updating the note
  info otherwise."
  (interactive)
  (let* ((note-info (planner-current-note-info t))
         (link (planner-note-link note-info)))
    (when link
      (save-window-excursion
        (save-restriction
          (unless (planner-narrow-to-note link-page link-number)
            (planner-create-note)
            )
          )
        (planner-visit-link link)
        (save-restriction)))))

(defun org-prep-subtree-for-planner-note ()
  "Grab the subtree at point and sanitize it for entry as a planner note."
  (when (org-back-to-heading)
    (org-copy-subtree)
    (with-temp-buffer
      (org-paste-subtree 1)
      (goto-char (point-min))
      (unless (org-on-heading-p)
        (outline-next-heading))
      (kill-line)
      (unless (outline-next-heading)    ; make sure there are no other sub-headings
        (goto-char (point-max))
        (delete-blank-lines)
        (buffer-string)))))

(defun org-remember-entry-as-planner-note ()
  "Copy or Move the current entry into a planner page as a planner note."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (if (looking-at org-remember-heading-regexp)
        (let ((date  (match-string-no-properties 1))
              (title (match-string-no-properties 2))
              (plan-page (or (org-get-planner-context)
                             (planner-read-non-date-page (planner-file-alist))))
              (text (org-prep-subtree-for-planner-note)))
          (unless text
            (error "Cannot convert org entry to planner note (has sub tree elements)"))
          (save-window-excursion
            (planner-create-note-with-timestamp plan-page)
            (insert title)
            (save-restriction
              (planner-narrow-to-note)
              (next-line)
              (insert text "\n"
                      "originated on: *" date "*\n\n"))
            (planner-update-note-xref))))))

(defun org-agenda-goto-planner-page ()
  "If the entry contains (Planner), goto the planner page."
  (interactive)
  (let* ((marker (or (get-text-property (point) 'org-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    (switch-to-buffer-other-window buffer)
    (goto-char pos)
    (when (eq major-mode 'org-mode)
      (org-goto-planner-page))))

(defalias 'pim-derived-mode-p 'planner-derived-mode-p)

(defun pim-schedule-planner-project (project)
  "Schedule a planner PROJECT in the `planner-project' into the
  Org agenda."
  (interactive (list (planner-read-non-date-page (planner-file-alist))))
  ;; search for project heading inside orgfile
  (unless (planner-page-exists-p project)
    (error "You must select existing planner page to be scheduled."))
  ;; put together the entry text
  (let ((heading (concat "* " project " (Planner)\n\n"))
        entry-loc)
    (with-org-update-setup
     (call-interactively 'org-goto-org-file)
     (goto-char (point-min))
     ;; search for project heading entry in the Org file.
     (while (and (not entry-loc) 
                 (outline-next-heading))
       (if (looking-at (org-planner-project-regexp project))
           (setq entry-loc (point))))
     (unless (and entry-loc (org-on-heading-p))
       (org-paste-entry-ask-maybe heading 1)
       (outline-next-heading))
     (when (org-on-heading-p)
       (save-excursion
         (outline-end-of-heading)
         (forward-line 1)
         (indent-for-tab-command)
         (call-interactively 'org-schedule)
         (newline))))))

;;; Keybindings

(add-hook 'org-mode-hook (lambda ()
                           (define-key org-mode-map (kbd "C-c C-g")  'org-goto-planner-page)
                           (define-key org-agenda-mode-map (kbd "v") 'org-agenda-goto-planner-page)))

(provide 'pim)
