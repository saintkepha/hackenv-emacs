;; -*- Mode: Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project planner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar planner-keymap (make-sparse-keymap
						"Planner (p|P)lan, (g)oto page, (t)ask, (r)emember, (n)ote, (d)iary, (c)urrent task")
  "Keymap used to globally access planner")

(define-key planner-keymap (kbd "P")   'plan)
(define-key planner-keymap (kbd "p")   'planner-goto-today)
(define-key planner-keymap (kbd "g")   'planner-goto-plan-page)
(define-key planner-keymap (kbd "t")   'planner-create-task-from-buffer)
(define-key planner-keymap (kbd "r")   'remember-in-planner)
(define-key planner-keymap (kbd "n")   'planner-create-note-with-timestamp)
(define-key planner-keymap (kbd "d")   'planner-diary-add-entry)
(define-key planner-keymap (kbd "c")   'sacha/planner-what-am-i-supposed-to-be-doing)
(define-key planner-keymap (kbd "TAB") 'saint/planner-prompt-next-plan-task)
(define-key planner-keymap (kbd "?")   'describe-prefix-bindings)

;;; add to global key map
(define-key mode-specific-map (kbd "p") planner-keymap)

(when (boundp 'organizer-keymap)
  (define-key organizer-keymap (kbd "p") planner-keymap))

;;;;;;;;;;;;;;;;
;; Planner	  ;;
;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "~/Archives/planner--peter"))

(require 'planner)

;; planner modules
(require 'planner-accomplishments)
(require 'planner-bbdb)
(require 'planner-bookmark)
(require 'planner-cyclic)
(require 'planner-deadline)
(require 'planner-diary)
(require 'planner-lisp)
(require 'planner-multi)
(require 'planner-notes-index)
(require 'planner-report)
(require 'planner-rss)
;;(require 'planner-schedule)
(require 'planner-timeclock)
(require 'planner-timeclock-summary)

(eval-after-load "xtla"
  '(progn
	 (require 'planner-xtla)))

(eval-after-load "erc"
  '(progn
     (require 'planner-erc)))
(require 'planner-w3m)

;;;;;;;;;;;;;;;
;; PIM...    ;;
;;;;;;;;;;;;;;;

(eval-after-load "org"
  '(progn
	 (require 'pim)					  ; my wrapper for planner and org
	 (define-key global-map (kbd "C-x p s") 'pim-schedule-planner-project)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Planner variables    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq planner-project "WikiPlanner")
(setq planner-day-page-directory "~/Plans/daily-plans")
(setq planner-use-other-window nil)		; don't use other window... 
(setq planner-tasks-file-behavior 'close) ; save & close newly generated files
(setq planner-tasks-never-suppress-fixing-flag t) ; don't suppress fixing on save (always fix when saving!)
(setq planner-carry-tasks-forward 3) ; carry tasks forward back from 3 days ago!
(setq planner-expand-name-favor-future-p nil)
(setq planner-task-dates-favor-future-p t)
(setq planner-default-task-priority "B")
(setq planner-expand-name-default ".")

(setq planner-diary-file diary-file)
(setq planner-diary-use-diary t)

(setq planner-day-page-template
	  "* Journal\n\n\n* Tasks\n\n\n* Diary\n\n\n* Timeclock\n\n\n* Accomplishments\n\n\n* Notes\n\n")

(setq planner-plan-page-template
	  "* Description\n\n\n* Tasks\n\n\n* Milestones\n\n\n* Notes\n\n")

(setq planner-timeclock-summary-summary-string "
** Clocking Statistics

Day began:     | %B
Day ended:     | %E
Time elapsed:  | %S secs
Time clocked:  | %C secs
Clocked ratio: | %R\n")


;;; don't use cal-desk-calendar since it breaks org-mode diary access
;(require 'cal-desk-calendar)
;(setq planner-diary-use-cal-desk t)
;(setq planner-diary-cal-desk-string "* Schedule")

;;;;;;;;;;;;;;;;;;;;
;; Insinuation    ;;
;;;;;;;;;;;;;;;;;;;;

(planner-accomplishments-insinuate)
(planner-diary-insinuate)
(planner-timeclock-summary-insinuate)
(planner-insinuate-calendar)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Planner and Gnus	   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load 'gnus
  '(progn
	 (require 'planner-gnus)
	 (planner-gnus-insinuate)
	 (defalias 'planner-gnus-browse-url 'my-planner-gnus-browse-url)))

(defun my-planner-gnus-browse-url (url)
  "If this is a Gnus URL, jump to it."
  (when (string-match "\\`gnus://\\(.+\\)/\\(.+\\)" url)
    (let ((group (match-string 1 url))
          (articles (match-string 2 url)))
      (when (featurep 'gnus-registry)
        (let ((reg-group (gnus-registry-fetch-group articles)))
          (when reg-group
            (if gnus-registry-use-long-group-names
                (setq group reg-group)
              (when (cadr (split-string group ":")) ;; group contains a :
                (setq group (concat (car (split-string group ":")) ":"
                                    reg-group)))))))
	  (message "going into group: %s for article: %s" group articles)
	  (gnus-fetch-group group)
      (let ((match-articles (if (fboundp 'gnus-find-matching-articles)
								(gnus-find-matching-articles "message-id" articles)
							  (gnus-summary-find-matching "message-id" articles
														  nil nil t))))
		(unless match-articles
		  (message "not found in default view, requesting search depth...")
		  (gnus-summary-insert-old-articles)
		  (setq match-articles (if (fboundp 'gnus-find-matching-articles)
								   (gnus-find-matching-articles "message-id" articles)
								 (gnus-summary-find-matching "message-id" articles
															 nil nil t))))
		(when match-articles
		  (gnus-summary-limit match-articles)
		  (gnus-summary-select-article))
		(unless match-articles
		  (message "requested article not found!")))
      t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remember in Planner	  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; allow planner integration with remember
(require 'remember-planner)

(defun remember-in-planner (&optional prefix)
  "Called to dynamically remember stuff in planner context"
  (interactive)
  (setq remember-handler-functions '(remember-planner-append))
  (setq remember-annotation-functions planner-annotation-functions)
  (remember prefix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project planner definition    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'muse-project-alist
             '("WikiPlanner"
               ("~/Plans"
                "~/Plans/daily-plans"
                "~/Plans/clearpath-networks"
                "~/Plans/corenova-consulting"
                "~/Plans/private"
                "~/Plans/quest"
                "~/Plans/development"
                :default "TaskPool"
                :major-mode planner-mode
                :visit-link planner-visit-link)

               (:base "planner-html" :path "~/Publishing/planner"
                      :exclude "/\\(clearpath-networks\\|corenova-consulting\\|private\\)/")
               (:base "planner-xml" :path "~/Publishing/planner/xml"
                      :exclude "/\\(clearpath-networks\\|corenova-consulting\\|private\\)/")
               ;; 
               (:base "planner-html" :path "~/Publishing/secure/private"
                      :include "/private/")
               (:base "cpn-planner-html" :path "~/Publishing/secure/clearpath"
                      :include "/clearpath-networks/")
               (:base "planner-html" :path "~/Publishing/secure/corenova"
                      :include "/corenova-consulting/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Planner Mode Keybindings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(planner-install-extra-task-keybindings)
(define-key planner-mode-map (kbd "TAB") 'indent-for-tab-command)
(define-key planner-mode-map (kbd "<tab>") 'indent-for-tab-command)
(define-key planner-mode-map (kbd "<backtab>") 'emacs-wiki-previous-reference)
(define-key planner-mode-map (kbd "C-c C-n") 'planner-create-note-from-task)
(define-key planner-mode-map (kbd "C-c C-j C-l") 'planner-jump-to-link)
(define-key planner-mode-map (kbd "C-c C-j TAB") 'muse-next-reference)
(define-key planner-mode-map (kbd "C-c C-j <backtab>") 'muse-previous-reference)
(define-key muse-mode-map    (kbd "C-c C-p") nil)
(define-key planner-mode-map (kbd "C-c C-p") nil)
(define-key planner-mode-map (kbd "C-c C-p C-t") 'muse-publish-this-file)
(define-key planner-mode-map (kbd "C-c C-p C-p") 'muse-project-publish)

(define-key planner-mode-map (kbd "C-c C-t C-e") 'planner-edit-task-description)
(define-key planner-mode-map (kbd "C-c C-t C-z") 'planner-task-open)
(define-key planner-mode-map (kbd "C-c C-t C-c") 'planner-task-cancelled)
(define-key planner-mode-map (kbd "C-c C-t C-d") 'planner-task-delegated)

(define-key planner-mode-map (kbd "C-c C-q") 'saint/planner-prompt-next-plan-task)
(define-key planner-mode-map (kbd "C-c C-s") 'sacha/planner-diary-schedule-task)
(define-key planner-mode-map (kbd "C-c C-z") 'sacha/planner-diary-unschedule-entry)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom publishing Styles    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'planner-publish)

(setq planner-html-style-sheet "<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" charset=\"utf-8\" href=\"/planner.css\">")
(setq planner-html-javascript "
  <script type=\"text/javascript\" src=\"/Master.js\"></script>
  <script type=\"text/javascript\" src=\"/planner.js\"></script>\n")

(setq planner-html-header 
      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">
<html>
  <head>
    <lisp>(muse-publishing-standard-html-header)</lisp>
    <lisp>planner-html-style-sheet</lisp>
    <lisp>planner-html-javascript</lisp>
  </head>
  <body>
    <div id=\"content\">
      <h1><span><lisp>(muse-publishing-standard-title)</lisp></span></h1>
      <div id=\"inner-header\">
        <lisp>planner-html-inner-header</lisp>
      </div>
      <div id=\"muse-sections\">
<!-- Page published by Emacs Muse begins here -->\n")

(setq planner-html-inner-header 
      "<div class=\"left\">
       </div>
       <div class=\"right\">
          <div class=\"text\">
          <span>Peter K. Lee's wikiblog</span>
          on personal information management, emacs, wireless, 
          future technology, invisible shadows
          <form class=\"search\" method=\"POST\" action=\"/search\">
            <input type=\"text\" name=\"q\" size=\"20\" maxlength=\"255\"
                   value=\"search\" onfocus=\"javascript:this.value=''\"/>
            <input type=\"submit\" value=\"Search\"/>
          </form>
          </div>
       </div>")

(setq planner-html-footer "
<!-- Page published by Emacs Muse ends here -->
      </div>
      <div id=\"inner-footer\">
        <lisp>planner-html-inner-footer</lisp>
      </div>
    </div>
  </body>
</html>\n")

(setq planner-html-inner-footer
      "<div class=\"separator\">
          <div class=\"text\">
          &copy; 2005 &nbsp; Peter K. Lee &nbsp; all rights reserved.
          </div>
       </div>
       <div class=\"image-link\">
          <a href=\"http://www.emacswiki.org/\">
            <img alt=\"Emacswiki.org Logo\" src=\"/static/logos/emacswiki.png\"/>
          </a>
          <a href=\"http://www.catb.org/~esr/hacker-emblem/\">
            <img alt=\"Hacker's Emblem\" src=\"/static/logos/glider.png\"/>
          </a>
       </div>")

;; new publishing style for work-related projects

(defun cpn-planner-before-publish ()
  "setup the inner-html and inner-footer for cpn publications.
  Must return nil to continue before hooks processing."
  (set (make-local-variable 'planner-html-inner-header) "")
  (set (make-local-variable 'planner-html-inner-footer) 
       "2004, 2005 &copy; Clearpath Networks, all rights reserved.")
  nil)

(unless (assoc "cpn-planner-html" muse-publishing-styles)
  (muse-derive-style "cpn-planner-html" "planner-html"
		     :maintainer "plee@clearpathnet.com"
             :before 'cpn-planner-before-publish))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Customizations    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice plan (after saint activate)
  "If using planner-timeclock, clock in when M-x plan."
  (when (fboundp 'timeclock-currently-in-p)
    (unless (timeclock-currently-in-p)
      (timeclock-in nil ": lounging inside planner"))))

(defun sacha/htmlfontify-insert-region (buffer begin end)
  "Insert into BUFFER the htmlified text between BEGIN and END."
  (require 'htmlfontify)
  (save-excursion
  (let* ((hfy-optimisations (cons 'skip-refontification hfy-optimisations))
		 (input-text (buffer-substring begin end))
		 (temp-file (make-temp-file "html-input"))
		 output-buffer)
	(with-temp-buffer
	  (insert input-text)
	  (setq buffer-file-name temp-file)
	  (save-excursion (setq output-buffer (htmlfontify-buffer nil nil)))
	  (set-buffer-modified-p nil))
	(unwind-protect
		(let (b e yanked-output)
		  (set-buffer output-buffer)
		  (goto-char (point-min))
		  (search-forward "<pre>\n")
		  (setq b (line-beginning-position))
		  (goto-char (point-max))
		  (search-backward "</pre>")
		  (forward-line -1)
		  (setq e (line-beginning-position))
		  (setq yanked-output (buffer-substring-no-properties b e))
		  (set-buffer buffer)
		  (insert yanked-output))
	  (set-buffer output-buffer)
	  (set-buffer-modified-p nil)
	  (delete-file temp-file)
	  (kill-buffer output-buffer)))))

;;;_+ Diary integration to Planner via scheduling

(defun sacha/planner-diary-schedule-task (start end)
  "Add a diary entry for the current task from START to END."
  (interactive "MStart: \nMEnd: ")
  (save-window-excursion
    (save-excursion
      (save-restriction
        (let* ((info (planner-current-task-info))
               (original (planner-task-description info))
               main
               description)
          ;; TODO: Mark the task as scheduled for a particular time
          (setq description
                (cond
                 ((string-match "^\\(.+\\)\\s-+{{Schedule:\\([^-]+\\)-\\([^}]+\\)}}\\(.*\\)" original)
                  (setq main (match-string 1 original))
                  (save-excursion
                    (save-match-data
                      (goto-char (point-min))
                      (when (re-search-forward
                             (concat (match-string 2 original)
                                     " | "
                                     (match-string 3 original)
                                     " | "
                                     (match-string 1 original))
                             nil t)
                        (sacha/planner-diary-unschedule-entry))))
                  (concat (match-string 1 original)
                          " {{Schedule:"
                          start
                          "-"
                          end
                          "}}"
                          (match-string 4 original)))
                 ((string-match "\\(.*\\)\\(\\s-*\\)$" original)
                  (setq main (match-string 1 original))
                  (replace-match (concat " {{Schedule:" start "-" end "}}")
                                 t t original 2))))
          (planner-edit-task-description description)
          ;; Add the diary entry
          (sacha/planner-diary-add-entry
           (planner-task-date info)
           (concat start " | " end " | " main)))))))

(defun sacha/planner-diary-add-entry (date text &optional annotation)
  "Prompt for a diary entry to add to `diary-file'."
  (interactive
   (list
    (if (or current-prefix-arg
            (not (string-match planner-date-regexp (planner-page-name))))
        (planner-read-date)
      (planner-page-name))
    (read-string
     "Diary entry: ")))
  (save-excursion
    (save-window-excursion
      (let ((inhibit-read-only t))
        (make-diary-entry
         (concat
          (let ((cal-date (planner-filename-to-calendar-date date)))
            (calendar-date-string cal-date t t))
          " " text
          (or annotation
              (let ((annotation (run-hook-with-args-until-success
                                 'planner-annotation-functions)))
                (if annotation
                    (concat " " annotation)
                  ""))))))
      (planner-goto date)
      (planner-diary-insert-diary-maybe))))

(defun sacha/planner-diary-unschedule-entry ()
  "Unschedule the current entry."
  (interactive)
  (goto-char (line-beginning-position))
  (let ((id
         (if (re-search-forward "{{Tasks:\\([^}]+\\)}}" (line-end-position) t)
             (match-string 0)
           nil)))
    (sacha/planner-diary-delete-entry)
    (when id
      (planner-seek-to-first "Tasks")
      (re-search-forward id nil t))))

(defun sacha/planner-diary-delete-entry ()
  "Delete the current entry from `diary-file'."
  (interactive)
  (let ((cal-date (planner-filename-to-calendar-date (planner-page-name)))
        (text (buffer-substring (line-beginning-position)
                                (line-end-position)))
        (case-fold-search nil))
    (save-excursion
      (save-window-excursion
        (let ((inhibit-read-only t))
          (find-file diary-file)
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-max))
              (when (re-search-backward
                     (concat "^"
                             (regexp-quote
                              (concat (calendar-date-string cal-date t t)
                                      " " text))))
                (delete-region (line-beginning-position)
                               (min (1+ (line-end-position)) (point-max))))
              (save-buffer))))
        (planner-diary-insert-diary-maybe t)))))

;;;_+ Keep track of what I'm supposed to be doing

;; I've bound sacha/planner-what-am-i-supposed-to-be-doing to F9 F9. I
;; start out by clocking into the task (use planner-timeclock.el and
;; C-c TAB to mark a task as in progress). Then, when I find myself
;; getting distracted, I hit F9 F9 to see my current task in the
;; minibuffer. C-u F9 F9 jumps back to the task so that I can either
;; mark it as postponed. M-x planner-task-pending (bound to C-c C-p in
;; my local config) and M-x planner-task-done (C-c C-x) both clock out
;; of the task. If I want to jump back to the previous window
;; configuration from that planner page, I can just hit F9 F9 again.

(defvar sacha/window-register "w"
  "Register for jumping back and forth between planner and wherever I am.")
(defvar sacha/planner-current-task nil
  "Current task info.")
(
 defadvice planner-task-in-progress (after sacha activate)
  "Keep track of the task info."
  (setq sacha/planner-current-task (planner-current-task-info)))

(defun sacha/planner-what-am-i-supposed-to-be-doing (&optional prefix)
  "Make it easy to keep track of what I'm supposed to be working on.
If PREFIX is non-nil, jump to the current task, else display it
in a message. If called from the plan page, jump back to whatever
I was looking at."
  (interactive "P")
  (if planner-timeclock-current-task
      (if (and (string= (planner-task-page sacha/planner-current-task)
                        (planner-page-name))
               sacha/window-register)
          (jump-to-register sacha/window-register)
        (if (null prefix)
            (message "%s" planner-timeclock-current-task)
          (frame-configuration-to-register sacha/window-register)
          (planner-find-file (planner-task-page sacha/planner-current-task))
          (planner-find-task sacha/planner-current-task)))
    (if prefix
        (planner-goto-today)
      (message "No current task. HEY!"))))

