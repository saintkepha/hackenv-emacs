;; -*- Mode: Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Muse - powerful authoring environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "~/Archives/muse--peter/lisp"))

(require 'outline)       ; I like outline-style faces
(require 'muse)          ; load generic module
(require 'muse-colors)   ; load coloring/font-lock module
(require 'muse-mode)     ; load authoring mode
(require 'muse-blosxom)  ; load blosxom module
(require 'muse-html)     ; load (X)HTML publishing style
(require 'muse-wiki)     ; load Wiki support
(require 'muse-xml)      ; load XML publishing style
(require 'muse-journal)  ; load journal module

(global-set-key (kbd "C-x p j") 'muse-journal-add-entry)

(add-hook 'muse-mode-hook (lambda () 
                            (set (make-local-variable 'footnote-section-tag) "* Footnotes")
                            (set (make-local-variable 'footnote-section-tag-regexp) "\\* Footnotes\\(\\[.\\]\\)?")
                            (footnote-mode) 
                            (turn-on-auto-fill)
                            (flyspell-mode)))

(add-to-list 'muse-project-alist
			 '("Muse"
			   ("~/Muse"
				:book-part "Journal"
				:book-style "journal-book-pdf"
				:book-funcall muse-insert-reset-chapter
				"~/Muse/journal/current.muse" ; the current journal file
				"~/Muse/journal/2005"
				:book-part "Publication"
				:book-style "chapbook-pdf"
				:book-funcall muse-insert-reset-chapter
				:nochapters t
				:book-chapter "Books"
				"~/Muse/books"
				:book-chapter "Commentary"
				"~/Muse/commentary"
				:book-chapter "Documentation"
				"~/Muse/documentation"
				:book-chapter "Research"
				"~/Muse/research"
				:book-end t
				:major-mode muse-mode
				:default "WelcomePage")

			   ;; journal
			   (:base "journal-html" :path "~/Publishing/journal"
					  :include "/journal/")
			   (:base "journal-pdf" :path "~/Publishing/pdf"
					  :include "/journal/")
			   (:base "journal-rss" :path "~/Publishing/journal"
					  :include "/journal/current.muse"
					  :base-url "http://saint.corenova.com/journal")
			   ;; books
			   (:base "html" :path "~/Publishing/books"
					  :include "/books/")
			   ;; commentary
			   (:base "html" :path "~/Publishing/commentary"
					  :include "/commentary/")
			   ;; documentation
			   (:base "html" :path "~/Publishing/documentation"
					  :include "/documentation/")
			   ;; research
			   (:base "html" :path "~/Publishing/research"
					  :include "/research/")))

(add-to-list 'muse-project-alist
			 '("WorkReference"
			   ("~/Muse/job-postings"
				:default "Listing"
				:major-mode muse-mode)

			   (:base "html" :path "~/Publishing/job-postings"
					  :include "/job-postings/")))

(setq muse-wiki-interwiki-alist
  '(("EmacsWiki" . "http://www.emacswiki.org/cgi-bin/wiki/")
    ("ArchWiki"  . "http://wiki.gnuarch.org/")
    ("MyPlanner" . "http://saint.corenova.com/planner/")))


;;;_ + extension routines

(defun muse-publishing-standard-title ()
  "Default page title."
  (concat (muse-publishing-directive "title")
          (let ((author (muse-publishing-directive "author")))
            (when author
              (if (not (string= author (user-full-name)))
                  (concat " (by " author ")"))))))

(defun muse-publishing-standard-html-header (&optional title)
  "Default standard html header."
  (concat "<title>"
          (or title (muse-publishing-standard-title))
          "</title>"
          "<meta name=\"generator\" content=\"muse.el\">"
          "<meta http-equiv=\"" muse-html-meta-http-equiv "\""
          " content=\"" muse-html-meta-content-type "\">"
          (let ((maintainer (muse-style-element :maintainer)))
            (when maintainer
              (concat "<link rev=\"made\" href=\"" maintainer "\">\n")))))

(defun muse-insert-reset-chapter ()
  (insert "\n\\setcounter{chapter}{1}\n"))

(defun muse-journal-add-entry ()
  (interactive)
  (muse-project-find-file "current.muse" "Writing")
  (goto-char (point-min))
  (forward-line 2)
  (insert "* " (format-time-string "%Y%m%d: ")
	  (read-string "Journal entry title: ")
	  "\n\n\n\n")
  (forward-line -2))

