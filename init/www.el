;; -*- Mode: Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WWW stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;
;; Wget    ;;
;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "emacs-wget-0.5.0" emacs-packages-dir))

(setq wget-download-directory "~/Download")

(autoload 'wget "wget" "wget interface for Emacs." t)
(autoload 'wget-web-page "wget" "wget interface to download while web page." t)
(eval-after-load "dired"
  `(if (boundp 'dired-mode-map)
       (define-key dired-mode-map "W" 'wget)))


;;;;;;;;;;;;
;; W3M    ;;
;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "emacs-w3m-1.4.4" emacs-packages-dir))

(if (= emacs-major-version 23)
	(provide 'w3m-e23)
  (require 'w3m-load))
(require 'w3m-wget)

(autoload 'w3m-browse-url "w3m" "Ask emacs-w3m to browse URL." t)

(define-key global-map (kbd "C-x w") 'w3m)
(define-key global-map (kbd "C-x W") 'browse-url-at-point)

(setq w3m-fill-column 100
      w3m-mailto-url-function 'message-mail
      w3m-use-cookies t
      w3m-use-ange-ftp t)

(setq browse-url-browser-function 'w3m-browse-url)

(add-hook 'w3m-display-hook 'saint/w3m-view-source)

(eval-after-load "w3m-form"
  '(progn
     (setq w3m-form-input-textarea-buffer-lines 25)
	 (defun saint/w3m-textarea-hook ()
	   "Hook to make buffer big :)"
       (mapcar (lambda (v)
         (if (string-match "^w3m-form-input-textarea.*"
						   (symbol-name (car v)))
             (put (car v) 'permanent-local t)))
			   (buffer-local-variables))
       ;; (saint:w3m-textarea-mode)
       (set (make-local-variable 'truncate-partial-width-windows) nil)
	   (save-excursion
         (goto-char (point-min))
		 (while (re-search-forward "\r\n" nil t)
		   (replace-match "\n"))))
	 (add-hook 'w3m-form-input-textarea-mode-hook 'saint/w3m-textarea-hook)
	 ;; (define-minor-mode saint:w3m-textarea-mode
	 ;;   "Minor mode used while editing w3m textareas."
	 ;;   nil " saint:w3m-textarea" w3m-form-input-textarea-keymap)
	 ))

;; (if (require 'w3m-session nil t)   ; enable session saving between loads
;; 	(setq w3m-session-file "~/.w3m/w3m-session"
;; 		  w3m-session-save-always nil
;; 		  w3m-session-load-always nil
;; 		  w3m-session-show-titles t
;; 		  w3m-session-duplicate-tabs 'ask)
;;   (message "w3m-session not available"))

(defun saint/w3m-view-source (url)
  "Suitable for adding to `w3m-display-hook'."
  (if (string-match "\\`about://source/" url)
	  (progn
		(require 'html-font)
		(html-font-setup)
		(font-lock-set-defaults) ; need to call this!
		(font-lock-fontify-buffer))))

; The following code makes it possible to save the w3m buffers between sessions with desktop.el
; the following is broken!
(defun desktop-buffer-w3m-misc-data ()
  "Save data necessary to restore a `w3m' buffer."
  (when (eq major-mode 'w3m-mode)
	w3m-current-url))

(defun desktop-buffer-w3m ()
  "Restore a `w3m' buffer on `desktop' load."
  (when (eq 'w3m-mode desktop-buffer-major-mode)
	(let ((url desktop-buffer-misc))
	  (when url
		(require 'w3m)
		(if (string-match "^file" url)
			(w3m-find-file (substring url 7))
		  (w3m-goto-url url))
		(current-buffer)))))

(if (require 'desktop nil t)   ; we have desktop
	(progn
;	  (add-to-list 'desktop-buffer-handlers 'desktop-buffer-w3m)
;	  (add-to-list 'desktop-buffer-misc-functions 'desktop-buffer-w3m-misc-data)
;	  (add-to-list 'desktop-buffer-modes-to-save 'w3m-mode)
	  )
  (message "desktop not available for w3m integration!"))
