;; -*- emacs-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gnus setup  (Info-goto-node "(gnus)Top")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "gnus/lisp"    emacs-packages-dir))
(add-to-list 'load-path (expand-file-name "gnus/contrib" emacs-packages-dir))

(setq gnus-init-file (expand-file-name "gnus.el" emacs-home-dir))
(setq gnus-directory (expand-file-name "~/Gnus"))

(setq mail-user-agent 'gnus-user-agent)

(defun saint/gnus-buffer-p (&optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (when (boundp 'gnus-buffers)
    (or (find buffer (gnus-buffers))
        (string-equal "*BBDB*" (buffer-name buffer)))))

;; take a look at gnus-buffers and see if we can find a match there.
(defun saint/gnus ()
  (interactive)
  (if (saint/gnus-buffer-p)
      (saint/bury-gnus)
	(saint/unbury-gnus)))

(defun saint/unbury-gnus ()
  (interactive)
  (setq saint/current-window-configuration (current-window-configuration))
  (if (or (not (fboundp 'gnus-alive-p))
          (not (gnus-alive-p)))
	  (gnus-unplugged)
	(when (boundp 'saint/gnus-window-configuration)
	  (set-window-configuration saint/gnus-window-configuration))))

(defun saint/bury-gnus ()
  "When inside a GNUS buffer, save the gnus window configuration,
  bury the gnus related buffers, and load previously known window
  configuration."
  (interactive)
  (setq saint/gnus-window-configuration (current-window-configuration))
  (let ((buf nil))
    (dolist (buf (buffer-list))
      (when (saint/gnus-buffer-p buf)
        (delete-other-windows)
        (if (eq (current-buffer) buf)
            (bury-buffer)
		  (bury-buffer buf))))
    (when (and (boundp 'saint/current-window-configuration)
               saint/current-window-configuration)
      (set-window-configuration saint/current-window-configuration))))

;; quit gnus properly instead of leaving auto-save files around
(
 defadvice save-buffers-kill-emacs (before quit-gnus (&rest args) activate)
  (let (buf)
    (when (and (fboundp 'gnus-alive-p)
               (gnus-alive-p)
               (bufferp (setq buf (get-buffer "*Group*"))))
      (with-current-buffer buf
        (gnus-group-exit)))))
    
;; i'm in the habit of quitting when i don't really need to
(add-hook 'gnus-group-mode-hook
		  (lambda ()
			(local-set-key (kbd "q") 'gnus-group-suspend)
			(local-set-key (kbd "Q") 'gnus-group-exit)))

(global-set-key (kbd "<f8>") 'saint/gnus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
(add-hook 'gnus-message-setup-hook 'turn-on-auto-fill)
