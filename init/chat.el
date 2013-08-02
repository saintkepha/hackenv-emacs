;; -*- emacs-lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Jabber Client ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "emacs-jabber-0.8.0" emacs-packages-dir))
(require 'sasl)
(require 'jabber-autoloads)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs IRC client
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/erc")

(defalias 'irc 'erc-select)

(require 'erc)

;; (setq erc-modules
;; 	  (quote (autoaway autojoin button fill irccontrols match netsplit noncommands pcomplete completion ring scrolltobottom services track)))
;; (erc-update-modules)

(setq erc-server "irc.freenode.net" 
	  erc-port 6667 
	  erc-nick "saintkepha"
	  erc-user-full-name user-full-name
	  erc-email-userid "plee"		 	; for when ident is not activated
	  erc-prompt-for-password nil	    ; OPN doesn't require passwords
	  erc-prompt-for-nickserv-password t)

;; (setq erc-auto-set-away t 
;; 	  erc-autoaway-use-emacs-idle nil
;; 	  erc-autoaway-idle-seconds 600
;; 	  erc-autoaway-message "This has been buried (autoaway after %i seconds of idletime)")

;; (setq erc-autojoin-channels-alist
;; 	  '(("clearpathnet.com" "#cpn")
;; 		("freenode.net" "#emacs" "#wiki" "#openstack" "#openstack-meeting")
;; 		("oftc.net" "#bitlbee")))

;; (setq erc-pals '("M0J0jojo" "elf" "forcer" "johnw" "bojohan" "johnsu01" "bpalmer" "alexv" "Mojo"))

;; (setq erc-keywords
;;       `(,(regexp-opt '("xemacs" "emacs" "elisp" "lisp" "gnus" "cl" "clos")
;; 		     'words)
;;         (,(regexp-opt '("ecl" "ecls" "erc") 'words) . erc-nick-msg-face))
;;       erc-current-nick-highlight-type 'nick-or-keyword)

;; (setq erc-echo-notices-in-minibuffer-flag t) ; notice notices. :)
;; (setq erc-auto-query 'window-noselect)		 ; open a query window when someone messages me
;; (setq erc-current-nick-highlight-type 'nick) ; highlight my references

;; (setq erc-fill-function 'erc-fill-static)
;; (setq erc-fill-static-center 12)
;; (setq erc-fill-column 88)

;; ;; erc-track
;; (setq erc-track-showcount t
;;       erc-track-switch-direction 'leastactive
;;       erc-track-position-in-mode-line nil)


;; (add-hook 'erc-mode-hook 'erc-add-scroll-to-bottom)

;; 										; I have big frame windows, so truncate is okay
;; (add-hook 'erc-mode-hook 
;; 		  (lambda ()
;;             ;; Read-only history
;;             (erc-readonly-mode 1)
;; 			(set (make-local-variable 'truncate-partial-width-windows) nil)
;; 										;(define-key erc-mode-map (kbd "C-c M-w") 'erc-kill-region-text)
;; 			))

(defun erc-kill-region-text (from to)
  "Copy all actual data in the region to the kill ring.
This ignores nicks, timestamps, markup added by ERC, and
whatnot."
  (interactive "r")
  (save-excursion
    (goto-char from)
    (forward-line 0)
    (let ((parsed nil)
          (lis '()))
      (while (< (point) to)
        (let ((last-parsed parsed))
          (setq parsed (get-text-property (point)
										  'erc-parsed))
          (when (and parsed
                     (string= (aref parsed 3)
                              "PRIVMSG")
                     (or (not last-parsed)
                         (not (string= (aref last-parsed 5)
                                       (aref parsed 5)))))
            (setq lis (cons (aref parsed 5)
                            lis))))
        (forward-line 1))
      (kill-new (mapconcat #'identity (reverse lis) "\n")))))

;;; ERC...err...prompt -----------------------------------------
(defun my-erc-prompt-magic-time-string ()
  (let ((string "00:00"))
    (propertize
     "$$:$$"
     'display `(when (store-substring ,string 0 (format-time-string "%H:%M"))
				 . ,string))))

(defun my-erc-prompt-color-erc ()
  (let ((string "ERC"))
    (dolist (props `((0 1 (face ((:foreground "red"))))
					 (1 2 (face ((:foreground "blue"))))
					 (2 3 (face ((:foreground "yellow"))))))
      (apply 'add-text-properties (append props (list string))))
    string))

(defun my-erc-prompt-dashes-1 (&optional col)
  (make-string (- (window-width)
				  (or col (save-excursion 
							(goto-char buffer-position)
							(current-column)))
				  2)
			   ?-))

(defun display-spec-hack (form)
  (let ((spec (list '(margin) "")))
    `(when (progn (setcdr ',spec ,form) t) . ,spec)))

(defun my-erc-prompt-dashes (len)
  (let ((form `(my-erc-prompt-dashes-1 ,len)))
    (propertize "$" 'display (display-spec-hack form))))

(defun my-erc-prompt ()
  (if (boundp 'cb)
      (erc-command-indicator)			; ?
    (let* ((string
			;;(erc-prepare-mode-line-format '("[ ERC : " target " ] " "\n"))
			(format "  %s          (%s)--------------------------------------[%s]\n"
					(propertize (erc-format-target)
								'face '((foreground-color . "white")))
					(my-erc-prompt-magic-time-string)
					(my-erc-prompt-color-erc))))
	  ;;	   (string (concat str1 (my-erc-prompt-dashes (length str1)) "\n")))
      
      (dolist (props '((face ((:background "magenta" 
										   :box (:line-width 2 :style released-button))))
					   (field erc-prompt)))
		(apply 'font-lock-append-text-property
			   0 (length string) (append props (list string))))
      string)))

;; helper defun to unfill lines that have been cut from elsewhere
(defun erc-unfill ()
  "Unfill the region after the prompt. Intended to be called just before you
    send a line"
  (interactive)
  (save-excursion
	(end-of-buffer)
	(goto-char (previous-single-property-change (point)
												'erc-prompt))
	(while (search-forward "\n" nil t)
	  (delete-backward-char 1)
	  (just-one-space))))

;; (setq erc-prompt            'my-erc-prompt
;;       erc-command-indicator nil)

(defun erc-cmd-ALIAS (command &rest words)
  (fset (intern (concat "erc-cmd-" (upcase command)))
		(list 'lambda '(&rest ignore)
			  (list 'erc-send-command (mapconcat 'identity words " ")))))


;;;;;;;;;;;;;;;;;;
;; Lisppaste    ;;
;;;;;;;;;;;;;;;;;;

(autoload 'lisppaste-create-new-paste "lisppaste" "Interactively create a new paste." t)
(autoload 'lisppaste-create-new-annotation "lisppaste" "Interactively annotate a paste." t)
(autoload 'lisppaste-display-paste "lisppaste" "Interactively show a paste." t)
