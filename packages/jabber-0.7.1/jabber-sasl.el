;; jabber-sasl.el - SASL authentication

;; Copyright (C) 2004, 2007 - Magnus Henoch - mange@freemail.hu

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(require 'cl)

;;; This file uses sasl.el from FLIM or Gnus.  If it can't be found,
;;; jabber-core.el won't use the SASL functions.
(eval-and-compile
  (condition-case nil
      (require 'sasl)
    (error nil)))

;;; Alternatives to FLIM would be the command line utility of GNU SASL,
;;; or anything the Gnus people decide to use.

;;; See XMPP-CORE and XMPP-IM for details about the protocol.

(require 'jabber-xml)

(defun jabber-sasl-start-auth (jc stream-features)
  ;; Find a suitable common mechanism.
  (let* ((mechanism-elements (car (jabber-xml-get-children stream-features 'mechanisms)))
	 (mechanisms (mapcar
		      (lambda (tag)
			(car (jabber-xml-node-children tag)))
		      (jabber-xml-get-children mechanism-elements 'mechanism)))
	 (mechanism
	  (if (and (member "ANONYMOUS" mechanisms)
		   (yes-or-no-p "Use anonymous authentication? "))
	      (sasl-find-mechanism '("ANONYMOUS"))
	    (sasl-find-mechanism mechanisms))))

    ;; No suitable mechanism?
    (if (null mechanism)
	;; Maybe we can use legacy authentication
	(let ((node (find "http://jabber.org/features/iq-auth"
			  (jabber-xml-get-children stream-features 'auth)
			  :key #'(lambda (node) (jabber-xml-get-attribute node 'xmlns))
			  :test #'string=)))
	  (if node
	      (fsm-send jc :use-legacy-auth-instead)
	    (message "No suitable SASL mechanism found")
	    (fsm-send jc :authentication-failed)))

      ;; Watch for plaintext logins over unencrypted connections
      (when (and (not *jabber-encrypted*)
		 (member (sasl-mechanism-name mechanism)
			 '("PLAIN" "LOGIN"))
		 (not (yes-or-no-p "Jabber server only allows cleartext password transmission!  Continue? ")))
	(error "Login cancelled"))

      ;; Start authentication.
      (let* ((client (sasl-make-client mechanism 
				       (plist-get (fsm-get-state-data jc) :username)
				       "xmpp"
				       (plist-get (fsm-get-state-data jc) :server)))
	     (step (sasl-next-step client nil)))
	(jabber-send-sexp
	 jc
	 `(auth ((xmlns . "urn:ietf:params:xml:ns:xmpp-sasl")
		 (mechanism . ,(sasl-mechanism-name mechanism)))
		,(when (sasl-step-data step)
		   (base64-encode-string (sasl-step-data step) t))))
	(cons client step)))))

(defun jabber-sasl-process-input (jc xml-data sasl-data)
  (let ((sasl-read-passphrase #'jabber-read-passwd)
	(client (car sasl-data))
	(step (cdr sasl-data)))
    (cond
     ((eq (car xml-data) 'challenge)
      (sasl-step-set-data step (base64-decode-string (car (jabber-xml-node-children xml-data))))
      (setq step (sasl-next-step client step))
      (jabber-send-sexp
       jc
       `(response ((xmlns . "urn:ietf:params:xml:ns:xmpp-sasl"))
		  ,(when (sasl-step-data step)
		     (base64-encode-string (sasl-step-data step) t)))))

     ((eq (car xml-data) 'failure)
      (message "SASL authentication failure: %s"
	       (jabber-xml-node-name (car (jabber-xml-node-children xml-data))))
      (fsm-send jc :authentication-failure))

     ((eq (car xml-data) 'success)
      (message "Authentication succeeded")
      (fsm-send jc :authentication-success)))
    (cons client step)))

(provide 'jabber-sasl)
;;; arch-tag: 2a4a234d-34d3-49dd-950d-518c899c0fd0
