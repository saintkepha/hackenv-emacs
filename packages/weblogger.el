;;; Saved through ges-version 0.3.2dev at 2002-12-06 09:18
;;; ;;; From: mah@everybody.org (Mark A. Hershberger)
;;; ;;; Subject: weblogger.el v. 
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: Thu, 05 Dec 2002 22:12:51 -0600


;;; This version adds the ability to edit weblog templates.

;;; ;; C-c C-t m -- (in the *weblogger-post* buffer) edit the main
;;; ;;              template.
;;; ;;
;;; ;; C-c C-t a -- (in the *weblogger-post* buffer) edit the archive
;;; ;;              index template.
;;; ;;
;;; ;; C-x C-s   -- (in the *weblogger-template* buffer) save the template


;;; NOTES: This code pings weblogs.com by default.  To avoid that
;;;        ping, remove weblogger-ping-weblogs from
;;;        weblogger-new-post-hook.

;;;        To change the server that it pings, change
;;;        weblogger-ping-urls.

;;;        weblogger-server-url is set to openweblog.com.  If you
;;;        want to use another server, you will have to customize
;;;        this variable.  If you want to use Blogger, for example,
;;;        it would have to be set to
;;;        "http://plant.blogger.com/api/RPC2".

;; weblogger.el - Weblog maintenance via XML-RPC APIs

;; Copyright (C) 2002 Mark A. Hershberger.
;; Inspired by code Copyright (C) 2001 by Simon Kittle.

;; Author: Mark A. Hershberger <mah@everybody.org>
;; Version: 1.2
;; Created: 2002 Oct 11
;; Keywords: weblog blogger cms movable type openweblog blog
;; URL: http://elisp.info/package/weblogger/

;; This file is not yet part of GNU Emacs.

;; weblogger.el free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; weblogger.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; weblogger.el implements the Blogger, MetaWeblog, and Movable Type
;; APIs to talk to server-side weblog software.
;;
;; For ease of use:
;; I use the following commands in my .emacs file:
;;
;; (load-file "weblogger.el")
;; (global-set-key "\C-cbs" 'weblogger-start-post)
;;
;; C-c b s will then switch to a new buffer where you can compose a
;; post.
;;
;; C-x C-s    -- post-and-publish current buffer to the weblog.
;;               Calling weblogger-save-post with an prefix argument
;;               (i.e. C-u C-x C-s) will prompt for which weblog
;;               to use.
;;
;; C-c C-c    -- identical to C-x C-s, but will also bury the buffer.
;;
;; C-c C-n    -- post (but not publish) the current message and
;;               load the next message.
;;
;; C-c C-p    -- post (but not publish) the current message and
;;               load the previous message.
;;
;; C-c C-k    -- delete the current post.
;;
;; M-g        -- synchronise weblogger.el's idea of the posts available
;;               with the weblog server.
;;
;; C-c C-t m -- (in the *weblogger-post* buffer) edit the main
;;              template.
;;
;; C-c C-t a -- (in the *weblogger-post* buffer) edit the archive
;;              index template.
;;
;; C-x C-s   -- (in the *weblogger-template* buffer) save the template
;;
;;
;;
;; Notes:
;; ----
;;
;; This code was originally based on Simon Kittle's blogger.el
;; (http://www.tswoam.co.uk/files/blogger.el.txt), but where his
;; code calls a Perl program, this code uses xml-rpc.el.  You can
;; get xml-rpc.el from <http://elisp.info/package/xml-rpc/>
;;
;; Differences between SK's blogger.el and weblogger.el
;;
;; - Doesn't need any external programs.  Uses xml-rpc.el.
;; - I've added a bunch of defcustom's here to make this integrate
;;   better with Emacs Customization interface. 
;; - Created a *weblogger-post* mode.
;; - Made selection of a weblog more intuitive.  It queries the
;;   server and allows the user to choose the name of the
;;   weblog from a list.
;; - Prompt for weblog on initial post if weblogger-id isn't set.
;; - Can "ping" http://weblogs.com/ and http://blo.gs/ whenever
;;   you update.
;; - Can "scroll" through posts on the weblog server and edit them.
;; - Many other features.
;;
;;  TODO:
;;  * Implement more MovableType extentions
;;
;; Bugs:
;;
;;  * When you delete a post it gets deleted, but it doesn't
;;    disappear from your post ring until you sync (M-g) with the
;;    server.  But this could be construed as a (mis)feature.


(require 'xml-rpc)
(require 'message)
(require 'ring)

(defgroup weblogger nil
  "Edit Weblogs with Emacs."
  :group 'emacs)

(defcustom weblogger-blogger-app-key "07C72E6970E0FBA5DE21BA9F4800C44534C19870"
  "The appkey to send to weblog server.  Generally this shouldn't be changed."
  :group 'weblogger
  :type 'string)

(defcustom weblogger-server-username nil
  "Your weblog server username.  You will be prompted if this is left nil."
  :group 'weblogger
  :type 'string)

(defcustom weblogger-server-password nil
  "Your password.  You will be prompted if this is left nil."
  :group 'weblogger
  :type 'string)

(defcustom weblogger-server-url "http://www.openweblog.com/xmlrpc/"
 "Server you want to use.  If this is an OpenWeblog.com site, leave this
at the default.  Otherwise, you will need to change it."
  :group 'weblogger
  :type 'string)

(defcustom weblogger-weblog-id nil
  "Your Blog ID.  If the server is a blogger server, you can
leave this nil and select which blog you wish to post to at
post-time.  If it is a Manila site, you need to provide the URL
of your site."
  :group 'weblogger
  :type 'string)

(defcustom weblogger-max-entries-in-ring 20
  "Maximum number of posts that can be editted.  There may be a
server-side limitation to this number."
  :group 'weblogger
  :type 'integer)

(defcustom weblogger-ping-urls '("http://rpc.weblogs.com/RPC2")
  "List of URLs to ping using the XML-RPC interface defined at 
<http://www.xmlrpc.com/weblogsCom>."
  :group 'weblogger
  :type 'list)

(defvar weblogger-message-list nil
  "List of blogger messages that we know about. Chronological
order, with newest first.")

(defvar weblogger-server-userid nil
  "Server-side ID of logged in user.")

(defvar *weblogger-post* nil
  "The blogger buffer where we compose posts")

(defvar weblogger-post-mode-hook nil
  "Hook to run after starting up weblogger mode.")

(defvar weblogger-new-post-hook '(weblogger-ping-weblogs)
  "Hook to run after sending a new post.  Typically, this is
where you would put weblogger-ping-weblogs to let weblog
aggregators know that you have updated.")

(defvar weblogger-edit-post-hook nil
  "Hook to run after updating a new post.")

(defvar weblogger-post-mode-map nil
  "Keymap for weblogger-post-mode.")

(defvar weblogger-template-mode-map nil
  "Keymap for weblogger-template-mode.")

(defvar weblogger-post-ring nil
  "Ring that holds all the posts")

(defvar weblogger-ring-index 0
  "Pointer to the index on the ring")

(defvar weblogger-capabilities '(("blogger.newPost" . nil)
				 ("blogger.editPost" . nil)
				 ("blogger.getRecentPosts" . nil)
				 ("blogger.getUsersBlogs" . nil)
				 ("blogger.getUserInfo" . nil)
				 ("blogger.deletePost" . nil)
				 ("blogger.getTemplate" . nil)
				 ("blogger.setTemplate" . nil)
				 ("metaWeblog.getPost" . nil)
				 ("metaWeblog.newPost" . nil)
				 ("metaWeblog.editPost" . nil)
				 ("metaWeblog.getRecentPosts" . nil)
				 ("mt.setPostCategories" . nil)
				 ("mt.getPostCategories" . nil)
				 ("mt.getTrackbackPings" . nil))
  "Known capabilities of the remote host")

(defvar weblogger-default-title ""
  "The Default title to use when making a post.  This is added if
your weblog server supports titles on posts but you don't have on
on your message.  Set to \"\" for no title.")

(defvar weblogger-default-categories "main"
   "The default list of categories to post in.   This is added if
your weblog server supports categories on posts but you don't have on
on your message.  Set to nil for no title.")

(defvar weblogger-api-new-post nil)
(defvar weblogger-api-send-edits nil)


(defvar weblogger-weblog-alist nil
  "Weblogs the user can use on the server")

(defconst weblogger-version "1.2"
  "Current version of weblogger.el")

(unless weblogger-post-mode-map
  (setq weblogger-post-mode-map
    (let ((map (copy-keymap message-mode-map))
	  (template-map (make-sparse-keymap)))
      (define-key map "\C-c\C-c" 'weblogger-send-post)
      (define-key map "\C-x\C-s" 'weblogger-save-post)
      (define-key map "\C-c\C-n" 'weblogger-next-post)
      (define-key map "\C-c\C-p" 'weblogger-prev-post)
      (define-key map "\C-c\C-k" 'weblogger-delete-post)
      (define-key map "\M-g"     'weblogger-fetch-posts)
      (define-key template-map "m" 'weblogger-edit-main-template)
      (define-key template-map "a" 'weblogger-edit-archive-template)
      (define-key map "\C-c\C-t" template-map)
      map)))

(unless weblogger-template-mode-map
  (setq weblogger-template-mode-map (copy-keymap text-mode-map))
  (define-key weblogger-template-mode-map "\C-x\C-s" 'weblogger-save-template))

(defun weblogger-post-mode ()
  "Major mode for editing text for Weblogger.  Based on message-mode."
  (interactive)
  (message-mode)
  (use-local-map weblogger-post-mode-map)
  (setq mode-name "weblogger-post")
  (setq major-mode 'weblogger-post-mode)
  (setq weblogger-post-ring (make-ring weblogger-max-entries-in-ring))
  (run-hooks 'weblogger-post-mode-hook))

(defun weblogger-template-mode ()
  "Major mode for editing templates for Weblogger. Based on text-mode."
  (interactive)
  (text-mode)
  (use-local-map weblogger-template-mode-map)
  (setq mode-name "weblogger-template")
  (setq major-mode 'weblogger-template-mode))

(defun weblogger-edit-template (type)
  "Edit a Template. TYPE indicates which one."
  (setq *weblogger-template* (switch-to-buffer "*weblogger-template*"))
  (weblogger-template-mode)
  (erase-buffer)
  (insert (xml-rpc-method-call
	   weblogger-server-url
	   'blogger.getTemplate 
	   weblogger-blogger-app-key
	   (weblogger-weblog-id)
	   (weblogger-server-username)
	   (weblogger-server-password)
	   type))
  (set-buffer-modified-p nil)
  (goto-char (point-min))
  (setq weblogger-template-type type))

(defun weblogger-save-template ()
  "Save a Template. TYPE indicates which one."
  (interactive)
  (if (buffer-modified-p)
      (progn (xml-rpc-method-call
	      weblogger-server-url
	      'blogger.setTemplate 
	      weblogger-blogger-app-key
	      (weblogger-weblog-id)
	      (weblogger-server-username)
	      (weblogger-server-password)
	      (buffer-substring-no-properties (point-min) (point-max))
	      weblogger-template-type)
	     (set-buffer-modified-p nil))))

(defun weblogger-edit-main-template ()
  "Edit the main template"
  (interactive)
  (weblogger-edit-template "main"))

(defun weblogger-edit-archive-template ()
  "Edit the template for archive listings"
  (interactive)
  (weblogger-edit-template "archive"))

(defun weblogger-start-post (&optional prompt)
  "Start creating a blogger post in the *weblogger-post* buffer.
With a prefix, it will check the available weblogs on the server
and prompt for the weblog to post to if multiple ones are
available."
  (interactive "P")
  (if prompt (weblogger-weblog-id prompt))
  (setq *weblogger-post* (switch-to-buffer "*weblogger-post*"))
  (weblogger-post-mode)
  (setq weblogger-ring-index nil)
  (erase-buffer)
  (weblogger-edit-message))

(defun weblogger-post-setup-headers ()
  "Add any pertinant headers to the blogger post"
  (if weblogger-ring-index
      (let* ((pre-postid 
		     (cdr
		      (assoc 'postid
			     (ring-ref 
			      weblogger-post-ring weblogger-ring-index))))
	     (postid (if (stringp pre-postid)
			 pre-postid
		       (int-to-string pre-postid)))
	     (title (cdr
		     (assoc 'title
			   (ring-ref 
			    weblogger-post-ring weblogger-ring-index))))
	    (author (cdr
		    (assoc 'author-name
			   (ring-ref 
			    weblogger-post-ring weblogger-ring-index)))))
	(if postid
	    (message-add-header 
	     (format "Message-ID: <%s/%s@%s>"
		     postid
		     (weblogger-weblog-id)
		     (url-host (url-generic-parse-url weblogger-server-url)))))
	(if (and title
		 (not (string-equal title "")))
	    (message-add-header (concat "Subject: " title)))
	(message-add-header (concat "From: " 
				    (or author weblogger-server-username)))))
  (message-add-header (concat "Newsgroup: " 
			      (weblogger-weblog-name-from-id 
			       (weblogger-weblog-id))))
  (insert mail-header-separator "\n"))

(defun weblogger-send-post (&optional arg)
  "Publish the current message.  With optional argument prompts
for blog to use."
  (interactive)
  (weblogger-save-post arg)
  (bury-buffer))

(defun weblogger-save-post (&optional arg)
  "publish the current message.  with optional argument prompts
for blog to use."
  (interactive)
  (if (not (equal (current-buffer) *weblogger-post*))
      (message 
       "you are not in the *weblogger-post* buffer.")
    (let ((post (weblogger-buffer-to-msg)))
      (cond ((and (buffer-modified-p)
		  (not (string-equal (cdr (assoc "description" post)) "")))
	     (weblogger-server-username arg)
	     (weblogger-server-password arg)
	     (weblogger-weblog-id arg)
	     (cond ((assoc "postid" post)
		    (weblogger-update-ring (list (assoc "postid" post)
						 (weblogger-buffer-to-msg)))
		    (weblogger-api-send-edits post t)
		    (set-buffer-modified-p nil))
		   (t (weblogger-api-new-post post t))))
	    (t (message "Nothing to post"))))))

(defun weblogger-update-ring (post)
  "Update the message ring with the contents of POST"
  (setcdr 
   (assoc 'content
	  (ring-ref weblogger-post-ring weblogger-ring-index))
   (cdr (assoc "description" post)))
  (setcdr
   (assoc 'title
	  (ring-ref weblogger-post-ring weblogger-ring-index))
   (cdr (assoc "title" post)))
  (setcdr
   (assoc 'postid
	  (ring-ref weblogger-post-ring weblogger-ring-index))
   (cdr (assoc "postid" post))))

(defun weblogger-server-username (&optional prompt)
  "Get the username.  If you've not yet logged in then prompt for
it"
  (setq weblogger-server-username
	(if (or prompt (not weblogger-server-username))
	    (read-from-minibuffer "Username: " weblogger-server-username)
	  weblogger-server-username)))

(defun weblogger-server-password (&optional prompt)
  "Get the password.  If you've not yet logged in then prompt for
it"
  (setq weblogger-server-password
	(if (or prompt (not weblogger-server-password))
	    (if weblogger-server-password
		(read-passwd "Password for weblog server: "
			     nil weblogger-server-password)
	      (read-passwd "Password for weblog server: " nil))
	    weblogger-server-password)))

(defun weblogger-weblog-id (&optional prompt)
  "Get the blogger ID."
  (setq weblogger-weblog-id
	(if (or prompt (not weblogger-weblog-id))
	    (weblogger-select-weblog prompt)
	  weblogger-weblog-id)))

(defun weblogger-api-blogger-send-edits (struct &optional publishp)
  "Blogger API method to post edits to a message specified by
msgid.  If publishp is non-nil, publishes the message as well."
  (xml-rpc-method-call
   weblogger-server-url
   'blogger.editPost
   weblogger-blogger-app-key
   (cdr (assoc 'postid (ring-ref weblogger-post-ring weblogger-ring-index)))
   (weblogger-server-username)
   (weblogger-server-password)
   (cdr (assoc "description"
	       (ring-ref weblogger-post-ring weblogger-ring-index)))
   publishp)
  (run-hooks 'weblogger-edit-post-hook))

(defun weblogger-api-meta-send-edits (struct &optional publishp)
  "MetaWeblog API method to post edits to a message specified by
msgid.  If publishp is non-nil, publishes the message as well."
  (xml-rpc-method-call
   weblogger-server-url
   'metaWeblog.editPost
   (cdr (assoc 'postid (ring-ref weblogger-post-ring weblogger-ring-index)))
   (weblogger-server-username)
   (weblogger-server-password)
   struct
   publishp)
  (run-hooks 'weblogger-edit-post-hook))

(defun weblogger-api-new-post (struct publishp)
  "Publish a new post using the best method available."
  (unless weblogger-api-new-post
    (weblogger-determine-capabilities))
  (eval `(,weblogger-api-new-post struct publishp)))

(defun weblogger-api-send-edits (struct publishp)
  "Publish a new post using the best method available."
  (unless weblogger-api-send-edits
    (weblogger-determine-capabilities))
  (eval `(,weblogger-api-send-edits struct publishp)))

(defun weblogger-api-blogger-new-post (struct publishp)
  "Post a new message.  If publishp is non-nil, publishes the
message as well."
  (let* ((msgid (xml-rpc-method-call
		 weblogger-server-url
		 'blogger.newPost
		 weblogger-blogger-app-key
		 (weblogger-weblog-id)
		 (weblogger-server-username)
		 (weblogger-server-password)
		 (cdr (assoc "description" struct))
		 publishp)))
	  (ring-insert weblogger-post-ring
		       (list (cons 'postid msgid)
			     (cons 'userid nil) ; Cause there
						; seems to be
						; some sort of
						; server problem.
						; Otherwise we
						; would say
						; "(weblogger-get-userid)"
			     (cons 'date-created nil)
			     (cons 'content 
				   (assoc "description" struct))
			     (cons 'title
				   (assoc "title" struct)))))
  (setq weblogger-ring-index 0)
  (run-hooks 'weblogger-new-post-hook))

(defun weblogger-api-meta-new-post (struct publishp)
  "Post a new message.  If publishp is non-nil, publishes the
message as well."
  (let* ((msgid (xml-rpc-method-call
		 weblogger-server-url
		 'metaWeblog.newPost
		 (weblogger-weblog-id)
		 (weblogger-server-username)
		 (weblogger-server-password)
		 struct
		 publishp))
	 (struct (list struct
			(cons "postid" msgid))))
    (ring-insert weblogger-post-ring (weblogger-message-to-struct struct)))
  (setq weblogger-ring-index 0) 
  (run-hooks 'weblogger-new-post-hook))

(defun weblogger-select-weblog (&optional fetch)
  "Allows the user to select a weblog and returns the weblog ID.
If there is only one weblog owned by the user on the server, then
that weblog is returned.  With FETCH defined, the server is
re-queried for a list of weblogs the user owns"
  (weblogger-weblog-id-from-weblog-name
   (let* ((completion-ignore-case t)
	  (seq 0)
	  (webloglist (mapcar
		     (lambda (weblog)
		       (cons weblog (setq seq (1+ seq))))
		     (weblogger-list-weblog-names fetch))))
     (if (= 1 (length webloglist))
	 (caar webloglist)
       (completing-read 
	"Weblog: " webloglist nil t)))))

(defun weblogger-weblog-id-from-weblog-name (name)
  "Returns the weblog id given the name."
  (cdr (assoc name
	 (mapcar 
	  (lambda (weblog)
	    (cons (cdr (assoc "blogName" weblog))
		  (cdr (assoc "blogid" weblog))))
	  (weblogger-weblog-alist)))))

(defun weblogger-weblog-name-from-id (id)
  "Returns the weblog name given the id."
  (cdr (assoc id
	 (mapcar 
	  (lambda (blog)
	    (cons (cdr (assoc "blogid" blog))
		  (cdr (assoc "blogName" blog))))
	  (weblogger-weblog-alist)))))

(defun weblogger-server-url-from-id (id)
  "Returns the weblog URL given the id."
  (cdr (assoc id
	      (mapcar
	       (lambda (blog)
		 (cons (cdr (assoc "blogid" blog))
		       (cdr (assoc "url" blog))))
	       (weblogger-weblog-alist)))))

(defun weblogger-list-weblog-names (&optional fetch)
  "Returns a list of weblog names."
  (mapcar 
   (lambda (blog)
     (cdr (assoc "blogName" blog)))
   (weblogger-weblog-alist fetch)))

(defun weblogger-weblog-alist (&optional fetch)
  "Returns the alist of weblogs owned by a user on the server."
  (setq weblogger-weblog-alist
	(if (or fetch (not weblogger-weblog-alist))
	    (xml-rpc-method-call 
	     weblogger-server-url
	     'blogger.getUsersBlogs
	     weblogger-blogger-app-key
	     (weblogger-server-username)
	     (weblogger-server-password))
	  weblogger-weblog-alist)))

(defun weblogger-ping-weblogs (&optional id)
  "Pings the blog aggregators listed in weblogger-ping-urls
when you post an update."
  (sit-for 10)
  (mapcar
   (lambda (url)
     (xml-rpc-method-call-async
      'weblogger-handle-weblog-ping-response
      url
      'weblogUpdates.ping
      (weblogger-weblog-name-from-id 
       (or id weblogger-weblog-id)				)
      (weblogger-server-url-from-id 
       (or id weblogger-weblog-id))))
   weblogger-ping-urls))

(defun weblogger-handle-weblog-ping-response (&optional resp)
  "Handle the response from a weblog ping.  Print a message with the result.\
\
For old w3.el, resp is expected.  Otherwise current-buffer is expected to \
contain the http result."
  (if resp
      (message (cdr (assoc "message" (cdr resp))))
    (message (cdr 
	      (assoc "message" 
		     (cdr 
		      (xml-rpc-xml-to-response
		       (xml-rpc-request-process-buffer (current-buffer)))))))))

(defun weblogger-goto-post (postnum &optional relativep)
  "Move to POSTNUM in the ring.  If RELATIVE is set, then add it
to the current index and go to that post"
  (if (buffer-modified-p)
      (weblogger-save-post nil))
  (unless weblogger-message-list
    (weblogger-list-messages))
  (let ((postid (if relativep
		    (+ (if weblogger-ring-index weblogger-ring-index 
			 -1)
		       postnum)
		  postnum)))
    (setq weblogger-ring-index postid))
  (if (ring-empty-p weblogger-post-ring)
      (weblogger-list-messages))
  (weblogger-edit-message
   (ring-ref weblogger-post-ring weblogger-ring-index)))

(defun weblogger-next-post ()
  "Edit the contents of the next message.  Don't change the
buffer if this is the last message."
  (interactive)
  (weblogger-goto-post -1 t))

(defun weblogger-prev-post ()
  "Edit the contents of the previous message.  Don't change the
buffer if this is the first message."
  (interactive)
  (weblogger-goto-post +1 t))

(defun weblogger-delete-post ()
  "Delete the message."
  (interactive)
  (unless weblogger-ring-index
    (message "You must have a blogger message loaded first."))
  (if (y-or-n-p "Do you really want to delete this message? ")
      (let* ((msgid (cdr 
		     (assoc 'postid 
			    (ring-ref weblogger-post-ring 
				      weblogger-ring-index)))))
	(xml-rpc-method-call
	 weblogger-server-url
	 'blogger.deletePost
	 weblogger-blogger-app-key
	 msgid
	 (weblogger-server-username)
	 (weblogger-server-password)
	 t))
    (ring-remove weblogger-post-ring weblogger-ring-index)
    (weblogger-edit-message
     (ring-ref weblogger-post-ring weblogger-ring-index))))

(defun weblogger-list-messages (&optional count)
  "Return a list of messages that the Blogger server has.  COUNT specifies
how many of the most recent messages to get.  If COUNT is not
specified, then the default is weblogger-max-entries-in-ring."
  (setq weblogger-message-list 
	(mapcar 
	 (lambda (post)
	   (ring-insert-at-beginning  weblogger-post-ring
				      (weblogger-message-to-struct post)))
	 (xml-rpc-method-call
	  weblogger-server-url
	  'blogger.getRecentPosts
	  weblogger-blogger-app-key
	  (weblogger-weblog-id)
	  (weblogger-server-username)
	  (weblogger-server-password)
	  (or count weblogger-max-entries-in-ring)))))

(defun weblogger-edit-message (&optional msg)
  "Edit a Message.  MSG specifies which message to edit."
  (setq *weblogger-post* (switch-to-buffer "*weblogger-post*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (weblogger-post-setup-headers)
  (if msg
      (insert (cdr (assoc 'content msg)))
    (message-goto-subject))
  (set-buffer-modified-p nil)
  (pop-to-buffer *weblogger-post*))

(defun weblogger-message-to-struct (msg)
  "Convert the result of the xml-rpc call to a structure we
like."
  (list
   (cons 'postid       (cdr (assoc "postid" msg)))
   (cons 'title        (cdr (assoc "title" msg)))
   (cons 'author-name  (cdr (assoc "authorName" msg)))
   (cons 'userid       (cdr (assoc "userid" msg)))
   (cons 'date-created (cdr (assoc "dateCreated" msg)))
   (cons 'content      (cdr (assoc "content" msg)))))

(defun weblogger-server-userid ()
  "Get information on user."
  (or weblogger-server-userid
      (setq weblogger-server-userid 
	    (cdr
	     (assoc "userid"
		    (xml-rpc-method-call
		     weblogger-server-url
		     'blogger.getUserInfo
		     weblogger-blogger-app-key
		     (weblogger-server-username)
		     (weblogger-server-password)))))))

(defun weblogger-fetch-posts ()
  "Sync the post ring with what is on the blogger server."
  (interactive)
  (setq weblogger-post-ring (make-ring weblogger-max-entries-in-ring))
  (weblogger-list-messages weblogger-max-entries-in-ring)
  (setq weblogger-ring-index 0)
  (weblogger-edit-message
   (ring-ref weblogger-post-ring weblogger-ring-index)))

(defun weblogger-determine-capabilities ()
  "Determine the capabilities of the remote blogger server."
  (let ((has-meta-api t)
	(has-blogger-api t))
    (condition-case nil
	(progn (mapcar
		(lambda (method)
		  (setcdr (assoc method weblogger-capabilities) t))
		(xml-rpc-method-call
		 weblogger-server-url
		 'mt.supportedMethods)))
      (error (setq has-mt-api nil))))
  (if (cdr (assoc "metaWeblog.editPost" weblogger-capabilities))
      (setq weblogger-api-send-edits 'weblogger-api-meta-send-edits)
    (setq weblogger-api-send-edits 'weblogger-api-blogger-send-edits))
  (if (cdr (assoc "metaWeblog.newPost" weblogger-capabilities))
      (setq weblogger-api-new-post 'weblogger-api-meta-new-post)
    (setq weblogger-api-new-post 'weblogger-api-blogger-new-post)))

(defun weblogger-buffer-to-msg (&optional encode buffer)
  "Extract from BUFFER.  If BUFFER is not given, use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))
  (save-excursion
    (set-buffer buffer)
    (delq nil 
	  (list
	   (cons "title" (or (message-fetch-field "Subject") 
			     weblogger-default-title))
	   (cons "category" (or (message-tokenize-header 
				 (message-fetch-field "Keywords") ", ")
				weblogger-default-categories))
	   (if weblogger-ring-index
	       (cons "postid" (cdr
			       (assoc 'postid
				      (ring-ref
				       weblogger-post-ring
				       weblogger-ring-index)))))
	   (cons "description"
		 (progn
		   (message-goto-body)
		   (if encode
		       (url-insert-entities-in-string (buffer-substring-no-properties (point) (point-max)))
		     (buffer-substring-no-properties (point) (point-max)))))))))

;; TODO -- Support for toolbar
(eval-when-compile (defvar tool-bar-map))
(if (featurep 'xemacs)
    nil					; no XEmacs support just yet.
  (when (and (fboundp 'tool-bar-add-item-from-menu)
 	     tool-bar-mode)
    (defvar weblogger-tool-bar-map
      (let ((tool-bar-map (copy-keymap tool-bar-map)))
 	;; Zap some items which aren't so relevant and take up space.
 	(dolist (key '(print-buffer kill-buffer save-buffer write-file
 				    dired open-file))
 	  (define-key tool-bar-map (vector key) nil))
 
 	(tool-bar-add-item-from-menu
 	 'message-send-and-exit "mail_send" message-mode-map)
 	(tool-bar-add-item-from-menu
 	 'message-kill-buffer "close" message-mode-map)
 	(tool-bar-add-item-from-menu
 	 'message-dont-send "cancel" message-mode-map)
 	(tool-bar-add-item-from-menu
 	 'mml-attach-file "attach" message-mode-map)
 	(tool-bar-add-item-from-menu
 	 'ispell-message "spell" message-mode-map)
 	tool-bar-map))))

(provide 'weblogger)

