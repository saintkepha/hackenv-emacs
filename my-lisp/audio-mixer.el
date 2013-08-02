;;; audio-mixer --- Utility function for setting the audio volume

;; Copyright (C) 2005 Peter K. Lee

;; Author: Peter K. Lee <saint@ c o r e n o v a .com>
;; Keywords: audio, multimedia, external, alsa
;; Time-stamp: 5 Jun 2005 17:22:26
;; Version: 0.1
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
;; This package sets the volume via the alsa mixer program
;;
;; To use audio-mixer, insert in your ~/.emacs:
;;
;;    (require 'audio-mixer)
;;
;; and bind some keys to the functions:
;;
;;    (global-set-key (kbd "C-c # +") 'audio-mixer-increment-volume)
;;    (global-set-key (kbd "C-c # -") 'audio-mixer-decrement-volume)
;;    (global-set-key (kbd "C-c # v") 'audio-mixer-set-volume)
;;
;; Todo:
;; -----
;;
;; Control other volume settings other than just Master
;;
;; Present a nice looking volume controller toolbar.
;;
;; History:
;; --------
;;
;; 2005-06-05 (0.1) : initial release
;;

(defgroup audio-mixer nil
  "Interface to the command line audio mixer."
  :group 'external)

(defcustom audio-mixer-master-volume 60
  "Volume in percent."
  :type 'integer
  :group 'audio-mixer)

(defcustom audio-mixer-volume-default-offset 5
  "Volume default increment/decrement offset value."
  :type 'integer
  :group 'audio-mixer)

(defcustom audio-mixer-device-file nil
  "Name of the mixer device, i.e. /dev/mixer.  If nil, use
  whatever the subsystem thinks is the default.  If unsure, just
  leave nil."
  :type 'string
  :group 'audio-mixer)

(defcustom audio-mixer-default-program 'aumix
  "*Select default mixer subsystem program to use.  Make sure the
  selected program exists in your system and is configured properly.
  Possible values: 
    `amixer' Use Alsa mixer 
    `aumix'  Use aumix"
  :type '(choice (const amixer)
                 (const aumix))
  :group 'audio-mixer)

(defvar audio-mixer-program nil
  "Stores the program to use when accessing the mixer system.
Its value is one of `amixer', `aumix'.  See
`audio-mixer-default-program' for details of values.")

(defvar audio-mixer-set-volume-function nil
  "Stores the function to call when setting the audio volume.")

;;; AMIXER - customize

(defgroup audio-mixer-amixer nil
  "Interface to the command line audio mixer (Alsa amixer)."
  :group 'audio-mixer)

(defcustom amixer-program-name "amixer"
  "The program name of Alsa amixer program"
  :type 'string
  :group 'audio-mixer-amixer)

(defcustom amixer-master-volume-id 40
  "Identifer of the master volume control.
Call 'amixer controls' and take the number of an entry that looks like:

  numid=40,iface=MIXER,name='Master Playback Volume'

The master volume id in this example is 40.
"
  :type 'integer
  :group 'audio-mixer-amixer)

(defcustom amixer-card-id 0
  "Alsa Id of the sound card. Use 0 for the first card."
  :type 'integer
  :group 'audio-mixer)

(defun amixer-set-volume-function (num)
  "Set volume using the Alsa amixer program"
  (start-process "mixer-process" nil 
                 amixer-program-name "-c" (int-to-string amixer-card-id) 
                 "cset" (concat "numid=" (int-to-string amixer-master-volume-id))
                 (concat  (int-to-string audio-mixer-master-volume) "%")))

;;; AUMIX - customize

(defgroup audio-mixer-aumix nil
  "Interface to the command line audio mixer (aumix)."
  :group 'audio-mixer)

(defcustom aumix-program-name "aumix"
  "The program name of aumix program"
  :type 'string
  :group 'audio-mixer-aumix)

(defun aumix-set-volume-function (num)
  "Set volume using the `aumix program-name'"
  ;; 0 = discard & don't wait for termination.
  (call-process aumix-program-name nil 0 nil
                "-v" (int-to-string audio-mixer-master-volume)))

;;; FUNCTIONS
(defun audio-mixer-setup ()
  "Main audio-mixer routine that sets up the settings."
  (setq audio-mixer-program audio-mixer-default-program)
  (cond
   ((eq audio-mixer-program 'amixer)
    (if (functionp 'amixer-set-volume-function)
        (setq audio-mixer-set-volume-function 'amixer-set-volume-function))
    )
   ((eq audio-mixer-program 'aumix)
    (if (functionp 'aumix-set-volume-function)
        (setq audio-mixer-set-volume-function 'aumix-set-volume-function)
      (error "aumix-set-volume-function undefined!"))
    )
   ))

;;;###autoload
(defun audio-mixer-set-volume (num)
  "Set volume using `audio-mixer-program' interface to specified NUM percent."
  (interactive "nVolume: ")
  (audio-mixer-setup)
  (setq audio-mixer-master-volume (max 0 (min 100 num)))
  (message "Volume: %d%%" audio-mixer-master-volume)
  (funcall audio-mixer-set-volume-function num))

;;;###autoload
(defun audio-mixer-increment-volume (&optional arg)
  "Ajust volume up by default increment, or prefix ARG if provided."
  (interactive "p")
  (let ((increment (if (> arg 1) arg audio-mixer-volume-default-offset)))
    (audio-mixer-set-volume (+ audio-mixer-master-volume increment))))

;;;###autoload
(defun audio-mixer-decrement-volume (&optional arg)
  "Ajust volume down by default increment, or prefix ARG if provided."
  (interactive "p")
  (let ((decrement (if (> arg 1) arg audio-mixer-volume-default-offset)))
    (audio-mixer-set-volume (- audio-mixer-master-volume decrement))))

(provide 'audio-mixer)
