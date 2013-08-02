; -*- Mode: Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Music time!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;
;; EMMS    ;;
;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "emms-latest/lisp" emacs-packages-dir))

(require 'emms-setup)
(require 'emms-streams)

(emms-standard)
(emms-default-players)

(setq emms-source-file-default-directory "/mnt/music/")
(setq emms-stream-default-action "play")
(setq emms-pbi-popup-default-width 50)

(defvar multimedia-keymap (make-sparse-keymap
						   "Multimedia (p)lay, (s)treams, (v)iew (l)ist, (+/-)volume, (up/down)next/previous")
  "Keymap used to globally access multimedia functions")
(define-key mode-specific-map (kbd "m") multimedia-keymap)
(define-key multimedia-keymap (kbd "?") 'describe-prefix-bindings)

(define-key multimedia-keymap (kbd "<up>")   'emms-next)
(define-key multimedia-keymap (kbd "<down>") 'emms-previous)
(define-key multimedia-keymap (kbd "s")      'emms-streams)
(define-key multimedia-keymap (kbd "v") 'emms-playlist-mode-go)
(define-key multimedia-keymap (kbd "l") 'emms-playlist-mode-go)

(defvar multimedia-play-keymap (make-sparse-keymap
								"Play (a)ll, p(l)aylist, (f)ile, (F)ind, (d)irectory-tree")
  "Keymap used to play multimedia content")

(define-key multimedia-keymap (kbd "p") multimedia-play-keymap)

(define-key multimedia-play-keymap (kbd "a")    'emms-start)
(define-key multimedia-play-keymap (kbd "l")    'emms-play-playlist)
(define-key multimedia-play-keymap (kbd "f")    'emms-play-file)
(define-key multimedia-play-keymap (kbd "F")    'emms-play-find)
(define-key multimedia-play-keymap (kbd "d")    'emms-play-directory-tree)
(define-key multimedia-play-keymap (kbd "s")    'emms-stop)
(define-key multimedia-play-keymap (kbd "w")    'emms-show)
(define-key multimedia-play-keymap (kbd "m")    'emms-shuffle)
(define-key multimedia-play-keymap (kbd "M")    'emms-sort)

;;;;;;;;;;;;;;;;;;;;
;; Audio Mixer    ;;
;;;;;;;;;;;;;;;;;;;;

(require 'audio-mixer)

(add-hook 'emms-player-started-hook (lambda nil
                                      (audio-mixer-set-volume audio-mixer-master-volume)))

(setq audio-mixer-default-program 'amixer)
(audio-mixer-setup)

(define-key multimedia-keymap [?+] 'audio-mixer-increment-volume)
(define-key multimedia-keymap [?-] 'audio-mixer-decrement-volume)
(define-key multimedia-keymap [?V] 'audio-mixer-set-volume)
