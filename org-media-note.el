;;; org-media-note.el --- Taking video and audio notes with Org-mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Requirements
(require 'mpv)
(require 'org)
(require 'org-timer)
(require 'pretty-hydra)


;;;; Customization

(defgroup org-media-note nil
  "Taking video and audio notes with Org-mode."
  :group 'org
  :prefix "org-media-note-")


(defcustom org-media-note-screenshot-image-dir "/Users/yuchen/Notes/imgs/"
  "Default base filename to use for screenshots."
  :type 'string)

(defcustom org-media-note-time-to-wait-after-open 0.3
  "Seconds to sleep after opening a media file."
  :type 'float)

(defcustom org-media-note-save-screenshot-p nil
  "Whether to save screenshot."
  :type 'boolean)

(defcustom org-media-note-display-inline-images t
  "When non-nil display inline images in org buffer after insert screenshot."
  :type
  '(choice
    (const :tag "On" t)
    (const :tag "Off" nil)
    ))

;;;; Variables

(defconst org-media-note--video-types '("avi" "rmvb" "ogg" "ogv" "mp4" "mkv" "mov" "webm" "flv" "ts"))
(defconst org-media-note--audio-types '("flac" "mp3" "wav"))

(defvar org-media-note-ref-key nil
  "The ref key which is noting with.")

(defvar org-media-note-last-play-speed 1.0
  "Last play speed in mpv.")


;;;; Commands

(defun org-media-note--get-media-type ()
  "Get current playing media type."
  (let* (
         (file-path (mpv-get-property "path"))
         (file-ext (if file-path (file-name-extension file-path)))
         )
    (cond
     ((member file-ext org-media-note--video-types)
      "video")
     ((member file-ext org-media-note--audio-types)
      "audio")
     (t nil)
     )
    )
  )

(defun org-media-note--get-current-hms ()
  "Get current media timestamp in format h:mm:ss"
  (let ((time (mpv-get-playback-position)))
    (org-timer-secs-to-hms (round time))
    )
  )

;;;;; Hydra

(defun org-media-note--hydra-title ()
  "Title for `org-media-note-hydra'"
  (let (
        (file-path (mpv-get-property "path"))
        speed hms duration
        )
    (if file-path
        ;; Title when mpv is playing media
        (progn
          (setq speed (number-to-string (mpv-get-property "speed")))
          (setq hms (org-media-note--get-current-hms))
          (setq duration (org-timer-secs-to-hms (round (mpv-get-property "duration"))))
          (with-material "ondemand_video"
                         (s-concat "org-media-note: "
                                   hms
                                   " / "
                                   duration
                                   "\t Current Speed:"
                                   speed
                                   "\n\t❯ "
                                   (if org-media-note-ref-key
                                       (concat (org-ref-get-XXX org-media-note-ref-key "title") " (" org-media-note-ref-key ")")
                                     file-path)
                                   )
                         1 -0.05)
          )
      ;; Title when no media is playing
      (with-material "ondemand_video" "org-media-note" 1 -0.05)
      )
    )
  )

(pretty-hydra-define org-media-note-hydra
  (:color red
   :title (org-media-note--hydra-title)
   :hint nil)
  (
   "File"
   (
    ("o" org-media-note-mpv-smart-play
     (if (org-media-note--current-org-ref-key)
         (concat "Open " org-media-note-ref-key)
       "Open")
     :width 20)
    )
   "Playback"
   (
    ("<SPC>" mpv-pause "Play/Pause")
    ("<left>" mpv-seek-backward "Back 5s")
    ("<right>" mpv-seek-forward "Forward 5s")
    ("c" (org-media-note-change-speed-by 0.1)  "increase speed")
    ("x" (org-media-note-change-speed-by -0.1)  "decrease speed")
    ("z" org-media-note-mpv-toggle-speed  "reset speed")

    )
   "Note"
   (
    ("i" org-media-note-insert-link "Insert timestamp")
    ("I" org-media-note-insert-screenshot "Insert Screenshot")
    )
   "Toggle"
   (
    ("t m" org-media-note-mode "Auto insert media item" :toggle t)
    ("t c" org-media-note-refcite-mode "Use ref key instead of absolute path" :toggle org-media-note-ref-key)
    ("t s" org-media-note-toggle-save-screenshot "Auto save screenshot" :toggle org-media-note-save-screenshot-p)
    )
   )
  )

;;;;; Add note

(defun org-media-note--current-org-ref-key ()
  (org-entry-get (point) "Custom_ID")
  )

(defun org-insert-item--media-note-item (orig-fn &rest args)
  "When item begins with media link, insert playback position."
  (interactive "P")
  (let ((itemp (org-in-item-p))
	(pos (point)))
    ;; If cursor isn't is a list or if list is invisible, return nil.
    (unless (or (not itemp)
		(save-excursion
		  (goto-char itemp)
		  (org-invisible-p)))
      (if (save-excursion
	    (goto-char itemp)
	    (org-at-item-meida-item-p))
	  ;; Timer list: delegate to `org-timer-item'.
	  (progn (org-media-note-item) t)
        ))))


(defun org-media-note-item (&optional arg)
  "Insert a item with link to media file."
  (interactive "P")
  (let (
        (itemp (org-in-item-p))
        (pos (point)))
    (cond
     ;; In a media note list and media file is open: insert with `org-list-insert-item',
     ((and itemp (goto-char itemp) (org-at-item-meida-item-p) (mpv-get-property "path"))
      (let* ((struct (org-list-struct))
	     (prevs (org-list-prevs-alist struct))
	     (s (concat (org-media-note--link) " ")))
	(setq struct (org-list-insert-item pos struct prevs nil s))
	(org-list-write-struct struct (org-list-parents-alist struct))
	(looking-at org-list-full-item-re)
        (move-end-of-line 1)
        (if org-media-note-save-screenshot-p (org-media-note-insert-screenshot))
	;; (goto-char (match-end 1))
        ))
     ;; In a list of another type, don't break anything: throw an error.
     (t
      (error "No playing media file now. Please open the media file first if you want to insert media note, \nor turn off ")
      )
     )))

(defun org-media-note-insert-link ()
  "Insert current mpv timestamp link into Org-mode note."
  (interactive)
  (insert (org-media-note--link))
  (insert " ")
  )

(defun org-media-note--link ()
  "Return media link."
  (let* (
         (file-path (mpv-get-property "path"))
         (link-type (if org-media-note-ref-key
                        (concat (org-media-note--get-media-type) "cite")
                      (org-media-note--get-media-type)
                      ))
         (hms (org-media-note--get-current-hms))
         )
    (if org-media-note-ref-key
        (concat "[[" link-type ":" org-media-note-ref-key "#" hms "][" hms "]]")
      (concat "[[" link-type ":" file-path "#" hms "][" hms "]]")
      )
    )
  )


(defun org-at-item-meida-item-p ()
  "Is point at a line starting a plain list item with a timer?"
  (org-list-at-regexp-after-bullet-p
   "\\(\\[\\[video.+\\)"))


(defun org-media-note--format-file-name (name)
  (let (new-name)
    (setq new-name (replace-regexp-in-string " - " "-" name))
    (setq new-name (replace-regexp-in-string ":" "_" name))
    (replace-regexp-in-string " " "_" new-name)
    )
  )

(defun org-media-note-insert-screenshot ()
  "Insert current mpv screenshot into Org-mode note."
  (interactive)
  (let* (
         (image-file-name (org-media-note--format-file-name
                           (concat (file-name-base (mpv-get-property "path"))
                                   "-"
                                   (org-media-note--get-current-hms)
                                   ".jpg"
                                   )
                           )
                          )
         (image-target-path (expand-file-name image-file-name org-media-note-screenshot-image-dir))
         )
    (mpv--enqueue `("screenshot-to-file" ,image-target-path) #'ignore)
    (insert (concat "[[file:" image-target-path "]] "))
    (org-media-note--display-inline-images)
    )
  )

(defun org-media-note--display-inline-images ()
  (cond
   ((eq org-media-note-display-inline-images t)
    ;; TODO beteer way?
    (sleep-for 0.1)
    (org-display-inline-images))
   ))

;;;;; Link Follow
(defun org-media-note-link-follow (link)
  "Open media link like video:example.mkv#0:02:13"
  (let* (
         (splitted (split-string link "#"))
         (file-path (nth 0 splitted))
         (hms (nth 1 splitted))
         )
    (if (not (string= file-path (mpv-get-property "path")))
        (progn
          (mpv-play file-path)
          (sleep-for org-media-note-time-to-wait-after-open) ;; without this, mpv-seek doesn't work for the new opend file.
          )
      )
    (mpv-seek (org-timer-hms-to-secs hms))
    )
  )

;;;;; Integreted with org-ref

;; TODO maybe move to a seperate file to integerate with org-ref?

(defun org-media-note-cite-link-follow (link)
  "Open media link like videocite:course.104#0:02:13"
  (let* (
         (splitted (split-string link "#"))
         (key (nth 0 splitted))
         (file-path (org-media-note-get-media-file-by-key key))
         (hms (nth 1 splitted))
         )
    (cond
     ((not file-path)
      (error "Cannot find media file for this Key."))
     (t
      (if (not (string= file-path (mpv-get-property "path")))
          (progn
            (mpv-play file-path)
            (sleep-for org-media-note-time-to-wait-after-open)
            )
        )
      (mpv-seek (org-timer-hms-to-secs hms))))))

(defun org-media-note-mpv-play-by-key (key)
  (mpv-play (org-media-note-get-media-file-by-key key))
  )

(defun org-media-note-get-media-file-by-key (key)
  (let* (
         (file-path-list (org-ref-get-file-list key))
         (file-types (get-file-types-from-file-list file-path-list))
         (file-path-raw (nth 0 file-path-list))
         (file-name (save-match-data (and (string-match " *:\\(.+\\)\\.\\(.+\\) *" file-path-raw) ; file: ":/Books/{author}/..."
                                          (match-string 1 file-path-raw))))
         (video-file-ext-candidates (seq-intersection file-types org-media-note--video-types))
         (audio-file-ext-candidates (seq-intersection file-types org-media-note--audio-types))
         (file-type (cond
                     (video-file-ext-candidates
                      (nth 0 video-file-ext-candidates))
                     (audio-file-ext-candidates
                      (nth 0 audio-file-ext-candidates))
                     (t nil)
                     ))
         )
    (if file-type
        (org-media-note--get-realpath-for-file (concat file-name "." file-type))
      nil)
    )
  )


(defun org-media-note--get-realpath-for-file (symlink)
  (replace-regexp-in-string "\n" ""
                                  (shell-command-to-string
                                   (concat "realpath \"" symlink "\"")
                                   ))
  )

;;;;; Media Control
(defun org-media-note-change-speed-by (speed-step)
  "Set playing speed."
  (let ((current-speed (mpv-get-property "speed")))
    (mpv-speed-set (+ current-speed speed-step))
    )
  )

(defun org-media-note-mpv-toggle-speed ()
  (interactive)
  (let ((current-speed (mpv-get-property "speed")))
    (if (= 1.0 current-speed)
        (mpv-speed-set org-media-note-last-play-speed)
      (progn
        (setq org-media-note-last-play-speed current-speed)
        (mpv-speed-set 1))
      )
    )
  )

(defun org-media-note-mpv-smart-play ()
  (interactive)
  (let* ((key (org-media-note--current-org-ref-key))
         )
    (if key
        (mpv-play (org-media-note-get-media-file-by-key key))
      (mpv-play)
      )
    )
  )

;;;;; Minor Mode

;;;###autoload
(define-minor-mode org-media-note-mode
  "Toggle `org-media-note-mode'.
When enabled, insert media note.
"
  :init-value t
  :global     nil
  (if org-media-note-mode
      (advice-add 'org-insert-item :before-until #'org-insert-item--media-note-item)
    (advice-remove 'org-insert-item #'org-insert-item--media-note-item)
    ))


(define-minor-mode org-media-note-refcite-mode
  "Toggle `org-media-note-refcite-mode'.
When enabled, will insert org-ref key instead of absolute file path.
"
  :init-value nil
  :global     nil
  (if org-media-note-refcite-mode
      (org-media-note--setup-refcite-mode)
    (org-media-note--tear-down-refcite-mode)))


(defun org-media-note--setup-refcite-mode ()
  (let* ((key (org-media-note--current-org-ref-key))
         )
    (if key
        (setq org-media-note-ref-key key)
      ;; TODO can not set up refcite-mode without key
      (error "Cannot set up refcite-mode without a key.")
      )
    ))

(defun org-media-note--tear-down-refcite-mode ()
  (setq org-media-note-ref-key nil)
  )


(defun org-media-note-toggle-save-screenshot ()
  (interactive)
  (if org-media-note-save-screenshot-p
      (setq org-media-note-save-screenshot-p nil)
    (setq org-media-note-save-screenshot-p t)))

;;;;; Customize Org link
(org-link-set-parameters "video"
                         :follow 'org-media-note-link-follow)

(org-link-set-parameters "audio"
                         :follow 'org-media-note-link-follow)

(org-link-set-parameters "videocite"
                         :follow 'org-media-note-cite-link-follow)

(org-link-set-parameters "audiocite"
                         :follow 'org-media-note-cite-link-follow)

;;;; Footer
(provide 'org-media-note)
;;; org-media-note.el ends here
