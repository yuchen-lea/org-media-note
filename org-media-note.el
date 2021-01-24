;;; org-media-note.el --- Taking video and audio notes with Org-mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Requirements
(require 'mpv)
(require 'org)
(require 'org-timer)
(require 'pretty-hydra)
(require 'bibtex-completion)

(declare-function org-timer-secs-to-hms 'org-timer)

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

(defcustom org-media-note-cite-keymap
  (let ((map (copy-keymap org-mouse-map)))
    (define-key map (kbd "H-o") 'org-media-note-open-ref-cite-function)
    map)
  "Keymap for cite links."
  :type 'symbol
  :group 'org-media-note)


;;;; Variables

(defconst org-media-note--video-types '("avi" "rmvb" "ogg" "ogv" "mp4" "mkv" "mov" "webm" "flv" "ts"))
(defconst org-media-note--audio-types '("flac" "mp3" "wav"))

(defvar org-media-note-last-play-speed 1.0
  "Last play speed in mpv.")

(defvar org-media-note-last-volume 100.0
  "Last Volume in mpv.")


;;;; Commands
;;;;; Hydra


(defun org-media-note--hydra-title ()
  "Title for `org-media-note-hydra'"
  (let (
        (file-path (mpv-get-property "path"))
        (ref-key (org-media-note--current-org-ref-key))
        speed current-hms total-hms duration remaining-hms
        )
    (if file-path
        ;; Title when mpv is playing media
        (progn
          (setq speed (mpv-get-property "speed"))
          (setq volume (mpv-get-property "volume"))
          (setq current-hms (org-media-note--get-current-hms))
          (setq duration (mpv-get-property "duration"))
          (setq total-hms (org-timer-secs-to-hms (round duration)))
          (setq remaining-hms (org-timer-secs-to-hms (round (mpv-get-property "playtime-remaining")
                                                               )))
          (with-material "ondemand_video"
                         (s-concat "org-media-note: "
                                   current-hms
                                   " / "
                                   total-hms
                                   "\t Volume: "
                                   (number-to-string volume)
                                   "\t Speed: "
                                   (number-to-string speed)
                                   "\t Remaining: "
                                   remaining-hms
                                   "\n\t❯ "
                                   (if ref-key
                                       ;; TODO 处理依赖
                                       (format "%s (%s)"
                                               (bibtex-completion-get-value-by-key ref-key "title")
                                               ref-key
                                               )
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
         (format "Open %s" (org-media-note--current-org-ref-key))
       "Open")
     :width 20)
    ("j" (mpv-cycle-property "sub") "toggle subtitles")
    )
   "Playback"
   (
    ("<SPC>" mpv-pause "Play/Pause")
    ("<left>" mpv-seek-backward "Back 5s")
    ("<right>" mpv-seek-forward "Forward 5s")
    ("C-<left>"   (mpv-run-command "sub-seek" -1)  "Previous subtitle")
    ("C-<right>" (mpv-run-command "sub-seek" 1) "Next subtitle")
    ("c" (org-media-note-change-speed-by 0.1)  "increase speed")
    ("x" (org-media-note-change-speed-by -0.1)  "decrease speed")
    ("z" org-media-note-mpv-toggle-speed  "reset speed")
    )
   "Volume"
   (
   ("+" (org-media-note-change-volume-by 5) "Up")
   ("-" (org-media-note-change-volume-by -5) "Down")
   ("0" org-media-note-mpv-toggle-volume "toggle")
   ("m" (mpv-cycle-property "mute") "(un)mute")
   )
   "Note"
   (
    ("i" org-media-note-insert-link "Insert timestamp")
    ("I" org-media-note-insert-screenshot "Insert Screenshot")
    ("p" org-media-note-insert-note-from-pbf "Import from pbf")
    ("s" (insert (mpv-get-property "sub-text")) "Insert sub")
    )
   "Toggle"
   (
    ("t m" org-media-note-mode "Auto insert media item" :toggle t)
    ("t c" org-media-note-refcite-mode "Use ref key instead of absolute path" :toggle t)
    ("t s" org-media-note-toggle-save-screenshot "Auto save screenshot" :toggle org-media-note-save-screenshot-p)
    )
   )
  )

;;;;; Utils

(defun org-media-note--millisecs-to-hms (millisecs)
  (org-timer-secs-to-hms (round (/ (string-to-number millisecs) 1000)))
)

(defun org-media-note--get-current-hms ()
  "Get current media timestamp in format h:mm:ss"
  (let ((time (mpv-get-playback-position)))
    (org-timer-secs-to-hms (round time))
    )
  )

(defun org-media-note--current-org-ref-key ()
  (org-entry-get (point) "Custom_ID")
  )

(defun org-media-note-get-media-file-by-key (key)
  (let* (
         ;; TODO
         (file-types (split-string (bibtex-completion-get-value-by-key key "formats") ", "))
         (file-path (car (bibtex-completion-find-pdf key)))
         (file-path-without-ext (file-name-sans-extension file-path))
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
        (org-media-note--get-realpath-for-file (concat file-path-without-ext "." file-type))
      nil)
    )
  )

(defun org-media-note--current-media-type ()
  "Get current playing media type."
  (let* (
         (file-path (mpv-get-property "path"))
         )
    (org-media-note--file-media-type file-path)
    )
  )

(defun org-media-note--file-media-type (file-path)
  "Get file media type."
  (let* (
         (file-ext (if file-path (file-name-extension file-path)))
         )
    (org-media-note--get-media-type file-ext)
    )
  )

(defun org-media-note--get-media-type (file-ext)
    (cond
     ((member file-ext org-media-note--video-types)
      "video")
     ((member file-ext org-media-note--audio-types)
      "audio")
     (t nil)
     )
  )
;;;;; Add note


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
  (insert (format "%s " (org-media-note--link)))
  )

(defun org-media-note--link ()
  "Return media link."
  (let* (
         (file-path (mpv-get-property "path"))
         (link-type (if org-media-note-refcite-mode
                        (concat (org-media-note--current-media-type) "cite")
                      (org-media-note--current-media-type)
                      ))
         (hms (org-media-note--get-current-hms))
         )
    (if org-media-note-refcite-mode
        (format "[[%s:%s#%s][%s]]" link-type (org-media-note--current-org-ref-key) hms hms)
      (format "[[%s:%s#%s][%s]]" link-type file-path hms hms)
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
    (insert (format "[[file:%s]] " image-target-path))
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


(defun org-media-note--get-realpath-for-file (symlink)
  "Get realpath for symlink."
  (replace-regexp-in-string "\n" ""
                                  (shell-command-to-string
                                   (concat "realpath \""
                                           (replace-regexp-in-string "~" (file-truename "~") symlink)
                                           "\"")
                                   ))
  )

(defun org-media-note-open-ref-cite-function ()
  (interactive)
  (let* ((object (org-element-context))
         (media-note-link (if (eq (org-element-type object) 'link)
                              (org-element-property :path object)
			    ))
         (ref-cite-key (car (split-string media-note-link "#")))
         )
    (with-temp-buffer
      (org-mode)
      ;; TODO bibtex-files dependency
      ;; insert bibliography in order to find entry in org-ref
      (insert (s-join "\n" (mapcar (lambda (bib)
                                     (format "bibliography:%s" bib)
                                     )
                                   bibtex-files
                                   )))
      (insert (format "\ncite:%s" ref-cite-key))
      (funcall org-ref-cite-onclick-function nil)
      )
    )
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

(defun org-media-note-change-volume-by (step)
  "Set playing speed."
  (mpv-run-command "add" "volume" step)
  (setq org-media-note-last-volume (mpv-get-property "volume"))
  )

(defun org-media-note-mpv-toggle-volume ()
  (interactive)
  (let ((current-volume (mpv-get-property "volume")))
    (if (= 100.0 current-volume)
        (mpv-set-property "volume" org-media-note-last-volume)
      (progn
        (setq org-media-note-last-volume current-volume)
        (mpv-set-property "volume" 100))
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

;;;;; Import From other apps

(defun org-media-note-insert-note-from-pbf ()
  (interactive)
  (let (
        (key (org-media-note--current-org-ref-key))
        pbf-file media-link-type media-file
        )
    (if key
        (progn
          (setq media-link-type "videocite")
          (setq media-file key)
          (let* (
                 (source-media (org-media-note-get-media-file-by-key key))
                 ;; (source-dir (file-name-directory source-media))
                 )
            (setq pbf-file (concat (file-name-sans-extension source-media) ".pbf"))
            )
          )
      (progn
        (setq media-link-type "video")
        ;; TODO
        (setq media-file (read-file-name "Find media file:"))
        (setq pbf-file (concat (file-name-sans-extension media-file) ".pbf"))
        )
      )
    (message pbf-file)
    (if (not (file-exists-p pbf-file))
        ;; TODO
        (setq pbf-file (read-file-name "Find pbf file:"))
      )
    (insert (org-media-note--convert-from-pbf pbf-file media-link-type media-file))
    ;; TODO
    (if (y-or-n-p "Delete the PBF File?")
        (delete-file pbf-file)
        )
    )
  )

(defun org-media-note-insert-note-from-noted ()
  (interactive)
  (let (
        (key (org-media-note--current-org-ref-key))
        noted-txt media-link-type media-file
        )
    (setq noted-txt (read-file-name "Find exported Noted txt:"))
    (if key
        (progn
          (setq media-file key)
          (setq media-link-type (concat
                                 (org-media-note--file-media-type (org-media-note-get-media-file-by-key key))
                                 "cite"))
          )
      (progn
        ;; TODO
        (setq media-file (read-file-name "Find media file:"))
        (setq media-link-type (org-media-note--file-media-type media-file))
        )
      )
    (insert (org-media-note--convert-from-noted noted-txt media-link-type media-file))
    ;; TODO
    (if (y-or-n-p "Delete Noted txt?")
        (delete-file noted-txt)
        )
    )
  )

(defun org-media-note--convert-from-noted (noted-file media-link-type media-file)
  ;; (with-current-buffer (get-buffer-create "*RESULTS*")
  (with-temp-buffer
    (insert-file-contents noted-file)
    (replace-string "￼" ""
                    nil (point-min) (point-max))
    ;; replace unordered list
    (replace-regexp "[•◦▫]" "-"
                    nil (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "^\\[\\([:0-9]+\\)\\]\\([[:blank:]]+\\)-[[:blank:]]+"  nil t)
      (let* (
            (hms (buffer-substring (match-beginning 1) (match-end 1)))
            (blank-indent (buffer-substring (match-beginning 2) (match-end 2)))
            )
        (replace-match (concat blank-indent "- [[" media-link-type ":" media-file "#" hms "][" hms "]] ") t)
        )
      )
    ;; replace ordered list
    (goto-char (point-min))
    (while (re-search-forward "^\\[\\([:0-9]+\\)\\]\\([[:blank:]]+\\)\\([[:digit:]]\\. \\)"  nil t)
      (let* (
            (hms (buffer-substring (match-beginning 1) (match-end 1)))
            (blank-indent (buffer-substring (match-beginning 2) (match-end 2)))
            (number-bullet (buffer-substring (match-beginning 3) (match-end 3)))
            )
        (replace-match (concat blank-indent number-bullet "[[" media-link-type ":" media-file "#" hms "][" hms "]] ") t)
        )
      )
    ;; replace timestamped text
    (goto-char (point-min))
    (while (re-search-forward "^\\[\\([:0-9]+\\)\\]\\([[:blank:]]+\\)"  nil t)
      (let* (
            (hms (buffer-substring (match-beginning 1) (match-end 1)))
            (blank-indent (buffer-substring (match-beginning 2) (match-end 2)))
            )
        (replace-match (concat blank-indent "- [[" media-link-type ":" media-file "#" hms "][" hms "]] ") t)
        )
      )
    ;; format
    (replace-regexp "\\]\\] +" "]] "
                    nil (point-min) (point-max))
    (buffer-string)
    )
  )


(defun org-media-note--convert-from-pbf (pbf-file media-link-type media-file)
  ;; (with-current-buffer (get-buffer-create "*RESULTS*")
  (with-temp-buffer
    (insert-file-contents pbf-file)
    (replace-string "[Bookmark]\n" ""
                    nil (point-min) (point-max))
    (replace-regexp "^[[:digit:]]+=$" ""
                    nil (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "^[[:digit:]]+=\\([[:digit:]]+\\)\\*\\([^\\*]+\\)\\*.+"  nil t)
      (let* (
            (millisecs (buffer-substring (match-beginning 1) (match-end 1)))
            (note (buffer-substring (match-beginning 2) (match-end 2)))
            (hms (org-media-note--millisecs-to-hms millisecs))
            )
        (replace-match
         (format "- [[%s:%s#%s][%s]] %s" media-link-type media-file hms hms note)
         t)
        )
      )
    (buffer-string)
    )
  )

;;;;; Customize Org link
(org-link-set-parameters "video"
                         :follow 'org-media-note-link-follow)

(org-link-set-parameters "audio"
                         :follow 'org-media-note-link-follow)

(org-link-set-parameters "videocite"
                         :follow 'org-media-note-cite-link-follow
                         :keymap org-media-note-cite-keymap
                         )

(org-link-set-parameters "audiocite"
                         :follow 'org-media-note-cite-link-follow)

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
    nil))


(defun org-media-note--setup-refcite-mode ()
  (let* ((key (org-media-note--current-org-ref-key))
         )
    (if key
        nil
      ;; TODO can not set up refcite-mode without key
      (error "Cannot set up refcite-mode without a key.")
      )
    ))


(defun org-media-note-toggle-save-screenshot ()
  (interactive)
  (if org-media-note-save-screenshot-p
      (setq org-media-note-save-screenshot-p nil)
    (setq org-media-note-save-screenshot-p t)))

;;;; Footer
(provide 'org-media-note)
;;; org-media-note.el ends here
