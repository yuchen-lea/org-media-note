;;; org-media-note-mpv.el --- MPV integration for org-media-note -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
;;;; Requirements

(require 'org-media-note-core)

(declare-function org-media-note-get-media-file-by-key "org-media-note-org-ref")
(declare-function org-media-note-get-url-by-key "org-media-note-org-ref")

;;;; Customization
;;;; Variables

(defvar org-media-note-last-play-speed 1.0
  "Last play speed in mpv.")

(defvar org-media-note-last-volume 100.0
  "Last Volume in mpv.")

;;;; Commands

(defun org-media-note-mpv-smart-play ()
  "Conditionally open media file in mpv.
1. When point at a file link, play it in mpv;
2. When citation key found, play the corresponding local media or online media;
3. With a single attachment, file, or url: play it in mpv;
4. With multiple attachments, open the attach dir to select;
5. Else, answer y to find local file to open, n to input URL."
  (interactive)
  (let* ((element (org-element-context))
         (type (org-element-type element))
         (attach-dir (if (org-attach-dir)
                         (format "%s/"
                                 (org-attach-dir))))
         key media-file-by-key media-url-by-key)
    (when org-media-note-use-org-ref
      (setq key (org-media-note--current-org-ref-key))
      (setq media-file-by-key (org-media-note-get-media-file-by-key key))
      (setq media-url-by-key (org-media-note-get-url-by-key key)))
    (cond
     ((and (eq type 'link)
           (string= (org-element-property :type element) "file"))
      (mpv-start (org-element-property :path element)))
     ((and (org-media-note-ref-cite-p)
           media-file-by-key)
      (mpv-play media-file-by-key))
     ((and (org-media-note-ref-cite-p)
           media-url-by-key)
      (org-media-note--mpv-play-online-video media-url-by-key))
     (attach-dir (let* ((media-files-in-attach-dir (org-media-note--media-files-in-dir attach-dir))
                        (number-of-media-files (length media-files-in-attach-dir)))
                   (if (= 1 number-of-media-files)
                       (mpv-play (car media-files-in-attach-dir))
                     (mpv-play (read-file-name "File to play: " attach-dir)))))
     (t (if (y-or-n-p "Local media (`n` to enter remote URL)? ")
            (mpv-play (read-file-name "File to play: "))
          (org-media-note-play-online-video))))))

(defun org-media-note--media-files-in-dir (dir)
  "Get supported media file list in DIR."
  (directory-files
   dir
   'full
   (rx (eval (cons 'or
                   (append org-media-note--video-types org-media-note--audio-types)))
       eos)))

(defun org-media-note-play-online-video ()
  "Open online media file in mpv."
  (interactive)
  (let ((video-url (read-string "Url to play: ")))
    (org-media-note--mpv-play-online-video video-url)))

(defun org-media-note--mpv-play-online-video (video-url)
  "Play remote VIDEO-URL."
  (if (org-media-note--online-video-p video-url)
      (if (or (executable-find "youtube-dl")
              (executable-find "yt-dlp"))
        (mpv-start video-url)
      (error "Warning: mpv needs the youtube-dl or yt-dlp program to play online videos"))
    (error (format "'%s' is not a valid url!" video-url))))

(defun org-media-note-change-speed-by (speed-step)
  "Modify playing media's speed by SPEED-STEP."
  (let ((current-speed (mpv-get-property "speed")))
    (mpv-speed-set (+ current-speed speed-step))))

(defun org-media-note-mpv-toggle-speed ()
  "Toggle playback speed of media."
  (interactive)
  (let ((current-speed (mpv-get-property "speed")))
    (if (= current-speed 1.0)
        (mpv-speed-set org-media-note-last-play-speed)
      (setq org-media-note-last-play-speed current-speed)
      (mpv-speed-set 1))))

(defun org-media-note-change-volume-by (step)
  "Set playing volume by STEP."
  (mpv-run-command "add" "volume" step)
  (setq org-media-note-last-volume (mpv-get-property "volume")))

(defun org-media-note-mpv-toggle-volume ()
  "Toggle plaback volume of media."
  (interactive)
  (let ((current-volume (mpv-get-property "volume")))
    (if (= 100.0 current-volume)
        (mpv-set-property "volume" org-media-note-last-volume)
      (progn
        (setq org-media-note-last-volume current-volume)
        (mpv-set-property "volume" 100)))))


;;;; Footer
(provide 'org-media-note-mpv)
;;; org-media-note-mpv.el ends here
