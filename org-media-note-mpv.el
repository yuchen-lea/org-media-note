;;; org-media-note-mpv.el -*- lexical-binding: t; -*-

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
  "Open media file in mpv:
1. When there's just one media file in attach dir, or found media file or url by key, play this im mpv;
2. When there're multiple media files in attach dir, open the attach dir to select;
3. Else, answer y to find local file to open, n to input online url."
  (interactive)
  (let* ((key (org-media-note--current-org-ref-key))
         (media-file-by-key (org-media-note-get-media-file-by-key key))
         (media-url-by-key (org-media-note-get-url-by-key key))
         (attach-dir (if (org-attach-dir)
                         (format "%s/"
                                 (org-attach-dir))))
         (media-files-in-attach-dir (org-media-note--media-files-in-dir attach-dir))
         (number-of-media-files (length media-files-in-attach-dir)))
    (cond
     ((and (org-media-note-ref-cite-p)
           media-file-by-key)
      (mpv-play media-file-by-key))
     ((and (org-media-note-ref-cite-p)
           media-url-by-key)
      (org-media-note--mpv-play-online-video media-url-by-key))
     (media-files-in-attach-dir
      (if (= 1 number-of-media-files)
          (mpv-play (car media-files-in-attach-dir))
        (mpv-play (read-file-name "File to play: " attach-dir))))
     (t
      (if (y-or-n-p "'y' to find local media, 'n' to input online url: ")
          (mpv-play (read-file-name "File to play: "))
        (org-media-note-play-online-video))))))

(defun org-media-note--media-files-in-dir (dir)
  "Get supported media file list in dir."
  (if dir
      (directory-files dir
                       'full
                       (rx (eval (cons 'or (append org-media-note--video-types org-media-note--audio-types)))
                           eos))
    nil))

(defun org-media-note-play-online-video ()
  "Open online media file in mpv."
  (interactive)
  (let ((video-url (read-string "Url to play: ")))
    (org-media-note--mpv-play-online-video video-url)))

(defun org-media-note--mpv-play-online-video (video-url)
  (if (org-media-note--online-video-p video-url)
      (mpv-start video-url)
    (error (format "'%s' is not a valid url!" video-url))))

(defun org-media-note-change-speed-by (speed-step)
  "Set playing speed."
  (let ((current-speed (mpv-get-property "speed")))
    (mpv-speed-set (+ current-speed speed-step))))

(defun org-media-note-mpv-toggle-speed ()
  (interactive)
  (let ((current-speed (mpv-get-property "speed")))
    (if (= 1.0 current-speed)
        (mpv-speed-set org-media-note-last-play-speed)
      (progn
        (setq org-media-note-last-play-speed current-speed)
        (mpv-speed-set 1)))))

(defun org-media-note-change-volume-by (step)
  "Set playing speed."
  (mpv-run-command "add" "volume" step)
  (setq org-media-note-last-volume (mpv-get-property "volume")))

(defun org-media-note-mpv-toggle-volume ()
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
