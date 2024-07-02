;;; org-media-note.el --- Take video and audio notes with Org-mode -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Yuchen Lea

;; Author: Yuchen Lea <yuchen.lea@gmail.com>
;; URL: https://github.com/yuchen-lea/org-media-note
;; Version: 1.11.0
;; Keywords: note-taking, multimedia, video
;; Package-Requires: ((emacs "27.1") (mpv "0.2.0") (pretty-hydra "0.2.2"))

;;; Commentary:

;; Take notes for video and audio files.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
;;;; Requirements
(require 'pretty-hydra)
(require 'org-media-note-core)
(require 'org-media-note-mpv)
(require 'org-media-note-import)

(declare-function org-media-note-setup-org-ref "org-media-note-org-ref")

;;;; Commands
;;;;; Hydra

;; A quick menu for org-media-note
(pretty-hydra-define org-media-note-hydra
  (:color red
   :title (org-media-note--hydra-title)
   :hint nil)
  ("File"
   (("o" org-media-note-play-smart
     (cl-multiple-value-bind (link _ _)
         (org-media-note--link-context)
       (cl-multiple-value-bind (ref-mode key _ _)
           (org-media-note--ref-context)
         (cl-multiple-value-bind (_ media-files-in-attach-dir)
             (org-media-note--attach-context)
           (cond
            (link "Open link")
            (ref-mode (format "Open %s" key))
            ((> (length media-files-in-attach-dir) 0) "Open attach")
            (t "Open media")))))
     :width 20
     :exit t)
    ("j"
     (mpv-cycle-property "sub")
     "toggle subtitles")
    ("T"
     (mpv-cycle-property "ontop")
     "toggle ontop")
    ("c"
     (org-media-note-change-speed-by 0.1)
     "increase speed")
    ("x"
     (org-media-note-change-speed-by -0.1)
     "decrease speed")
    ("z" org-media-note-mpv-toggle-speed "reset speed"))
   "Playback"
   (("<SPC>" mpv-pause "Play/Pause")
    ("l"
     (mpv-run-command "ab-loop")
     (let ((time-a (mpv-get-property "ab-loop-a"))
           (time-b (mpv-get-property "ab-loop-b")))
       (if (org-media-note--ab-loop-p)
           (format "Clear AB-loop (%s-%s)"
                   (org-media-note--seconds-to-timestamp time-a)
                   (org-media-note--seconds-to-timestamp time-b))
         (if (numberp time-a)
             (format "Set B of AB-loop (%s-)"
                     (org-media-note--seconds-to-timestamp time-a))
           "Set A of AB-loop")))
     :width 31)
    ("g" org-media-note-goto-timestamp "Jump to timestamp")
    ("<left>" (org-media-note-seek 'backward) (format "Backward %s" (org-media-note--seek-step t)))
    ("<right>" (org-media-note-seek 'forward) (format "Forward %s" (org-media-note--seek-step t)))
    ("C-<left>"
     (mpv-run-command "sub-seek" -1)
     "Previous subtitle")
    ("C-<right>"
     (mpv-run-command "sub-seek" 1)
     "Next subtitle")
    ("<prior>"
     (mpv-run-command "add" "chapter" -1)
     "Previous Chapter")
    ("<next>"
     (mpv-run-command "add" "chapter" 1)
     "Next Chapter"))
   "Volume"
   (("+"
     (org-media-note-change-volume-by 5)
     "Up")
    ("-"
     (org-media-note-change-volume-by -5)
     "Down")
    ("0" org-media-note-mpv-toggle-volume "toggle")
    ("m"
     (mpv-cycle-property "mute")
     "(un)mute"))
   "Note"
   (("i" org-media-note-insert-link "Insert timestamp")
    ("a" org-media-note-adjust-timestamp-offset "Adjust timestamp")
    ("S"
     (if (org-media-note--ab-loop-p)
         (org-media-note-capture-ab-loop-and-insert)
       (org-media-note-insert-screenshot))
     (if (org-media-note--ab-loop-p)
         "Insert ab-loop clip"
       "Insert Screenshot"))
    ("s" org-media-note-insert-sub-text "Insert subtitle")
    ("H-m" org-media-note-merge-item "Merge items"))
   "Import"
   (("I p" org-media-note-insert-note-from-pbf
     "from pbf")
    ("I n" org-media-note-insert-note-from-noted
     "from Noted")
    ("I t" org-media-note-convert-from-org-timer
     "from org-timer")
    ("I s" org-media-note-insert-note-from-subtitle
     "from subtitle")
    ("I c" org-media-note-insert-note-from-chapter-list
     "from chapters"))
   "Config"
   (("t m" toggle-org-media-note-auto-insert-item
     "Auto insert media item" :toggle org-media-note-auto-insert-item)
    ("t s" org-media-note-toggle-save-screenshot
     "Auto insert screenshot" :toggle org-media-note-save-screenshot-p)
    ("t S" org-media-note-toggle-screenshot-with-sub
     "Screenshot with sub" :toggle org-media-note-screenshot-with-sub)
    ("t l" org-media-note-config-ab-loop-capture-method
     (format "AB-loop Clip: %s"
             (if org-media-note-capture-ab-loop-ask-each-time
                 "always ask" org-media-note-default-capture-ab-loop-function-name)))
    ("t c" org-media-note-toggle-refcite
     "Cite key instead of path" :toggle org-media-note-use-refcite-first)
    ("t p" org-media-note-toggle-pause-after-insertion
     "Pause after insert link" :toggle org-media-note-pause-after-insert-link)
    ("t t" org-media-note-toggle-timestamp-pattern
     (format "Timestamp format: %s"
             (cond
              ((eq org-media-note-timestamp-pattern 'hms) "hh:mm:ss")
              ((eq org-media-note-timestamp-pattern 'hmsf) "hh:mm:ss.fff")))
     :width 29)
    ("t M" org-media-note-set-separator
     (format "Separator when merge: %s" org-media-note-separator-when-merge))
    ("t <right>" org-media-note-set-seek-method
     (format "Seek step: %s" (org-media-note--seek-step t))))))


(defun org-media-note--hydra-title ()
  "Return title string for `org-media-note-hydra'."
  (cl-multiple-value-bind (file-path title current-timestamp)
      (org-media-note--current-media-info)
    (let ((icon (if (featurep 'all-the-icons)
                    (all-the-icons-material "ondemand_video")
                  "")))
      (if file-path
          ;; Title when mpv is playing media
          (let ((speed (mpv-get-property "speed"))
                (volume (mpv-get-property "volume"))
                (total-timestamp (org-media-note--get-duration-timestamp))
                (remaining-hms (org-media-note--seconds-to-timestamp (mpv-get-property "playtime-remaining"))))
            (s-concat icon
                      " org-media-note: "
                      current-timestamp
                      " / "
                      total-timestamp
                      "\t Volume: "
                      (number-to-string volume)
                      "\t Speed: "
                      (number-to-string speed)
                      "\t Remaining: "
                      remaining-hms
                      "\n\t❯ "
                      (if (org-media-note-ref-cite-p)
                          (format "%s (%s)"
                                  title
                                  (org-media-note--current-org-ref-key))
                        (if title
                            (format "%s (%s)" title file-path)
                          file-path))))
        ;; Title when no media is playing
        (concat icon " org-media-note")))))

(defun org-media-note--seek-step (&optional in-macro?)
  "Return a formatted string based on the current seek method and value."
  (format "%s%s"
          org-media-note-seek-value
          (cond
           ((eq org-media-note-seek-method 'seconds) "s")
           ((eq org-media-note-seek-method 'percentage)
            (if in-macro? "%%" "%"))
           ((eq org-media-note-seek-method 'frames)
            (if (= org-media-note-seek-value 1)
                " frame"
              " frames")))))

;;;;; Customize Org link
(defun org-media-note--default-desc-fn (base timestamp seconds desc)
  "Default function to generate link description.
`BASE': base URL, local path or citekey.
`TIMESTAMP': original timestamp string.
`SECONDS': timestamp converted into total seconds.
`DESC': link description."
  timestamp)

(defun org-media-note-export-link (path desc format)
  "Process the link based on the export format."
  (cond
   ((eq format 'html)
    (let* ((splitted (split-string path "#"))
           (url (nth 0 splitted))
           (timestamp (nth 1 splitted))
           (seconds (org-media-note--timestamp-to-seconds timestamp t))
           (param-symbol (if (string-match org-media-note--youtube-link-pattern url)
                             "&"
                           "?")) ;; youtube, bilibili tested
           (new-url (format "%s%st=%s" url param-symbol seconds)))
      (format "<a href=\"%s\">%s</a>"
              new-url
              (funcall org-media-note-desc-fn
                       url timestamp
                       seconds desc))))
   (t (format "[[%s][%s]]" path desc))))

(defun org-media-note-default-timestamp-description (&optional _ _)
  "Return the timestamp description based on `org-media-note-timestamp-pattern'."
  (if (eq org-media-note-timestamp-pattern 'hmsf)
      "00:00:00.000"
    "00:00:00"))

(defun org-media-note-complete-link (type &optional arg)
  "Create a media link using completion.
TYPE is the media type 'video' or 'audio'.
With optional ARG, abbreviate the file name in the link."
  (let ((file-link (org-link-complete-file arg))
        (timestamp (org-media-note-default-timestamp-description)))
    (concat (replace-regexp-in-string "^file:" (concat type ":") file-link) "#" timestamp)))

(dolist (link '("video" "audio"))
  (org-link-set-parameters link
                           :follow #'org-media-note-media-link-follow
                           :export #'org-media-note-export-link
                           :complete (lambda (&optional arg) (org-media-note-complete-link link arg))
                           :insert-description #'org-media-note-default-timestamp-description))

;;;;; Config

(defun toggle-org-media-note-auto-insert-item ()
  "Toggle the automatic insertion of media items."
  (interactive)
  (setq org-media-note-auto-insert-item (not org-media-note-auto-insert-item))
  (org-media-note--update-auto-insert-advice org-media-note-auto-insert-item)
  (message "Auto insert media item: %s" (if org-media-note-auto-insert-item "enabled" "disabled")))


(defun org-media-note-toggle-refcite ()
  "Toggle refcite for links."
  (interactive)
  (if org-media-note-use-refcite-first
      (setq org-media-note-use-refcite-first nil)
    (setq org-media-note-use-refcite-first t)))

(defun org-media-note-toggle-pause-after-insertion ()
  "Toggle pausing after inserting a link."
  (interactive)
  (if org-media-note-pause-after-insert-link
      (setq org-media-note-pause-after-insert-link nil)
    (setq org-media-note-pause-after-insert-link t)))


(defun org-media-note-toggle-save-screenshot ()
  "Toggle `org-media-note-save-screenshot-p'."
  (interactive)
  (if org-media-note-save-screenshot-p
      (setq org-media-note-save-screenshot-p nil)
    (setq org-media-note-save-screenshot-p t)))

(defun org-media-note-toggle-screenshot-with-sub ()
  "Toggle screenshot with sub."
  (interactive)
  (if org-media-note-screenshot-with-sub
      (setq org-media-note-screenshot-with-sub nil)
    (setq org-media-note-screenshot-with-sub t)))

(defun org-media-note-config-ab-loop-capture-method ()
  "Config the method for capturing AB-loop clips."
  (interactive)
  (let ((choice (org-media-note--select "Capture method: "
                                        '("Always ask" "Select a default"))))
    (if (equal choice "Select a default")
        (progn
          (setq org-media-note-capture-ab-loop-ask-each-time
                nil)
          (let ((selected-function (org-media-note--select-capture-function)))
            (setq org-media-note-default-capture-ab-loop-function-name
                  selected-function)))
      (setq org-media-note-capture-ab-loop-ask-each-time
            t))))

(defun org-media-note-toggle-timestamp-pattern ()
  "Toggle screenshot with sub."
  (interactive)
  (cond
   ((eq org-media-note-timestamp-pattern 'hms)
    (setq org-media-note-timestamp-pattern 'hmsf))
   ((eq org-media-note-timestamp-pattern 'hmsf)
    (setq org-media-note-timestamp-pattern 'hms))))

(defun org-media-note-set-separator (new-separator)
  "Set the value of org-media-note-media-note-separator to NEW-SEPARATOR."
  (interactive "sEnter the new separator: ")
  (setq org-media-note-separator-when-merge new-separator)
  (message "org-media-note-media-note-separator set to: %s" org-media-note-separator-when-merge))

(defun org-media-note-set-seek-method ()
  "Config the seek method and value for playback."
  (interactive)
  (let* ((method (org-media-note--select "Select seek method: "
                                         '("seconds" "percentage" "frames")))
         (value (read-number (format "Enter value for %s: " method))))
    (setq org-media-note-seek-method (cond
                                      ((string-equal method "seconds") 'seconds)
                                      ((string-equal method "percentage") 'percentage)
                                      ((string-equal method "frames") 'frames))
          org-media-note-seek-value value))
  (message "Seek method set to: %s."
           (org-media-note--seek-step)))

;;;;; Minor Mode

(defun org-media-note--update-auto-insert-advice (add-advice)
  "Update the advice for org-insert-item based on ADD-ADVICE."
  (if add-advice
      (advice-add 'org-insert-item :before-until #'org-media-note--insert-item-advice)
    (advice-remove 'org-insert-item #'org-media-note--insert-item-advice)))

;;;###autoload
(define-minor-mode org-media-note-mode
  "Minor mode for taking audio and video notes with `org-mode'."
  :init-value t
  :global t
  (if org-media-note-mode
      (progn
        (org-media-note--update-auto-insert-advice org-media-note-auto-insert-item)
        (advice-add 'org-beginning-of-line :around #'org-media-note--beginning-of-line-advice)
        (when org-media-note-use-org-ref
          (org-media-note-setup-org-ref)))
    (org-media-note--update-auto-insert-advice nil)
    (advice-remove 'org-beginning-of-line #'org-media-note--beginning-of-line-advice)))

;;;; Footer
(provide 'org-media-note)
;;; org-media-note.el ends here
