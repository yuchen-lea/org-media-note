;;; org-media-note.el --- Take video and audio notes with Org-mode -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Yuchen Lea

;; Author: Yuchen Lea <yuchen.lea@gmail.com>
;; URL: https://github.com/yuchen-lea/org-media-note
;; Version: 1.12.0
;; Keywords: note-taking, multimedia, video
;; Package-Requires: ((emacs "24.4") (transient "0.1.0") (mpv "0.2.0"))


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
(require 'transient)

(require 'org-media-note-core)

(declare-function org-media-note-cite-setup "org-media-note-cite")

;;;; Commands
;;;;; UI

(defun org-media-note-show-interface ()
  (interactive)
  (org-media-note-transient))

(transient-define-prefix org-media-note-transient ()
  "Main transient for org-media-note."
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-leave
  [:description org-media-note--ui-title
                ["\nFile"
                 ("o" org-media-note-play-smart
                  :description org-media-note--ui-play-smart-title
                  :transient nil)
                 ("M-o" "Browse url"
                  org-media-note-open-url-at-point
                  :transient nil
                  :if (lambda ()
                        (and org-media-note-interface-display-browse-url
                             (org-media-note--url-at-point))))
                 ""
                 ("T"
                  (lambda ()
                    (interactive)
                    (mpv-cycle-property "ontop"))
                  :description (lambda ()
                                 (concat "Ontop "
                                         (org-media-note--ui-toggle-state (eq (mpv-get-property "ontop") t)))))
                 ("c" "Increase speed"
                  (lambda ()
                    (interactive)
                    (org-media-note-change-speed-by 0.1)))
                 ("x" "Decrease speed"
                  (lambda ()
                    (interactive)
                    (org-media-note-change-speed-by -0.1)))
                 ("z" "Reset speed" org-media-note-mpv-toggle-speed)
                 ""
                 "Volume"
                 ("+" "Up"
                  (lambda ()
                    (interactive)
                    (org-media-note-change-volume-by 5)))
                 ("-" "Down"
                  (lambda ()
                    (interactive)
                    (org-media-note-change-volume-by -5)))
                 ("0" "Toggle" org-media-note-mpv-toggle-volume)
                 ("m"
                  (lambda ()
                    (interactive)
                    (mpv-cycle-property "mute"))
                  :description (lambda ()
                                 (concat "Mute "
                                         (org-media-note--ui-toggle-state (eq (mpv-get-property "mute") t)))))]
                ["Playback"
                 ("<SPC>" mpv-pause
                  :description (lambda ()
                                 (if (eq (mpv-get-property "pause") t)
                                     "Play"
                                   "Pause")))
                 ("tp" org-media-note-toggle-pause-after-insertion
                  :description (lambda ()
                                 (concat "Auto Pause "
                                         (org-media-note--ui-toggle-state 'org-media-note-pause-after-insert-link))))
                 ("l"
                  (lambda ()
                    (interactive)
                    (mpv-run-command "ab-loop"))
                  :description org-media-note--ui-ab-loop-title)
                 ("g" "Jump to timestamp" org-media-note-goto-timestamp)
                 ""
                 ("t <right>" org-media-note-set-seek-method
                  :description (lambda ()
                                 (concat "Seek step: "
                                         (org-media-note--ui-hightlight (org-media-note--ui-seek-step nil)))))
                 ("<left>"
                  (lambda ()
                    (interactive)
                    (org-media-note-seek 'backward))
                  :description (lambda ()
                                 (format "Backward %s"
                                         (org-media-note--ui-seek-step nil))))
                 ("<right>"
                  (lambda ()
                    (interactive)
                    (org-media-note-seek 'forward))
                  :description (lambda ()
                                 (format "Forward %s"
                                         (org-media-note--ui-seek-step nil))))
                 ("C-<left>" "Previous subtitle"
                  (lambda ()
                    (interactive)
                    (mpv-run-command "sub-seek" -1)))
                 ("C-<right>" "Next subtitle"
                  (lambda ()
                    (interactive)
                    (mpv-run-command "sub-seek" 1)))
                 ("<prior>" "Previous Chapter"
                  (lambda ()
                    (interactive)
                    (mpv-run-command "add" "chapter" -1)))
                 ("<next>" "Next Chapter"
                  (lambda ()
                    (interactive)
                    (mpv-run-command "add" "chapter" 1)))]
                ["Note"
                 ("i" "Insert timestamp" org-media-note-insert-link
                  :transient nil)
                 ("a" "Adjust timestamp" org-media-note-adjust-timestamp-offset)
                 ("M-RET" "Insert item" org-meta-return :transient nil)
                 ("tm" org-media-note-toggle-auto-insert-item
                  :description (lambda ()
                                 (concat "Auto insert item "
                                         (org-media-note--ui-toggle-state 'org-media-note-auto-insert-item))))
                 ("I" "Import from" org-media-note-import-transient)
                 ""
                 ("s" "Insert subtitle" org-media-note-insert-sub-text)
                 ("j" "Toggle subtitle"
                  (lambda ()
                    (interactive)
                    (mpv-cycle-property "sub")))
                 ""
                 ("H-m" "Merge items" org-media-note-merge-item)
                 ("tM" org-media-note-set-separator
                  :description (lambda ()
                                 (concat "Separator for merge: "
                                         (org-media-note--ui-hightlight org-media-note-separator-when-merge))))
                 ""]
                ["Note Format"
                 ("tt" org-media-note-toggle-timestamp-pattern
                  :description (lambda ()
                                 (concat "timestamp: "
                                         (org-media-note--ui-hightlight (cond
                                                                         ((eq org-media-note-timestamp-pattern 'hms) "hh:mm:ss")
                                                                         ((eq org-media-note-timestamp-pattern 'hmsf) "hh:mm:ss.fff"))))))
                 ("tc" org-media-note-toggle-refcite
                  :description (lambda ()
                                 (concat "citekey instead of path "
                                         (org-media-note--ui-toggle-state 'org-media-note-use-refcite-first))))
                 ""
                 "Screenshot & Clip"
                 ("S"
                  (lambda ()
                    (interactive)
                    (if (org-media-note--ab-loop-p)
                        (org-media-note-capture-ab-loop-and-insert)
                      (org-media-note-insert-screenshot)))
                  :description (lambda ()
                                 (if (org-media-note--ab-loop-p)
                                     "Insert ab-loop clip"
                                   "Insert screenshot")))
                 ("ts" org-media-note-toggle-save-screenshot
                  :description (lambda ()
                                 (concat "Auto insert screenshot "
                                         (org-media-note--ui-toggle-state 'org-media-note-save-screenshot-p))))
                 ("tS" org-media-note-toggle-screenshot-with-sub
                  :description (lambda ()
                                 (concat "Screenshot with sub "
                                         (org-media-note--ui-toggle-state 'org-media-note-screenshot-with-sub))))
                 ("tl" org-media-note-set-ab-loop-capture-method
                  :description (lambda ()
                                 (concat "AB-loop clip: "
                                         (org-media-note--ui-hightlight (if org-media-note-capture-ab-loop-ask-each-time
                                                                            "always ask" org-media-note-default-capture-ab-loop-function-name)))))]])

(transient-define-prefix org-media-note-import-transient ()
  "Transient for org-media-note import commands."
  ["Import from\n"
   [("s" "Subtitle" org-media-note-insert-note-from-subtitle)
    ("c" "Chapter" org-media-note-insert-note-from-chapter-list)]
   [("t" "Org-timer item" org-media-note-convert-from-org-timer)
    ("p" "PBF (Potplayer)" org-media-note-insert-note-from-pbf)
    ("n" "Noted" org-media-note-insert-note-from-noted)]])


;;;;; Customize Org link
(defun org-media-note--default-desc-fn (base timestamp desc)
  "Default function to generate link description when exporting.
`BASE': base URL, local path or citekey.
`TIMESTAMP': original timestamp string.
`DESC': link description."
  desc)

(defun org-media-note-export-link (path desc format)
  "Process the link based on the export format."
  (cond
   ((eq format 'html)
    (let* ((new-url (org-media-note--generate-url-with-timestamp
                     path))
           (timestamp (nth 1
                           (split-string path "#"))))
      (format "<a href=\"%s\">%s</a>"
              new-url
              (funcall org-media-note-desc-fn new-url timestamp desc))))
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
                           :follow #'org-media-note--open
                           :export #'org-media-note-export-link
                           :complete (lambda (&optional arg) (org-media-note-complete-link link arg))
                           :insert-description #'org-media-note-default-timestamp-description))

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
        (when org-media-note-use-cite
          (org-media-note-cite-setup)))
    (org-media-note--update-auto-insert-advice nil)
    (advice-remove 'org-beginning-of-line #'org-media-note--beginning-of-line-advice)))

;;;; Footer
(provide 'org-media-note)
;;; org-media-note.el ends here
