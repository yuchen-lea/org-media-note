;;; org-media-note.el --- Take video and audio notes with Org-mode -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Yuchen Lea

;; Author: Yuchen Lea <yuchen.lea@gmail.com>
;; URL: https://github.com/yuchen-lea/org-media-note
;; Version: 1.7.0
;; Package-Requires: ((emacs "27.1") (mpv "0.1.0") (pretty-hydra "0.2.2"))

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

;;;; Commands
;;;;; Hydra

;; A quick menu for org-media-note
(pretty-hydra-define org-media-note-hydra
  (:color red
   :title (org-media-note--hydra-title)
   :hint nil)
  ("File"
   (("o" org-media-note-mpv-smart-play
     (if (org-media-note-ref-cite-p)
         (format "Open %s"
                 (org-media-note--current-org-ref-key))
       "Open file")
     :width 20)
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
           (format "Clear A-B loop (%s - %s)"
                   (org-media-note--seconds-to-hms time-a)
                   (org-media-note--seconds-to-hms time-b))
         (if (numberp time-a)
             (format "Set B of A-B loop (%s - )"
                     (org-media-note--seconds-to-hms time-a))
           "Set A of A-B loop")))
     :width 35)
    ("g" org-media-note-goto-timestamp "Jump to the timestamp")
    ("<left>" mpv-seek-backward "Back 5s")
    ("<right>" mpv-seek-forward "Forward 5s")
    ("C-<left>"
     (mpv-run-command "sub-seek" -1)
     "Previous subtitle")
    ("C-<right>"
     (mpv-run-command "sub-seek" 1)
     "Next subtitle"))
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
    ("S" org-media-note-insert-screenshot "Insert Screenshot")
    ("s" org-media-note-insert-sub-text "Insert subtitle")
    ("I p" org-media-note-insert-note-from-pbf
     "Import from pbf")
    ("I n" org-media-note-insert-note-from-noted
     "Import from Noted")
    ("I t" org-media-note-convert-from-org-timer
     "Import from org-timer"))
   "Toggle"
   (("t m" org-media-note-mode "Auto insert media item"
     :toggle t)
    ("t c" org-media-note-toggle-refcite "Use ref key instead of absolute path"
     :toggle org-media-note-use-refcite-first)
    ("t p" org-media-note-toggle-pause-after-insertion
     "Pause media after insert link" :toggle org-media-note-pause-after-insert-link)
    ("t s" org-media-note-toggle-save-screenshot
     "Auto save screenshot" :toggle org-media-note-save-screenshot-p)
    ("t S" org-media-note-toggle-screenshot-with-sub
     "Screenshot with subtitles" :toggle org-media-note-screenshot-with-sub)
    ("t t" org-media-note-toggle-timestamp-pattern
     (format "Timestamp format: %s"
             (cond
              ((eq org-media-note-timestamp-pattern 'hms)
               "hh:mm:ss")
              ((eq org-media-note-timestamp-pattern 'hmsf)
               "hh:mm:ss.fff")))))))


(defun org-media-note--hydra-title ()
  "Return title string for `org-media-note-hydra'."
  (let ((file-path (mpv-get-property "path"))
        (ref-key (org-media-note--current-org-ref-key))
        (icon (if (featurep 'all-the-icons)
                  (all-the-icons-material "ondemand_video")
                ""))
        speed
        current-timestamp
        total-timestamp
        remaining-hms
        bib-entry
        title)
    (if file-path
        ;; Title when mpv is playing media
        (progn
          (setq speed (mpv-get-property "speed"))
          (setq volume (mpv-get-property "volume"))
          (setq current-timestamp (org-media-note--get-current-timestamp))
          (setq total-timestamp (org-media-note--get-duration-timestamp))
          (setq remaining-hms (org-media-note--seconds-to-timestamp (mpv-get-property "playtime-remaining")))
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
                        (progn
                          (setq bib-entry (bibtex-completion-get-entry ref-key))
                          (setq title (bibtex-completion-get-value "title" bib-entry))
                          (format "%s (%s)" title ref-key))
                      (if (org-media-note--online-video-p file-path)
                          (format "%s (%s)"
                                  (mpv-get-property "media-title")
                                  file-path)
                        file-path))))
      ;; Title when no media is playing
      (concat icon " org-media-note"))))
;;;;; Customize Org link
(org-link-set-parameters "video"
                         :follow 'org-media-note-media-link-follow)

(org-link-set-parameters "audio"
                         :follow 'org-media-note-media-link-follow)

;;;;; Toggle

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

(defun org-media-note-toggle-timestamp-pattern ()
  "Toggle screenshot with sub."
  (interactive)
  (cond
   ((eq org-media-note-timestamp-pattern 'hms)
    (setq org-media-note-timestamp-pattern 'hmsf))
   ((eq org-media-note-timestamp-pattern 'hmsf)
    (setq org-media-note-timestamp-pattern 'hms))))


;;;;; Minor Mode

;;;###autoload
(define-minor-mode org-media-note-mode
  "Minor mode for taking audio and video notes with `org-mode'."
  :init-value t
  :global t
  (if org-media-note-mode
      (progn
        (advice-add 'org-insert-item :before-until #'org-insert-item--media-note-item)
        (when org-media-note-use-org-ref
          (org-media-note-setup-org-ref)))
    (advice-remove 'org-insert-item #'org-insert-item--media-note-item)))

;;;; Footer
(provide 'org-media-note)
;;; org-media-note.el ends here
