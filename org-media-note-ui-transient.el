;;; org-media-note-ui-transient.el --- Transient interface for org-media-note -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements
(require 'transient)
(require 'org-media-note-core)
(require 'org-media-note-import)

;;;; transient interface
(transient-define-prefix org-media-note-transient ()
  "Main transient for org-media-note."
  :transient-suffix 'transient--do-stay
  [:description org-media-note--ui-title
                ["\nFile"
                 ("o" org-media-note-play-smart
                  :description org-media-note--ui-play-smart-title
                  :transient nil)
                 ("T" "Toggle ontop"
                  (lambda ()
                    (interactive)
                    (mpv-cycle-property "ontop")))
                 ""
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
                 ("m" "(un)mute"
                  (lambda ()
                    (interactive)
                    (mpv-cycle-property "mute")))]
                ["Playback"
                 ("<SPC>" "Play/Pause" mpv-pause)
                 ("tp" org-media-note-toggle-pause-after-insertion
                  :description (lambda ()
                                 (format "Auto Pause [%s]"
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
                                 (format "Seek step: %s"
                                         (org-media-note--ui-seek-step nil))))
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
                 ("tm" org-media-note-toggle-auto-insert-item
                  :description (lambda ()
                                 (format "Auto insert item [%s]"
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
                                 (format "Separator for merge: %s" org-media-note-separator-when-merge)))
                 ""]
                ["Note Format"
                 ("tt" org-media-note-toggle-timestamp-pattern
                  :description (lambda ()
                                 (format "timestamp: %s"
                                         (cond
                                          ((eq org-media-note-timestamp-pattern 'hms) "hh:mm:ss")
                                          ((eq org-media-note-timestamp-pattern 'hmsf) "hh:mm:ss.fff")))))
                 ("tc" org-media-note-toggle-refcite
                  :description (lambda ()
                                 (format "citekey instead of path [%s]"
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
                                 (format "Auto insert screenshot [%s]"
                                         (org-media-note--ui-toggle-state 'org-media-note-save-screenshot-p))))
                 ("tS" org-media-note-toggle-screenshot-with-sub
                  :description (lambda ()
                                 (format "Screenshot with sub [%s]"
                                         (org-media-note--ui-toggle-state 'org-media-note-screenshot-with-sub))))
                 ("tl" org-media-note-set-ab-loop-capture-method
                  :description (lambda ()
                                 (format "AB-loop clip: %s"
                                         (if org-media-note-capture-ab-loop-ask-each-time
                                             "always ask" org-media-note-default-capture-ab-loop-function-name))))]])

(transient-define-prefix org-media-note-import-transient ()
  "Transient for org-media-note import commands."
  ["Import from\n"
   [("s" "Subtitle" org-media-note-insert-note-from-subtitle)
    ("c" "Chapter" org-media-note-insert-note-from-chapter-list)]
   [("t" "Org-timer item" org-media-note-convert-from-org-timer)
    ("p" "PBF (Potplayer)" org-media-note-insert-note-from-pbf)
    ("n" "Noted" org-media-note-insert-note-from-noted)]])

(provide 'org-media-note-ui-transient)

;;; org-media-note-ui-transient.el ends here
