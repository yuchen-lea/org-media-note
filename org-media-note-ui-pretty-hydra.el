;;; org-media-note-ui-pretty-hydra.el --- pretty-hydra interface for org-media-note -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements
(require 'pretty-hydra)
(require 'org-media-note-core)
(require 'org-media-note-import)

;;;; pretty-hydra interface
(pretty-hydra-define org-media-note-pretty-hydra
  (:color red
   :title (org-media-note--ui-title)
   :hint nil)
  ("File"
   (("o" org-media-note-play-smart
     (org-media-note--ui-play-smart-title)
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
     (org-media-note--ui-ab-loop-title)
     :width 31)
    ("g" org-media-note-goto-timestamp "Jump to timestamp")
    ("<left>" (org-media-note-seek 'backward) (format "Backward %s" (org-media-note--ui-seek-step t)))
    ("<right>" (org-media-note-seek 'forward) (format "Forward %s" (org-media-note--ui-seek-step t)))
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
   (("t m" org-media-note-toggle-auto-insert-item
     "Auto insert media item" :toggle org-media-note-auto-insert-item)
    ("t s" org-media-note-toggle-save-screenshot
     "Auto insert screenshot" :toggle org-media-note-save-screenshot-p)
    ("t S" org-media-note-toggle-screenshot-with-sub
     "Screenshot with sub" :toggle org-media-note-screenshot-with-sub)
    ("t l" org-media-note-set-ab-loop-capture-method
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
     (format "Seek step: %s" (org-media-note--ui-seek-step t))))))

(provide 'org-media-note-ui-pretty-hydra)

;;; org-media-note-ui-pretty-hydra.el ends here
