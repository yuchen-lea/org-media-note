;;; org-media-note.el --- Take video and audio notes with Org-mode -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Yuchen Lea

;; Author: Yuchen Lea <yuchen.lea@gmail.com>
;; URL: https://github.com/yuchen-lea/org-media-note
;; Version: 1.11.0
;; Keywords: note-taking, multimedia, video
;; Package-Requires: ((emacs "25.1") (transient "0.1.0") (mpv "0.2.0"))

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

(require 'org-media-note-core)

(declare-function org-media-note-setup-org-ref "org-media-note-org-ref")

;;;; Commands
;;;;; UI

(defun org-media-note-show-interface ()
  "Show the interface based on `org-media-note-interface`."
  (interactive)
  (cond
   ((eq org-media-note-interface 'pretty-hydra)
    (require 'org-media-note-ui-pretty-hydra)
    (org-media-note-pretty-hydra/body))
   ((eq org-media-note-interface 'transient)
    (require 'org-media-note-ui-transient)
    (org-media-note-transient))
   (t (error "Unsupported interface: %s" org-media-note-interface))))

;;;;; Customize Org link
(defun org-media-note--default-desc-fn (base timestamp desc)
  "Default function to generate link description.
`BASE': base URL, local path or citekey.
`TIMESTAMP': original timestamp string.
`DESC': link description."
  timestamp)

(defun org-media-note-export-link (path desc format)
  "Process the link based on the export format."
  (cond
   ((eq format 'html)
    (let* ((new-url (org-media-note--generate-url-with-timesamp
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
                           :follow #'org-media-note-media-link-follow
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
        (when org-media-note-use-org-ref
          (org-media-note-setup-org-ref)))
    (org-media-note--update-auto-insert-advice nil)
    (advice-remove 'org-beginning-of-line #'org-media-note--beginning-of-line-advice)))

;;;; Footer
(provide 'org-media-note)
;;; org-media-note.el ends here
