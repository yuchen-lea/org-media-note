;;; .local/straight/repos/org-media-note/org-media-note-org-ref.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Requirements
(require 'org-ref)
(require 'org-media-note)

;;;; Customization
;;;; Variables
;;;; Commands
;;;;; Help echo
(defun org-media-note-help-echo (window object position)
  "A help-echo function for ref links."
  (save-excursion
    (goto-char position)
      (let* ((object (org-element-context))
             (media-note-link (if (eq (org-element-type object) 'link)
                                      (org-element-property :path object)
                                                     ))
             (ref-cite-key (car (split-string media-note-link "#")))
             (hms (cdr (split-string media-note-link "#")))
                                  )
        (format "%s @ %s"
                (org-ref-format-entry ref-cite-key) hms)
        )
    ))

;;;;; Keymap
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

(defcustom org-media-note-cite-keymap
  (let ((map (copy-keymap org-mouse-map)))
    (define-key map (kbd "H-o") 'org-media-note-open-ref-cite-function)
    map)
  "Keymap for cite links."
  :type 'symbol
  :group 'org-media-note)

;;;;; Link Follow
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

;;;;; Customize Org link

(org-link-set-parameters "videocite"
                         :follow 'org-media-note-cite-link-follow
                         :keymap org-media-note-cite-keymap
                         :help-echo #'org-media-note-help-echo
                         )

(org-link-set-parameters "audiocite"
                         :follow 'org-media-note-cite-link-follow)

;;;; Footer
(provide 'org-media-note-org-ref)
;;; org-media-note-org-ref.el ends here
