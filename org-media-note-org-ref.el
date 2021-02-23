;;; org-media-note-org-ref.el --- Integrate org-media-note with org-ref -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Yuchen Lea

;; Author: Yuchen Lea <yuchen.lea@gmail.com>
;; URL: https://github.com/yuchen-lea/org-media-note
;; Package-Requires: ((emacs "27.1") (org-ref "1.1.1"))


;;; Commentary:

;;; Code:
;;;; Requirements
(require 'org-ref)
(require 'org-media-note)

;;;; Customization
(defcustom org-media-note-bibtex-files bibtex-files
  "List of BibTeX files that are searched for entry keys."
  :type '(repeat (choice (const :tag "bibtex-file-path" bibtex-file-path)
                         directory file)))
;;;; Variables
;;;; Commands
;;;;; Utils
(defun org-media-note-get-media-file-by-key (key)
  (let* ((bib-entry (bibtex-completion-get-entry key))
         (file-types (split-string (bibtex-completion-get-value "formats" bib-entry)
                                   ", "))
         (file-path (car (bibtex-completion-find-pdf key)))
         (file-path-without-ext (file-name-sans-extension file-path))
         (video-file-ext-candidates (seq-intersection file-types org-media-note--video-types))
         (audio-file-ext-candidates (seq-intersection file-types org-media-note--audio-types))
         (file-type (cond
                     (video-file-ext-candidates (nth 0 video-file-ext-candidates))
                     (audio-file-ext-candidates (nth 0 audio-file-ext-candidates))
                     (t nil))))
    (if file-type
        (org-media-note--get-realpath-for-file (concat file-path-without-ext "." file-type))
      nil)))

(defun org-media-note--get-realpath-for-file (symlink)
  "Get realpath for symlink."
  (replace-regexp-in-string "\n"
                            ""
                            (shell-command-to-string (concat "realpath \""
                                                             (replace-regexp-in-string "~"
                                                                                       (file-truename "~")
                                                                                       symlink)
                                                             "\""))))

;;;;; Help echo
(defun org-media-note-help-echo (window object position)
  "A help-echo function for ref links."
  (save-excursion
    (goto-char position)
    (let ((s (org-media-note-link-message)))
      (with-temp-buffer
        (insert s)
        (fill-paragraph)
        (buffer-string)))))

(defun org-media-note-link-message ()
  "Print a minibuffer message about the link that point is on."
  (interactive)
  ;; the way links are recognized in org-element-context counts blank spaces
  ;; after a link and the closing brackets in literal links. We don't try to get
  ;; a message if the cursor is on those, or if it is on a blank line.
  (when (not (or (looking-at " ") ;looking at a space
                 (looking-at "^$") ;looking at a blank line
                 (looking-at "]") ;looking at a bracket at the end
                 (looking-at "$"))) ;looking at the end of the line.
    (save-restriction (widen)
                      (when (eq major-mode 'org-mode)
                        (let* ((object (org-element-context))
                               (type (org-element-property :type object)))
                          (save-excursion
                            (cond
                             ((or (string= type "videocite")
                                  (string= type "audiocite"))
                              (let* ((media-note-link (org-element-property :path object))
                                     (ref-cite-key (car (split-string media-note-link "#")))
                                     (hms (cdr (split-string media-note-link "#"))))
                                (format "%s @ %s"
                                        (org-ref-format-entry ref-cite-key)
                                        hms))))))))))

(defun org-media-note-link-message-display-in-eldoc (&rest _)
  (org-media-note-link-message))

;;;;; Keymap
(defun org-media-note-open-ref-cite-function ()
  (interactive)
  (let* ((object (org-element-context))
         (media-note-link (if (eq (org-element-type object) 'link)
                              (org-element-property :path object)))
         (ref-cite-key (car (split-string media-note-link "#"))))
    (with-temp-buffer
      (org-mode)
      ;; insert bibliography in order to find entry in org-ref
      (insert (s-join "\n"
                      (mapcar (lambda (bib)
                                (format "bibliography:%s" bib))
                              org-media-note-bibtex-files)))
      (insert (format "\ncite:%s" ref-cite-key))
      (funcall org-ref-cite-onclick-function nil))))

(defcustom org-media-note-cite-keymap
  (let ((map (copy-keymap org-mouse-map)))
    (define-key map (kbd "H-o") 'org-media-note-open-ref-cite-function)
    map)
  "Keymap for cite links."
  :type 'symbol
  :group 'org-media-note)

;;;;; Link Follow
(defun org-media-note-cite-link-follow (link)
  "Open videocite and audiocite links, supported formats:
1. videocite:course.104#0:02:13: jump to 0:02:13
2. videocite:course.104#0:02:13-0:02:20: jump to 0:02:13 and loop between 0:02:13 and 0:02:20
"
  (let* ((splitted (split-string link "#"))
         (key (nth 0 splitted))
         (file-path (org-media-note-get-media-file-by-key key))
         (timestamps (split-string (nth 1 splitted)
                                   "-"))
         (time-a (int-to-string (org-timer-hms-to-secs (nth 0 timestamps))))
         (time-b (if (= (length timestamps) 2)
                     (int-to-string (org-timer-hms-to-secs (nth 1 timestamps))))))
    (cond
     ((not file-path)
      (error "Cannot find media file for this Key."))
     (t (org-media-note--follow-link file-path time-a time-b)
        ))))

;;;;; Setup

;;;###autoload
(defun org-media-note-setup-org-ref ()
  (dolist (link '("videocite" "audiocite"))
    (org-link-set-parameters link :follow 'org-media-note-cite-link-follow
                             :keymap org-media-note-cite-keymap
                             :help-echo #'org-media-note-help-echo))

  ;; Display media link description in minibuffer when cursor is over it.
  (advice-add #'org-eldoc-documentation-function
              :before-until #'org-media-note-link-message-display-in-eldoc))
;;;; Footer
(provide 'org-media-note-org-ref)
;;; org-media-note-org-ref.el ends here
