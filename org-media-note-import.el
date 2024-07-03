;;; package --- org-media-note-import.el -*- lexical-binding: t; -*-


;;; Commentary:
;;
;;; Code:

(require 'org-media-note-core)


;;;; Customization

(defcustom org-media-note-delete-pbf 'ask
  "Controls the deletion of PBF files.
   'always - Always delete the PBF file without asking.
   'never  - Never delete the PBF file.
   'ask    - Ask whether to delete the PBF file."
  :type '(choice
          (const :tag "Always" always)
          (const :tag "Never" never)
          (const :tag "Ask" ask)))


;;;; utils
(defun org-media-note--import-context (&optional throw-error-on-mismatch)
  "Resolve the source media file or URL and determine media type."
  (cl-multiple-value-bind (_ key file-by-key url-by-key)
      (org-media-note--ref-context)
    (cl-multiple-value-bind (_ media-files-in-attach-dir)
        (org-media-note--attach-context)
      (let* ((file-or-url-from-key (or file-by-key url-by-key))
             (file-from-attach (and (= 1 (length media-files-in-attach-dir))
                                    (car media-files-in-attach-dir)))
             (file-or-url-from-mpv (mpv-get-property "path"))
             (file-without-cite (or file-or-url-from-mpv file-from-attach))
             ignore-mpv-subtitle)
        (cond
         ((and file-or-url-from-mpv
               file-or-url-from-key
               (not (string= file-or-url-from-mpv file-or-url-from-key)))
          (if throw-error-on-mismatch
              (error "The currently playing file does not match the file associated with the key.")
            (if (equal file-or-url-from-mpv (org-media-note--select "The currently playing file does not match the file associated with the key. Choose which subtitle to insert: "
                                                                    (list file-or-url-from-mpv file-or-url-from-key)))
                (setq file-or-url-from-key nil)
              (setq ignore-mpv-subtitle t)))
          )
         ((and file-or-url-from-mpv
               file-from-attach
               (not (string= file-or-url-from-mpv file-from-attach)))
          (if throw-error-on-mismatch
              (error "The currently playing file does not match the file associated with the attachment.")
            (if (equal file-or-url-from-mpv (org-media-note--select "The currently playing file does not match the file associated with the attachment. Choose which subtitle to insert: "
                                                                    (list file-or-url-from-mpv file-from-attach)))
                nil
              (setq file-without-cite file-from-attach
                    ignore-mpv-subtitle t))
            )
          ))
        (let* ((source-media (if file-or-url-from-key key file-without-cite))
               (source-media-path (or file-by-key
                                      file-without-cite
                                      (read-file-name "Find media file:")))
               (media-type (org-media-note--file-media-type source-media-path))
               (media-link-type (if file-or-url-from-key
                                    (format "%scite" media-type)
                                  media-type)))
          (list source-media source-media-path
                media-link-type ignore-mpv-subtitle))))))

;;;; import from pbf (potplayer bookmark):
(defun org-media-note-insert-note-from-pbf ()
  "Insert note from PBF file."
  (interactive)
  (let ((key (org-media-note--current-org-ref-key))
        pbf-file
        media-link-type
        media-file)
    (if (org-media-note-ref-cite-p)
        (progn
          (setq source-media (org-media-note-get-media-file-by-key key))
          (setq media-link-type (format "%scite"
                                        (org-media-note--file-media-type source-media)))
          (setq media-file key)
          (setq pbf-file (concat (file-name-sans-extension source-media)
                                 ".pbf")))
      (progn
        ;; TODO need more test
        (setq media-file (read-file-name "Find media file:"))
        (setq media-link-type (org-media-note--file-media-type media-file))
        (setq pbf-file (concat (file-name-sans-extension media-file)
                               ".pbf"))))
    (message pbf-file)
    (if (not (file-exists-p pbf-file))
        (setq pbf-file (read-file-name "Find pbf file:")))
    (insert (org-media-note--convert-from-pbf pbf-file
                                              media-link-type media-file))
    (cond
     ((eq org-media-note-delete-pbf 'always)
      (delete-file pbf-file))
     ((eq org-media-note-delete-pbf 'ask)
      (if (y-or-n-p "Delete the PBF File? ")
          (delete-file pbf-file)))
     ;; For 'never, do nothing.
     )))

(defun org-media-note--convert-from-pbf (pbf-file media-link-type media-file)
  "Return link for MEDIA-FILE of MEDIA-LINK-TYPE from PBF-FILE."
  (with-temp-buffer
    (insert-file-contents pbf-file)
    (replace-string "[Bookmark]\n"
                    ""
                    nil
                    (point-min)
                    (point-max))
    (replace-regexp "^[[:digit:]]+=$"
                    ""
                    nil
                    (point-min)
                    (point-max))
    (goto-char (point-min))
    (while (re-search-forward "^[[:digit:]]+=\\([[:digit:]]+\\)\\*\\([^\\*]+\\)\\*.+"
                              nil t)
      (let* ((millisecs (buffer-substring (match-beginning 1)
                                          (match-end 1)))
             (note (buffer-substring (match-beginning 2)
                                     (match-end 2)))
             (beg (match-beginning 0))
             (end (match-end 0))
             (hms (org-media-note--millisecs-to-timestamp millisecs))
             (new-text (format "- [[%s:%s#%s][%s]] %s" media-link-type
                               media-file hms hms note)))
        (goto-char beg)
        (delete-region beg end)
        (insert new-text)))
    (buffer-string)))

;;;; import from subtitle: srt, vtt, ass
(defun org-media-note-insert-note-from-subtitle ()
  "Insert note from subtitle.
Currently supports SRT and VTT."
  (interactive)
  (cl-multiple-value-bind (source-media source-media-path
                                        media-link-type ignore-mpv-subtitle)
      (org-media-note--import-context)
    (insert (org-media-note--convert-from-subtitle (org-media-note--selected-subtitle-content
                                                    source-media-path ignore-mpv-subtitle)
                                                   (org-media-note--select "Select timestamp format: "
                                                                           '("time1" "time1-time2"))
                                                   media-link-type
                                                   source-media))))

(defun org-media-note--convert-from-subtitle (subtitle-content timestamp-format media-link-type media-file &optional is-ass)
  "Return note of MEDIA-FILE from SUBTITLE-CONTENT.
In format of TIMESTAMP-FORMAT and MEDIA-LINK-TYPE.
Currently supports SRT, VTT, and ASS."
  (with-temp-buffer
    (insert subtitle-content)
    (setq is-ass (string-match-p "\\[V4\\+ Styles\\]" subtitle-content))
    ;; Local function to search for timestamp pattern
    (cl-flet ((search-timestamp ()
                (re-search-forward (if is-ass
                                       (concat "Dialogue: [[:digit:]]+\\," org-media-note--hmsf-timestamp-pattern
                                               "\\," org-media-note--hmsf-timestamp-pattern
                                               "\\,.*?,.*?,.*?,.*?,.*?,.*?," "\\(.*\\)")
                                     (concat "\\(?:[[:digit:]]+\\)?\n?" org-media-note--hmsf-timestamp-pattern
                                             " --> " org-media-note--hmsf-timestamp-pattern
                                             "\\(?:.*\n\\)?\\(.*\\(?:\n.+\\)*\\)"))
                                   nil
                                   t)))
      (goto-char (point-min))
      ;; Delete any content before the first timestamp
      (when (search-timestamp)
        (delete-region (point-min)
                       (match-beginning 0)))
      ;; Reset to the beginning of the buffer to start processing
      (goto-char (point-min))
      ;; Process each subtitle block
      (while (search-timestamp)
        (let ((time-a (match-string 1))
              (time-b (match-string 3))
              (note-block (match-string 5))
              (beg (match-beginning 0))
              (end (match-end 0)))
          ;; Check if note-block contains vtt time-synchronized text formatting
          (if (string-match-p (concat org-media-note--hmsf-timestamp-pattern
                                      "><c>")
                              note-block)
              (delete-region beg end) ; Delete the region to avoid dupliccate
            ;; Else, process the block as usual
            ;; Adjust time format
            (cond
             ((eq org-media-note-timestamp-pattern 'hms)
              (setq time-a (car (split-string time-a "[,\\.]")))
              (setq time-b (car (split-string time-b "[,\\.]"))))
             ((eq org-media-note-timestamp-pattern 'hmsf)
              (setq time-a (s-replace-regexp "," "." time-a))
              (setq time-b (s-replace-regexp "," "." time-b))))
            (let* ((timestamp (cond
                               ((string= timestamp-format "time1") time-a)
                               ((string= timestamp-format "time1-time2")
                                (format "%s-%s" time-a time-b))))
                   (note-lines (split-string note-block "\n"))
                   (formatted-note (mapconcat (lambda (line)
                                                (let ((new-line (if is-ass
                                                                    (replace-regexp-in-string "{.*?}" "" line)
                                                                  line)))
                                                  (if (eq line (car note-lines))
                                                      new-line
                                                    (concat "  " new-line))))
                                              note-lines
                                              "\n"))
                   (new-text (format "- [[%s:%s#%s][%s]] %s" media-link-type
                                     media-file timestamp timestamp formatted-note)))
              ;; Replace old text with new text
              (goto-char beg)
              (delete-region beg end)
              (insert new-text)
              (insert "\n")))))
      ;; Return the buffer content, ensuring no empty lines
      (goto-char (point-min))
      (while (re-search-forward "^[\s-]*\n" nil t)
        (replace-match ""))
      (buffer-string))))

(defun org-media-note--is-subtitle-track (item)
  "Check if ITEM is a subtitle track.
ITEM is a cons cell from mpv track-list.
Supports SRT, ASS format currently."
  (let ((type (cdr (assoc 'type item)))
        (codec (cdr (assoc 'codec item)))
        (title (cdr (assoc 'title item))))
    (and (equal type "sub")
         (or (member codec '("srt" "ass" "subrip"))
             (member title '("srt" "ass"))
             (string-suffix-p ".srt" title)
             (string-suffix-p ".ass" title)))))

(defun org-media-note--selected-subtitle-content (file-from-context ignore-mpv-subtitle)
  "Get the content of the selected subtitle.
The source of the subtitle counld be:
- Attempts to match a subtitle file based on FILE-FROM-CONTEXT.
- Subtitle in the MPV player, if available and not IGNORE-MPV-SUBTITLE.
- If neither of the above available, prompts the user to select."
  (let ((subtitle-prompt "Select a subtitle: ")
        (track-list (mpv-get-property "track-list"))
        (file-from-context-base-name (if (org-media-note--online-video-p file-from-context)
                                         (replace-regexp-in-string "[\]\[\/:：【】\|\\]"
                                                                   " "
                                                                   (mpv-get-property "media-title"))
                                       (file-name-base file-from-context)))
        (file-from-context-dir (if (not (org-media-note--online-video-p file-from-context))
                                   (file-name-directory file-from-context)))
        srt-local-file
        srt-online-content)
    (if (or ignore-mpv-subtitle
            (not track-list))
        ;; No subtitle track is playing in MPV
        (setq srt-local-file (or (car (directory-files file-from-context-dir
                                                       t
                                                       (concat (regexp-quote file-from-context-base-name)
                                                               "\\.srt$")))
                                 (read-file-name subtitle-prompt file-from-context-dir
                                                 nil nil file-from-context-base-name)))
      ;; subtitle track playing in MPV
      (let* ((srt-tracks (seq-filter #'org-media-note--is-subtitle-track
                                     track-list))
             (manual-select-srt "Select Sub File")
             (choices (append (mapcar (lambda (item)
                                        (let ((name (or (cdr (assoc 'lang item))
                                                        (cdr (assoc 'title item)))))
                                          name))
                                      srt-tracks)
                              (list manual-select-srt)))
             (selected-lang (org-media-note--select subtitle-prompt choices))
             selected-track)
        (if (equal selected-lang manual-select-srt)
            (setq srt-local-file (read-file-name subtitle-prompt
                                                 (or file-from-context-dir org-media-note-mpv-webstream-download-path)
                                                 nil nil file-from-context-base-name))
          (progn
            (setq selected-track (seq-find (lambda (item)
                                             (equal (or (cdr (assoc 'lang item))
                                                        (cdr (assoc 'title item))) selected-lang))
                                           srt-tracks))
            (when selected-track
              (let ((external-filename (cdr (assoc 'external-filename selected-track))))
                (if (file-exists-p external-filename)
                    (setq srt-local-file external-filename)
                  (setq srt-online-content external-filename))))))))
    (if srt-online-content
        (let* ((srt-lines (split-string srt-online-content "\n"))
               (processed-srt-lines (cons "1" (cdr srt-lines))))
          (mapconcat 'identity processed-srt-lines "\n"))
      (with-temp-buffer
        (insert-file-contents srt-local-file)
        (buffer-string)))))

;;;; import from noted:
(defun org-media-note-insert-note-from-noted ()
  "Insert note from noted txt."
  (interactive)
  (let ((key (org-media-note--current-org-ref-key))
        noted-txt
        media-link-type
        media-file)
    (setq noted-txt (read-file-name "Find exported Noted txt:"))
    (if (org-media-note-ref-cite-p)
        (progn
          (setq media-file key)
          (setq media-link-type (concat (org-media-note--file-media-type (org-media-note-get-media-file-by-key key))
                                        "cite")))
      (progn
        ;; TODO  need more test
        (setq media-file (read-file-name "Find media file:"))
        (setq media-link-type (org-media-note--file-media-type media-file))))
    (insert (org-media-note--convert-from-noted noted-txt
                                                media-link-type media-file))
    (if (y-or-n-p "Delete Noted txt? ")
        (delete-file noted-txt))))

(defun org-media-note--convert-from-noted (noted-file media-link-type media-file)
  "Return converted link for MEDIA-FILE of MEDIA-LINK-TYPE from NOTED-FILE."
  (with-temp-buffer
    (insert-file-contents noted-file)
    (replace-string "￼"
                    ""
                    nil
                    (point-min)
                    (point-max))
    ;; replace unordered list
    (replace-regexp "[•◦▫]"
                    "-"
                    nil
                    (point-min)
                    (point-max))
    (goto-char (point-min))
    (while (re-search-forward "^\\[\\([:0-9]+\\)\\]\\([[:blank:]]+\\)-[[:blank:]]+"
                              nil t)
      (let* ((hms (buffer-substring (match-beginning 1)
                                    (match-end 1)))
             (blank-indent (buffer-substring (match-beginning 2)
                                             (match-end 2))))
        (replace-match (concat blank-indent "- [[" media-link-type
                               ":" media-file "#" hms "][" hms "]] ")
                       t)))
    ;; replace ordered list

    (goto-char (point-min))
    (while (re-search-forward "^\\[\\([:0-9]+\\)\\]\\([[:blank:]]+\\)\\([[:digit:]]\\. \\)"
                              nil t)
      (let* ((hms (buffer-substring (match-beginning 1)
                                    (match-end 1)))
             (blank-indent (buffer-substring (match-beginning 2)
                                             (match-end 2)))
             (number-bullet (buffer-substring (match-beginning 3)
                                              (match-end 3))))
        (replace-match (concat blank-indent number-bullet "[[" media-link-type
                               ":" media-file "#" hms "][" hms "]] ")
                       t)))
    ;; replace timestamped text

    (goto-char (point-min))
    (while (re-search-forward "^\\[\\([:0-9]+\\)\\]\\([[:blank:]]+\\)"
                              nil t)
      (let* ((hms (buffer-substring (match-beginning 1)
                                    (match-end 1)))
             (blank-indent (buffer-substring (match-beginning 2)
                                             (match-end 2))))
        (replace-match (concat blank-indent "- [[" media-link-type
                               ":" media-file "#" hms "][" hms "]] ")
                       t)))
    ;; format

    (replace-regexp "\\]\\] +"
                    "]] "
                    nil
                    (point-min)
                    (point-max))
    (buffer-string)))

;;;; import org-timer:
(defun org-media-note-convert-from-org-timer ()
  "Convert `org-timer' to media link."
  (interactive)
  (let* (key source-media media-link-type media-file)
    (if (org-media-note-ref-cite-p)
        (progn
          (setq key (org-media-note--current-org-ref-key))
          (setq source-media (org-media-note-get-media-file-by-key key))
          (setq media-file key)
          (setq media-link-type (format "%scite" (org-media-note--file-media-type source-media))))
      (progn
        (setq source-media (or
                            (mpv-get-property "path")
                            (read-file-name "Find media file:")))
        (setq media-file (org-media-note--format-file-path source-media))
        (setq media-link-type (org-media-note--file-media-type source-media))))
    (save-excursion
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (while (re-search-forward (concat org-media-note--hms-timestamp-pattern "::")
                                nil t)
        (let* ((hms (buffer-substring (match-beginning 1)
                                      (match-end 1))))
          (replace-match (format "[[%s:%s#%s][%s]] " media-link-type
                                 media-file hms hms)
                         'fixedcase)))
      (widen))))


;;;; import chapter list:
(defun org-media-note-insert-note-from-chapter-list ()
  "Insert note from chapter list."
  (interactive)
  (cl-multiple-value-bind (source-media _ media-link-type _)
      (org-media-note--import-context t)
    (let* ((chapter-list (mpv-get-property "chapter-list"))
           (chapter-note (cl-map 'list
                                 (lambda (chapter)
                                   (let* ((title (cdr (assoc 'title chapter)))
                                          (time (cdr (assoc 'time chapter)))
                                          (timestamp (org-media-note--seconds-to-timestamp time)))
                                     (format "- [[%s:%s#%s][%s]] %s" media-link-type
                                             source-media timestamp timestamp title)))
                                 chapter-list)))
      (insert (mapconcat 'identity chapter-note "\n")))))

;;;; Footer
(provide 'org-media-note-import)

;;; org-media-note-import.el ends here
