;;; org-media-note-obsidian.el --- Export org-media-note to Obsidian -*- lexical-binding: t; -*-

;;; Commentary:
;; Export org-media-note headlines to Obsidian vault with Media Extended format

;;; Code:

(require 'org-media-note-core)
(require 'ox-md)

(declare-function org-media-note-cite--file-path "org-media-note-cite" (key))

;;;; Customization

(defcustom org-media-note-obsidian-vault-root nil
  "Root directory of the Obsidian vault."
  :type 'directory)

(defcustom org-media-note-obsidian-copy-media t
  "Whether to copy media files to vault or reference external files.
When t, copy media files to vault.
When nil, reference external files (not recommended for Obsidian)."
  :type 'boolean)

(defcustom org-media-note-obsidian-md-path-template "{series}/{cite_key}_{title}.md"
  "Template for generating markdown file paths within vault.
Available variables:
- {series}: SERIES property value
- {cite_key}: citation key (Custom_ID)  
- {title}: headline title (sanitized)
- {ext}: media file extension"
  :type 'string)

(defcustom org-media-note-obsidian-overwrite 'ask
  "How to handle existing markdown files.
- 'always: Always overwrite existing files
- 'ask: Ask user whether to overwrite
- 'never: Never overwrite, skip existing files"
  :type '(choice
          (const :tag "Always overwrite" always)
          (const :tag "Ask user" ask)))

(defcustom org-media-note-obsidian-exclude-properties 
  '("CUSTOM_ID" "FILE" "ITEM" "BLOCKED")
  "List of org properties to exclude from frontmatter.
Property names are compared case-insensitively."
  :type '(repeat string))

;;;; Helper Functions

(defun org-media-note-obsidian--sanitize-filename (name)
  "Sanitize NAME for use as filename by removing/replacing illegal characters."
  (let ((sanitized (replace-regexp-in-string "[/\\:*?\"<>|]" "_" name)))
    (replace-regexp-in-string "[ \t]+" "_" sanitized)))

(defun org-media-note-obsidian--expand-path-template (template vars)
  "Expand TEMPLATE string using VARS alist.
VARS should be an alist of (variable-name . value) pairs."
  (let ((result template))
    (dolist (var vars)
      (let ((placeholder (format "{%s}" (car var)))
            (value (cdr var)))
        (when value
          (setq result (replace-regexp-in-string 
                       (regexp-quote placeholder) 
                       (org-media-note-obsidian--sanitize-filename value) 
                       result)))))
    result))

(defun org-media-note-obsidian--get-headline-info ()
  "Extract information from current headline.
Returns a plist with :title, :properties, :tags, :cite-key, :content."
  (save-excursion
    (org-back-to-heading t)
    (let* ((element (org-element-at-point))
           (title (org-element-property :title element))
           (tags (org-get-tags))
           (properties (org-entry-properties))
           (cite-key (cdr (assoc (upcase org-media-note-ref-key-field) properties)))
           (content-start (save-excursion
                           (org-end-of-meta-data t)
                           (point)))
           (content-end (save-excursion
                         (org-end-of-subtree t)
                         (point)))
           (content (buffer-substring-no-properties content-start content-end)))
      (list :title (org-element-interpret-data title)
            :properties properties
            :tags tags
            :cite-key cite-key
            :content content))))

(defun org-media-note-obsidian--get-media-file-info (cite-key)
  "Get media file information for CITE-KEY.
Returns a plist with :path, :filename, :extension."
  (when cite-key
    (let ((media-path (if (fboundp 'org-media-note-cite--file-path)
                         (org-media-note-cite--file-path cite-key)
                       nil)))
      (when (and media-path (file-exists-p media-path))
        (list :path media-path
              :filename (file-name-nondirectory media-path)
              :extension (file-name-extension media-path))))))

(defun org-media-note-obsidian--generate-frontmatter (properties tags)
  "Generate YAML frontmatter from PROPERTIES and TAGS."
  (let ((frontmatter '("---")))
    ;; Add tags
    (when tags
      (push "tags:" frontmatter)
      (dolist (tag tags)
        (push (format "  - %s" tag) frontmatter)))
    
    ;; Add properties (excluding those in exclude list)
    (dolist (prop properties)
      (unless (member (car prop)
                      (mapcar #'upcase org-media-note-obsidian-exclude-properties))
        (let ((key (car prop))
              (value (cdr prop)))
          (push (format "%s: \"%s\"" key value) frontmatter))))
    
    (push "---" frontmatter)
    (push "" frontmatter)
    (string-join (nreverse frontmatter) "\n")))

(defun org-media-note-obsidian--convert-org-to-md (content media-filename)
  "Convert org-mode CONTENT to markdown, replacing media links.
MEDIA-FILENAME is the target media file name in the vault."
  ;; First convert to markdown using ox-md, but disable citations processing
  (let* ((org-cite-activate-processor nil)
         (org-cite-export-processors nil)
         (org-export-with-toc nil)
         (org-md-export-with-properties nil)
         (md-content (org-export-string-as content 'md t)))
    
    ;; Then replace the converted markdown links with Obsidian format
    (with-temp-buffer
      (insert md-content)
      (goto-char (point-min))
      (while (re-search-forward 
              "\\[\\([^]]+\\)\\](\\(audiocite\\|videocite\\):[^#]+#\\([^)]+\\))"
              nil t)
        (let* ((match-start (match-beginning 0))
               (match-end (match-end 0))
               (label (buffer-substring (match-beginning 1) (match-end 1)))
               (timestamp (buffer-substring (match-beginning 3) (match-end 3)))
               (obsidian-link (org-media-note-obsidian--convert-timestamp-link 
                              timestamp media-filename))
               (replacement (format "[%s](%s)" label obsidian-link)))
          (goto-char match-start)
          (delete-region match-start match-end)
          (insert replacement)))
      
      (buffer-string))))

(defun org-media-note-obsidian--convert-timestamp-link (timestamp media-filename)
  "Convert org-media-note timestamp link to Obsidian format.
TIMESTAMP: e.g., '0:01:29-0:01:31' or '00:01:25'
MEDIA-FILENAME: target media file name"
  (let* ((time-parts (split-string timestamp "-"))
         (start-time (car time-parts))
         (end-time (when (> (length time-parts) 1) (cadr time-parts)))
         (start-seconds (org-media-note--timestamp-to-seconds start-time))
         (end-seconds (when end-time (org-media-note--timestamp-to-seconds end-time))))
    
    (if end-time
        ;; AB-loop format: media.ext#t=start,end&loop
        (format "%s#t=%s,%s&loop" 
                media-filename start-seconds end-seconds)
      ;; Single timestamp: media.ext#t=start
      (format "%s#t=%s" 
              media-filename start-seconds))))


(defun org-media-note-obsidian--copy-media-file (source-path target-dir target-filename)
  "Copy media file from SOURCE-PATH to TARGET-DIR with TARGET-FILENAME.
Create target directory if it doesn't exist."
  (let ((target-path (expand-file-name target-filename target-dir)))
    (unless (file-directory-p target-dir)
      (make-directory target-dir t))
    (copy-file source-path target-path t)
    target-path))

(defun org-media-note-obsidian--should-overwrite-file (file-path)
  "Check if FILE-PATH should be overwritten based on user settings.
Returns t if should proceed, nil otherwise."
  (cond
   ((not (file-exists-p file-path)) t)
   ((eq org-media-note-obsidian-overwrite 'always) t)
   ((eq org-media-note-obsidian-overwrite 'ask)
    (y-or-n-p (format "File %s already exists. Overwrite? " file-path)))
   (t nil)))

;;;; Main Export Function

;;;###autoload
(defun org-media-note-export-to-obsidian ()
  "Export current org-media-note headline to Obsidian vault."
  (interactive)
  (unless org-media-note-obsidian-vault-root
    (error "Please set org-media-note-obsidian-vault-root first"))
  
  (unless (file-directory-p org-media-note-obsidian-vault-root)
    (error "Vault root directory does not exist: %s" org-media-note-obsidian-vault-root))
  
  (let* ((headline-info (org-media-note-obsidian--get-headline-info))
         (title (plist-get headline-info :title))
         (properties (plist-get headline-info :properties))
         (tags (plist-get headline-info :tags))
         (cite-key (plist-get headline-info :cite-key))
         (content (plist-get headline-info :content)))
    
    ;; TODO only check when contained in ...
    (unless cite-key
      (error "No citation key found. Please set %s property" org-media-note-ref-key-field))
    
    (let* ((media-info (org-media-note-obsidian--get-media-file-info cite-key))
           (media-path (plist-get media-info :path))
           (media-filename (plist-get media-info :filename))
           (media-extension (plist-get media-info :extension))
           (series (cdr (assoc "SERIES" properties))))
      
      (unless media-path
        (error "Cannot find media file for citation key: %s" cite-key))
      
      ;; Generate target paths
      (let* ((path-vars `(("series" . ,series)
                         ("cite_key" . ,cite-key)
                         ("title" . ,title)
                         ("ext" . ,media-extension)))
             (md-relative-path (org-media-note-obsidian--expand-path-template 
                               org-media-note-obsidian-md-path-template path-vars))
             (md-full-path (expand-file-name md-relative-path org-media-note-obsidian-vault-root))
             (md-dir (file-name-directory md-full-path))
             (target-media-filename (if org-media-note-obsidian-copy-media
                                       (format "%s.%s" cite-key media-extension)
                                     media-filename)))
        
        ;; Check if should proceed with overwriting
        (if (org-media-note-obsidian--should-overwrite-file md-full-path)
            (progn
              ;; Copy media file if needed
              (when org-media-note-obsidian-copy-media
                (org-media-note-obsidian--copy-media-file media-path md-dir target-media-filename)
                (message "Copied media file: %s" target-media-filename))
              
              ;; Generate markdown content
              (let* (
		     (frontmatter (org-media-note-obsidian--generate-frontmatter properties tags))
                     (md-content (org-media-note-obsidian--convert-org-to-md content target-media-filename))
                     (full-content (concat frontmatter "\n# " title "\n\n" md-content)))
                
                ;; Write markdown file
                (unless (file-directory-p md-dir)
                  (make-directory md-dir t))
                
                (with-temp-file md-full-path
                  (insert full-content))
                
                (message "Exported to: %s" md-full-path)))
          (message "Export cancelled - file already exists: %s" md-full-path))))))

;;;; Footer
(provide 'org-media-note-obsidian)
;;; org-media-note-obsidian.el ends here
