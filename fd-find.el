(defvar fd-find-command "fd")
(defvar fd-command-history nil)

(defun fd-find-files (base-dir query-string &optional sort-by-size)
  "Find files in BASE-DIR that match QUERY-STRING using fd.
Returns a list of file paths.
If SORT-BY-SIZE is non-nil, sort the files by size (in bytes)."
  (let* ((default-directory base-dir)
         (fd-command (concat fd-find-command " --no-ignore --absolute-path -0 " query-string))
         (sort-command (when sort-by-size
                         " | xargs -0 du --bytes | sort -nr | awk '{printf \"%s\\0\", $2}'")))
    (split-string
     (shell-command-to-string
      (concat fd-command sort-command)) "\0" t)))

(defun fd-filter-files (files match-string)
  "Filter FILES to include only those that contain MATCH-STRING.
Returns a list of matching file paths."
  (cl-remove-if-not
   (lambda (file)
     (string-match-p match-string file))
   files))

(defun fd-find (&optional sort-by-size)
  "Find files using fd and display them in a buffer.
If SORT-BY-SIZE is non-nil, sort the files by size (in bytes)."
  (interactive "P")
  (let* ((base-dir default-directory)
         (query-string (read-string "Query: " nil 'fd-command-history))
         (match-string
          (read-string "Filter (regexp): " nil 'fd-command-history))
         (files (fd-find-files base-dir query-string sort-by-size)))
    (let ((filtered-files
           (when match-string
             (fd-filter-files files match-string))))
      (with-current-buffer (get-buffer-create "*fd-find*")
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert (format "Find results for '%s' in '%s'.\n\n" query-string base-dir))
        (insert (format "%d matching files found.\n" (length files)))
        (when filtered-files
          (insert (format "Filtering with '%s'.\n" match-string))
          (setq files filtered-files)
         (insert (format "%d filtered files found.\n" (length files))))
		 (insert "-----\n")
       (dolist (file files)
		  (let ((file-info (file-attributes file)))
         (insert (format "%s %s\n" (nth 7 file-info) file)))
		  )
		 (goto-char (point-min))
       (goto-char (re-search-forward "-----" nil t))
       (while (re-search-forward "\\(^[[:digit:]]+ \\)\\(/.*\\)$" nil t)
        (make-text-button (match-beginning 2) (match-end 2)
         'action 'fd-find-open-file
         'follow-link t
         'path (match-string-no-properties 2)))
		 (setq buffer-read-only t))

     (pop-to-buffer "*fd-find*"))))

(defun fd-find-open-file (button)
  "Open the file corresponding to BUTTON."
  (when-let ((path (button-get button 'path)))
    (find-file path)))

(provide 'fd-find)
