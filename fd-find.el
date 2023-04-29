(defcustom fd-find-command "fd"
  "find executable"
  :type 'string)

(defcustom fd-find-arguments "--follow"
  "Additional arguments to fd."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Argument string")
                 (repeat :tag "Argument list" string)))

(defvar fd-command-history nil)

(defun fd-find-files (base-dir query-string &optional sort-by-size no-ignore)
 "Find files in BASE-DIR that match QUERY-STRING using fd.
Returns a list of file paths.
If SORT-BY-SIZE is non-nil, sort the files by size (in bytes)."
  (let* ((default-directory base-dir)
		    (fd-command (concat fd-find-command " --absolute-path --type f -0 " query-string))
          (no-ignore-command (when no-ignore " --no-ignore "))
		    (sort-command (when sort-by-size
							     " | xargs -0 du --bytes | sort -nr | awk '{printf \"%s\\0\", $2}'")))

    (split-string
	   (shell-command-to-string
	     (concat fd-command no-ignore-command sort-command)) "\0" t)))

(defun fd-filter-files (files match-string)
 "Filter FILES to include only those that contain MATCH-STRING.
Returns a list of matching file paths."
 (cl-remove-if-not
  (lambda (file)
	(string-match-p match-string file))
  files))

(defun fd-find-complete (&optional initial)
 "Find files using fd and display them in the completion system."
  (interactive)
  (let* ((dir default-directory)
		    (file-list (fd-find-files dir "" t (if current-prefix-arg t nil)))
          (metadata '((category . file)))
          (file (completing-read (format "Find file in %s: " (abbreviate-file-name dir))
                  (lambda (str pred action)
                    (if (eq action 'metadata)
                      `(metadata . ,metadata)
                      (complete-with-action action file-list str pred)))
                  nil t initial 'file-name-history)))
    (when file (find-file (expand-file-name file dir)))))

(defun fd-find (query-string match-string &optional sort-by-size)
 "Find files using fd and display them in a buffer.
If SORT-BY-SIZE is non-nil, sort the files by size (in bytes)."
 (interactive
  (list
	(read-string "Query: " nil 'fd-command-history)
	(read-string "Filter (regexp): " nil 'fd-command-history)
	(setq sort-by-size current-prefix-arg)
	))
 (let* ((base-dir default-directory)
		  (files (fd-find-files base-dir query-string sort-by-size t)))
  (let ((filtered-files
			(when (not (string-equal match-string ""))
			 (fd-filter-files files match-string))))
	;; (kill-buffer "*fd-find*")
	(with-current-buffer (get-buffer-create "*fd-find*")
	 (setq buffer-read-only nil)
	 (erase-buffer)
	 (insert (format "'%s'\n\n" base-dir))
	 (insert "Filter by:\n")
	 (insert "Query: ")
	 (insert-text-button query-string
	  'action 'fd-find-query-string
	  'query-str query-string
	  'match-str match-string
	  'sort sort-by-size)
	 ;; (insert " | Match: ")
	 (insert " | Sort: ")
	 (insert-text-button (if sort-by-size "true" "false")
	  'action 'fd-find-size-toggle
	  'query-str query-string
	  'match-str match-string
	  'sort sort-by-size)
	 (insert (format "\n%d matching files found.\n" (length files)))
	 (insert "\n")

	 (if (not (string-equal match-string ""))
	  (progn
		(insert "Filtering with ")
		(insert-text-button match-string
		 'action 'fd-find-match-string
		 'query-str query-string
		 'match-str match-string
		 'sort sort-by-size)
		(setq files filtered-files)
		(insert (format "\n%d filtered files found." (length files))))
	 (progn
	  (insert "Filter ")
	  (insert-text-button "off"
		'action 'fd-find-match-string
		'query-str query-string
		'match-str match-string
		'sort sort-by-size)
	  ))

	 (insert "\n-----\n")
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
	 (goto-char (point-min))
	 (setq buffer-read-only t))

	(pop-to-buffer "*fd-find*"))))

(defun fd-find-query-string (button)
 ""
 (fd-find
  (read-string "Query: " (button-get button 'query-str) 'fd-command-history)
  (button-get button 'match-str)
  (button-get button 'sort)
  )
 )

(defun fd-find-match-string (button)
 ""
 (fd-find
  (button-get button 'query-str)
  (read-string "Match: " (button-get button 'match-str) 'fd-command-history)
  (button-get button 'sort)
  )
 )

(defun fd-find-size-toggle (button)
 ""
	(fd-find
	 (button-get button 'query-str)
	 (button-get button 'match-str)
	 (if (button-get button 'sort) nil t)
	 )
 )

(defun fd-find-open-file (button)
 "Open the file corresponding to BUTTON."
 (when-let ((path (button-get button 'path)))
  (find-file path)))

(provide 'fd-find)
