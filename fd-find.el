(defcustom fd-find-command "fd"
  ;; (defcustom fd-find-command "rg"
  ;; (defcustom fd-find-command "find"
  "find executable"
  :type 'string)

(defcustom fd-find-arguments "--absolute-path --type f -0"
  ;; (defcustom fd-find-arguments "--files --null"
  ;; (defcustom fd-find-arguments "-type f -printf \"$PWD/%p\\0\""
  "Additional arguments to fd."
  :type '(choice (const :tag "None" nil)
           (string :tag "Argument string")
           (repeat :tag "Argument list" string)))

(defvar fd-command-history nil)

(defun fd-find-files (base-dir query-string &optional sort no-ignore)
  "Find files in BASE-DIR that match QUERY-STRING using fd.
Returns a list of file paths.
If SORT-BY-SIZE is non-nil, sort the files by size (in bytes)."
  (let* ((fd-command (concat fd-find-command " " fd-find-arguments query-string))
          (no-ignore-command (if (not (string-equal fd-find-command "find")) (when no-ignore "--no-ignore")))
		    (sort-command (cond
                          ((string-equal sort "size")
							       " | xargs -0 du --bytes | sort -nr | awk '{printf \"%s\\0\", $2}'")
                          ((string-equal sort "name")
							       " | xargs -0 ls | sort | awk '{printf \"%s\\0\", $1}'"))))

    (print (concat fd-command " " no-ignore-command " . " base-dir " " sort-command))

    (mapcar (lambda (path)
              (file-relative-name path base-dir))
      (split-string
	     (shell-command-to-string (concat fd-command " " no-ignore-command " . " base-dir " " sort-command))
        "\0" t)
      )
    )
  )

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
		    (file-list (fd-find-files dir nil (if current-prefix-arg "size" "name")))
          (metadata '((category . file)))
          (file (completing-read (format "Find file in %s: " (abbreviate-file-name dir))
                  file-list)))
    (when file (find-file (expand-file-name file dir)))))

(defun fd-find (base-dir query-string match-string &optional sort)
  "Find files using fd and display them in a buffer.
If SORT-BY-SIZE is non-nil, sort the files by size (in bytes)."
  (interactive
    (list
      (setq base-dir default-directory)
	   (read-string "Query: " nil 'fd-command-history)
	   (read-string "Filter (regexp): " nil 'fd-command-history)
	   (setq sort (if current-prefix-arg "size" "name"))
	   ))
  (setq default-directory base-dir)
  (let* (
          (files (fd-find-files base-dir query-string sort t)))
    (let ((filtered-files
			   (when (not (string-equal match-string ""))
			     (fd-filter-files files match-string))))
	   ;; (kill-buffer "*fd-find*")
	   (with-current-buffer (get-buffer-create "*fd-find*")
	     (setq buffer-read-only nil)
	     (erase-buffer)
        (insert "Dir : ")
        (insert-text-button base-dir
	       'action 'fd-find-base-dir
	       'base-str base-dir
	       'query-str query-string
	       'match-str match-string
	       'sort-str sort)
        (insert "\n\n")
	     (insert "Filter by:\n")
	     (insert "Query: ")
	     (insert-text-button query-string
	       'action 'fd-find-query-string
          'base-str base-dir
	       'query-str query-string
	       'match-str match-string
	       'sort-str sort)
	     ;; (insert " | Match: ")
	     (insert " | Sort: ")
	     (insert-text-button sort
	       'action 'fd-find-size-toggle
          'base-str base-dir
	       'query-str query-string
	       'match-str match-string
	       'sort-str sort)
	     (insert (format "\n%d matching files found.\n" (length files)))
	     (insert "\n")

	     (if (not (string-equal match-string ""))
	       (progn
		      (insert "Filtering with ")
		      (insert-text-button match-string
		        'action 'fd-find-match-string
              'base-str base-dir
		        'query-str query-string
		        'match-str match-string
		        'sort-str sort)
		      (setq files filtered-files)
		      (insert (format "\n%d filtered files found." (length files))))
	       (progn
	         (insert "Filter ")
	         (insert-text-button "off"
		        'action 'fd-find-match-string
              'base-str base-dir
              'query-str query-string
		        'match-str match-string
		        'sort-str sort)
	         ))

	     (insert "\n-----\n")
	     (dolist (file files)
	       (let ((file-info (file-attributes (expand-file-name file base-dir))))
		      (insert (format "%10s %s\n" (nth 7 file-info) file)))
	       )
	     (goto-char (point-min))
	     (goto-char (re-search-forward "-----" nil t))
	     (while (re-search-forward "^[[:space:]]*[[:digit:]]+[[:space:]]+\\(.*\\)$" nil t)
	       (make-text-button (match-beginning 1) (match-end 1)
		      'action 'fd-find-open-file
		      'follow-link t
		      'base-str (concat base-dir "/" (match-string 1))
            ))
	     (goto-char (point-min))
	     (setq buffer-read-only t))

	   (pop-to-buffer "*fd-find*"))))

(defun fd-find-query-string (button)
  ""
  (fd-find
    (button-get button 'base-str)
    (read-string "Query: " (button-get button 'query-str) 'fd-command-history)
    (button-get button 'match-str)
    (button-get button 'sort-str)))

(defun fd-find-match-string (button)
  ""
  (fd-find
    (button-get button 'base-str)
    (button-get button 'query-str)
    (read-string "Match: " (button-get button 'match-str) 'fd-command-history)
    (button-get button 'sort-str)))

(defun fd-find-size-toggle (button)
  ""
  (fd-find
    (button-get button 'base-str)
	 (button-get button 'query-str)
	 (button-get button 'match-str)
    (read-string "Match: " (button-get button 'sort-str) 'fd-command-history)))

(defun fd-find-base-dir (button)
  ""
  (fd-find
    (read-string "Dir: " (button-get button 'base-str) 'fd-command-history)
	 (button-get button 'query-str)
	 (button-get button 'match-str)
    (button-get button 'sort-str)))

(defun fd-find-open-file (button)
  "Open the file corresponding to BUTTON."
  (when-let ((path (button-get button 'base-str)))
    (find-file path)))

(provide 'fd-find)
