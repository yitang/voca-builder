(defun voca-builder/html-find-tag (tag &optional begining)
  "search the tag and return the point, 
if end is non-nil, return the point at the begining of tag"
  (search-forward tag)
  (if begining
      ((backward-char (length tag))
       (point))
    (point)))

(defun voca-builder/html-find-tag-2 (tag1 tag2)
  (cons (voca-builder/html-find-tag tag1)
	(voca-builder/html-find-tag tag2 t)))
				    
(defun voca-builder/html-find-content-of-tag (tag1 tag2)
  (let ((pts (voca-builder/html-find-tag-2 tag1 tag2)))
    (buffer-substring (car pts) (cdr pts))))

(defun voca-builder/fetch-meaning-new (voca)
  (let ((short-meaning (voca-builder/html-find-content-of-tag "<p class=\"short\">" "</p>"))
	(long-meaning (voca-builder/html-find-content-of-tags "<p class=\"long\">" "</p>")))
    (cons (decode-coding-string short-meaning 'utf-8)
	  (decode-coding-string long-meaning 'utf-8))))

(defun voca-builder/voca-org-entry (voca exp extra)
  (let ((ts (if voca-builder/record-with-ts
		(format-time-string voca-builder/ts-format)))
	(tag (if voca-builder/current-tag
		 (concat ":" voca-builder/current-tag ":"))))
    (concat "\n* " voca " " tags "\n" ts "\n\n" exp "\n\n" extra)))


(defun voca-builder/record-voca (voca meaning extra)
  "reocrd the meaning of a vocabulary"
  (cond ((voca-builder/record-vocabulary-p)
	 (setq a-meaning (concat (car meaning) "\n\n" (cdr meaning)))
  	 (setq org-entry (voca-builder/voca-org-entry voca a-meaning extra))
  	 (message "%s" org-entry)
  	 (append-to-file org-entry nil voca-builder/voca-file)))
  )

(defun voca-builder/record-voca (voca meaning extra)
  (cond ((voca-builder/record-vocabulary-p)
	 (let ((string-meaning (concat
				(car meaning)
				"\n\n"
				(cdr meaning)))
	       (org-entry (voca-builder/voca-org-entry voca string-meaning extra)))
	   (append-to-file org-entry nil voca-builder/voca-file))
	 )
	)
  )


(defun voca-builder/search-popup ()
  (interactive)
  (let ((this-voca (thing-at-point 'word))
	(this-sentence (if voca-builder/popup-record-sentence
			   (thing-at-point 'sentence)))
	(meaning (voca-builder/fetch-meaning (this-voca)))
	(meaning-after-clean (voca-builder/popup-clean-text meaning)))
    (if voca-builder/record-vocabulary-p
	(voca-builder/record-voca this-voca
				  meaning-after-clean
				  sentence))
    (if voca-builder/popup-short
	(popup-tip (car meaning)
		   :width voca-builder/popup-line-width)
      (popup-tip (mapconcat 'identity
			    meaning-after-clean
			    "\n")
		 :width voca-builder/popup-line-width))
    )
  )


;;;; section: export 
(defun org-write-subtree ()
  "append current subtree to the voca-builder/export-file"
  (org-copy-subtree)
  (let ((str (with-temp-buffer
	       (org-paste-subtree)
	       (buffer-string)))
	(append-to-file str nil voca-builder/export-file))
    )
  )

(defun voca-builder/extract-by-tags (tags)
  (interactive)
  "export all vocabulary records with tags"
  (org-map-entries 'org-write-subtree tags (list voca-builder/voca-file))
  )


(defun voca-builder/encode-date (date1)
  "encode date
date: YYYY-MM-DD, for exmaple, 2015-12-01"
  (let ((date1-s (split-string date1 "-")))
    (encode-time 0 0 0
		 (string-to-number (nth 2 date1-s))
		 (string-to-number (nth 1 date1-s))
		 (string-to-number (nth 0 date1-s)))
    )
  )

(defun voca-builder/extract-by-periods-helper ()
  (let ((ts-sub-tree (org-get-ts-for-subtree))
	(p1 (time-less-p ts-sub-tree time2-internal))
	(p2 (time-less-p time1-internal ts-sub-tree)))
    (if (and p1 p2)
	(org-write-subtree))
    )
  )


(defun voca-builder/extract-period (p1 p2)
  "extract all vocabulary entries that are recorered between period p1 and p2.
period: YYYY-MM-DD, for exmaple, 2015-12-01"
  (interactive)
  (let ((time1-internal (voca-builder/encode-date p1))
	(time2-internal (voca-builder/encode-date p2)))
    (org-map-entries 'voca-builder/extract-by-periods-helper nil (list voca-builder/voca-file)))
  )


(provide 'voca-builder)

