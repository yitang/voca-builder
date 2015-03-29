(require 'org)
(require 'popup)

(defgroup voca-builder nil
  "Group voca-builder entries according to their version control status."
  :prefix "voca-builder-"
  :group 'convenience)

(defcustom voca-builder/voca-file "~/.vocabulary.org"
  "the file to store vocabularies" 
  :type 'file
  :group 'voca-builder)

(defcustom voca-builder/record-new-vocabulary t
  "If non-nil, record the new vocabulary that is checked and save the notes to voca-builder/voca-file."
  :type 'boolean
  :group 'voca-builder)

(defcustom voca-builder/record-with-ts t
  "if non-nil, record the vocabulary with a timestamp.

The timestamps are needed for export function"
  :type 'boolean
  :group 'voca-builder)

;; (defcustom voca-builder/ts-format "[%Y-%m-%d %a %H:%M]"
;;   "The timestsamp format for the records
;; Donot change in this version."
;;   :type 'string
;;   :group 'voca-builder)
      
(defcustom voca-builder/popup-record-sentence t
  "If non-nil, record the sentence which contain the word that was looked into."
  :type 'boolean
  :group 'voca-builder)

(defcustom voca-builder/popup-show-short-meaning t
  "if non-nil, shows the short explnation of the vocabulary"
  :type 'boolean
  :group 'voca-builder)

(defcustom voca-builder/current-tag "Gene"
  "if non-nil, add tags to the vocabulary notes in org-mode"
  :type 'string
  :group 'voca-builder)

(setq voca-builder/popup-line-width 40)

;;;; section: functions 
(defun voca-builder/make-url (voca)
  (concat "http://www.vocabulary.com/dictionary/" voca))

(defun voca-builder/html-find-tag (tag &optional begining)
  "search for a html tag and return the point, 
if begining is non-nil, return the point at the begining of the tag, instead of at the end"
  (search-forward tag)
  (cond (begining
	 (- (point) (length tag)))
	(t 
	 (point))))

(defun voca-builder/html-remove-emphasis-tags (a-string)
  "remove html tags beofre display in emacs"
  (with-temp-buffer
    (insert a-string)
    (goto-char (point-min))
    (replace-string "<i>" "")
    (goto-char (point-min))
    (replace-string "</i>" "")
    (buffer-string)
    ))

(defun voca-builder/html-find-content-of-tags (tag1 tag2)
  "It searchs the content that is wrapped by tag1 and tag2 in a HTML file, and return as a UTF-8 stirng. "
  (let* ((p-tag1 (voca-builder/html-find-tag tag1))
	(p-tag2 (voca-builder/html-find-tag tag2 t))
	(content (buffer-substring p-tag1 p-tag2))
	(content-without-emphasis (voca-builder/html-remove-emphasis-tags content)))
    (decode-coding-string content-without-emphasis 'utf-8)
    )
  )


(defun voca-builder/fetch-meaning (voca)
  ;; "Parse the html content from www.vocabulary.com, and return the short and long meaning "
  (interactive)
  (with-current-buffer
      (url-retrieve-synchronously (voca-builder/make-url voca))
    (let ((short-meaning (voca-builder/html-find-content-of-tags "<p class=\"short\">"
								"</p>"))
	  (long-meaning (voca-builder/html-find-content-of-tags "<p class=\"long\">"
								"</p>")))
      (cons short-meaning
	    long-meaning)))
  )

(defun voca-builder/voca-org-entry (voca exp extra)
  (let ((ts (if voca-builder/record-with-ts
		;; (format-time-string voca-builder/ts-format)))
		(format-time-string "[%Y-%m-%d %a %H:%M]")))
	(tag (if voca-builder/current-tag
		 (concat ":" voca-builder/current-tag ":"))))
    (concat "\n* " voca " " tag "\n" ts "\n\n" exp "\n\n" extra)))

;; (defun voca-builder/record-voca (voca meaning extra)
;;   "reocrd the meaning of a vocabulary"
;;   (cond ((eq t voca-builder/record-new-vocabulary))
;; 	 (setq a-meaning (concat (car meaning) "\n\n" (cdr meaning)))
;;   	 (setq org-entry (voca-builder/voca-org-entry voca a-meaning extra))
;;   	 (message "%s" org-entry)
;;   	 (append-to-file org-entry nil voca-builder/voca-file)))
  
(defun voca-builder/record-voca (voca meaning extra)
  (cond (voca-builder/record-new-vocabulary
	 (let* ((string-meaning (concat
				(car meaning)
				"\n\n"
				(cdr meaning)))
	       (org-entry (voca-builder/voca-org-entry voca string-meaning extra)))
	   (append-to-file org-entry nil voca-builder/voca-file))
	 )
	))


(defun voca-builder/search-popup ()
  (interactive)
  (let* ((this-voca (thing-at-point 'word))
	(this-sentence (if voca-builder/popup-record-sentence
			   (thing-at-point 'sentence)))
	(meaning (voca-builder/fetch-meaning this-voca)))
    (if voca-builder/record-new-vocabulary
	(voca-builder/record-voca this-voca
				  meaning
				  this-sentence))
    (if voca-builder/popup-show-short-meaning
	(popup-tip (car meaning)
		   :width voca-builder/popup-line-width)
      (popup-tip (mapconcat 'identity
			    meaning
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
	       (buffer-string))))
    (append-to-file str nil voca-builder/export-file))
  )
 

(defun voca-builder/extract-by-tags (tags)
  (interactive)
  "export all vocabulary records with tags"
  (org-map-entries 'org-write-subtree tags (list voca-builder/voca-file))
  )


(defun org-get-ts-for-subtree ()
  "search timesamp in the current subtree, for example [2015-03-28 Sat 12:01], and parse it to date"
  (search-forward-regexp "[0-9]+-[0-9]+-[0-9]+")
  (beginning-of-line)
  (forward-char)
  (voca-builder/encode-date (buffer-substring (point) (+ (point) 10)))
  )


(defun voca-builder/encode-date (date1)
  "encode date
date: YYYY-MM-DD, for exmaple, 2015-12-01"
  (let* ((date1-s (split-string date1 "-")))
    (encode-time 0 0 0
		 (string-to-number (nth 2 date1-s))
		 (string-to-number (nth 1 date1-s))
		 (string-to-number (nth 0 date1-s)))
    )
  )

(defun voca-builder/extract-by-periods-helper ()
  (let* ((ts-sub-tree (org-get-ts-for-subtree))
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


(setq voca-builder/voca-file "~/git/Learning/Emacs_Voca/voca_example.org") 
(setq voca-builder/current-tag "Demo")
(global-set-key (kbd "<f4>") 'voca-builder/search-popup)

(setq voca-builder/export-file "~/voca-builder-temp.org") 
(voca-builder/extract-by-tags "Demo") 
(voca-builder/extract-period "2015-01-05" "2015-04-01")
