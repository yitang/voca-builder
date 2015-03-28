(defgroup voca-builder nil
  "Group voca-builder entries according to their version control status."
  :prefix "voca-builder-"
  :group 'convenience)

(defcustom voca-builder-record-voca t
  "If non-nil, record the vocabulary looked-into and save to voca-builder-voca-file."
  :type 'boolean
  :group 'voca-builder)

(defcustom voca-builder-voca-file "~/.vocabulary.org"
  :type 'string 
  :group 'voca-builder)

(defun my/record-vocabulary-p ()
  (if (eq voca-builder-record-voca 'true)
      'true
    'false))

;;;; section: html helper function 
(defun my/html-find-content-of-tags-by-points (s-tag e-tag)
  (interactive)
  (search-forward s-tag)
  (setq p1 (point))
  (search-forward e-tag)
  (backward-char (length e-tag))
  (setq p2 (point))
  (cons p1 p2)
  )

(defun my/make-url (voca)
  (concat "http://www.vocabulary.com/dictionary/" voca))


(defun my/fetch-meaning (voca)
  (setq url (my/make-url voca))
  (setq meaning (with-current-buffer
		(url-retrieve-synchronously url)
	      (setq pts (my/html-find-content-of-tags-by-points "<p class=\"short\">" "</p>"))
	      (setq short-exp (buffer-substring (car pts) (cdr pts)))
	      (setq pts (my/html-find-content-of-tags-by-points "<p class=\"long\">" "</p>"))
	      (setq long-exp (buffer-substring (car pts) (cdr pts)))
	      ;; (concat short-exp "\n" long-exp)
	      (cons short-exp long-exp)
	      ))
  ;; (message "%s" meaning)
  meaning 
  )

(defun my/voca-org-entry (voca exp)
  (concat "* " voca "\n\n" exp))

(defun my/record-voca (meaning)
  "reocrd the meaning of a vocabulary"
  (cond ((my/record-vocabulary-p)
	 (setq a-meaning (concat (car meaning) "\n" (cdr meaning)))
  	 (setq org-entry (my/voca-org-entry voca a-meaning))
  	 (message "%s" org-entry)
  	 (append-to-file org-entry nil my/vocabulary-file)))
  )


;; (my/fetch-meaning "apple")

(defun my/popup-clean-text (para)
  "remove html tags beofre display in emacs"
  (with-temp-buffer
    (insert para)
    (goto-char (point-min))
    (replace-string "<i>" "")
    (goto-char (point-min))
    (replace-string "</i>" "")
    (buffer-string))
  )

(defun my/search-popup ()
  (interactive)
  (setq voca-builder-record-voca 'false)
  (setq voca (thing-at-point 'word))
  (setq meaning (my/fetch-meaning voca))
  (my/record-voca meaning)
  (if (eq my/popup-short 'true)
      (setq meaning (car meaning)))

  ;;  (message "%s" meaning)
  (setq b-meaning (my/popup-clean-text meaning))
  (popup-tip b-meaning)
)

(setq my/popup-short 'true)

