;;; voca-builder.el --- Helps you build up your vocabulary
;;
;; Copyright (C) 2011-2014 Steve Purcell
;;
;; Author: Yi Tang <yi.tang.uk@me.com>
;; Keywords: English vocabulary 
;; Created: 28th March 2015
;; Package-Requires: (org popup)
;; X-URL: http://github.com/yitang/voca-builder
;; URL: http://github.com/yitang/voca-builder
;; Version: 0.0.20150328
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; voca-builder is an Emacs package that aimed to help you build up your vocabulary by automating the most step in the process, so that you actually spent time in learning new words.
;; It will do the checking, and shows the meaning as a popup above the text. It also records the meaning and the sentence containing the word.  Finally, it can export vocabularies that have the same tags, or are between two date. 
;;; Use:
;;
;; To use voca-builder 
;;   (setq voca-builder/voca-file "~/.vocabulary.org")
;;   (setq voca-builder/export-file "~/.voca-builder-temp.org")
;;   (global-set-key (kbd "<f4>") 'voca-builder/search-popup) 
;;
;; To export all the vocabulary tagged by TLOTR 
;;   (voca-builder/extract-by-tags "TLOTR") , get all the vocabularies of The Lord of Rings.
;; To export all the vocabulary recored between 2015-01-01 and 2015-03-01
;;   (voca-builder/extract-period "2015-01-01" "2015-03-01")
;;
;;; Code:

;; requires

(require 'org)
(require 'popup)

(defgroup voca-builder nil
  "Group voca-builder entries according to their version control status."
  :prefix "voca-builder-"
  :group 'convenience)

(defcustom voca-builder/record-voca nil
  "If non-nil, record the vocabulary looked-into and save to voca-builder/voca-file."
  :type 'boolean
  :group 'voca-builder)

(defcustom voca-builder/voca-file "~/.vocabulary.org"
  "the vobulary file"
  :type 'file
  :group 'voca-builder)


(defcustom voca-builder/record-with-ts t
  "if non-nil, record the vocabulary with a timestamp."
  :type 'boolean
  :group 'voca-builder)

;; (defcustom voca-builder/ts-format nil
;;   "the timestsamp format for the records"
;;   :type 'string
;;   :group 'voca-builder)
(setq voca-builder/ts-format "[%Y-%m-%d %a %H:%M]") 

(defun voca-builder/record-vocabulary-p ()
  (if voca-builder/record-voca
      t
    f))

(defcustom voca-builder/popup-record-sentence nil
  "If non-nil, record the sentence which contain the word that was looked up."
  :type 'boolean
  :group 'voca-builder)
(setq voca-builder/popup-record-sentence t)

(defcustom voca-builder/popup-show-short-meaning nil
  "if non-nil, shows the short expnation of the vocabulary"
  :type 'boolean
  :group 'voca-builder)
(setq voca-builder/popup-short t)

(defcustom voca-builder/current-tag nil
  "if non-nil, add tags to the vocabulary node in org-mode"
  :type 'string
  :group 'voca-builder)
(setq voca-builder/current-tag "Gene") ;; 



;;;; config 
(setq voca-builder/popup-line-width 40)

;;;; section: html helper function 
(defun voca-builder/html-find-content-of-tags-by-points (s-tag e-tag)
  (interactive)
  (search-forward s-tag)
  (setq p1 (point))
  (search-forward e-tag)
  (backward-char (length e-tag))
  (setq p2 (point))
  (cons p1 p2)
  )

(defun voca-builder/make-url (voca)
  (concat "http://www.vocabulary.com/dictionary/" voca))

(defun voca-builder/fetch-meaning (voca)
  (setq url (voca-builder/make-url voca))
  (setq meaning (with-current-buffer
		    (url-retrieve-synchronously url)
		  (setq pts (voca-builder/html-find-content-of-tags-by-points "<p class=\"short\">" "</p>"))
		  (setq short-exp (buffer-substring (car pts) (cdr pts)))
		  (setq pts (voca-builder/html-find-content-of-tags-by-points "<p class=\"long\">" "</p>"))
		  (setq long-exp (buffer-substring (car pts) (cdr pts)))
		  (cons (decode-coding-string short-exp 'utf-8)
			(decode-coding-string long-exp 'utf-8))
		  
		  ))
  meaning 
  )

(defun voca-builder/voca-org-entry (voca exp extra)
  (setq ts (if voca-builder/record-with-ts
	       (format-time-string voca-builder/ts-format)))
  (setq tags (if (not (eq nil voca-builder/current-tag))
		 (concat ":" voca-builder/current-tag ":")))
  (concat "\n* " voca " " tags "\n" ts "\n\n" exp "\n\n" extra))

(defun voca-builder/record-voca (voca meaning extra)
  "reocrd the meaning of a vocabulary"
  (cond ((voca-builder/record-vocabulary-p)
	 (setq a-meaning (concat (car meaning) "\n\n" (cdr meaning)))
  	 (setq org-entry (voca-builder/voca-org-entry voca a-meaning extra))
  	 (message "%s" org-entry)
  	 (append-to-file org-entry nil voca-builder/voca-file)))
  )

(defun voca-builder/popup-clean-text (para)
  "remove html tags beofre display in emacs"
  (with-temp-buffer
    (insert para)
    (goto-char (point-min))
    (replace-string "<i>" "")
    (goto-char (point-min))
    (replace-string "</i>" "")
    (buffer-string))
  )

(defun voca-builder/search-popup ()
  (interactive)
  (setq voca (thing-at-point 'word))
  (if voca-builder/popup-record-sentence
      (setq sentence (thing-at-point 'sentence)))
  (setq meaning (voca-builder/fetch-meaning voca))
  (voca-builder/record-voca voca meaning sentence)
  (if voca-builder/popup-short
      (setq meaning (car meaning)))
  ;;  (message "%s" meaning)
  (setq b-meaning (voca-builder/popup-clean-text meaning))
  (popup-tip b-meaning :width voca-builder/popup-line-width)
  )



;;;; section: export 
(defun org-write-subtree ()
  "append current subtree to the voca-builder/export-file"
  (org-copy-subtree)
  (setq str (with-temp-buffer
	      (org-paste-subtree)
	      (buffer-string)))
  (append-to-file str nil voca-builder/export-file))

(defun voca-builder/extract-by-tags (tags)
  (setq res (org-map-entries 'org-write-subtree tags (list voca-builder/voca-file)))
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
  (setq date1-s (split-string date1 "-"))
  (encode-time 0 0 0
	       (string-to-number (nth 2 date1-s))
	       (string-to-number (nth 1 date1-s))
	       (string-to-number (nth 0 date1-s))))

(defun voca-builder/extract-by-periods-helper ()
  (setq ts-sub-tree (org-get-ts-for-subtree))
  (setq p1 (time-less-p ts-sub-tree time2-internal))
  (setq p2 (time-less-p time1-internal ts-sub-tree))
  (if (and p1 p2)
      (org-write-subtree)))

(defun voca-builder/extract-period (p1 p2)
  "extract all vocabulary entries that are recorered between period p1 and p2.
period: YYYY-MM-DD, for exmaple, 2015-12-01"
  (interactive)
  (setq time1-internal (voca-builder/encode-date p1))
  (setq time2-internal (voca-builder/encode-date p2))
  (org-map-entries 'voca-builder/extract-by-periods-helper nil (list voca-builder/voca-file))
  )

(provide 'voca-builder)
;;;; voca-builder.el ends her


;;;; section: test
;; (setq voca-builder/voca-file "~/git/Learning/Emacs_Voca/voca_example.org") 
;; (setq voca-builder/export-file "~/voca-builder-temp.org") 
;; (voca-builder/extract-by-tags "Gene");; test for example, get all the vocabularies of The Lord of Rings.
;; (global-set-key (kbd "<f4>") 'voca-builder/search-popup)
