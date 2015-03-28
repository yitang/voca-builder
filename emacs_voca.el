;;; voca-builder.el --- Group ibuffer's list by VC project, or show VC status
;;
;; Copyright (C) 2011-2014 Steve Purcell
;;
;; Author: Yi Tang <yi.tang.uk@me.com>
;; Keywords: English vocabulary 
;; Package-Requires: ((cl-lib "0.2"))
;; X-URL: http://github.com/yi-tang/voca-builder
;; URL: http://github.com/yi-tang/voca-builder
;; Version: DEV
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
;; Vocabulary builder in Emacs, it can do Lookup english dictionary for a word, shows the meaning as a poopup manue, save the searched word and meaning into a file as a node in org-mode.;;
;;; Use:
;;
;; To group buffers by vc parent dir:
;;
;;   M-x ibuffer-vc-set-filter-groups-by-vc-root
;;
;; or, make this the default:
;;
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
(setq voca-builder/record-voca t)


(defcustom voca-builder/voca-file "~/.vocabulary.org"
  "the vobulary file"
  :type 'file
  :group 'voca-builder)

(defcustom voca-builder/record-with-ts t
  "if non-nil, record the vocabulary with a timestamp."
  :type 'boolean
  :group 'voca-builder)
(setq voca-builder/record-with-ts t)

(defcustom voca-builder/ts-format nil
  "the timestsamp format for the records"
  :type 'string
  :group 'voca-builder)
(setq voca-builder/ts-format "[%Y-%m-%d %a %H:%M]")

(defun voca-builder/record-vocabulary-p ()
  (if voca-builder/record-voca
      t
    f))

(defcustom voca-builder/popup-record-sentence nil
  "If non-nil, record the sentence which contain the word we are looking into."
  :type 'boolean
  :group 'voca-builder)
(setq voca-builder/popup-record-sentence t)

(defcustom voca-builder/popup-show-short-meaning nil
  "if non-nil, shows the short meaning of the vocabulary"
  :type 'boolean
  :group 'voca-builder)
(setq voca-builder/popup-short t)

(defcustom voca-builder/current-tag nil
  "if non-nil, add tags to the vocabulary node in org-mode"
  :type 'string
  :group 'voca-builder)
(setq voca-builder/current-tag nil)

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
		  (cons short-exp long-exp)
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


(global-set-key (kbd "<f5>") 'voca-builder/search-popup)


(provide 'voca-builder)

;;;; voca-builder.el ends her









(defun org-write-subtree ()
  "return current subtree"
  (org-copy-subtree)
  (setq str (with-temp-buffer
	      (org-paste-subtree)
	      (buffer-string)))
  (append-to-file str nil voca-builder/temp-file))
(setq voca-builder/temp-file "~/voca-builder-temp.org")
(defun voca-builder/extract-by-tags (tags)
  (setq res (org-map-entries 'org-write-subtree tags '("/home/yitang/.vocabulary.org")))
  )
(voca-builder/extract-by-tags "TLOR")



;;;;;;; period
(defun org-get-ts-for-subtree ()
  (search-forward-regexp "[0-9]+-[0-9]+-[0-9]+")
  (beginning-of-line)
  (forward-char)
  (voca-builder/encode-date (buffer-substring (point) (+ (point) 10)))
  )

(defun voca-builder/encode-date (date1)
  "date: YYYY-MM-DD, for exmaple, 2015-12-01"
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

(defun voca-builder/extract-by-periods (p1 p2)
  "period: YYYY-MM-DD, for exmaple, 2015-12-01"
  (setq time1-internal (voca-builder/encode-date p1))
  (setq time2-internal (voca-builder/encode-date p2))
  (org-map-entries 'voca-builder/extract-by-periods-helper nil '("/home/yitang/.vocabulary.org"))
  )


;; (defun voca-builder/extract-by-periods-wrapper (time1 time2)
;;   (setq time1-internal (voca-builder/encode-date time1))
;;   (setq time2-internal (voca-builder/encode-date time2))
;;   (voca-builder/extract-by-periods (time1-internal time2-internal)))

;; (format-time-string "%Y%M" (voca-builder/encode-date "2015-03-27"))

;; (setq time1 (voca-builder/encode-date "2015-03-01"))
;; (setq time2 (voca-builder/encode-date "2015-03-26"))

;; (voca-builder/extract-by-periods t1 t2)




;; (voca-builder/test "2015-03-01" "2015-03-25")

;; (org-map-entries 'org-get-ts-for-subtree nil '("/home/yitang/.vocabulary.org"))



;; (search-forward-regexp "[0-9]+-[0-9]+-[0-9]+")
;; 2015
;; 2015-12
;; 2015-12-03 
