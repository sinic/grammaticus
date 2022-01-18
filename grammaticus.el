;;; grammaticus.el --- Latin grammar helper -*- lexical-binding: t; -*-
;; Copyright (C) 2022 Simon Nicolussi

;; Author: Simon Nicolussi <sinic@sinic.name>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: wp
;; Homepage: https://github.com/sinic/grammaticus

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This Emacs package helps with composing text in the Latin language
;; by displaying information from the built-in database of the excellent
;; latin-macronizer by Johan Winge.

;;; Code:
(defconst grammaticus--buffer (generate-new-buffer " *Grammaticus DB*")
  "Buffer that will contains the database of words.")
(defvar grammaticus--index (make-hash-table :test #'equal :size 282939)
  "Hash table that will map from words to file positions.")

(define-minor-mode grammaticus-mode
  "Automatically look up information for the Latin word at point."
  :lighter " Grammaticus"
  (if grammaticus-mode
      (add-hook 'post-command-hook #'grammaticus-lookup t t)
    (remove-hook 'post-command-hook #'grammaticus-lookup t)))

(defun grammaticus-lookup (&optional word)
  "Look up grammatical information to a WORD and display it."
  (interactive (list (read-string "Look up word: " (current-word))))
  (setq word (ucs-normalize-NFKD-string (downcase (or word (current-word) ""))))
  (let ((all (list (replace-regexp-in-string "\\(qu\\|[nv]\\)e\\'" "" word)))
        (message-log-max))
    (unless (string= (car all) word) (push word all))  ; with -que/-ne/-ve
    (message "%s" (string-join (mapcan #'grammaticus--get all) "\n"))))

;;;###autoload
(defun grammaticus-add-words (path)
  "Add the words from file at PATH to the database."
  (interactive "fAdd new words from file: ")
  (with-current-buffer grammaticus--buffer
    (message "Adding new words from %s to the database..."
             (progn (goto-char (point-max))
                    (car (insert-file-contents-literally path))))
    ;; Putting all the fields into the hash table is too slow, so cheat a bit:
    ;; Just parse keys for now, store the position after as the value, and keep
    ;; the buffer around. Only parse the remaining fields at lookup-time.
    (while (not (eobp)) (let ((key (grammaticus--next-field)))
                          (unless (eq (aref key 0) ?#)  ; comment
                            (push (point) (gethash key grammaticus--index)))
                          (forward-line))))
  (message "Database now has %d words" (hash-table-size grammaticus--index)))

(defun grammaticus--get (word)
  "Return list of strings with information pertaining to WORD."
  (mapcar (apply-partially #'grammaticus--at word '(772))
          (gethash (grammaticus--to-ASCII word) grammaticus--index)))

(defun grammaticus--at (exact marks index)
  "Return string with information at INDEX, with EXACT highlighted.

Ignore case and diacritics when determining matches, except for MARKS."
  (with-current-buffer grammaticus--buffer
    (goto-char index)
    (let* ((tag (grammaticus--interpret-tag (grammaticus--next-field)))
           (lemma (grammaticus--next-field))
           (canon (grammaticus--to-UCS (grammaticus--next-field)))
           (hit (string= exact (downcase (grammaticus--to-ASCII canon marks)))))
      (format "%s:%s (%s)" (propertize canon 'face (if hit 'bold)) tag lemma))))

(defun grammaticus--next-field ()
  "Return the field at point and proceed to the next."
  (prog1 (buffer-substring (point) (re-search-forward "[[:graph:]]+"))
    (forward-char)))  ; \t or \n

(defun grammaticus--interpret-tag (tag)
  "Return a human-readable interpretation of the nine character TAG."
  (let ((features  ; from table 2.1 on page 8 of winge2015.pdf
         '(((?a . " adjective,") (?c . " conjunction,") (?d . " adverb,")
            (?e . " exclamation,") (?i . " interjection,") (?m . " numeral,")
            (?n . " noun,") (?p . " pronoun,") (?r . " preposition,")
            (?u . " punctuation,") (?v . " verb,"))
           ((?1 . " 1st person") (?2 . " 2nd person") (?3 . " 3rd person"))
           ((?s . " singular") (?p . " plural"))
           ((?p . " present") (?i . " imperfect") (?f . " future")
            (?r . " perfect") (?l . " pluperfect") (?t . " future perfect"))
           ((?i . " indicative") (?s . " subjunctive") (?m . " imperative")
            (?n . " infinitive") (?p . " participle") (?d . " gerund")
            (?g . " gerundive") (?s . " supine"))
           ((?a . " active") (?p . " passive"))
           ((?m . " male") (?f . " female") (?n . " neuter"))
           ((?n . " nominative") (?v . " vocative") (?a . " accusative")
            (?g . " genitive") (?d . " dative") (?b . " ablative")
            (?l . " locative"))
           ((?c . " comparative") (?s . " superlative")))))
    (mapconcat (lambda (c) (alist-get c (pop features))) tag "")))

(defun grammaticus--to-ASCII (string &optional except)
  "Remove non-ASCII characters from STRING, except for EXCEPT."
  (concat (seq-filter (lambda (c) (or (< c 128) (member c except))) string)))

(defun grammaticus--to-UCS (string)
  "Replace ASCII punctuation in STRING by UCS diacritics."
  (dolist (pair '((?_ . 772) (?^ . 774) (?+ . 776)))
    (subst-char-in-string (car pair) (cdr pair) string t))
  (let ((hidden (string 772 774)))  ; hidden quantity (_^)
    (ucs-normalize-NFKD-string (replace-regexp-in-string hidden "" string))))

(provide 'grammaticus)
;;; grammaticus.el ends here
