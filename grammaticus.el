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
(defvar grammaticus--db (cons (generate-new-buffer " *Grammaticus DB*")
                              (make-hash-table :test #'equal :size 282939))
  "Database the known words will be loaded to.")

(defcustom grammaticus-use-J t "If non-nil, use letter J for consonantal Is."
  :type 'boolean)
(defcustom grammaticus-use-V t "If non-nil, use letter V for consonantal Us."
  :type 'boolean)
(defcustom grammaticus-diacritics '(?\N{COMBINING MACRON}) "Set of diacritics."
  :type 'list)

(defvar grammaticus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-.] #'grammaticus-correct)
    map)
  "Minor mode keymap for Grammaticus mode.")

(define-minor-mode grammaticus-mode
  "Automatically look up information for the Latin word at point."
  :lighter " Grammaticus" :keymap grammaticus-mode-map
  (if grammaticus-mode
      (add-hook 'post-command-hook #'grammaticus-lookup t t)
    (remove-hook 'post-command-hook #'grammaticus-lookup t)))

(defun grammaticus-lookup (&optional word)
  "Look up grammatical information to a WORD and display it."
  (interactive (list (read-string "Look up word: " (current-word))))
  (grammaticus--show (grammaticus--get grammaticus--db
                                       (or word (current-word)))))

(defun grammaticus-correct ()
  "Replace word at point by next near match."
  (interactive)
  (when-let* ((word (thing-at-point 'word t))
              (at (bounds-of-thing-at-point 'word))
              (all (mapcar #'car (grammaticus--get grammaticus--db word)))
              (by (or (cadr (memq nil (delete-dups all))) (car all))))
    (delete-region (car at) (cdr at))
    (insert (ucs-normalize-NFC-string by))))

;;;###autoload
(defun grammaticus-add-words (path)
  "Add the words from file at PATH to the database."
  (interactive "fAdd new words from file: ")
  (with-current-buffer (car grammaticus--db)
    (message "Adding new words from %s to the database..."
             (progn (goto-char (point-max))
                    (car (insert-file-contents-literally path))))
    ;; Putting all the fields into the hash table is too slow, so cheat a bit:
    ;; Just parse keys for now, store the position after as the value, and keep
    ;; the buffer around. Only parse the remaining fields at lookup-time.
    (while (not (eobp)) (let ((key (grammaticus--next-field)))
                          (unless (eq (aref key 0) ?#)  ; comment
                            (push (point) (gethash key (cdr grammaticus--db))))
                          (forward-line))))
  (message "Database now has %d words" (hash-table-size (cdr grammaticus--db))))

(defun grammaticus--highlight ()
  "Check word at point and underline it if it's unknown."
  (let ((result (grammaticus--get grammaticus--db (thing-at-point 'word t))))
    (when-let* ((at (bounds-of-thing-at-point 'word)))
      (remove-overlays (car at) (cdr at) 'grammaticus-overlay t)
      (when (seq-every-p #'car result)
        (let ((overlay (make-overlay (car at) (cdr at) nil t))
              (color (if result "DarkOrange" "Red1")))  ; like in Flyspell
          (overlay-put overlay 'face `(:underline (:style wave :color ,color)))
          (overlay-put overlay 'grammaticus-overlay t)))
      (grammaticus--show result))))

(defun grammaticus--get (db word)
  "Return list of pairs with information pertaining to WORD."
  (setq word (ucs-normalize-NFKD-string (downcase (or word ""))))
  (let* ((to (string-match "\\(qu\\|[nuv]\\)e\\'" word))
         (split (cons (substring word 0 to) (when to (substring word to)))))
    (mapcan (lambda (p)
              (mapcar (apply-partially #'grammaticus--at (car p) (or (cdr p) "")
                                       grammaticus-diacritics (car db))
                      (gethash (grammaticus--to-ASCII (car p)) (cdr db))))
            (if to (list (list word) split) (list split)))))

(defun grammaticus--show (result)
  "Display the list of pairs returned from grammaticus--get."
  (let ((message-log-max)) (message "%s" (mapconcat #'cdr result "\n"))))

(defun grammaticus--at (exact enclitic marks buffer index)
  "Return pair with information in BUFFER at INDEX

Entries that exactly match EXACT are highlighted, ENCLITIC is shadowed.
Ignore case/diacritics when determining near matches, except for MARKS."
  (with-current-buffer buffer
    (goto-char index)
    (let* ((tag (grammaticus--interpret-tag (grammaticus--next-field)))
           (lemma (grammaticus--next-field))
           (canon (grammaticus--to-UCS (grammaticus--next-field)))
           (expect (downcase (grammaticus--to-ASCII canon marks)))
           (match (equal expect exact)))
      (cons (unless match (concat expect enclitic))
            (format "%s%s:%s (%s)" (propertize canon 'face (when match 'bold))
                    (propertize enclitic 'face 'shadow) tag lemma)))))

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
  (dolist (pair `(,@(unless grammaticus-use-J '((?J . ?I) (?j . ?i)))
                  ,@(unless grammaticus-use-V '((?V . ?u) (?v . ?u)))
                  (?_ . ?\N{COMBINING MACRON}) (?^ . ?\N{COMBINING BREVE})
                  (?+ . ?\N{COMBINING DIAERESIS})))
    (subst-char-in-string (car pair) (cdr pair) string t))
  (let ((hidden "\N{COMBINING MACRON}\N{COMBINING BREVE}"))  ; hidden quantity
    (ucs-normalize-NFKD-string (replace-regexp-in-string hidden "" string))))

(provide 'grammaticus)
;;; grammaticus.el ends here
