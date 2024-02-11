;;; logseq-org-roam-tests.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Sylvain Bougerel
;;

;; Author: Sylvain Bougerel <sylvain.bougerel.devel@gmail.com>
;; Maintainer: Sylvain Bougerel <sylvain.bougerel.devel@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;;  Test for 'logseq-org-roam'.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'mocker)

(require 'logseq-org-roam)

(defun make-hash-table-from (alist)
  "Create a hash table from ALIST."
  (let ((inventory (make-hash-table :test #'equal)))
    (pcase-dolist (`(,key . ,val) alist)
      (puthash key val inventory))
    inventory))

(ert-deftest logseq-org-roam--inventory-init ()
  ;; `logseq-org-roam--files-not-cached' find the difference between
  ;; `org-roam-list-files' and the files present in the org-roam cache which
  ;; we'll have to mock here.
  (let ((result (logseq-org-roam--inventory-init '("a" "b" "c" "d"))))
    (should (hash-table-p result))
    (should (equal (hash-table-count result) 4))
    (should (equal (gethash "a" result) nil))
    (should (equal (gethash "b" result) nil))
    (should (equal (gethash "c" result) nil))
    (should (equal (gethash "d" result) nil))))

(ert-deftest logseq-org-roam--inventory-from-cache ()
  (mocker-let
   ((org-roam-db-query (sql &rest args)
                       ((:input '([:select [file id title]
                                   :from nodes    
                                   :where (= 0 level)])
                                :output '(("a" "x" "A") ("d" "y" "D")))
                        (:input '([:select [alias]
                                   :from aliases
                                   :where (= node-id $s1)]
                                  "x")
                                :output nil)
                        (:input '([:select [alias]
                                   :from aliases
                                   :where (= node-id $s1)]
                                  "y")
                                :output '(("e"))))))
   (let* ((inventory (logseq-org-roam--inventory-init '("a" "b" "c" "d")))
          (count (logseq-org-roam--inventory-from-cache inventory)))
     (should (eq count 2))
     (should (hash-table-p inventory))
     (should (equal (hash-table-count inventory) 4))
     (should (equal (gethash "a" inventory 'not-found) '(:cache-p t
                                                         :id "x"
                                                         :title "A")))
     (should (equal (gethash "b" inventory 'not-found) nil))
     (should (equal (gethash "c" inventory 'not-found) nil))
     (should (equal (gethash "d" inventory 'not-found) '(:cache-p t
                                                         :id "y"
                                                         :title "D"
                                                         :roam-aliases ("e")))))))

(ert-deftest logseq-org-roam--inventory-mark-modified ()
  (mocker-let
   ((logseq-org-roam--find-buffer-visiting (f) ((:input '("a") :output 'a)
                                                (:input '("b") :output nil)))
    (buffer-modified-p (b) ((:input '(a) :output t))))
   (let* ((inventory (logseq-org-roam--inventory-init '("a" "b")))
          (actual (logseq-org-roam--inventory-mark-modified '("a" "b") inventory)))
     (should (eq actual 1))
     (should (hash-table-p inventory))
     (should (equal (hash-table-count inventory) 2))
     (should (equal (gethash "a" inventory 'not-found)
                    '(:modified-p t)))
     (should (equal (gethash "b" inventory 'not-found)
                    nil)))))

(ert-deftest logseq-org-roam--parse-buffer--empty ()
  (with-temp-buffer
    (insert "")
    (org-mode)
    (let ((actual (logseq-org-roam--parse-buffer
                   nil '(first-section file-links fuzzy-links)))
          (expected nil))
      (should (equal actual expected)))))

(ert-deftest logseq-org-roam--parse-buffer--simple ()
  (with-temp-buffer
    (insert "* Typical Logseq page
*")
    (org-mode)
    (let ((actual (logseq-org-roam--parse-buffer
                   nil '(first-section file-links fuzzy-links)))
          (expected nil))
      (should (equal actual expected)))))

(ert-deftest logseq-org-roam--parse-buffer--title-only ()
  (with-temp-buffer
    (insert "#+title: Test note
* Typical Logseq page
*")
    (org-mode)
    (let ((actual (logseq-org-roam--parse-buffer
                   nil '(first-section file-links fuzzy-links)))
          (expected '(:first-section-p t
                      :title-point 1
                      :title "Test note")))
      (should (equal actual expected)))))

(ert-deftest logseq-org-roam--parse-buffer--prop-only ()
  (with-temp-buffer
    (insert ":PROPERTIES:
:ID:   9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:

* Typical Logseq page
*")
    (org-mode)
    (let ((actual (logseq-org-roam--parse-buffer
                   nil '(first-section file-links fuzzy-links)))
          (expected '(:first-section-p t
                      :title-point 64
                      :id "9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f")))
      (should (equal actual expected)))))

(ert-deftest logseq-org-roam--parse-buffer--heading-id ()
  (with-temp-buffer
    (insert "* Typical Logseq page
:PROPERTIES:
:ID:   9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:
*")
    (org-mode)
    (let ((actual (logseq-org-roam--parse-buffer
                   nil '(first-section file-links fuzzy-links)))
          (expected nil))
      (should (equal actual expected)))))

(ert-deftest logseq-org-roam--parse-buffer--aliases ()
  (with-temp-buffer
    (insert ":PROPERTIES:
:ID:   9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:ROAM_ALIASES:  a \"C d\"
:END:
#+title: Note
#+alias: a, B, c d, e
*")
    (org-mode)
    (let ((actual (logseq-org-roam--parse-buffer
                   nil '(first-section file-links fuzzy-links)))
          (expected '(:first-section-p t
                      :title-point 88
                      :id "9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f"
                      :roam-aliases ("a" "c d")
                      :title "Note"
                      :aliases ("a" "b" "c d" "e"))))
      (should (equal actual expected)))))

(ert-deftest logseq-org-roam--parse-buffer--links ()
  (with-temp-buffer
    (insert ":PROPERTIES:
:ID:       9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:
#+title: Test note

* TODO Test links [/]
** <<Target>>A target
#+NAME: code-block
#+BEGIN_SRC emacs-lisp -n -r
  (save-excursion                 (ref:example)
     (search-forward \"[[not-a-link]]\")
#+END_SRC
** A fuzzy [[Logseq]] link
:PROPERTIES:
:CUSTOM_ID: some-custom-id
:END:
A coderef [[(example)]] link.
** A file link to a [[file:dir/note.org][note]]
An inline image [[image.jpg]] matched by image-file-name-extensions.
** A file link with a search option [[file:dir/note.org::1][note]]
A link to [[code-block]].
A [[#some-custom-id]] link.
** Link to [[target]]
A [[*Test links][headline link]].
A [[test links]] matching headline.
")
    (org-mode)
    (mocker-let
     ((logseq-org-roam--expand-file (p)
                                    ((:input '("dir/note.org") :output-generator #'identity)))
      (org-roam-file-p (p)
                       ((:input '("dir/note.org") :output t :max-occur 2))))
     (let ((actual (logseq-org-roam--parse-buffer
                    nil '(first-section file-links fuzzy-links)))
           (expected '(:first-section-p t
                       :title-point 68
                       :id "9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f"
                       :title "Test note"
                       :links ((fuzzy 288 298 "Logseq" nil "[[Logseq]]")
                               (file 400 427 "dir/note.org" "note" "[[file:dir/note.org][note]]")))))
       (should (equal actual expected))))))

(ert-deftest logseq-org-roam--parse-buffer--idempotent ()
  (with-temp-buffer
    (insert ":PROPERTIES:
:ID:       9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:
#+title: Test note

* TODO Test links [/]
** <<Target>>A target
#+NAME: code-block
#+BEGIN_SRC emacs-lisp -n -r
  (save-excursion                 (ref:example)
     (search-forward \"[[not-a-link]]\")
#+END_SRC
** A fuzzy [[Logseq]] link
:PROPERTIES:
:CUSTOM_ID: some-custom-id
:END:
A coderef [[(example)]] link.
** A file link to a [[file:dir/note.org][note]]
An inline image [[image.jpg]] matched by image-file-name-extensions.
** A file link with a search option [[file:dir/note.org::1][note]]
A link to [[code-block]].
A [[#some-custom-id]] link.
** Link to [[target]]
A [[*Test links][headline link]].
A [[test links]] matching headline.
")
    (org-mode)
    (mocker-let
     ((logseq-org-roam--expand-file (p)
                                    ((:input '("dir/note.org")
                                      :output-generator #'identity
                                      :max-occur 2)))
      (org-roam-file-p (p)
                       ((:input '("dir/note.org")
                         :output t
                         :max-occur 4))))
     (let* ((expected (logseq-org-roam--parse-buffer
                       nil '(first-section file-links fuzzy-links)))
            (actual (logseq-org-roam--parse-buffer
                     expected '(first-section file-links fuzzy-links))))
       (should (equal actual expected))))))

(ert-deftest logseq-org-roam--calculate-fuzzy-dict--with-conflicts ()
  (let ((inventory (make-hash-table :test #'equal))
        (org-roam-directory default-directory)
        (files '("x" "y" "z")))
    (puthash "x" '(:title "A"
                   :aliases ("b" "c")) inventory)
    (puthash "y" '(:title "B"
                   :aliases ("d" "c")) inventory)
    (puthash "z" '(:title "D"
                   :aliases ("e" "f")) inventory)
    (with-temp-buffer
      (let* ((standard-output (current-buffer))
             (actual_dict (logseq-org-roam--calculate-fuzzy-dict
                           files
                           inventory)))
        (should (= (hash-table-count actual_dict) 6))
        (should (equal (gethash "a" actual_dict) "x"))
        (should (equal (gethash "b" actual_dict) 'conflict))
        (should (equal (gethash "c" actual_dict) 'conflict))
        (should (equal (gethash "d" actual_dict) 'conflict))
        (should (equal (gethash "e" actual_dict) "z"))
        (should (equal (gethash "f" actual_dict) "z"))
        (should (equal (buffer-string)
                       "** Building dictionary of titles and aliases:
- The title \"b\" in [[file:y][y]] conflicts with the alias in [[file:x][x]], links to \"b\" will not be converted.
- The alias \"c\" in [[file:y][y]] conflicts with the alias in [[file:x][x]], links to \"c\" will not be converted.
- The title \"d\" in [[file:z][z]] conflicts with the alias in [[file:y][y]], links to \"d\" will not be converted.
"))))))

(ert-deftest logseq-org-roam--update-links ()
  (with-temp-buffer
    (insert ":PROPERTIES:
:ID:       9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:
#+title: Test note

* TODO Test links [/]
** <<Target>>A target
#+NAME: code-block
#+BEGIN_SRC emacs-lisp -n -r
  (save-excursion                 (ref:example)
     (search-forward \"[[not-a-link]]\")
#+END_SRC
** A fuzzy [[Logseq]] link
:PROPERTIES:
:CUSTOM_ID: some-custom-id
:END:
A coderef [[(example)]] link.
** A file link to a [[file:dir/note.org][note]]
An inline image [[image.jpg]] matched by image-file-name-extensions.
** A file link with a search option [[file:dir/note.org::1][note]]
A link to [[code-block]].
A [[#some-custom-id]] link.
** Link to [[target]]
A [[*Test links][headline link]].
A [[test links]] matching headline.
")
    (org-mode)
    (let ((inventory (make-hash-table :test #'equal))
          (fuzzy-dict (make-hash-table :test #'equal)))
      (puthash "dir/note.org" '(:id "A") inventory)
      (puthash "p" '(:id "B") inventory)
      (puthash "logseq" "p" fuzzy-dict)
      (mocker-let
       ((logseq-org-roam--expand-file (p)
                                      ((:input '("dir/note.org") :output-generator #'identity))))
       (let* ((links '((file 400 427 "dir/note.org" "note" "[[file:dir/note.org][note]]")
                       (title 288 298 "Logseq" nil "[[Logseq]]")))
              (result ":PROPERTIES:
:ID:       9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:
#+title: Test note

* TODO Test links [/]
** <<Target>>A target
#+NAME: code-block
#+BEGIN_SRC emacs-lisp -n -r
  (save-excursion                 (ref:example)
     (search-forward \"[[not-a-link]]\")
#+END_SRC
** A fuzzy [[id:B][Logseq]] link
:PROPERTIES:
:CUSTOM_ID: some-custom-id
:END:
A coderef [[(example)]] link.
** A file link to a [[id:A][note]]
An inline image [[image.jpg]] matched by image-file-name-extensions.
** A file link with a search option [[file:dir/note.org::1][note]]
A link to [[code-block]].
A [[#some-custom-id]] link.
** Link to [[target]]
A [[*Test links][headline link]].
A [[test links]] matching headline.
")
              (expected t)
              (actual (logseq-org-roam--update-links links inventory fuzzy-dict)))
         (should (equal result (buffer-string)))
         (should (eq actual expected)))))))

(ert-deftest logseq-org-roam--update-links--faulty ()
  (with-temp-buffer
    (insert ":PROPERTIES:
:ID:       9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:
#+title: Test note

* TODO Test links [/]
** <<Target>>A target
#+NAME: code-block
#+BEGIN_SRC emacs-lisp -n -r
  (save-excursion                 (ref:example)
     (search-forward \"[[not-a-link]]\")
#+END_SRC
** A fuzzy [[XXXXX]] link
:PROPERTIES:
:CUSTOM_ID: some-custom-id
:END:
A coderef [[(example)]] link.
** A file link to a [[file:dir/note.org][note]]
An inline image [[image.jpg]] matched by image-file-name-extensions.
** A file link with a search option [[file:dir/note.org::1][note]]
A link to [[code-block]].
A [[#some-custom-id]] link.
** Link to [[target]]
A [[*Test links][headline link]].
A [[test links]] matching headline.
")
    (org-mode)
    (let* ((links '((file 400 427 "dir/note.org" "note" "[[file:dir/note.org][note]]" "A")
                    (title 288 298 "Logseq" nil "[[Logseq]]" "B")))
           (result (buffer-string)) ;; unchanged
           (expected 'mismatch-link)
           (actual (logseq-org-roam--update-links links nil nil)))
      (should (equal result (buffer-string)))
      (should (eq actual expected)))))

(ert-deftest logseq-org-roam--update-first-section--typical ()
  (with-temp-buffer
    (insert "* Typical Logseq page
*")
    (mocker-let
     ((org-id-get-create (&optional f)
                         ((:input '(nil)
                           :output-generator
                           (lambda (&optional f)
                             (goto-char (point-min))
                             (insert ":PROPERTIES:
:ID:       9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:
")
                             "9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f"))))
      (logseq-org-roam--buffer-title () ((:output "Test note"))))
     (org-mode)
     (let* ((plist (logseq-org-roam--parse-buffer nil '(first-section)))
            (result ":PROPERTIES:
:ID:       9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:
#+title: Test note
* Typical Logseq page
*"))
       (logseq-org-roam--update-first-section plist)
       (should (equal result (buffer-string)))))))

(ert-deftest logseq-org-roam--update-first-section--with-alias ()
  (with-temp-buffer
    (insert "#+alias: a, b, \"c d\"
* Typical Logseq page
*")
    (mocker-let
     ((org-id-get-create (&optional f)
                         ((:input '(nil)
                           :output-generator
                           (lambda (&optional f)
                             (goto-char (point-min))
                             (insert ":PROPERTIES:
:ID:       9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:
")
                             "9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f"))))
      (org-roam-property-add (p v)
                             ((:max-occur 3
                               :input-matcher
                               (lambda (p _) (equal p "ROAM_ALIASES"))
                               :output-generator
                               (lambda (p v)
                                 (goto-char (point-min))
                                 (forward-line 2)
                                 (beginning-of-line)
                                 (when (looking-at ":END:")
                                   (insert ":ROAM_ALIASES: a b \"c d\"\n"))))))
      (logseq-org-roam--buffer-title () ((:output "Test note"))))
     (org-mode)
     (let* ((plist (logseq-org-roam--parse-buffer nil '(first-section)))
            (result ":PROPERTIES:
:ID:       9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:ROAM_ALIASES: a b \"c d\"
:END:
#+title: Test note
#+alias: a, b, \"c d\"
* Typical Logseq page
*"))
       (logseq-org-roam--update-first-section plist)
       (should (equal result (buffer-string)))))))

(ert-deftest logseq-org-roam--update-first-section--missing-title ()
  (with-temp-buffer
    (insert ":PROPERTIES:
:ID:       9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:
* Typical Logseq page
*")
    (mocker-let
     ((logseq-org-roam--buffer-title () ((:output "Test note"))))
     (org-mode)
     (let* ((plist (logseq-org-roam--parse-buffer nil '(first-section)))
            (result ":PROPERTIES:
:ID:       9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:
#+title: Test note
* Typical Logseq page
*"))
       (logseq-org-roam--update-first-section plist)
       (should (equal result (buffer-string)))))))


;; TODO mock internal functions instead?
(ert-deftest logseq-org-roam--inventory-all--normal ()
  (let ((org-roam-directory default-directory)
        inventory
        logs)
    (mocker-let
     ((logseq-org-roam-logseq-p
       (f)
       ((:input '("a") :output t)
        (:input '("b") :output t)
        (:input '("c") :output t)))
      (logseq-org-roam--insert-file-contents
       (f &optional v b e r)
       ((:input '("a" nil nil nil nil)
         :output-generator (lambda (f &optional v b e r)
                             (insert "* Typical Logseq page
*")))
        (:input '("b" nil nil nil nil)
         :output-generator (lambda (f &optional v b e r)
                             (insert "#+alias: d
* Logseq page with alias
*")))
        (:input '("c" nil nil nil nil)
         :output-generator (lambda (f &optional v b e r)
                             (insert "#+title: Test note
* Logseq page with title
*"))))))
     (with-temp-buffer
       (let ((standard-output (current-buffer)))
         (setq inventory
               (logseq-org-roam--inventory-all
                '("a" "b" "c") t '(first-section file-links fuzzy-links))))
       (setq logs (buffer-string))))
    (should (hash-table-p inventory))
    (should (equal (hash-table-count inventory) 3))
    (should (equal (gethash "a" inventory 'not-found)
                   '(:hash "0402a679d81d52814c2f30094d1bcbf8aeb2da2c7b98a0624267585405219b76")))
    (should (equal (gethash "b" inventory 'not-found)
                   '(:hash "651d9ddca6e5f8cfce534d1f4ae7f8cbb85a2dd1a6b942d0723f6a451cf67799"
                     :first-section-p t
                     :aliases ("d"))))
    (should (equal (gethash "c" inventory 'not-found)
                   '(:hash "b16e53419b3e9c1fabf6bdfa6476d4ab523e25b9e7ea14fa4a6a9c9ce3d3a8bc"
                     :first-section-p t
                     :title-point 1
                     :title "Test note")))
    (should (equal (substring logs 0 200)
                   "** Inventory initially:
3 files found in org-roam directory
0 files being visited in a modified buffer will be skipped
0 files are external to Logseq and will not be modified
3 files have been parsed
"))))

(ert-deftest logseq-org-roam--inventory-update--no-change ()
  (mocker-let
   ((logseq-org-roam-logseq-p
     (f)
     ((:input '("a") :output t)
      (:input '("b") :output t)
      (:input '("c") :output t)))
    (logseq-org-roam--insert-file-contents
     (f &optional v b e r)
     :ordered nil
     ((:input '("a" nil nil nil nil)
       :occur 2
       :output-generator (lambda (f &optional v b e r)
                           (insert "* Typical Logseq page
*")))
      (:input '("b" nil nil nil nil)
       :occur 2
       :output-generator (lambda (f &optional v b e r)
                           (insert "#+alias: d
* Logseq page with alias
*")))
      (:input '("c" nil nil nil nil)
       :occur 2
       :output-generator (lambda (f &optional v b e r)
                           (insert "#+title: Test note
* Logseq page with title
*"))))))
   (let ((org-roam-directory default-directory)
         (expected-logs "** Inventory update:\n3 files have been parsed")
         inventory
         logs)
     (with-temp-buffer
       (let ((standard-output (current-buffer)))
         (setq inventory
               (logseq-org-roam--inventory-all
                '("a" "b" "c") t
                '(first-section file-links fuzzy-links)))))
     (with-temp-buffer
       (let ((standard-output (current-buffer)))
         (logseq-org-roam--inventory-update
          '("a" "b" "c")
          inventory
          '(first-section file-links fuzzy-links)))
       (setq logs (buffer-string)))
     (should (hash-table-p inventory))
     (should (equal (hash-table-count inventory) 3))
     (should (equal (gethash "a" inventory 'not-found)
                    '(:hash "0402a679d81d52814c2f30094d1bcbf8aeb2da2c7b98a0624267585405219b76")))
     (should (equal (gethash "b" inventory 'not-found)
                    '(:hash "651d9ddca6e5f8cfce534d1f4ae7f8cbb85a2dd1a6b942d0723f6a451cf67799"
                      :first-section-p t
                      :aliases ("d"))))
     (should (equal (gethash "c" inventory 'not-found)
                    '(:hash "b16e53419b3e9c1fabf6bdfa6476d4ab523e25b9e7ea14fa4a6a9c9ce3d3a8bc"
                      :first-section-p t
                      :title-point 1
                      :title "Test note")))
     (should (equal (substring logs 0 (length expected-logs))
                    expected-logs)))))

(ert-deftest logseq-org-roam--update-all--first-sections ()
  (mocker-let
   ((logseq-org-roam--update-first-section
     (p)
     ((:occur 2 :input-matcher #'always :output-generator #'ignore)))
    (logseq-org-roam--find-buffer-visiting
     (f &optional p)
     ((:occur 2 :input-matcher #'always :output nil)))
    (logseq-org-roam--secure-hash
     (algo obj &optional s e b)
     ((:min-occur 2 :max-occur 2
       :input-matcher #'always :output "hash")))
    (logseq-org-roam--find-file-noselect
     (f &optional n r w)
     ((:input '("a" nil nil nil) ; no modifications
       :output-generator (lambda (f &optional n r w)
                           (get-buffer-create " *foo*" t)))
      (:input '("h" nil nil nil) ; with modifications
       :output-generator (lambda (f &optional n r w)
                           (let ((buf (get-buffer-create " *foo*" t)))
                             (with-current-buffer buf
                               (set-buffer-modified-p t))
                             buf)))))
    (save-buffer
     (&optional b)
     ((:occur 1
       :input-matcher #'always :output-generator #'ignore))))
   (let* (logs
          actual
          (org-roam-directory default-directory)
          (inventory (make-hash-table-from
                      '(("a" . (:hash "hash"))
                        ("b" . (:modified-p t))
                        ("c" . (:external-p t))
                        ("d" . (:cache-p t))
                        ("e" . (:parse-error t))
                        ("f" . (:update-error t))
                        ("g" . (:title "Foo"
                                :id "foo"
                                :aliases '("foo")
                                :roam-aliases '("foo")))
                        ("h" . (:hash "hash")))))
          (expected '("h")))
     (with-temp-buffer
       (let ((standard-output (current-buffer)))
         (setq actual (logseq-org-roam--update-all
                       '("a" "b" "c" "d" "e" "f" "g" "h")
                       inventory))
         (setq logs (buffer-string))))
     (should (equal actual expected))
     (should (equal (gethash "a" inventory) '(:hash "hash")))
     (should (equal (gethash "h" inventory) '(:hash "hash")))
     (should (equal logs
                    "** First sections of the following files are updated:\n- Updated [[file:h][h]]\n")))))

(ert-deftest logseq-org-roam--update-all--links ()
  (mocker-let
   ((logseq-org-roam--update-links
     (p i d)
     ((:occur 2
       :input-matcher (lambda (p i d)
                        (and (consp p)
                             (hash-table-p i)))
       :output-generator #'ignore)))
    (logseq-org-roam--find-buffer-visiting
     (f &optional p)
     ((:occur 2
       :input-matcher (lambda (f &optional p)
                        (stringp f))
       :output nil)))
    (logseq-org-roam--secure-hash
     (algo obj &optional s e b)
     ((:min-occur 2 :max-occur 2
       :input-matcher #'always :output "hash")))
    (logseq-org-roam--find-file-noselect
     (f &optional n r w)
     ((:input '("a" nil nil nil) ; no modifications
       :output-generator (lambda (f &optional n r w)
                           (get-buffer-create " *foo*" t)))
      (:input '("h" nil nil nil) ; with modifications
       :output-generator (lambda (f &optional n r w)
                           (let ((buf (get-buffer-create " *foo*" t)))
                             (with-current-buffer buf
                               (set-buffer-modified-p t))
                             buf)))))
    (save-buffer
     (&optional b)
     ((:occur 1
       :input-matcher #'always :output-generator #'ignore))))
   (let* (logs
          actual
          (org-roam-directory default-directory)
          (inventory (make-hash-table-from
                      '(("a" . (:hash "hash" :links '("foo")))
                        ("b" . (:modified-p t))
                        ("c" . (:external-p t))
                        ("d" . (:cache-p t))
                        ("e" . (:parse-error t))
                        ("f" . (:update-error t))
                        ("g" . (:title "Foo"
                                :id "foo"
                                :aliases '("foo")
                                :roam-aliases '("foo")))
                        ("h" . (:hash "hash" :links '("foo"))))))
          (expected '("h")))
     (with-temp-buffer
       (let ((standard-output (current-buffer)))
         (setq actual (logseq-org-roam--update-all
                       '("a" "b" "c" "d" "e" "f" "g" "h")
                       inventory t nil))
         (setq logs (buffer-string))))
     (should (equal actual expected))
     (should (equal (gethash "a" inventory) '(:hash "hash" :links '("foo"))))
     (should (equal (gethash "h" inventory) '(:hash "hash" :links '("foo"))))
     (should (equal logs
                    "** Links of the following files are updated:\n- Updated [[file:h][h]]\n")))))

(ert-deftest logseq-org-roam-create-translate-default--normal ()
  (mocker-let
   ((logseq-org-roam--expand-file
     (f &optional d)
     ((:input-matcher #'always
       :output-generator (lambda (f &optional d) (concat d "/" f))))))
   (let* ((org-roam-directory "/roam")
          (logseq-org-roam-create-replace '(("[\\/]" . "_")))
          (logseq-org-roam-journals-directory "journals")
          (logseq-org-roam-journals-title-format "%Y_%m_%d") ;; non-ISO date
          (logseq-org-roam-journals-file-name-format "%Y_%m_%d")
          (logseq-org-roam-pages-directory "pages")
          (testdata '(("Test" . "/roam/pages/Test.org")
                      ("Use spaces" . "/roam/pages/Use spaces.org")
                      ("Replace/all/slash" . "/roam/pages/Replace_all_slash.org")
                      ("Replace\\all\\backslash" . "/roam/pages/Replace_all_backslash.org")
                      ("Not date 2024" . "/roam/pages/Not date 2024.org")
                      ("Not date July 2024" . "/roam/pages/Not date July 2024.org")
                      ("2023-12-01" . "/roam/pages/2023-12-01.org")
                      ("2023_12_01" . "/roam/journals/2023_12_01.org"))))
     (pcase-dolist (`(,input . ,expected) testdata)
       (should (equal
                (logseq-org-roam-create-translate-default input)
                expected))))))

(ert-deftest logseq-org-roam--create-from--normal ()
  (let ((org-roam-directory default-directory)
        (logseq-org-roam-create-accept-func #'always)
        (logseq-org-roam-journals-directory "journals")
        (logseq-org-roam-pages-directory "pages"))
    (mocker-let
     ((logseq-org-roam--create-path-file
       (p f i)
       ((:input-matcher #'always
         :output-generator (lambda (p f i) (if (equal p "b") p)))))
      (logseq-org-roam--create-path-fuzzy
       (p f)
       ((:input-matcher #'always
         :output-generator (lambda (p f) (if (equal p "C") p)))))
      (file-name-directory
       (f)
       ((:input-matcher #'always :output "dir")))
      (directory-file-name
       (f)
       ((:input-matcher #'always :output-generator #'identity)))
      (logseq-org-roam--file-exists-p
       (f)
       ((:input-matcher #'always
         :output-generator
         (lambda (f)
           (member f '("dir"))))))
      (org-roam-file-p
       (p)
       ((:input-matcher #'always :output t)))
      (logseq-org-roam--fl
       (f)
       ((:input-matcher #'always
         :output-generator (lambda (f) (format "[[file:%s][%s]]" f f)))))
      (logseq-org-roam--find-file-noselect
       (f &optional n r w)
       ((:input-matcher #'always
         :output-generator (lambda (f &optional n r w)
                             (get-buffer-create f t)))))
      (org-id-get-create
       (&optional f)
       ((:input-matcher #'always
         :output-generator #'ignore)))
      (save-buffer
       (&optional b)
       ((:input-matcher #'always :output-generator #'ignore))))
     (let* ((inventory (make-hash-table-from
                        '(("a" . (:links
                                  ((file 1 2 "a" "A" "[[file:a][A]")
                                   (file 1 2 "b" "B" "[[file:b][B]")
                                   (fuzzy 1 2 "C" "c" "[[C][c]]")
                                   (fuzzy 1 2 "D" "d" "[[D][d]]")))))))
            (fuzzy-dict (make-hash-table-from '(("D" . "a"))))
            (expected '("C" "b"))
            actual
            log)
       (with-temp-buffer
         (let ((standard-output (current-buffer)))
           (setq actual (logseq-org-roam--create-from
                         '("a") inventory fuzzy-dict))
           (setq logs (buffer-string))))
       (should (equal actual expected))
       (should (equal logs
                      "** Creating new files:\n- Created [[file:b][b]] from the file link in [[file:a][a]]\n- Created [[file:C][C]] from the fuzzy link in [[file:a][a]]\n"))))))

;; TODO test logseq-org-roam - mocking all internal functions

(provide 'logseq-org-roam-tests)
;;; logseq-org-roam-tests.el ends here
