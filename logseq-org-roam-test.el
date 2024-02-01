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
   ((find-buffer-visiting (f) ((:input '("a") :output 'a)
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
                       :links ((title 288 298 "Logseq" nil "[[Logseq]]")
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
    (puthash "x" '(:title-p t
                   :title "A"
                   :aliases ("b" "c")) inventory)
    (puthash "y" '(:title-p t
                   :title "B"
                   :aliases ("d" "c")) inventory)
    (puthash "z" '(:title-p t
                   :title "D"
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
                       "** Issues building dictionary of titles and aliases:
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

;; TODO: reformat to make it look nicer.
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
     ((logseq-org-roam-file-p
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
  (let ((org-roam-directory default-directory)
        inventory
        logs)
    (mocker-let
     ((logseq-org-roam-file-p
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
    (should (equal (substring logs 0 53)
                   "** Inventory update:
3 updated files have been parsed"))))

;; TODO test update all first sections, mocking internal functions
;; TODO test create translate default
;; TODO test create from
;; TODO test log errors
;; TODO test logseq-org-roam - mocking all internal functions

(provide 'logseq-org-roam-tests)
;;; logseq-org-roam-tests.el ends here
