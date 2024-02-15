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
  (let ((inventory (make-hash-table-from '(("a") ("b") ("c") ("d"))))
        count)
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
     (setq count (logseq-org-roam--inventory-from-cache inventory)))
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
                                                        :roam-aliases ("e"))))))

(ert-deftest logseq-org-roam--inventory-mark-modified ()
  (let ((inventory (make-hash-table-from '(("a") ("b"))))
        actual)
    (mocker-let
     ((logseq-org-roam--find-buffer-visiting (f) ((:input '("a") :output 'a)
                                                  (:input '("b") :output nil)))
      (buffer-modified-p (b) ((:input '(a) :output t))))
     (setq actual (logseq-org-roam--inventory-mark-modified '("a" "b") inventory)))
    (should (eq actual 1))
    (should (hash-table-p inventory))
    (should (equal (hash-table-count inventory) 2))
    (should (equal (gethash "a" inventory 'not-found)
                   '(:modified-p t)))
    (should (equal (gethash "b" inventory 'not-found)
                   nil))))

(ert-deftest logseq-org-roam--parse-buffer--empty ()
  (let (actual
        (expected nil))
    (with-temp-buffer
      (insert "")
      (org-mode)
      (setq actual  (logseq-org-roam--parse-buffer
                     nil '(first-section file-links fuzzy-links))))
    (should (equal actual expected))))

(ert-deftest logseq-org-roam--parse-buffer--simple ()
  (let (actual
        (expected nil))
    (with-temp-buffer
      (insert "* Typical Logseq page
*")
      (org-mode)
      (setq actual  (logseq-org-roam--parse-buffer
                     nil '(first-section file-links fuzzy-links))))
    (should (equal actual expected))))

(ert-deftest logseq-org-roam--parse-buffer--title-only ()
  (let (actual
        (expected '(:first-section-p t
                    :title-point 1
                    :title "Test note")))
    (with-temp-buffer
      (insert "#+title: Test note
* Typical Logseq page
*")
      (org-mode)
      (setq actual (logseq-org-roam--parse-buffer
                    nil '(first-section file-links fuzzy-links))))
    (should (equal actual expected))))

(ert-deftest logseq-org-roam--parse-buffer--prop-only ()
  (let (actual
        (expected '(:first-section-p t
                    :title-point 64
                    :id "9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f")))
    (with-temp-buffer
      (insert ":PROPERTIES:
:ID:   9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:

* Typical Logseq page
*")
      (org-mode)
      (setq actual (logseq-org-roam--parse-buffer
                    nil '(first-section file-links fuzzy-links))))
    (should (equal actual expected))))

(ert-deftest logseq-org-roam--parse-buffer--heading-id ()
  (let (actual
        (expected nil))
    (with-temp-buffer
      (insert "* Typical Logseq page
:PROPERTIES:
:ID:   9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:
*")
      (org-mode)
      (setq actual (logseq-org-roam--parse-buffer
                    nil '(first-section file-links fuzzy-links))))
    (should (equal actual expected))))

(ert-deftest logseq-org-roam--parse-buffer--aliases ()
  (let (actual
        (expected '(:first-section-p t
                    :title-point 88
                    :id "9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f"
                    :roam-aliases ("a" "C d")
                    :title "Note"
                    :aliases ("a" "B" "c d" "e"))))
    (with-temp-buffer
      (insert ":PROPERTIES:
:ID:   9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:ROAM_ALIASES:  a \"C d\"
:END:
#+title: Note
#+alias: a, B, c d, e
*")
      (org-mode)
      (setq actual (logseq-org-roam--parse-buffer
                    nil '(first-section file-links fuzzy-links))))
    (should (equal actual expected))))

(ert-deftest logseq-org-roam--parse-buffer--links ()
  (let (actual
        (expected '(:first-section-p t
                    :title-point 68
                    :id "9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f"
                    :title "Test note"
                    :links ((fuzzy 288 298 "Logseq" nil "[[Logseq]]")
                            (file 400 427 "dir/note.org" "note" "[[file:dir/note.org][note]]")))))
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
                         ((:input '("dir/note.org") :output t))))
       (setq actual (logseq-org-roam--parse-buffer
                     nil '(first-section file-links fuzzy-links)))))
    (should (equal actual expected))))

(ert-deftest logseq-org-roam--parse-buffer--idempotent ()
  (let* ((expected '(:first-section-p t
                     :title-point 68
                     :id "9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f"
                     :title "Test note"
                     :links ((fuzzy 288 298 "Logseq" nil "[[Logseq]]")
                             (file 400 427 "dir/note.org" "note" "[[file:dir/note.org][note]]"))))
         actual)
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
                                        :output-generator #'identity)))
        (org-roam-file-p (p)
                         ((:input '("dir/note.org")
                           :output t))))
       (setq actual (logseq-org-roam--parse-buffer
                     expected '(first-section file-links fuzzy-links)))))
    (should (equal actual expected))))

(ert-deftest logseq-org-roam--fill-fuzzy-dict--with-conflicts ()
  (let ((org-roam-directory default-directory)
        (inventory (make-hash-table-from '(("x" . (:title "A"
                                                   :aliases ("b" "c")))
                                           ("y" . (:title "B"
                                                   :aliases ("d" "c")))
                                           ("z" . (:title "D"
                                                   :aliases ("e" "f"))))))
        (fuzzy-dict (make-hash-table :test #'equal))
        (files '("x" "y" "z"))
        actual
        logs)
    (with-temp-buffer
      (let* ((standard-output (current-buffer)))
        (mocker-let ((logseq-org-roam--fl (p)
                                          :ordered nil
                                          ((:input '("x") :output "[[x]]")
                                           (:input '("y") :output "[[y]]")
                                           (:input '("z") :output "[[z]]"))))
                    (setq actual (logseq-org-roam--fill-fuzzy-dict
                                  fuzzy-dict files
                                  inventory)))
        (setq logs (buffer-string))))
    (should (= (hash-table-count fuzzy-dict) 6))
    (should (equal (gethash "a" fuzzy-dict) "x"))
    (should (equal (gethash "b" fuzzy-dict) '("x")))
    (should (equal (gethash "c" fuzzy-dict) '("x")))
    (should (equal (gethash "d" fuzzy-dict) '("y")))
    (should (equal (gethash "e" fuzzy-dict) "z"))
    (should (equal (gethash "f" fuzzy-dict) "z"))
    (should (equal logs
                   "** Filling dictionary of titles and aliases:
- The title \"b\" in [[y]] conflicts with the alias in [[x]], links will not be converted.
- The alias \"c\" in [[y]] conflicts with the alias in [[x]], links will not be converted.
- The title \"d\" in [[z]] conflicts with the alias in [[y]], links will not be converted.
6 entries in total
3 conflicts
"))))

(ert-deftest logseq-org-roam--fill-file-dict--with-conflict ()
  (let ((file-dict (make-hash-table :test #'equal))
        (org-roam-directory default-directory)
        (files '("/Path/X.org"
                 "/Path/y_.org"
                 "/Path/y___.org"
                 "/Path/z.org"
                 "/Path/Z.org"))
        actual
        log)
    (mocker-let ((logseq-org-roam--fl (p)
                                      :ordered nil
                                      ((:input '("/Path/y_.org") :output "[[y_.org]]")
                                       (:input '("/Path/y___.org") :output "[[y___.org]]")
                                       (:input '("/Path/z.org") :output "[[z.org]]")
                                       (:input '("/Path/Z.org") :output "[[Z.org]]"))))
                (with-temp-buffer
                  (let* ((standard-output (current-buffer)))
                    (setq actual (logseq-org-roam--fill-file-dict
                                  file-dict
                                  files)))
                  (setq log (buffer-string))))
    (should (= actual 2))
    (should (equal (hash-table-count file-dict) 3))
    (should (equal (gethash "/Path/x.org" file-dict) "/Path/X.org"))
    (should (equal (gethash "/Path/y_.org" file-dict) '("/Path/y_.org")))
    (should (equal (gethash "/Path/z.org" file-dict) '("/Path/z.org")))
    (should (equal log
                   "** Filling dictionary of similar paths:
- Path to [[y___.org]] and [[y_.org]] are too similar and will not be converted
- Path to [[Z.org]] and [[z.org]] are too similar and will not be converted
3 entries in total
2 conflicts
"))))

(ert-deftest logseq-org-roam--update-links ()
  (let ((org-roam-directory default-directory)
        (inventory (make-hash-table-from '(("dir/Note.org" . (:id "A"))
                                           ("b" . (:id "B")))))
        (fuzzy-dict (make-hash-table-from '(("logseq" . "b"))))
        (file-dict (make-hash-table-from '(("dir/note.org" . "dir/Note.org"))))
        (links '((file 400 427 "dir/note.org" "note" "[[file:dir/note.org][note]]")
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
        logs)
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
       ((logseq-org-roam--expand-file
         (p)
         ((:input '("dir/note.org") :output "dir/note.org"))))
       (logseq-org-roam--update-links links inventory file-dict fuzzy-dict))
      (setq logs (buffer-string)))
    (should (equal result logs))))

(ert-deftest logseq-org-roam--update-links--faulty ()
  (let ((links '((file 400 427 "dir/note.org" "note" "[[file:dir/note.org][note]]" "A")
                 (title 288 298 "Logseq" nil "[[Logseq]]" "B")))
        (content ":PROPERTIES:
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
        (expected 'mismatch-link)
        actual)
    (with-temp-buffer
      (insert content)
      (org-mode)
      (setq actual (catch 'update-error
                     (logseq-org-roam--update-links links nil nil nil)))
      (setq result (buffer-string)))
    (should (equal result content))
    (should (eq actual expected))))

(ert-deftest logseq-org-roam--update-first-section--with-alias ()
  (let ((content "#+alias: a, b, \"C d\"\n* Typical Logseq page\n*")
        (plist '(:first-section-p t
                 :title-point 1
                 :aliases ("a" "b" "C d")))
        (expected "#+title: Test note\n#+alias: a, b, \"C d\"\n* Typical Logseq page\n*")
        actual)
    (with-temp-buffer
      (insert content)
      (org-mode)
      (mocker-let
       ((org-id-get-create (&optional f)
                           ((:input '(nil) :output nil)))
        (org-roam-property-add (p v)
                               ((:input '("ROAM_ALIASES" "a") :output-generator #'ignore)
                                (:input '("ROAM_ALIASES" "b") :output-generator #'ignore)
                                (:input '("ROAM_ALIASES" "C d") :output-generator #'ignore)))
        (logseq-org-roam--buffer-title () ((:output "Test note"))))
       (logseq-org-roam--update-first-section plist))
      (setq actual (buffer-string)))
    (should (equal actual expected))))

(ert-deftest logseq-org-roam--update-first-section--missing-title ()
  (let ((plist '(:first-section-p t
                 :id "9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f"
                 :title-point 68))
        (content ":PROPERTIES:
:ID:       9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:
* Typical Logseq page
*")
        (expected ":PROPERTIES:
:ID:       9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:
#+title: Test note
* Typical Logseq page
*")
        actual)
    (with-temp-buffer
      (insert content)
      (org-mode)
      (mocker-let
       ((logseq-org-roam--buffer-title () ((:output "Test note"))))
       (logseq-org-roam--update-first-section plist))
      (setq actual (buffer-string)))
    (should (equal expected actual))))


;; TODO mock internal functions instead?
(ert-deftest logseq-org-roam--inventory-all--normal ()
  (let* ((org-roam-directory default-directory)
         (files '("a" "b" "c"))
         (inventory (logseq-org-roam--inventory-init files))
         logs actual)
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (mocker-let
         ((logseq-org-roam--fl
           (f)
           ((:occur 3
             :input-matcher (lambda (f) (stringp f))
             :output-generator (lambda (f) (format "[[%s]]" f f)))))
          (logseq-org-roam--secure-hash
           (a o &optional s e b)
           ((:occur 3
             :input-matcher (lambda (a o &optional s e b)
                              (and (eq a 'sha256)
                                   (bufferp o)
                                   (not s) (not e) (not b)))
             :output "hash")))
          (logseq-org-roam-logseq-p
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
         (setq actual
               (logseq-org-roam--inventory-all
                files inventory t '(first-section file-links fuzzy-links)))))
      (setq logs (buffer-string)))
    (should (equal actual 3))
    (should (equal (gethash "a" inventory 'not-found)
                   '(:hash "hash")))
    (should (equal (gethash "b" inventory 'not-found)
                   '(:hash "hash"
                     :first-section-p t
                     :aliases ("d"))))
    (should (equal (gethash "c" inventory 'not-found)
                   '(:hash "hash"
                     :first-section-p t
                     :title-point 1
                     :title "Test note")))
    (should (equal logs
                   "** Initial inventory:
- Parsed [[a]]
- Parsed [[b]]
- Parsed [[c]]
3 files found in org-roam directory
0 files being visited in a modified buffer will be skipped
0 files are external to Logseq and will not be modified
3 files have been parsed without errors
"))))

(ert-deftest logseq-org-roam--inventory-update--no-change ()
  (let ((org-roam-directory default-directory)
        (expected-logs "** Inventory update:
- Parsed [[a]]
- Parsed [[b]]
- Parsed [[c]]
3 files have been parsed")
        (files '("a" "b" "c"))
        (inventory (make-hash-table-from '(("a" . (:hash "hash"))
                                           ("b" . (:hash "hash"
                                                   :first-section-p t
                                                   :aliases ("d")))
                                           ("c" . (:hash "hash"
                                                   :first-section-p t
                                                   :title-point 1
                                                   :title "Test note")))))
        logs)
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (mocker-let
         ((logseq-org-roam--fl
           (f)
           ((:input-matcher (lambda (f) (stringp f))
             :output-generator (lambda (f) (format "[[%s]]" f f)))))
          (logseq-org-roam--secure-hash
           (a o &optional s e b)
           ((:occur 3
             :input-matcher (lambda (a o &optional s e b)
                              (and (eq a 'sha256)
                                   (bufferp o)
                                   (not s) (not e) (not b)))
             :output "hash")))
          (logseq-org-roam--insert-file-contents
           (f &optional v b e r)
           :ordered nil
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
         (logseq-org-roam--inventory-update
          files inventory '(first-section file-links fuzzy-links))))
      (setq logs (buffer-string)))
    (should (equal (hash-table-count inventory) 3))
    (should (equal (gethash "a" inventory 'not-found)
                   '(:hash "hash")))
    (should (equal (gethash "b" inventory 'not-found)
                   '(:hash "hash"
                     :first-section-p t
                     :aliases ("d"))))
    (should (equal (gethash "c" inventory 'not-found)
                   '(:hash "hash"
                     :first-section-p t
                     :title-point 1
                     :title "Test note")))
    (should (equal (substring logs 0 (length expected-logs))
                   expected-logs))))

(ert-deftest logseq-org-roam--update-all--first-sections ()
  (let (logs
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
                      ("h" . (:hash "hash"))
                      ("i" . (:hash "wrong hash")))))
        (expected '("h")))
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (mocker-let
         ((logseq-org-roam--update-first-section
           (p)
           ((:occur 2 :input-matcher #'always :output-generator #'ignore)))
          (logseq-org-roam--find-buffer-visiting
           (f &optional p)
           ((:occur 3 :input-matcher #'always :output nil)))
          (logseq-org-roam--secure-hash
           (a o &optional s e b)
           ((:occur 3
             :input-matcher (lambda (a o &optional s e b)
                              (and (eq a 'sha256)
                                   (bufferp o)
                                   (not s) (not e) (not b)))
             :output "hash")))
          (logseq-org-roam--find-file-noselect
           (f &optional n r w)
           ((:input '("a" nil nil nil) ; no modifications
             :output-generator (lambda (f &optional n r w)
                                 (get-buffer-create " *foo*" t)))
            (:input '("h" nil nil nil) ; with modifications!
             :output-generator (lambda (f &optional n r w)
                                 (let ((buf (get-buffer-create " *foo*" t)))
                                   (with-current-buffer buf
                                     (set-buffer-modified-p t))
                                   buf)))
            (:input '("i" nil nil nil) ; no modifications
             :output-generator (lambda (f &optional n r w)
                                 (get-buffer-create " *foo*" t)))))
          (save-buffer
           (&optional b)
           ((:occur 1
             :input-matcher (lambda (b) (not b)) :output-generator #'ignore))))
         (setq actual (logseq-org-roam--update-all
                       '("a" "b" "c" "d" "e" "f" "g" "h" "i")
                       inventory))))
      (setq logs (buffer-string)))
    (should (equal actual expected))
    (should (equal (gethash "a" inventory) '(:hash "hash")))
    (should (equal (gethash "h" inventory) '(:hash "hash")))
    (should (equal (gethash "i" inventory) '(:hash "wrong hash" :update-error hash-mismatch)))
    (should (equal logs
                   "** Updating files:\n- Updated first section of [[file:h][h]]\n- Error updating first section of [[file:i][i]]\n"))))

(ert-deftest logseq-org-roam--update-all--links ()
  (let (logs
        actual
        (org-roam-directory default-directory)
        (inventory (make-hash-table-from
                    '(("a" . (:hash "hash" :links '("foo")))
                      ("h" . (:hash "hash" :links '("foo"))))))
        (expected '("h")))
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (mocker-let
         ((logseq-org-roam--fl
           (f)
           ((:occur 1
             :input-matcher (lambda (f) (stringp f))
             :output-generator (lambda (f) (format "[[%s]]" f f)))))
          (logseq-org-roam--update-links
           (p i d1 d2)
           ((:occur 2
             :input-matcher (lambda (p i d1 d2)
                              (and (consp p)
                                   (hash-table-p i)
                                   (not d1) (not d2)))
             :output-generator #'ignore)))
          (logseq-org-roam--find-buffer-visiting
           (f &optional p)
           ((:occur 2
             :input-matcher (lambda (f &optional p)
                              (stringp f))
             :output nil)))
          (logseq-org-roam--secure-hash
           (a o &optional s e b)
           ((:occur 2
             :input-matcher (lambda (a o &optional s e b)
                              (and (eq a 'sha256)
                                   (bufferp o)
                                   (not s) (not e) (not b)))
             :output "hash")))
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
             :input-matcher (lambda (b) (not b)) :output-generator #'ignore))))
         (setq actual (logseq-org-roam--update-all
                       '("a" "h") inventory t nil nil))))
      (setq logs (buffer-string)))
    (should (equal actual expected))
    (should (equal (gethash "a" inventory) '(:hash "hash" :links '("foo"))))
    (should (equal (gethash "h" inventory) '(:hash "hash" :links '("foo"))))
    (should (equal logs
                   "** Updating files:\n- Updated links of [[h]]\n"))))

(ert-deftest logseq-org-roam-create-translate-default--normal ()
  (let ((org-roam-directory "/roam")
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
                    ("2023_12_01" . "/roam/journals/2023_12_01.org")))
        actual)
    (pcase-dolist (`(,input . ,expected) testdata)
      (mocker-let
       ((logseq-org-roam--expand-file
         (f &optional d)
         ((:min-occur 0 :max-occur 2
           :input-matcher (lambda (f &optional d) (and (stringp f) (stringp d)))
           :output-generator (lambda (f &optional d) (concat d "/" f))))))
       (setq actual (logseq-org-roam-create-translate-default input)))
      (should (equal actual expected)))))

(ert-deftest logseq-org-roam--create-from--normal ()
  (let ((org-roam-directory default-directory)
        (logseq-org-roam-create-accept-func #'always)
        (logseq-org-roam-journals-directory "journals")
        (logseq-org-roam-pages-directory "pages")
        (inventory (make-hash-table-from
                    '(("a" . (:links
                              ((file 1 2 "a" "A" "[[file:a][A]")
                               (file 1 2 "b" "B" "[[file:b][B]")
                               (fuzzy 1 2 "C" "c" "[[C][c]]")
                               (fuzzy 1 2 "D" "d" "[[D][d]]")))))))
        (fuzzy-dict (make-hash-table-from '(("D" . "a"))))
        (file-dict (make-hash-table-from '(("a" . "a"))))
        (expected '("C" "b"))
        actual
        log)
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
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
           ((:input-matcher (lambda (f) (stringp f))
             :output-generator (lambda (f) (format "[[%s]]" f f)))))
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
           ((:input-matcher (lambda (b) (not b)) :output-generator #'ignore))))
         (setq actual (logseq-org-roam--create-from
                       '("a") inventory file-dict fuzzy-dict)))
        (setq logs (buffer-string))))
    (should (equal actual expected))
    (should (equal logs
                   "** Creating new files:\n- Created [[b]] from the file link in [[a]]\n- Created [[C]] from the fuzzy link in [[a]]\n"))))

;; TODO test logseq-org-roam - mocking all internal functions

(provide 'logseq-org-roam-tests)
;;; logseq-org-roam-tests.el ends here
