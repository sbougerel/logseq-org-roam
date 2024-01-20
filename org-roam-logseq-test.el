;;; org-roam-logseq-tests.el --- Description -*- lexical-binding: t; -*-
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

;;  Test 'org-roam-logseq'.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'mocker)

(require 'org-roam-logseq)

(ert-deftest org-roam-logseq--inventory-indexed ()
  ;; `org-roam-logseq--files-not-cached' find the difference between
  ;; `org-roam-list-files' and the files present in the org-roam cache which
  ;; we'll have to mock here.
  (mocker-let
   ((org-roam-db-query (q)
                       ((:input '([:select file :from files])
                         :output '(("a") ("d") ("e"))))))
   (let ((result (org-roam-logseq--inventory-indexed '("a" "b" "c" "d"))))
     (should (hash-table-p result))
     (should (equal (hash-table-count result) 4))
     (should (equal (gethash "a" result) '(:indexed t)))
     (should (equal (gethash "b" result) '(:indexed nil)))
     (should (equal (gethash "c" result) '(:indexed nil)))
     (should (equal (gethash "d" result) '(:indexed t)))
     )))

(ert-deftest org-roam-logseq--buffer-props--empty ()
  (with-temp-buffer
    (insert "")
    (let ((fake-inventory (make-hash-table :test #'equal)))
      (delay-mode-hooks
        (let ((org-inhibit-startup t))
          (org-mode)
          (mocker-let
           ((buffer-file-name (&optional b)
                              ((:input '(nil) :output "dir/note.org")))
            (org-id-new ()
                        ((:output "123456789"))))
           (let ((actual (org-roam-logseq--buffer-props '(:indexed nil) fake-inventory))
                 (expected '(:indexed nil
                             :id-p nil
                             :id "123456789"
                             :title-p nil
                             :title "note"
                             :links nil)))
             (should (equal actual expected)))))))))

(ert-deftest org-roam-logseq--buffer-props--simple ()
  (with-temp-buffer
    (insert "* Typical Logseq page
*")
    (let ((fake-inventory (make-hash-table :test #'equal)))
      (delay-mode-hooks
        (let ((org-inhibit-startup t))
          (org-mode)
          (mocker-let
           ((buffer-file-name (&optional b)
                              ((:input '(nil) :output "dir/note.org")))
            (org-id-new ()
                        ((:output "123456789"))))
           (let ((actual (org-roam-logseq--buffer-props '(:indexed nil) fake-inventory))
                 (expected '(:indexed nil
                             :id-p nil
                             :id "123456789"
                             :title-p nil
                             :title "note"
                             :links nil)))
             (should (equal actual expected)))))))))

(ert-deftest org-roam-logseq--buffer-props--heading-id ()
  (with-temp-buffer
    (insert "* Typical Logseq page
:PROPERTIES:
:ID:   9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:
*")
    (let ((fake-inventory (make-hash-table :test #'equal)))
      (delay-mode-hooks
        (let ((org-inhibit-startup t))
          (org-mode)
          (mocker-let
           ((buffer-file-name (&optional b)
                              ((:input '(nil) :output "dir/note.org")))
            (org-id-new ()
                        ((:output "123456789"))))
           (let ((actual (org-roam-logseq--buffer-props '(:indexed nil) fake-inventory))
                 (expected '(:indexed nil
                             :id-p nil
                             :id "123456789"
                             :title-p nil
                             :title "note"
                             :links nil)))
             (should (equal actual expected)))))))))

(ert-deftest org-roam-logseq--buffer-props--aliases ()
  (with-temp-buffer
    (insert ":PROPERTIES:
:ID:   9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:ROAM_ALIASES:  a \"C d\"
:END:
#+title: Note
#+alias: a, B, c d, e
*")
    (let ((fake-inventory (make-hash-table :test #'equal)))
      (delay-mode-hooks
        (let ((org-inhibit-startup t))
          (org-mode)
          (let ((actual (org-roam-logseq--buffer-props '(:indexed t) fake-inventory))
                (expected '(:indexed t
                            :id-p t
                            :id "9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f"
                            :title-p t
                            :title "Note"
                            :aliases ("b" "e")
                            :links nil)))
            (should (equal actual expected))))))))

(ert-deftest org-roam-logseq--buffer-props--links ()
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
    (let ((fake-inventory (make-hash-table :test #'equal)))
      (puthash "dir/logseq.org" '(:indexed t) fake-inventory)
      (puthash "dir/note.org" '(:indexed nil) fake-inventory)
      (delay-mode-hooks
        (let ((org-inhibit-startup t))
          (org-mode)
          (mocker-let
           ((expand-file-name (p)
                              ((:input '("dir/note.org") :output-generator #'identity))))
           (let ((actual (org-roam-logseq--buffer-props '(:indexed t) fake-inventory))
                 (expected '(:indexed t
                             :id-p t
                             :id "9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f"
                             :title-p t
                             :title "Test note"
                             :links ((file 400 427 "dir/note.org" "[[file:dir/note.org][note]]")
                                     (title 288 299 "logseq" "[[Logseq]] ")))))
             (should (equal actual expected)))))))))

(ert-deftest org-roam-logseq--reverse-map--with-conflicts ()
  (let ((fake-inventory (make-hash-table :test #'equal)))
    (puthash "x" '(:title-p t
                   :title "A"
                   :aliases ("b" "c")) fake-inventory)
    (puthash "y" '(:title-p t
                   :title "B"
                   :aliases ("d" "c")) fake-inventory)
    (puthash "z" '(:title-p t
                   :title "D"
                   :aliases ("e" "f")) fake-inventory)
    (let* ((actual_tuple (org-roam-logseq--reverse-map fake-inventory))
           (actual_reverse_map (nth 0 actual_tuple))
           (actual_conflicts (nth 1 actual_tuple)))
      (should (= (hash-table-count actual_reverse_map) 3))
      (should (equal (gethash "a" actual_reverse_map) "x"))
      (should (equal (gethash "e" actual_reverse_map) "z"))
      (should (equal (gethash "f" actual_reverse_map) "z"))
      (should (equal (seq-map (lambda (c)
                                (cond
                                 ((equal (nth 0 c) "b")
                                  (or (and (equal "x" (nth 1 c)) (equal "y" (nth 4 c)))
                                      (and (equal "y" (nth 1 c)) (equal "x" (nth 4 c)))))
                                 ((equal (nth 0 c) "c")
                                  (or (and (equal "x" (nth 1 c)) (equal "y" (nth 4 c)))
                                      (and (equal "y" (nth 1 c)) (equal "x" (nth 4 c)))))
                                 ((equal (nth 0 c) "d")
                                  (or (and (equal "z" (nth 1 c)) (equal "y" (nth 4 c)))
                                      (and (equal "y" (nth 1 c)) (equal "z" (nth 4 c)))))))
                              actual_conflicts)
                     '(t t t))))))

(ert-deftest org-roam-logseq--filter-links ()
  (let ((links '((file 10 11 "a" "[[file:a][A]]")
                 (title 1 2 "b" "[[B]] ")))
        (inventory (make-hash-table :test #'equal))
        (reverse-map (make-hash-table :test #'equal)))
    (puthash "a" '(:id "x") inventory)
    (puthash "b" '(:id "y") inventory)
    (puthash "b" "b" reverse-map)
    (let ((actual (org-roam-logseq--filter-links links inventory reverse-map))
          (expected '((file 10 11 "a" "[[file:a][A]]" "x")
                      (title 1 2 "b" "[[B]] " "y"))))
      (should (equal actual expected)))))

(ert-deftest org-roam-logseq--update-links ()
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
    (let* ((links '((file 400 427 "dir/note.org" "[[file:dir/note.org][note]]" "A")
                    (title 288 299 "logseq" "[[Logseq]] " "B")))
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
           (actual (org-roam-logseq--update-links links)))
      (should (equal result (buffer-string)))
      (should (eq actual expected)))))

(ert-deftest org-roam-logseq--update-links--faulty ()
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
    (delay-mode-hooks
      (let ((org-inhibit-startup t))
        (org-mode)
        (let* ((links '((file 400 427 "dir/note.org" "[[file:dir/note.org][note]]" "A")
                        (title 288 299 "logseq" "[[Logseq]] " "B")))
               (result (buffer-string)) ;; unchanged
               (expected 'mismatch)
               (actual (org-roam-logseq--update-links links)))
          (should (equal result (buffer-string)))
          (should (eq actual expected)))))))

(ert-deftest org-roam-logseq--update-top--typical ()
  (with-temp-buffer
    (insert "* Typical Logseq page
*")
    (delay-mode-hooks
      (let ((org-inhibit-startup t))
        (org-mode)
        (let* ((id "9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f")
               (aliases '("c d" "b" "a"))
               (title "Test note")
               (result ":PROPERTIES:
:ID:       9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:ROAM_ALIASES: a b \"c d\"
:END:
#+title: Test note

* Typical Logseq page
*")
               (expected t)
               (actual (org-roam-logseq--update-top id title aliases)))
          (should (equal result (buffer-string)))
          (should (eq actual expected)))))))

(ert-deftest org-roam-logseq--update-top--missing-title ()
  (with-temp-buffer
    (insert ":PROPERTIES:
:ID:       9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:
* Typical Logseq page
*")
    (delay-mode-hooks
      (let ((org-inhibit-startup t))
        (org-mode)
        (let* ((title "Test note")
               (result ":PROPERTIES:
:ID:       9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:
#+title: Test note
* Typical Logseq page
*")
               (expected t)
               (actual (org-roam-logseq--update-top nil title nil)))
          (should (equal result (buffer-string)))
          (should (eq actual expected)))))))

;; TODO: test inventory-all

(ert-deftest org-roam-logseq-import ()
  (should nil)
  )

(provide 'org-roam-logseq-tests)
;;; org-roam-logseq-tests.el ends here
