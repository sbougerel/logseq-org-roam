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

(ert-deftest org-roam-logseq--inventory-init ()
  ;; `org-roam-logseq--files-not-cached' find the difference between
  ;; `org-roam-list-files' and the files present in the org-roam cache which
  ;; we'll have to mock here.
  (let ((result (org-roam-logseq--inventory-init '("a" "b" "c" "d"))))
    (should (hash-table-p result))
    (should (equal (hash-table-count result) 4))
    (should (equal (gethash "a" result) '(:cache-p nil)))
    (should (equal (gethash "b" result) '(:cache-p nil)))
    (should (equal (gethash "c" result) '(:cache-p nil)))
    (should (equal (gethash "d" result) '(:cache-p nil)))))

(ert-deftest org-roam-logseq--inventory-from-cache ()
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
   (let* ((inventory (org-roam-logseq--inventory-init '("a" "b" "c" "d")))
          (count (org-roam-logseq--inventory-from-cache inventory)))
     (should (eq count 2))
     (should (hash-table-p inventory))
     (should (equal (hash-table-count inventory) 4))
     (should (equal (gethash "a" inventory) '(:cache-p t
                                              :id-p t
                                              :id "x"
                                              :title-p t
                                              :title "A"
                                              :aliases nil)))
     (should (equal (gethash "b" inventory) '(:cache-p nil)))
     (should (equal (gethash "c" inventory) '(:cache-p nil)))
     (should (equal (gethash "d" inventory) '(:cache-p t
                                              :id-p t
                                              :id "y"
                                              :title-p t
                                              :title "D"
                                              :aliases ("e")))))))

(ert-deftest org-roam-logseq--inventory-mark-modified ()
  (mocker-let
   ((find-buffer-visiting (f) ((:input '("a") :output 'a)
                               (:input '("b") :output nil)))
    (buffer-modified-p (b) ((:input '(a) :output t))))
   (let* ((inventory (org-roam-logseq--inventory-init '("a" "b")))
          (actual (org-roam-logseq--inventory-mark-modified inventory)))
     (should (eq actual 1))
     (should (hash-table-p inventory))
     (should (equal (hash-table-count inventory) 2))
     (should (equal (gethash "a" inventory)
                    '(:cache-p nil :modified-p t)))
     (should (equal (gethash "b" inventory)
                    '(:cache-p nil))))))

(ert-deftest org-roam-logseq--parse-buffer--empty ()
  (with-temp-buffer
    (insert "")
    (delay-mode-hooks
      (let ((org-inhibit-startup t))
        (org-mode)
        (mocker-let
         ((buffer-file-name (&optional b)
                            ((:input '(nil) :output "dir/note.org")))
          (org-id-new ()
                      ((:output "123456789"))))
         (let ((actual (org-roam-logseq--parse-buffer '(:cache-p nil)))
               (expected '(:cache-p nil
                           :id-p nil
                           :id "123456789"
                           :title-p nil
                           :title "note"
                           :links nil)))
           (should (equal actual expected))))))))

(ert-deftest org-roam-logseq--parse-buffer--simple ()
  (with-temp-buffer
    (insert "* Typical Logseq page
*")
    (delay-mode-hooks
      (let ((org-inhibit-startup t))
        (org-mode)
        (mocker-let
         ((buffer-file-name (&optional b)
                            ((:input '(nil) :output "dir/note.org")))
          (org-id-new ()
                      ((:output "123456789"))))
         (let ((actual (org-roam-logseq--parse-buffer '(:cache-p nil)))
               (expected '(:cache-p nil
                           :id-p nil
                           :id "123456789"
                           :title-p nil
                           :title "note"
                           :links nil)))
           (should (equal actual expected))))))))

(ert-deftest org-roam-logseq--parse-buffer--heading-id ()
  (with-temp-buffer
    (insert "* Typical Logseq page
:PROPERTIES:
:ID:   9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:
*")
    (delay-mode-hooks
      (let ((org-inhibit-startup t))
        (org-mode)
        (mocker-let
         ((buffer-file-name (&optional b)
                            ((:input '(nil) :output "dir/note.org")))
          (org-id-new ()
                      ((:output "123456789"))))
         (let ((actual (org-roam-logseq--parse-buffer '(:cache-p nil)))
               (expected '(:cache-p nil
                           :id-p nil
                           :id "123456789"
                           :title-p nil
                           :title "note"
                           :links nil)))
           (should (equal actual expected))))))))

(ert-deftest org-roam-logseq--parse-buffer--aliases ()
  (with-temp-buffer
    (insert ":PROPERTIES:
:ID:   9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:ROAM_ALIASES:  a \"C d\"
:END:
#+title: Note
#+alias: a, B, c d, e
*")
    (delay-mode-hooks
      (let ((org-inhibit-startup t))
        (org-mode)
        (let ((actual (org-roam-logseq--parse-buffer '(:cache-p t)))
              (expected '(:cache-p t
                          :id-p t
                          :id "9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f"
                          :title-p t
                          :title "Note"
                          :aliases ("b" "e")
                          :links nil)))
          (should (equal actual expected)))))))

(ert-deftest org-roam-logseq--parse-buffer--links ()
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
    (delay-mode-hooks
      (let ((org-inhibit-startup t))
        (org-mode)
        ;; TODO: HERE need to mock org-roam-file-p
        (mocker-let
         ((org-roam-logseq--expand-file (p)
                                        ((:input '("dir/note.org") :output-generator #'identity)))
          (org-roam-logseq--file-p (p)
                                   ((:input '("dir/note.org") :output t :max-occur 2))))
         (let ((actual (org-roam-logseq--parse-buffer '(:cache-p t)))
               (expected '(:cache-p t
                           :id-p t
                           :id "9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f"
                           :title-p t
                           :title "Test note"
                           :links ((file 400 427 "dir/note.org" "note" "[[file:dir/note.org][note]]")
                                   (title 288 298 "Logseq" nil "[[Logseq]]")))))
           (should (equal actual expected))))))))

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
  (let ((links '((file 10 11 "a" "A" "[[file:a][A]]")
                 (title 1 2 "B" nil "[[B]]")))
        (inventory (make-hash-table :test #'equal))
        (reverse-map (make-hash-table :test #'equal)))
    (puthash "a" '(:id "x") inventory)
    (puthash "b" '(:id "y") inventory)
    (puthash "b" "b" reverse-map)
    (let ((actual (org-roam-logseq--filter-links links inventory reverse-map))
          (expected '((file 10 11 "a" "A" "[[file:a][A]]" "x")
                      (title 1 2 "B" nil "[[B]]" "y"))))
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
    (let* ((links '((file 400 427 "dir/note.org" "note" "[[file:dir/note.org][note]]" "A")
                    (title 288 298 "Logseq" nil "[[Logseq]]" "B")))
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
        (let* ((links '((file 400 427 "dir/note.org" "note" "[[file:dir/note.org][note]]" "A")
                        (title 288 298 "Logseq" nil "[[Logseq]]" "B")))
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
