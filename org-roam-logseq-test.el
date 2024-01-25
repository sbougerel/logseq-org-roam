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
    (should (equal (gethash "a" result) nil))
    (should (equal (gethash "b" result) nil))
    (should (equal (gethash "c" result) nil))
    (should (equal (gethash "d" result) nil))))

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
     (should (equal (gethash "a" inventory 'not-found) '(:cache-p t
                                                         :id-p t
                                                         :id "x"
                                                         :title-p t
                                                         :title "A")))
     (should (equal (gethash "b" inventory 'not-found) nil))
     (should (equal (gethash "c" inventory 'not-found) nil))
     (should (equal (gethash "d" inventory 'not-found) '(:cache-p t
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
     (should (equal (gethash "a" inventory 'not-found)
                    '(:modified-p t)))
     (should (equal (gethash "b" inventory 'not-found)
                    nil)))))

(ert-deftest org-roam-logseq--parse-buffer--empty ()
  (with-temp-buffer
    (insert "")
    (delay-mode-hooks
      (let ((org-inhibit-startup t))
        (org-mode)
        (let ((actual (org-roam-logseq--parse-buffer nil))
              (expected nil))
          (should (equal actual expected)))))))

(ert-deftest org-roam-logseq--parse-buffer--simple ()
  (with-temp-buffer
    (insert "* Typical Logseq page
*")
    (delay-mode-hooks
      (let ((org-inhibit-startup t))
        (org-mode)
        (let ((actual (org-roam-logseq--parse-buffer nil))
              (expected nil))
          (should (equal actual expected)))))))

(ert-deftest org-roam-logseq--parse-buffer--title-only ()
  (with-temp-buffer
    (insert "#+title: Test note
* Typical Logseq page
*")
    (delay-mode-hooks
      (let ((org-inhibit-startup t))
        (org-mode)
        (let ((actual (org-roam-logseq--parse-buffer nil))
              (expected '(:first-section-p t
                          :title-point 1
                          :title "Test note")))
          (should (equal actual expected)))))))

(ert-deftest org-roam-logseq--parse-buffer--prop-only ()
  (with-temp-buffer
    (insert ":PROPERTIES:
:ID:   9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:

* Typical Logseq page
*")
    (delay-mode-hooks
      (let ((org-inhibit-startup t))
        (org-mode)
        (let ((actual (org-roam-logseq--parse-buffer nil))
              (expected '(:first-section-p t
                          :title-point 64
                          :id "9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f")))
          (should (equal actual expected)))))))

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
        (let ((actual (org-roam-logseq--parse-buffer nil))
              (expected nil))
          (should (equal actual expected)))))))

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
        (let ((actual (org-roam-logseq--parse-buffer nil))
              (expected '(:first-section-p t
                          :title-point 88
                          :id "9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f"
                          :roam-aliases ("a" "c d")
                          :title "Note"
                          :aliases ("a" "b" "c d" "e"))))
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
        (mocker-let
         ((org-roam-logseq--expand-file (p)
                                        ((:input '("dir/note.org") :output-generator #'identity)))
          (org-roam-logseq--file-p (p)
                                   ((:input '("dir/note.org") :output t :max-occur 2))))
         (let ((actual (org-roam-logseq--parse-buffer nil))
               (expected '(:first-section-p t
                           :title-point 68
                           :id "9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f"
                           :title "Test note"
                           :links ((title 288 298 "Logseq" nil "[[Logseq]]")
                                   (file 400 427 "dir/note.org" "note" "[[file:dir/note.org][note]]")))))
           (should (equal actual expected))))))))

(ert-deftest org-roam-logseq--parse-buffer--idempotent ()
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
        (mocker-let
         ((org-roam-logseq--expand-file (p)
                                        ((:input '("dir/note.org")
                                          :output-generator #'identity
                                          :max-occur 2)))
          (org-roam-logseq--file-p (p)
                                   ((:input '("dir/note.org")
                                     :output t
                                     :max-occur 4))))
         (let* ((expected (org-roam-logseq--parse-buffer nil))
                (actual (org-roam-logseq--parse-buffer expected)))
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

;; TODO: reformat to make it look nicer.
(ert-deftest org-roam-logseq--update-first-section--typical ()
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
      (org-roam-logseq--buffer-title () ((:output "Test note"))))
     (delay-mode-hooks
       (let ((org-inhibit-startup t))
         (org-mode))
       (let* ((plist (org-roam-logseq--parse-buffer nil))
              (result ":PROPERTIES:
:ID:       9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:
#+title: Test note
* Typical Logseq page
*")
              (expected 86)
              (actual (org-roam-logseq--update-first-section plist)))
         (should (equal result (buffer-string)))
         (should (eq actual expected)))))))

(ert-deftest org-roam-logseq--update-first-section--with-alias ()
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
      (org-roam-logseq--buffer-title () ((:output "Test note"))))
     (delay-mode-hooks
       (let ((org-inhibit-startup t))
         (org-mode))
       (let* ((plist (org-roam-logseq--parse-buffer nil))
              (result ":PROPERTIES:
:ID:       9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:ROAM_ALIASES: a b \"c d\"
:END:
#+title: Test note
#+alias: a, b, \"c d\"
* Typical Logseq page
*")
              (expected 111)
              (actual (org-roam-logseq--update-first-section plist)))
         (should (equal result (buffer-string)))
         (should (eq actual expected)))))))

(ert-deftest org-roam-logseq--update-first-section--missing-title ()
  (with-temp-buffer
    (insert ":PROPERTIES:
:ID:       9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:
* Typical Logseq page
*")
    (mocker-let
     ((org-roam-logseq--buffer-title () ((:output "Test note"))))
     (delay-mode-hooks
       (let ((org-inhibit-startup t))
         (org-mode))
       (let* ((plist (org-roam-logseq--parse-buffer nil))
              (result ":PROPERTIES:
:ID:       9f9f9f9f-9f9f-9f9f-9f9f-9f9f9f9f9f9f
:END:
#+title: Test note
* Typical Logseq page
*")
              (expected 19)
              (actual (org-roam-logseq--update-first-section plist)))
         (should (equal result (buffer-string)))
         (should (eq actual expected)))))))

;; TODO: test inventory-all

(ert-deftest org-roam-logseq-import ()
  (should nil)
  )

(provide 'org-roam-logseq-tests)
;;; org-roam-logseq-tests.el ends here
