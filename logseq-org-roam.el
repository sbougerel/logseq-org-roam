;;; logseq-org-roam.el --- Logseq Org-roam converter -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2024, Sylvain Bougerel

;; Author: Sylvain Bougerel
;; URL: https://github.com/sbougerel/logseq-org-roam.el/
;; Keywords: tools outlines
;; Version: 1.0.0
;; Package-Requires: ((org-roam "2.2.2") (emacs "27.2") (org "9.3"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; This package provides facilities to convert logseq files to org-roam files,
;; provided Logseq is configured appropriately (see below for more explanation).
;;
;; Characteristics, in order of importance:
;;
;;   - Fast: should handle graphs with a thousand files in less than a second
;;     (when using native compilation),
;;   - Idempotent: can be run multiple times on the same files without changing
;;     them,
;;   - Safe: does not perform destructive operations on your notes, does not leave
;;     files in a broken state, does not delete files.
;;   - Interactive: when visiting an org-roam file, prompt for conversion
;;     (if available),
;;   - Descriptive: logs are printed in a dedicated buffer so you can keep track
;;     of the changes performed,
;;   - Compliant: follows Emacs and Org conventions
;;
;; Speed is primarily achieved firstly by ensuring that during processing, no
;; unecessary work is performed (e.g. no loading of images, rendering of latex,
;; etc) beyond parsing the org AST.  And secondly, by ensuring that all required
;; modifications have been identified before they are performed to reduce the
;; number of passes to the minimum.
;;
;; This package features a single point of entry: `logseq-org-roam' which
;; examines Logseq files under your org-roam directory and converts them to
;; org-roam.
;;
;; What are the rules for import and the resulting actions?
;;
;;   1. A note has no org-id -> org-id will be added and org-roam DB will be
;;      updated
;;
;;   2. A note has no "#+title:" element -> a title element will be created
;;      based on the file name and the org-roam DB will be updated
;;
;;   3. A Logseq link "[[Some link]]" is found and there is no existing org-roam
;;      DB entry corresponding -> create an empty page for it (according to
;;      user's default capture template) and update the link to
;;      "[[id:123456789][Some link]]"
;;
;;   4. A Logseq link "[[Some link]]" is found and there is a corresponding
;;      org-roam DB entry -> update the link to [[id:123456789][Some link]]
;;
;; The ordering of these rules matters, to ensure that when links are being
;; processed (step 3 and 4), all org-id are already set.
;;
;; TODO: rewrite all this below
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BACK UP YOUR LOGSEQ DIR BEFORE RUNNING THIS!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LICENSE
;;
;; This code is a fork of
;;    https://github.com/idanov/logseq-org-roam/ which is
;; licensed under MIT, so this code is also MIT licensed.
;;
;; The work from Ivan Danov is a derivative work of
;;    https://gist.github.com/zot/ddf1a89a567fea73bc3c8a209d48f527
;; by William R. Burdick Jr.
;;
;; The license of the derivative work here is MIT.
;;
;; Logseq compatibility:
;; - change fuzzy links from [[PAGE]] to [[id:2324234234][PAGE]]
;; - change fuzzy links from [[ALIAS]] to [[id:2324234234][ALIAS]]
;; - also change file links to id links, provided that the links
;;   expand to file names that have ids in the roam database.
;;
;; NOTE:
;; - This package converts the links only if they are not alias links due to a bug in Logseq:
;;    https://github.com/logseq/logseq/issues/9342
;;
;; Changes by Sylvain Bougerel from the original work of Ivan Danov & William R. Burdick Jr.:
;;
;; - This package used to avoid putting ID in journal files, however this is
;;   needed for backlinks to journal entries to work in Roam V2.
;;
;; - The hook that runs on opening a file to import it from Logseq avoids empty
;;   buffers.
;;
;; - The hook that runs on opening a file now runs on `org-mode-hook' instead.
;;   Running it on opening a file created a dependency or `org-mode' which could
;;   not be met on time, as `org-element' requires `org-mode'.  This could
;;   sometimes lead to issues with running `org-mode-db-sync'.

;;; Code:
(require 'org)
(require 'org-roam)

(defgroup logseq-org-roam nil
  "Convert Logseq files to `org-roam' files."
  :group 'org-roam)

;;;###autoload (put 'logseq-org-roam-link-types 'safe-local-variable #'symbolp)
(defcustom logseq-org-roam-link-types nil
  "The kind of links `logseq-org-roam' should convert.
Value is a symbol, only the following are recognized:
- \\='files
- \\='fuzzy
- nil or \\='both (default, if unrecognized)

You should customize this value based on your
\":org-mode/insert-file-link?\" setting in Logseq.  Values other
than \\='both save some processing time.

Links considered as candidates to be converted to `org-roam'
ID links are of 2 types:

- File links such as:
  [[file:path/to/pages/page.org][DESCRIPTION]].
- Fuzzy links such as [[TITLE-OR-ALIAS][DESCRIPTION]].


Matching rules for each kind of links are as follows.

When dealing with file links, `logseq-org-roam' ignores links
that do not contain a description since Logseq always populates
it when referencing another page.  It also ignores links that
contain search options since Logseq does not create those.  And
finally it discards any links that is not a link to an `org-roam'
file (since these are not convertible to ID links).

When dealing with fuzzy links, it first ignores dedicated internal
link formats that have specific meaning in `org-mode' (even if
they are broken):

- [[#custom-id]] links,
- [[*heading]] links,
- [[(coderef)]] links,
- [[image.jpg]] inline links to images,

Of the remaining fuzzy links, it discards links that match
internally (as per `org-mode' rules) with:

 - <<targets>> or,
 - #+name: named elements or,
 - a headline by text search,

The leftover links are the candidates to be converted to
`org-roam' external ID links.


Notes on using file links in Logseq.

It is usually recommended to set \":org-mode/insert-file-link?\"
to true in Logseq, presumably to ensure the correct target is
being pointed to.

Unfortunately, Logseq does not always provide a correct path (as
of version 0.10.3) on platforms tested (Android, Linux).  TODO:
replace following by a bug report and link it.

When a note does not exist yet (or when it is aliased, see
`https://github.com/logseq/logseq/issues/9342'), the path
provided by Logseq is an incorrect concatenation of a relative
parent '../' to an absolute path to the root of the graph.
Logseq does not fix this path, even after the page is created.
The user must fix it manually each time.

On the other hand `logseq-org-roam' cares to implement the
complex matching rules set by `org-roam' to convert the right
fuzzy links, making Logseq and `org-roam' mostly interoperable
even when using fuzzy links in Logseq."
  :type 'symbol
  :options '(fuzzy file both)
  :group 'logseq-org-roam)

(defcustom logseq-org-roam-date-like
  (rx string-start (or (seq (= 4 (in digit)) (= 2 (in "_-" blank) (= 2 (in digit))))
                       (seq (= 2 (** 1 2 (in digit)) (in "/._-" blank))
                            (or (= 4 (in digit)) (= 2 (in digit))))
                       (seq (** 1 2 (in digit)) (in blank)
                            (>= 3 (in alpha)) (or (seq (? (in ",")) (in blank)) (in blank))
                            (or (= 4 (in digit)) (= 2 (in digit))))
                       (seq (>= 3 (in alpha)) (in blank)
                            (** 1 2 (in digit)) (or (seq (? (in ",")) (in blank)) (in blank))
                            (or (= 4 (in digit)) (= 2 (in digit))))
                       (seq (>= 3 (in alpha)) (or (seq (? (in ",")) (in blank)) (in blank))
                            (** 1 2 (in digit)) (in blank)
                            (>= 3 (in alpha)) (or (seq (? (in ",")) (in blank)) (in blank))
                            (or (= 4 (in digit)) (= 2 (in digit))))
                       (seq (>= 3 (in alpha)) (or (seq (? (in ",")) (in blank)) (in blank))
                            (>= 3 (in alpha)) (or (seq (? (in ",")) (in blank)) (in blank))
                            (** 1 2 (in digit)) (in blank)
                            (or (= 4 (in digit)) (= 2 (in digit)))))
      string-end)
  "This regexp tells apart strings that look like dates.
It's rather loose by default, but you can set this variable so
that it matches the setting :journal/page-title-format in Logseq
if it does not already.

Note that even if the regex above matches,
`logseq-org-roam-create-translate-default' will still attempt to
parse it with `parse-time-string' and verify that it is actually
a real date (e.g. 2023-02-31 won't be accepted as a date)."
  :type 'string
  :group 'logseq-org-roam)

;;;###autoload (put 'logseq-org-roam-create-translate-link 'safe-local-variable #'symbolp)
(defcustom logseq-org-roam-create-translate
  #'logseq-org-roam-create-translate-default
  "Function translating a fuzzy link to a file path.

By default, it will attempt to translate a fuzzy link path to a
file path under the pages directory (see
`logseq-org-roam-pages-directory').  If the fuzzy link represents
a date, it will translate the link to a path under the journal
directory (see `logseq-org-roam-journals-directory').  To tell
apart dates from other strings, it uses `logseq-org-roam-date-p'.

Special characters in the link path are also replaced, see:
`logseq-org-roam-create-replace'.

Setting this value to nil disables creation of pages for fuzzy
links"
  :type 'symbol
  :group 'logseq-org-roam)

;; TODO: filter files futher from `org-roam-list-files'
;;;###autoload (put 'logseq-org-roam-pages-directory 'safe-local-variable #'string)
(defcustom logseq-org-roam-pages-directory "pages"
  "Set this variable to mirror Logseq :pages-directory setting."
  :type 'string
  :group 'logseq-org-roam)

;;;###autoload (put 'logseq-org-roam-journals-directory 'safe-local-variable #'string)
(defcustom logseq-org-roam-journals-directory "journals"
  "Set this variable to mirror Logseq :journals-directory setting."
  :type 'string
  :group 'logseq-org-roam)

;;;###autoload (put 'logseq-org-roam-create-accept 'safe-local-variable #'symbol)
(defcustom logseq-org-roam-create-accept #'logseq-org-roam-pages-p
  "Tells aparts paths that should be created from paths that should not.
When non-nil, it is called as a function with a single argument:
the path.  It is expected to return a boolean value.  If non-nil,
the path is accepted and the file is created.

The default value (`logseq-org-roam-creat-pages-only') will not
create journal entires.  If you want journal entries to be
created too, you can simply set this to
`logseq-org-roam-create-logseq'.  Note that if you set it to nil,
any non-existant Org file under `org-roam-directory' can be
created."
  :type 'symbol
  :group 'logseq-org-roam)

;;;###autoload
(defcustom logseq-org-roam-updated-hook nil
  "Hook called  by `logseq-org-roam' if any files was updated."
  :type 'hook
  :group 'logseq-org-roam)

(defconst logseq-org-roam--named
  '(babel-call
    center-block
    dynamic-block
    example-block
    export-block
    fixed-width
    footnote-definition
    horizontal-rule
    latex-environment
    paragraph
    plain-list
    quote-block
    special-block
    src-block
    table
    verse-block)
  "List of org-elements that can be affiliated with a :name attribute.")

(defconst logseq-org-roam--log-buffer-name "*Org-roam Logseq*"
  "Name for the log buffer.")

(defcustom logseq-org-roam-create-replace '(("[\\/-.,]" . "_"))
  "Alist specifying replacements for fuzzy links.

Car and cdr of each cons will be given as arguments to
`replace-regexp-in-string' when converting fuzzy links to paths
in `logseq-org-roam-create-translate-default'."
  :type 'alist
  :group 'logseq-org-roam)

;; Aliased file function for mocking as they could interact with the file system
(defalias 'logseq-org-roam--expand-file #'expand-file-name)
(defalias 'logseq-org-roam--find-file-noselect #'find-file-noselect)
(defalias 'logseq-org-roam--insert-file-contents #'insert-file-contents)

(defmacro logseq-org-roam--with-log-buffer (&rest body)
  "Bind standard output to a dedicated buffer for the duration of BODY."
  (declare (debug t))
  `(let* ((standard-output
           (with-current-buffer
               (get-buffer-create logseq-org-roam--log-buffer-name)
             (if (= (point-min) (point-max))
                 (insert "\n\n"))
             (kill-all-local-variables) ;; return to fundamental for logging
             (setq buffer-read-only nil)
             (setq buffer-file-name nil)
             (setq buffer-undo-list t) ;; disable undo
             (setq inhibit-read-only t)
             (setq inhibit-modification-hooks t)
             (current-buffer))))
     (prog1 (progn ,@body)
       (with-current-buffer standard-output
         (setq inhibit-read-only nil)
         (setq buffer-read-only t)
         (setq org-inhibit-startup t)
         (setq org-startup-with-beamer-mode nil)
         (setq org-startup-with-inline-images nil)
         (setq org-startup-with-latex-preview nil)
         (org-mode) ;; back to org-mode
         (goto-char (point-max))))))

(defmacro logseq-org-roam--with-edit-buffer (file &rest body)
  "Find an existing buffer for FILE, set `org-mode' and execute BODY.
If the buffer is new, `org-mode' startup is inhibited.  This
macro does not save the file, but will *always* kill the buffer
if it was previously created."
  (declare (indent 1) (debug t))
  `(let* ((existing-buffer (find-buffer-visiting ,file))
          (buf
           (or existing-buffer
               (let ((auto-mode-alist nil)
                     (find-file-hook nil))
                 (logseq-org-roam--find-file-noselect ,file)))))
     (unwind-protect
         (with-current-buffer buf
           (unless (derived-mode-p 'org-mode)
             (delay-mode-hooks
               (let ((org-inhibit-startup t)) (org-mode))))
           ,@body)
       (unless existing-buffer (kill-buffer buf)))))

(defmacro logseq-org-roam--with-temp-buffer (file &rest body)
  "Visit FILE into an `org-mode' temp buffer and execute BODY.
If the buffer is new, `org-mode' startup is inhibited.  This
macro does not save the file, but will kill the buffer if it was
previously created."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (delay-mode-hooks
       (let ((org-inhibit-startup t)) (org-mode)))
     (logseq-org-roam--insert-file-contents ,file)
     ,@body))

(defmacro logseq-org-roam--catch-fun (sym fun &rest body)
  "Catch SYM during BODY's execution and pass it to FUN."
  (declare (indent 2) (debug t))
  (let ((result (make-symbol "result")))
    `(let ((,result (catch ',sym ,@body)))
       (when (and ,result (symbolp ,result))
         (,fun ,result)))))

(defun logseq-org-roam--fl (file)
  "Make an org link to FILE relative to `org-roam-directory'."
  (format "[[file:%s][%s]]"
          file
          (file-relative-name file org-roam-directory)))

(defun logseq-org-roam-date-p (maybe-date)
  "Return non-nil if MAYBE-DATE is an actual date string.

Used in `logseq-org-roam-create-translate-default'."
  (if-let ((date-like (string-match-p logseq-org-roam-date-like
                                      maybe-date))
           (time (parse-time-string maybe-date))
           (year (nth 5 time))
           (month (nth 4 time))
           (day (nth 3 time))
           (date-string (format "%d-%02d-%02d" year month day))
           (real-date (format-time-string "%Y-%m-%d"
                                          (date-to-time date-string))))
      (equal date-string real-date)))

(defun logseq-org-roam-pages-p (file)
  "Return non-nil if FILE path is under the Logseq pages directory."
  (file-in-directory-p file
                       (expand-file-name logseq-org-roam-pages-directory
                                         org-roam-directory)))

(defun logseq-org-roam-file-p (file)
  "Return non-nil if FILE path is under the Logseq pages directory."
  (or (file-in-directory-p file
                           (expand-file-name logseq-org-roam-pages-directory
                                             org-roam-directory))
      (file-in-directory-p file
                           (expand-file-name logseq-org-roam-journals-directory
                                             org-roam-directory))))

(defun logseq-org-roam--image-file-p (file)
  "Non-nil if FILE is a supported image type."
  ;; This function exists purely because `image-supported-file-p' has made
  ;; `image-type-from-file-name' obsolete since Emacs 29.1; while it is not
  ;; supported by `compat'.  So `image-type-available-p' is just copied here
  ;;
  ;; The function below is a copy from `image.el' distributed with Emacs version
  ;; 29.1.  Copyright (C) 1998-2023 Free Software Foundation, Inc.
  (let ((case-fold-search t)
        type)
    (catch 'found
      (dolist (elem image-type-file-name-regexps)
	(when (and (string-match-p (car elem) file)
                   (image-type-available-p (setq type (cdr elem))))
	  (throw 'found type))))))

(defun logseq-org-roam--value-string-p (element)
  "Return non-nil if ELEMENT has a string value that is not empty."
  (and (org-element-property :value element)
       (not (string-empty-p (org-element-property :value element)))))

(defun logseq-org-roam--inventory-init (files)
  "Initialise inventory with FILES."
  (let ((inventory (make-hash-table :test #'equal)))
    (mapc (lambda (elem) (puthash elem nil inventory)) files)
    inventory))

(defun logseq-org-roam--inventory-from-cache (inventory)
  "Populate INVENTORY with `org-roam' cache for FILES.
Return the number of files whose metadata was retreived from the
cache."
  (let ((count 0)
        (data-cached (org-roam-db-query [:select [file id title]
                                         :from nodes
                                         :where (= 0 level)])))
    (pcase-dolist (`(,file ,id ,title) data-cached)
      ;; TODO: Consider throwing an error if cache is not updated
      (unless (eq 'not-found (gethash file inventory 'not-found))
        (setq count (1+ count))
        (let ((aliases (mapcar #'car
                               (org-roam-db-query [:select [alias] :from aliases
                                                   :where (= node-id $s1)]
                                                  id))))
          (puthash file
                   (append (list :cache-p t :id id)
                           (if (and title (not (string-empty-p title)))
                               (list :title title))
                           (if aliases
                               (list :roam-aliases aliases)))
                   inventory))))
    count))

(defun logseq-org-roam--inventory-mark-external (files inventory)
  "Mark FILES in INVENTORY that are not Logseq files."
  (let ((count 0))
    (mapc (lambda (file)
            (unless (logseq-org-roam-file-p file)
              (setq count (1+ count))
              (let* ((plist (gethash file inventory))
                     (new_plist (plist-put plist :external-p t)))
                (puthash file new_plist inventory))))
          files)
    count))

(defun logseq-org-roam--inventory-mark-modified (files inventory)
  "Update INVENTORY with modified buffer visiting any FILES.
Return the number of files from INVENTORY that are currently
being modified in a buffer."
  (let ((count 0))
    (dolist (file files)
      (when-let* ((existing_buf (find-buffer-visiting file))
                  (mod-p (buffer-modified-p existing_buf)))
        (setq count (1+ count))
        (let* ((plist (gethash file inventory))
               (new_plist (plist-put plist :modified-p t)))
          (puthash file new_plist inventory))))
    count))

(defun logseq-org-roam--parse-first-section-properties (section plist)
  "Return updated PLIST based on first SECTION properties."
  (org-element-map section 'property-drawer
    (lambda (property-drawer)
      ;; Set `:title-point' after the drawer, and reset if there's a title
      (setq plist (plist-put plist :title-point
                             (progn
                               (goto-char
                                (- (org-element-property :end
                                                         property-drawer)
                                   (org-element-property :post-blank
                                                         property-drawer)))
                               (beginning-of-line)
                               (point))))
      (org-element-map property-drawer 'node-property
        (lambda (node-property)
          (let ((key (org-element-property :key node-property)))
            (cond
             ((and (equal "ID" key)
                   (logseq-org-roam--value-string-p node-property))
              (setq plist (plist-put plist :id
                                     (org-element-property :value
                                                           node-property))))
             ((and (equal "ROAM_ALIASES" key)
                   (logseq-org-roam--value-string-p node-property))
              (setq plist (plist-put plist :roam-aliases
                                     (split-string-and-unquote
                                      (downcase
                                       (org-element-property
                                        :value
                                        node-property))))))))))))
  plist)

(defun logseq-org-roam--parse-first-section-keywords (section plist)
  "Return updated PLIST based on first SECTION keywords."
  (org-element-map section 'keyword
    (lambda (keyword)
      (let ((key (org-element-property :key keyword)))
        (cond
         ((equal "TITLE" key)
          (setq plist (plist-put plist :title-point
                                 (org-element-property :begin keyword)))
          (when (logseq-org-roam--value-string-p keyword)
            (setq plist (plist-put plist :title
                                   (org-element-property :value keyword)))))
         ((and (equal "ALIAS" key)
               (logseq-org-roam--value-string-p keyword))
          ;; TODO: support anycase aliases, to ensure they appear in the same
          ;; case in ROAM_ALIASES.  This should be done with computing union /
          ;; diff.
          (setq plist (plist-put plist :aliases
                                 (split-string
                                  (downcase
                                   (org-element-property :value keyword))
                                  "\\s-*,\\s-*"))))))))
  plist)

(defun logseq-org-roam--parse-first-section (data plist)
  "Return updated PLIST based on first section of DATA."
  (declare (pure t) (side-effect-free t))
  (cond
   ((or (not (consp data))
        (not (eq 'org-data (car data))))
    (throw 'fault 'invalid-ast))
   ((or (not (cddr data))
        (not (consp (caddr data)))
        (not (eq 'section (caaddr data))))
    ;; no content or no first section
    plist)
   (t
    (setq plist (plist-put plist :first-section-p t))
    (let ((section (caddr data)))
      (setq plist (logseq-org-roam--parse-first-section-properties section plist))
      (setq plist (logseq-org-roam--parse-first-section-keywords section plist)))
    plist)))

(defun logseq-org-roam--parse-file-links (data plist)
  "Return updated PLIST based on file links in DATA."
  (let ((links (plist-get plist :links)))
    (org-element-map data 'link
      (lambda (link)
        (when (and (equal (org-element-property :type link) "file")
                   (org-element-property :contents-begin link)
                   (not (org-element-property :search-option link)))
          (let* ((path (org-element-property :path link))
                 (true-path (logseq-org-roam--expand-file path)))
            ;; Convert only links that point to org-roam files
            (when (org-roam-file-p true-path)
              (let* ((descr (buffer-substring-no-properties
                             (org-element-property :contents-begin link)
                             (org-element-property :contents-end link)))
                     (begin (org-element-property :begin link))
                     (end (- (org-element-property :end link)
                             (org-element-property :post-blank link)))
                     (raw (buffer-substring-no-properties begin end)))
                (push `(file ,begin ,end ,path ,descr ,raw) links)))))))
    (when links (setq plist (plist-put plist :links links))))
  plist)

(defun logseq-org-roam--parse-fuzzy-links (data plist)
  "Return updated PLIST based on fuzzy links in DATA.
This function ensures that we do not convert fuzzy links that
already match internal links; they take precedence over external
ID links."
  (let ((links (plist-get plist :links))
        (text-targets (make-hash-table :test #'equal)))
    (org-element-map data (append '(link target headline)
                                  logseq-org-roam--named)
      (lambda (element)
        (let ((type (org-element-type element)))
          (if-let ((name (org-element-property :name element)))
              ;; Org-mode searches are case incensitive
              (puthash (downcase name) t text-targets))
          (cond
           ;; See `org-link-search' to understand what fuzzy link point to
           ((eq type 'headline)
            (puthash (downcase
                      ;; TODO: remove internal function dependency
                      (org-link--normalize-string
                       (org-element-property :raw-value element))) t text-targets))
           ((and (eq type 'link)
                 (equal (org-element-property :type element) "fuzzy"))
            (let ((path (org-element-property :path element))
                  (content (org-element-property :contents-begin element)))
              (unless (or
                       ;; "[[*Heading]]" links qualify as fuzzy, ignore them
                       (string-match "\\`\\*" path)
                       ;; When the link has no content, ignore image types
                       (and (not content)
                            (logseq-org-roam--image-file-p path)))
                (let* ((descr (if content
                                  (buffer-substring-no-properties
                                   (org-element-property :contents-begin element)
                                   (org-element-property :contents-end element))))
                       (begin (org-element-property :begin element))
                       (end (- (org-element-property :end element)
                               (org-element-property :post-blank element)))
                       (raw (buffer-substring-no-properties begin end)))
                  (push `(title ,begin ,end ,path ,descr, raw) links)))))
           ((eq type 'target)
            (puthash (downcase (org-element-property :value element)) t text-targets))))))
    ;; Filter out link that match targets, headlines or named elements
    (setq links (seq-filter (lambda (link) (not (gethash (nth 3 link) text-targets)))
                            links))
    (when links (setq plist (plist-put plist :links links))))
  plist)

(defun logseq-org-roam--parse-buffer (plist parts)
  "Return updated PLIST based on current buffer's content.
This function updates PLIST with based on selected PARTS, a list
of keywords which defaults to \\='(first-section file-links
fuzzy-links)."
  (org-with-wide-buffer
   (let* ((data (org-element-parse-buffer)))
     (when (memq 'first-section parts)
       (setq plist (logseq-org-roam--parse-first-section data plist)))
     ;; links are never updated for external files
     (unless (plist-get plist :external-p)
       (when (memq 'file-links parts)
         (setq plist (logseq-org-roam--parse-file-links data plist)))
       (when (memq 'fuzzy-links parts)
         (setq plist (logseq-org-roam--parse-fuzzy-links data plist))))))
  plist)

(defun logseq-org-roam--parse-files (files inventory parts)
  "Populate INVENTORY by parsing content of FILES.
Restrict parsing to PARTS if provided.  Return the number of files
that were parsed."
  (let ((count 0))
    (dolist (file files)
      ;; It's OK if plist is nil!
      (let ((plist (gethash file inventory)))
        (unless (or (plist-get plist :modified-p)
                    (plist-get plist :cache-p)
                    (plist-get plist :parse-error)
                    (plist-get plist :update-error))
          (setq count (1+ count))
          (logseq-org-roam--catch-fun
           'fault (lambda (err)
                    (puthash file
                             (plist-put plist :parse-error err)
                             inventory)))
          (logseq-org-roam--with-temp-buffer file
                                             (let ((new_plist (plist-put plist :hash
                                                                         (secure-hash 'sha256
                                                                                      (current-buffer)))))
                                               (setq new_plist
                                                     (logseq-org-roam--parse-buffer new_plist parts))
                                               (puthash file new_plist inventory))))))
    count))

(defun logseq-org-roam--inventory-all (files force parts)
  "Build inventory of `org-roam' metadata for FILES.
Returns a hashtable mapping absolute paths to a plist describing
relevant metadata to convert Logseq files to `org-roam'.  The
absolute paths point to existing and new `org-roam'
files (presumably created with Logseq).

The argument FORCE ensure that all files are parsed, instead of
relying on information from the `org-roam' cache (in which case,
files already indexed are ever modififed).

The argument PARTS ensures that the function only parses the
necessary parts of each files."
  (princ "** Inventory initially:\n")
  (let* ((start (current-time))
         (inventory (logseq-org-roam--inventory-init files))
         count_cached
         count_external
         count_modified
         count_parsed
         elapsed)
    (unless force
      (setq count_cached
            (logseq-org-roam--inventory-from-cache inventory)))
    (setq count_modified
          (logseq-org-roam--inventory-mark-modified files inventory))
    (setq count_external
          (logseq-org-roam--inventory-mark-external files inventory))
    (setq count_parsed
          (logseq-org-roam--parse-files files inventory parts))
    (setq elapsed (float-time (time-subtract (current-time) start)))
    (princ
     (concat
      (format "%s files found in org-roam directory\n"
              (hash-table-count inventory))
      (unless force
        (format "%s files' metadata retrieved from cache\n"
                count_cached))
      (format "%s files being visited in a modified buffer will be skipped\n"
              count_modified)
      (format "%s files are external to Logseq and will not be modified\n"
              count_external)
      (format "%s files have been parsed\n"
              count_parsed)
      (format "This took %.3f seconds\n" elapsed)))
    inventory))

(defun logseq-org-roam--inventory-update (files inventory parts)
  "Update INVENTORY by reparsing FILES."
  (princ "** Inventory update:\n")
  (let* ((start (current-time))
         count_parsed
         elapsed)
    (setq count_parsed
          (logseq-org-roam--parse-files files inventory parts))
    (setq elapsed (float-time (time-subtract (current-time) start)))
    (princ
     (concat
      (format "%s updated files have been parsed\n"
              count_parsed)
      (format "This took %.3f seconds\n" elapsed)))
    inventory))

(defun logseq-org-roam--calculate-fuzzy-dict (files inventory)
  "Map titles and aliases of FILES to keys in INVENTORY.
This function ensures that titles and aliases found across all
files are unique.  If any 2 titles or alias conflicts with each
other, there is no unique target for titled links to these files.

Return a map of fuzzy links of INVENTORY keys or nil if conflicts
were found."
  (let ((dict (make-hash-table :test #'equal))
        (conflicts (make-hash-table :test #'equal))
        conflicts-p)
    (dolist (file files)
      (when-let ((plist (gethash file inventory)))
        (unless (or (plist-get plist :modified-p)
                    (plist-get plist :parse-error)
                    (plist-get plist :update-error))
          ;; Aliases and title from the same file don't conflict
          (let ((merged (seq-uniq
                         (append
                          (list (downcase (plist-get plist :title)))
                          (plist-get plist :aliases)
                          (plist-get plist :roam-aliases)))))
            (dolist (target merged)
              (if (and (not (gethash target conflicts))
                       (not (gethash target dict)))
                  (puthash target file dict)
                (unless (gethash target conflicts)
                  (puthash target (gethash target dict) conflicts)
                  (puthash target 'conflict dict))
                ;; Log the conflict
                (let* ((this-file file)
                       (this-title-p (equal target (downcase (plist-get plist :title))))
                       (other-file (gethash target conflicts))
                       (other-plist (gethash other-file inventory))
                       (other-title-p (equal target (downcase (plist-get other-plist :title)))))
                  ;; TODO log empty titles too!!
                  (unless conflicts-p
                    (princ "** Issues building dictionary of titles and aliases:\n")
                    (setq conflicts-p t))
                  (princ (concat "- The "
                                 (if this-title-p "title" "alias")
                                 " \"" target "\" in "
                                 (logseq-org-roam--fl this-file)
                                 " conflicts with the "
                                 (if other-title-p "title" "alias")
                                 " in "
                                 (logseq-org-roam--fl other-file)
                                 ", links to \"" target
                                 "\" will not be converted.\n")))))))))
    dict))

(defun logseq-org-roam--buffer-title ()
  "Return a title based on current buffer's file name."
  (file-name-base (buffer-file-name)))

(defun logseq-org-roam--update-first-section (plist)
  "Update current buffer first section based on PLIST.
This function returns the amount of characters added to the
buffer during the update."
  (org-with-wide-buffer
   (let ((start-size (buffer-size))
         (first-section-p (plist-get plist :first-section-p))
         (beg (point-min)))
     (goto-char beg)
     (unless first-section-p
       (insert "\n") ;; External functions should only modify buffer within first section
       (backward-char))
     (unless (plist-get plist :id)
       (org-id-get-create))
     (unless (plist-get plist :title)
       ;; TODO: handle case where title keyword is present but empty
       (let ((title (logseq-org-roam--buffer-title)))
         (goto-char (+ (or (plist-get plist :title-point) beg)
                       (- (buffer-size) start-size)
                       (if first-section-p 0 -1)))
         (unless (bolp) (throw 'fault 'mismatch-before-title))
         (insert (concat "#+title: " title "\n"))))
     (when-let ((diff (seq-difference (plist-get plist :aliases)
                                      (plist-get plist :roam-aliases))))
       (goto-char beg)
       (dolist (alias diff)
         (org-roam-property-add "ROAM_ALIASES" alias)))
     (unless first-section-p
       (goto-char (+ beg (- (buffer-size) start-size) -1))
       (unless (looking-at "\n") (throw 'fault 'mismatch-first-section))
       (delete-char 1)))))

(defun logseq-org-roam--update-links (links inventory fuzzy-dict)
  "Convert LINKS in current buffer to a target in INVENTORY.
This function returns t or an error code if there was an issue
updating the buffer.

The argument FUZZY-DICT is a hash-table needed to map a fuzzy
link target to a key in inventory (a file path).  When dealing
only with file links, this hashtable is not used."
  ;; Even hash has a small chance of collision
  (catch 'fault
    (pcase-dolist (`(_ ,beg ,end _ _ ,raw) links)
      (unless (equal (buffer-substring-no-properties beg end) raw)
        (throw 'fault 'mismatch-link)))
    ;; Avoid offset calculations with buffer updates
    (sort links (lambda (a b) (> (nth 1 a) (nth 1 b))))
    (pcase-dolist (`(,type ,beg ,end ,path ,descr _) links)
      (when-let ((id (if (eq 'file type)
                         (plist-get (gethash (logseq-org-roam--expand-file path)
                                             inventory)
                                    :id)
                       ;; title type: fuzzy-dict's key can be \\='conflict
                       (plist-get (gethash (gethash (downcase path)
                                                    fuzzy-dict)
                                           inventory)
                                  :id))))
        (save-excursion
          (save-restriction
            (narrow-to-region beg end)
            (goto-char beg)
            (delete-region beg end)
            (if descr
                (insert (concat "[[id:" id "][" descr "]]"))
              (insert (concat "[[id:" id "][" path "]]")))))))
    t))

(defun logseq-org-roam--update-all-first-sections (files inventory)
  "Update the first sections of all FILES according to INVENTORY.
After the update is completed, we update inventory with the new information."
  (let (updated-files log-p)
    (princ "** First sections of the following files are updated:\n")
    (dolist (file files)
      (when-let ((plist (gethash file inventory)))
        (unless (or (plist-get plist :modified-p)
                    (plist-get plist :external-p)
                    (plist-get plist :parse-error)
                    (plist-get plist :cache-p)
                    (plist-get plist :update-error)
                    (and (plist-get plist :title)
                         (plist-get plist :id)
                         ;; TODO store diff and union instead?
                         (not (seq-difference (plist-get plist :aliases)
                                              (plist-get plist :roam-aliases)))))
          (logseq-org-roam--catch-fun
           'fault (lambda (err)
                    (puthash file
                             (plist-put plist :update-error err)
                             inventory))
           (logseq-org-roam--with-edit-buffer file
                                              (unless (equal (plist-get plist :hash)
                                                             (secure-hash 'sha256 (current-buffer)))
                                                (throw 'fault 'hash-mismatch))
                                              (logseq-org-roam--update-first-section plist)
                                              (when (buffer-modified-p)
                                                (save-buffer)  ;; NOTE: runs org-roam hook and formatters
                                                (push file updated-files)
                                                (setq log-p t)
                                                (princ (concat "- Updated " (logseq-org-roam--fl file) "\n"))))))))
    (unless log-p
      (princ "No updates found\n"))
    updated-files))

(defun logseq-org-roam--update-all-links (files inventory fuzzy-dict)
  "Update the links in FILES according to INVENTORY.
Return non-nil if any file was updated.

The argument FUZZY-DICT is a hash-tables needed to map a fuzzy
link target to a key in inventory (a file path).  When dealing
only with file links, this hashtable is nil."
  (let (log-p updates-p)
    (princ "** Links of the following files are updated:\n")
    (dolist (file files)
      (when-let ((plist (gethash file inventory)))
        (unless (or (plist-get plist :modified-p)
                    (plist-get plist :external-p)
                    (plist-get plist :parse-error)
                    (plist-get plist :cache-p)
                    (plist-get plist :update-error)
                    (not (plist-get plist :links))) ;; minimal check
          (logseq-org-roam--catch-fun
           'fault (lambda (err)
                    (puthash file
                             (plist-put plist :update-error err)
                             inventory))
           (logseq-org-roam--with-edit-buffer file
                                              (unless (equal (plist-get plist :hash)
                                                             (secure-hash 'sha256 (current-buffer)))
                                                (throw 'fault 'hash-mismatch))
                                              (logseq-org-roam--update-links plist inventory fuzzy-dict)
                                              (when (buffer-modified-p)
                                                (save-buffer) ;; NOTE: important to run org-roam hook
                                                (setq updates-p t)
                                                (setq log-p t)
                                                (princ (concat "- Updated " (logseq-org-roam--fl file) "\n"))))))))
    (unless log-p
      (princ "No links to update\n"))
    updates-p))

(defun logseq-org-roam-create-translate-default (slug)
  "Transform SLUG into a file path."
  (let ((normalized slug))
    (pcase-dolist (`(,regex . ,rep) logseq-org-roam-create-replace)
      (setq normalized (replace-regexp-in-string regex rep normalized)))
    (concat
     (logseq-org-roam--expand-file
      normalized (logseq-org-roam--expand-file
                  (if (logseq-org-roam-date-p slug)
                      logseq-org-roam-journals-directory
                    logseq-org-roam-pages-directory)
                  org-roam-directory))
     ".org")))

(defun logseq-org-roam--create-from (files inventory fuzzy-dict)
  "Author new files for dead links present in FILES.
INVENTORY and FUZZY-DICT are used for dead links detection with
file links and fuzzy links respectively.  Any dead links is
considered a candidate for creation of new files.

Fuzzy links are first transformed to an expended path by calling
`logseq-org-roam-create-translate' (via `funcall') with the link
path as an argument.

If file links are expanded against the parent directory of the
file containing them.

The path is given as the first argument to
`logseq-org-roam-create-accept' (via `funcall') when it is
non-nil.  Note that by default, journal entries will not be
created (see `logseq-org-roam-create-accept'), only pages will.

The resulting path must match `org-roam-file-p', it's
parent directory must exist, and the file must not exist.

Finally, for file links only, the description is used as the
title for the new file.

When the file is created, it is inserted an Org ID and a title,
then saved.  There is no support for templates at the moment.

Return the list of new files created."
  ;; TODO: add more logs to explain why links are not created.
  (let (created-files)
    (princ "** The following files are created:\n")
    (dolist (file files)
      (when-let ((plist (gethash file inventory)))
        (unless
            (or (not plist)
                (plist-get plist :modified-p)
                (plist-get plist :external-p)
                (plist-get plist :parse-error)
                (plist-get plist :cache-p)
                (plist-get plist :update-error)
                (not (plist-get plist :links)))
          (pcase-dolist (`(,type _ _ ,path ,descr _) (plist-get plist :links))
            (when (if (eq type 'file)
                      (eq 'not-found (gethash
                                      (expand-file-name path
                                                        (file-name-directory file))
                                      inventory 'not-found))
                    (and
                     (eq 'not-found (gethash (downcase path)
                                             fuzzy-dict 'not-found))
                     logseq-org-roam-create-translate))
              (let ((new-path
                     (if (eq type 'file)
                         (expand-file-name path
                                           (file-name-directory file))
                       (expand-file-name
                        (funcall logseq-org-roam-create-translate path)
                        (file-name-directory file))))
                    (new-title (if (eq type 'file) descr path)))
                (unless (or (file-exists-p new-path)
                            (not (org-roam-file-p new-path)))
                  (logseq-org-roam--with-edit-buffer new-path
                                                     (org-id-get-create)
                                                     (goto-char (point-max))
                                                     (insert (concat "#+title: " new-title "\n"))
                                                     (save-buffer))
                  (princ (concat "- Created " (logseq-org-roam--fl new-path)
                                 " from the " (if (eq type 'file) "file" "fuzzy")
                                 " link in " (logseq-org-roam--fl new-path) "\n"))
                  (push new-path created-files))))))))
    (unless created-files
      "No files were created\n")
    created-files))

(defun logseq-org-roam--log-start (force create)
  "Log start of execution and state of FORCE and CREATE flags."
  ;; TODO: log other settings
  (princ
   (concat
    (format "* Ran %s\n" (format-time-string "%x at %X"))
    (format "Using Org-roam directory: %s\n" org-roam-directory)
    (format "Logseq pages directory is: %s\n" logseq-org-roam-pages-directory)
    (format "Logseq journal directory is: %s\n" logseq-org-roam-journals-directory)
    "With flags:\n"
    (format "- ~force~ was: %s\n" force)
    (format "- ~create~ was: %s\n" create)
    "With settings:\n"
    (format "- ~logseq-org-roam-link-types~: %S\n" logseq-org-roam-link-types)
    "\n")))

(defun logseq-org-roam--check-errors (files inventory)
  "Throw when FILES in INVENTORY have errors."
  (dolist (file files)
    (when-let ((plist (gethash file inventory)))
      (when (or (plist-get plist :modified-p)
                (plist-get plist :parse-error)
                (plist-get plist :update-error))
        (throw 'stop 'error-encountered)))))

(defun logseq-org-roam--log-errors (files inventory)
  "Log parsing issues with FILES in INVENTORY.
Return non-nil if issues where found."
  (let (error-p modified)
    (dolist (file files)
      (when-let ((plist (gethash file inventory)))
        (when (plist-get plist :modified-p)
          (push file modified))
        (when-let ((err (plist-get plist :parse-error)))
          (unless error-p
            (princ "** Verify the syntax in the following files:\n")
            (setq error-p t))
          (princ (concat "- Error " (format "%s" err)
                         " parsing " (logseq-org-roam--fl file) ", skipped\n")))
        (when-let ((err (plist-get plist :update-error)))
          (unless error-p
            (princ "** Verify the syntax in the following files:\n")
            (setq error-p t))
          (princ (concat "- Error " (format "%s" err)
                         " updating " (logseq-org-roam--fl file)
                         ", left as-is\n")))))
    (when modified
      (princ "** Save the following files before continuing:\n")
      (dolist (file modified)
        (princ (concat "- " (logseq-org-roam--fl file)))))
    (when (or modified error-p)
      (princ (concat "** Stopped with errors:
Please save any work in progress or fix syntax issues in the
files mentionned above before re-running the function again.
Errors can impact the accuracy of file creation or link
conversion.\n\n")))))

(defun logseq-org-roam--sanity-check ()
  "Check that `org-roam' is installed and configured."
  (cond
   ((not (featurep 'org-roam))
    (display-warning 'logseq-org-roam
                     "`org-roam' is not installed" :error) nil)
   (org-roam-directory
    (display-warning 'logseq-org-roam
                     "`org-roam-directory' is not set" :error) nil)
   ((not (file-directory-p org-roam-directory))
    (display-warning 'logseq-org-roam
                     "`org-roam-directory' is not a directory" :error) nil)
   ;; TODO sanity check that pages and journal can be found
   (t))

;;;###autoload
  (defun logseq-org-roam (&optional mode)
    "Migrate files edited with Logseq to `org-roam'.
Parse files returned by `org-roam-list-files' that are not part
of the `org-roam' index, and if it finds files that were created
by Logseq, it updates these files to set ID, title, aliases, or
convert links to other files by using ID-links.

Please note that while care was taken to ensure its reliability,
you should have a mean to revert the changes it makes on your
files (e.g. backup or version control) before running this
function.

While using this facility regularly makes `org-roam' and Logseq
mostly interoperable; ID-links in Logseq show up as \"Unlinked
Reference\" and thus break the connection in your Logseq graph.
If you primarily rely on Logseq, this could be a deal-breaker.

When called with \\[universal-argument] or MODE is not nil, it
parses any files returned by `org-roam-list-files' (even if they
are already indexed by `org-roam').  This is useful if you've had
a lot of edits with Logseq (or other), and some of the files that
are in `org-roam' may still contain links to other files that are
not converted to ID-links yet.

When `logseq-org-roam' encounters a link created with Logseq to a
page that does not exists yet, it can create that page for you,
leveraging your `org-roam-capture-templates', as you would if you
were using the normal `org-roam' workflow.  This mode of
operation is enabled when `logseg-to-roam-create' is t or when
calling this function with (double) \\[universal-argument]
\\[universal-argument].

To active both creation and forcing to parse all files returned
by `org-roam-list-files' call this function with (triple)
\\[universal-argument] \\[universal-argument]
\\[universal-argument].

When calling `logseq-org-roam' programmatically it accepts the
following arguments:

nil: parse only files that are not yet indexed (by `org-roam')
  and does not create any new files (when it encounters a link
  created by Logseq without an existing target).

\\='(4) or 4 or \\='force: parse all files (even those already
  indexed) and does not create any new files.  Equivalent to
  \\[universal-argument] \\[logseq-org-roam].

\\='(16) or 16 or \\='create: parse only files that are not yet
  indexed and create new files using your capture templates (when
  it encounters a Logseq link without target).  Equivalent to
  \\[universal-argument] \\[universal-argument]
  \\[logseq-org-roam].

\\='(64) or 64 or \\='force-create: parse all files and create new
  files using your capture templates.  Equivalent to
  \\[universal-argument] \\[universal-argument]
  \\[universal-argument] \\[logseq-org-roam].

To find out how `logseq-org-roam' dectect Logseq links, read the
documentation string of `logseq-org-roam-link-types'.  To find
out how `logseq-org-roam' uses your own capture templates, read
the documentation string of `logseq-org-roam-capture'."
    (interactive "P")
    (unless (logseq-org-roam--sanity-check)
      (let (force_flag create_flag)
        (cond
         ((or (equal mode '(4))
              (eq mode 4)
              (eq mode 'force))
          (setq force_flag t))
         ((or (equal mode '(16))
              (eq mode 16)
              (eq mode 'create))
          (setq create_flag t))
         ((or (equal mode '(64))
              (eq mode 64)
              (eq mode 'force-create))
          (setq force_flag t)
          (setq create_flag t)))
        (logseq-org-roam--with-log-buffer
         (logseq-org-roam--log-start force_flag create_flag)
         ;; Main flow
         ;;
         ;; - inventory files fully
         ;; - update first-sections and re-parse where needed
         ;; - (optionally) author new files where needed and re-parse
         ;; - update links
         ;;
         ;; 2 factors make the implementation rather complex:
         ;;
         ;; - On save, hooks may reformat the buffer in unexpected ways, thus
         ;; it's safer to reparse every time the files are modified.
         ;; - Creation adds complexity since we must parse the entire
         ;; content for all files to discover dead links first.
         (let* ((files (org-roam-list-files))
                (link-parts (cond ((eq logseq-org-roam-link-types 'files)
                                   '(file-links))
                                  ((eq logseq-org-roam-link-types 'fuzzy)
                                   '(fuzzy-links))
                                  (t '(file-links fuzzy-links))))
                inventory
                modified-files
                created-files
                not-created-files
                fuzzy-dict)
           (logseq-org-roam--catch-fun 'stop
                                       (lambda (_)
                                         (logseq-org-roam--log-errors files inventory)
                                         (display-warning 'logseq-org-roam
                                                          (concat "Stopped with errors, see "
                                                                  logseq-org-roam--log-buffer-name
                                                                  " buffer")
                                                          :error))
                                       (setq inventory
                                             (logseq-org-roam--inventory-all
                                              files force_flag (append '(first-section) link-parts)))
                                       (setq modified-files
                                             (logseq-org-roam--update-all-first-sections files inventory))
                                       (logseq-org-roam--inventory-update modified-files inventory
                                                                          (append '(first-section) link-parts))
                                       (when (memq 'fuzzy-links link-parts)
                                         (setq fuzzy-dict (logseq-org-roam--calculate-fuzzy-dict files inventory)))
                                       ;; Do as much work as possible, but beyond this point, the errors
                                       ;; (modified files, parse or update errors) could affect accuracy
                                       ;; of the changes
                                       (logseq-org-roam--check-errors files inventory)
                                       (setq not-created-files files)
                                       (when create_flag
                                         (setq created-files
                                               (logseq-org-roam--create-from files inventory
                                                                             fuzzy-dict))
                                         (logseq-org-roam--inventory-update created-files inventory
                                                                            ;; No links added to new files
                                                                            '(first-section))
                                         (logseq-org-roam--check-errors created-files inventory)
                                         (setq files (append created-files files)))
                                       (when (logseq-org-roam--update-all-links not-created-files
                                                                                inventory fuzzy-dict)
                                         (run-hooks 'logseq-org-roam-updated-hook))
                                       (logseq-org-roam--check-errors files inventory)
                                       ;; TODO: Add summary of results
                                       )))))))

(provide 'logseq-org-roam)
;;; logseq-org-roam.el ends here
