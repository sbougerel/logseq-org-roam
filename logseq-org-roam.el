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
- nil (default, if unrecognized)

You should customize this value based on your
\":org-mode/insert-file-link?\" setting in Logseq.  Values other
than nil save some processing time.

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

;;;###autoload (put 'logseq-org-roam-journals-file-name-format 'safe-local-variable #'string)
(defcustom logseq-org-roam-journals-file-name-format "%Y-%m-%d"
  "Set this variable to mirror Logseq :journal/file-name-format setting.
You should pick a format that `logseq-org-roam-maybe-date-func'
can use.  Otherwise, titles for journal entries will not be
formated correctly: `logseq-org-roam' first parses the file name
into a time before feeding it back to `format-time-string' to
create the title (See: `logseq-org-roam-jounals-title-format')."
  :type 'string
  :group 'logseq-org-roam)

;;;###autoload (put 'logseq-org-roam-journals-title-format 'safe-local-variable #'string)
(defcustom logseq-org-roam-journals-title-format "%Y-%m-%d"
  "Set this variable to mirror Logseq :journal/file-name-format setting.
This is used to create a title for journal entires and to find
out which fuzzy links point to journal entries (See
`logseq-org-roam-maybe-date-func').

You can set this to any format that `format-time-string' accepts.
However, you should only use it to create date strings, and not
time strings.  Generating hours and seconds in the format will
make it impossible to find out journal entries from fuzzy links."
  :type 'string
  :group 'logseq-org-roam)

;;;###autoload (put 'logseq-org-roam-maybe-date-func 'safe-local-variable #'symbolp)
(defcustom logseq-org-roam-maybe-date-func
  #'logseq-org-roam-maybe-date-default
  "Try parsing a string into a date and return time when successful.
When non-nil, this variable is called with `funcall'.  It is
given 2 arguments: the first is a time format for
`format-time-string', the second is the string to evaluate.  Tt
is expected to return a time, like `date-to-time' or
`encode-time'.  If the time returned is 0, it assumes that the
string is not a date.  See `logseq-org-roam-maybe-date-default'
for a description of the default behaviour.

If nil, date parsing is disabled."
  :type 'string
  :group 'logseq-org-roam)

;;;###autoload (put 'logseq-org-roam-create-translate-func 'safe-local-variable #'symbolp)
(defcustom logseq-org-roam-create-translate-func
  #'logseq-org-roam-create-translate-default
  "Function translating a fuzzy link to a file path.
When non-nil, it is called with `funcall' and a single argument,
the fuzzy link.  It is expected to return an absolute file path.
This variable provide complete control over how fuzzy links are
translated to file paths.

Default to `logseq-org-roam-create-translate-default'.  Setting
this value to nil disables creation of pages for fuzzy links."
  :type 'symbol
  :group 'logseq-org-roam)

;;;###autoload (put 'logseq-org-roam-create-accept-func 'safe-local-variable #'symbol)
(defcustom logseq-org-roam-create-accept-func #'logseq-org-roam-pages-p
  "Tells aparts paths that should be created from paths that should not.
When non-nil, it is called as a function with a single argument:
the path.  When the return value is non-nil, the path is accepted
and the file is created.

The default value (`logseq-org-roam-pages-p') will not create
journal entires.  If you want journal entries to be created too,
you can set this to `logseq-org-roam-logseq-p'.  If you want to
allow files to be created anywhere, you can set this to `always'.

When set to nil, creation is disabled."
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

(defconst logseq-org-roam--log-buffer-name "*Logseq Org-roam %s*"
  "Name for the log buffer.
'%s' will be replaced by `org-roam-directory' if present")

(defcustom logseq-org-roam-create-replace '(("[\\/]" . "_"))
  "Alist specifying replacements for fuzzy links.

Car and cdr of each cons will be given as arguments to
`replace-regexp-in-string' when converting fuzzy links to paths
in `logseq-org-roam-create-translate-default'."
  :type 'alist
  :group 'logseq-org-roam)

(defmacro logseq-org-roam--with-log-buffer (&rest body)
  "Bind standard output to a dedicated buffer for the duration of BODY."
  (declare (debug t))
  `(let* ((standard-output
           (with-current-buffer
               (get-buffer-create
                ;; One buffer per org-roam-directory
                (format logseq-org-roam--log-buffer-name
                        org-roam-directory))
             (kill-all-local-variables) ;; return to fundamental for logging
             (setq default-directory org-roam-directory)
             (setq buffer-read-only nil)
             (setq buffer-file-name nil)
             (setq buffer-undo-list t)
             (goto-char (point-max))
             (if (/= (point-min) (point-max))
                 (insert "\n\n"))
             (current-buffer))))
     (prog1 (progn ,@body)
       (with-current-buffer standard-output
         (goto-char (point-max))
         (insert "You can set this buffer to `org-mode' to navigate links\n")
         (setq buffer-read-only t)))))

(defmacro logseq-org-roam--with-edit-buffer (file &rest body)
  "Find an existing buffer for FILE, set `org-mode' and execute BODY.
If the buffer is new, `org-mode' startup is inhibited.  This
macro does not save the file, but will *always* kill the buffer
if it was previously created."
  (declare (indent 1) (debug t))
  (let ((exist-buf (make-symbol "exist-buf"))
        (buf (make-symbol "buf"))
        (bimf (make-symbol "bimf"))
        (biro (make-symbol "biro")))
    `(let* ((,bimf inhibit-modification-hooks)
            (,biro inhibit-read-only)
            (,exist-buf (find-buffer-visiting ,file))
            (,buf
             (or ,exist-buf
                 (let ((auto-mode-alist nil)
                       (find-file-hook nil))
                   (find-file-noselect ,file)))))
       (unwind-protect
           (with-current-buffer ,buf
             (setq inhibit-read-only t)
             (setq inhibit-modification-hooks (if ,exist-buf t ,bimf))
             (unless (derived-mode-p 'org-mode)
               (let ((org-inhibit-startup t)) (org-mode)))
             ,@body)
         (setq inhibit-modification-hooks ,bimf)
         (setq inhibit-read-only ,biro)
         (unless ,exist-buf (kill-buffer ,buf))))))

(defmacro logseq-org-roam--with-temp-buffer (file &rest body)
  "Visit FILE into an `org-mode' temp buffer and execute BODY.
If the buffer is new, `org-mode' startup is inhibited.  This
macro does not save the file, but will kill the buffer if it was
previously created."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     ;; relative path expansion needs this
     (setq default-directory (file-name-directory ,file))
     (delay-mode-hooks
       (let ((org-inhibit-startup t)) (org-mode)))
     (insert-file-contents ,file)
     ,@body))

;; TODO: test
(defmacro logseq-org-roam--catch-fun (sym errs fun &rest body)
  "Catch ERRS for SYM during BODY's execution and pass it to FUN."
  (declare (indent 3) (debug t))
  (let ((result (make-symbol "result")))
    `(let ((,result (catch ,sym ,@body)))
       (if (and ,result
                (symbolp ,result)
                (memq ,result ,errs))
           (,fun ,result)))))

(defun logseq-org-roam--fl (file)
  "Make an org link to FILE relative to `org-roam-directory'."
  (format "[[file:%s][%s]]"
          file
          (file-relative-name file org-roam-directory)))

(defun logseq-org-roam-pages-p (file)
  "Return non-nil if FILE path is under the Logseq pages directory."
  (string= (directory-file-name (file-name-directory file))
           (expand-file-name logseq-org-roam-pages-directory
                             org-roam-directory)))

(defun logseq-org-roam-journals-p (file)
  "Return non-nil if FILE path is under the Logseq journal directory."
  (string= (directory-file-name (file-name-directory file))
           (expand-file-name logseq-org-roam-journals-directory
                             org-roam-directory)))

(defun logseq-org-roam-logseq-p (file)
  "Return non-nil if FILE path is under the Logseq journal or pages directory."
  (or (logseq-org-roam-pages-p file)
      (logseq-org-roam-journals-p file)))

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
	(if (and (string-match-p (car elem) file)
                 (image-type-available-p (setq type (cdr elem))))
	    (throw 'found type))))))

(defun logseq-org-roam--value-string-p (element)
  "Return non-nil if ELEMENT has a string value that is not empty."
  (and (org-element-property :value element)
       (not (string-empty-p (org-element-property :value element)))))

(defun logseq-org-roam-maybe-date-default (date-format maybe-date)
  "Turn MAYBE-DATE into time if it matches DATE-FORMAT.

Attempts to parse MAYBE-DATE with `parse-time-string' first and
convert it back to a string with `format-time-string' using
DATE-FORMAT.  If both string match, it is taken as a journal
date, and the corresponding time is returned."
  ;; TODO: Compile a reverse regex to format-time-string?
  ;; NOTE: hack because '_' are not ISO date chars
  (let* ((hacked-date (replace-regexp-in-string "_" "-" maybe-date))
         (parsed (parse-time-string hacked-date))
         (year (nth 5 parsed))
         (month (nth 4 parsed))
         (day (nth 3 parsed))
         (time (safe-date-to-time hacked-date)))
    (if (and year month day
             (string= (format-time-string date-format time)
                      maybe-date))
        time
      0)))

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
            (unless (logseq-org-roam-logseq-p file)
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
             ((and (string= "ID" key)
                   (logseq-org-roam--value-string-p node-property))
              (setq plist (plist-put plist :id
                                     (org-element-property :value
                                                           node-property))))
             ((and (string= "ROAM_ALIASES" key)
                   (logseq-org-roam--value-string-p node-property))
              (setq plist (plist-put plist :roam-aliases
                                     (split-string-and-unquote
                                      (org-element-property
                                       :value
                                       node-property)))))))))))
  plist)

(defun logseq-org-roam--parse-first-section-keywords (section plist)
  "Return updated PLIST based on first SECTION keywords."
  (org-element-map section 'keyword
    (lambda (keyword)
      (let ((key (org-element-property :key keyword)))
        (cond
         ((string= "TITLE" key)
          (setq plist (plist-put plist :title-point
                                 (org-element-property :begin keyword)))
          (if (logseq-org-roam--value-string-p keyword)
              (setq plist (plist-put plist :title
                                     (org-element-property :value keyword)))))
         ((and (string= "ALIAS" key)
               (logseq-org-roam--value-string-p keyword))
          (setq plist (plist-put plist :aliases
                                 (split-string
                                  (org-element-property :value keyword)
                                  "\\s-*,\\s-*"))))))))
  plist)

(defun logseq-org-roam--parse-first-section (data plist)
  "Return updated PLIST based on first section of DATA."
  (declare (pure t) (side-effect-free t))
  (cond
   ((or (not (consp data))
        (not (eq 'org-data (car data))))
    (throw 'parse-error 'invalid-ast))
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
        (if (and (string= (org-element-property :type link) "file")
                 (org-element-property :contents-begin link)
                 (not (org-element-property :search-option link)))
            (let* ((path (org-element-property :path link))
                   (descr (buffer-substring-no-properties
                           (org-element-property :contents-begin link)
                           (org-element-property :contents-end link)))
                   (begin (org-element-property :begin link))
                   (end (- (org-element-property :end link)
                           (org-element-property :post-blank link)))
                   (raw (buffer-substring-no-properties begin end)))
              (push (list 'file begin end path descr raw) links)))))
    (if links (setq plist (plist-put plist :links links))))
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
                      ;; This function skips tasks, priority, a statistics cookies
                      (org-link--normalize-string
                       (org-element-property :raw-value element))) t text-targets))
           ((and (eq type 'link)
                 (string= (org-element-property :type element) "fuzzy"))
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
                  (push (list 'fuzzy begin end path descr raw) links)))))
           ((eq type 'target)
            (puthash (downcase (org-element-property :value element)) t text-targets))))))
    ;; Filter out link that match targets, headlines or named elements
    (setq links (seq-filter (lambda (link) (not (gethash (nth 3 link) text-targets)))
                            links))
    (if links (setq plist (plist-put plist :links links))))
  plist)

(defun logseq-org-roam--parse-buffer (plist parts)
  "Return updated PLIST based on current buffer's content.
This function updates PLIST with based on selected PARTS, a list
of keywords which defaults to \\='(first-section file-links
fuzzy-links)."
  (org-with-wide-buffer
   (let* ((data (org-element-parse-buffer)))
     (if (memq 'first-section parts)
         (setq plist (logseq-org-roam--parse-first-section data plist)))
     ;; links are never updated for external files
     (unless (plist-get plist :external-p)
       (if (memq 'file-links parts)
           (setq plist (logseq-org-roam--parse-file-links data plist)))
       (if (memq 'fuzzy-links parts)
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
          (logseq-org-roam--catch-fun
              'parse-error '(invalid-ast)
              (lambda (err)
                (princ (concat "- Error parsing " (logseq-org-roam--fl file) "\n"))
                (puthash file
                         (plist-put plist :parse-error err)
                         inventory))
            (logseq-org-roam--with-temp-buffer file
              (let ((new_plist (plist-put plist :hash
                                          (secure-hash
                                           'sha256 (current-buffer)))))
                (setq new_plist
                      (logseq-org-roam--parse-buffer new_plist parts))
                (puthash file new_plist inventory)
                (princ (concat "- Parsed " (logseq-org-roam--fl file) "\n"))
                (setq count (1+ count))))))))
    count))

(defun logseq-org-roam--inventory-all (files inventory force parts)
  "Build inventory of `org-roam' metadata for FILES.
Update INVENTORY (hashtable) with a plist describing relevant
metadata to convert Logseq files to `org-roam'.  The
keys (absolute paths) point to both existing and new `org-roam'
files (presumably created with Logseq).

The argument FORCE ensure that all files are parsed, instead of
relying on information from the `org-roam' cache (in which case,
files already indexed are ever modififed).

The argument PARTS ensures that the function only parses the
necessary parts of each files.

Returns the number of files parsed without error."
  (princ "** Initial inventory:\n")
  (let* (count_cached
         count_external
         count_modified
         count_parsed)
    (unless force
      (setq count_cached
            (logseq-org-roam--inventory-from-cache inventory)))
    (setq count_modified
          (logseq-org-roam--inventory-mark-modified files inventory))
    (setq count_external
          (logseq-org-roam--inventory-mark-external files inventory))
    (setq count_parsed
          (logseq-org-roam--parse-files files inventory parts))
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
      (format "%s files have been parsed without errors\n"
              count_parsed)))
    count_parsed))

(defun logseq-org-roam--inventory-update (files inventory parts)
  "Update INVENTORY by reparsing FILES."
  (princ "** Inventory update:\n")
  (let* (count_parsed)
    (setq count_parsed
          (logseq-org-roam--parse-files files inventory parts))
    (princ
     (concat
      (format "%s files have been parsed without errors\n"
              count_parsed)))))

(defalias 'logseq-org-roam--normalize-text (symbol-function #'downcase)
  "Return a normalized version of TEXT.")
;; Maintain optimizations
(put 'logseq-org-roam--normalize-text 'side-effect-free t)
(put 'logseq-org-roam--normalize-text 'byte-compile 'byte-compile-one-arg)
(put 'logseq-org-roam--normalize-text 'byte-opcode 'byte-downcase)

(defun logseq-org-roam--fill-fuzzy-dict (fuzzy-dict files inventory)
  "Fill titles and aliases of FILES into FUZZY-DICT.
Map each title and alias to a key in INVENTORY or to a `cons'
containing a key to INVENTORY when a conflict is found.

Ensure that titles and aliases found across all files are unique.
If any 2 titles or alias conflicts with each other, there is no
unique target for titled links to these files.

Returns the number of conflicts found"
  (princ "** Filling dictionary of titles and aliases:\n")
  (let ((conflict-count 0))
    (dolist (file files)
      (when-let ((plist (gethash file inventory)))
        (unless (or (plist-get plist :modified-p)
                    (plist-get plist :parse-error)
                    (plist-get plist :update-error))
          ;; NOTE: Similar title and aliases from the same file are not marked as conflict
          ;; TODO: cl-* could be faster
          (let ((merged (seq-uniq
                         (seq-map #'logseq-org-roam--normalize-text
                                  (append
                                   (list (plist-get plist :title))
                                   (plist-get plist :aliases)
                                   (plist-get plist :roam-aliases)))
                         #'string=)))
            (dolist (target merged)
              (let ((val (gethash target fuzzy-dict 'not-found))
                    other-file)
                (if (eq 'not-found val)
                    (puthash target file fuzzy-dict)
                  (setq conflict-count (1+ conflict-count))
                  (if (consp val)
                      (setq other-file (car val))
                    (setq other-file val)
                    (puthash target (cons other-file) fuzzy-dict))
                  ;; Log the conflict
                  (let* ((this-title-p (string= target (logseq-org-roam--normalize-text
                                                        (plist-get plist :title))))
                         (other-plist (gethash other-file inventory))
                         (other-title-p (string= target (logseq-org-roam--normalize-text
                                                         (plist-get other-plist :title)))))
                    (princ (format "- The %s \"%s\" in %s conflicts with the %s in %s, links will not be converted.\n"
                                   (if this-title-p "title" "alias")
                                   target
                                   (logseq-org-roam--fl file)
                                   (if other-title-p "title" "alias")
                                   (logseq-org-roam--fl other-file)))))))))))
    (princ (format "%s entries in total\n" (hash-table-count fuzzy-dict)))
    (princ (format "%s conflicts\n" conflict-count))
    conflict-count))

(defun logseq-org-roam--normalize-path (path)
  "Return a new PATH that is normalized.
The file base portion of path will be downcased, and lowbar
repetitions will be removed.  This helps with mapping
\"triple-lowbar\" setting in Logseq to file slug created by
`org-roam'."
  (let ((dir (file-name-directory path))
        (ext (file-name-extension path))
        (base (file-name-base path)))
    (concat dir
            (replace-regexp-in-string "_+" "_" (downcase base))
            "." ext)))

(defun logseq-org-roam--fill-file-dict (file-dict files)
  "Fill FILE-DICT with the normalized path of each FILES.
Maps each normalized path to the original path or to a `cons' with
original path.  If the mapping is to a cons, it means a conflict
was found and the `car' contains the first path to this
entry."
  (princ "** Filling dictionary of similar paths:\n")
  (let ((conflict-count 0))
    (dolist (file files)
      (let ((normalized (logseq-org-roam--normalize-path file)))
        (if (eq 'not-found (gethash normalized file-dict 'not-found))
            (puthash normalized file file-dict)
          (setq conflict-count (1+ conflict-count))
          (let ((val (gethash normalized file-dict))
                original)
            (if (consp val)
                (setq original (car val))
              (puthash normalized (cons val nil) file-dict)
              (setq original val))
            (princ (format "- Path to %s and %s are too similar and will not be converted\n"
                           (logseq-org-roam--fl file)
                           (logseq-org-roam--fl original)))))))
    (princ (format "%s entries in total\n" (hash-table-count file-dict)))
    (princ (format "%s conflicts\n" conflict-count))
    conflict-count))

(defun logseq-org-roam--buffer-title ()
  "Return a title based on current buffer's file name.
If the file is a journal entry, format the title accroding to
`logseq-org-roam-journals-title-format'."
  (let* ((base-name (file-name-base (buffer-file-name))))
    (if (logseq-org-roam-journals-p (buffer-file-name))
        (let ((time (condition-case ()
                        (funcall logseq-org-roam-maybe-date-func
                                 logseq-org-roam-journals-file-name-format
                                 base-name)
                      (error 0))))
          (if (time-equal-p 0 time) base-name
            (format-time-string logseq-org-roam-journals-title-format time)))
      base-name)))

(defun logseq-org-roam--update-first-section (plist)
  "Update current buffer first section based on PLIST."
  (org-with-wide-buffer
   (let ((start-size (buffer-size))
         (first-section-p (plist-get plist :first-section-p))
         (beg (point-min)))
     (goto-char beg)
     (unless first-section-p
       (insert "\n") ;; Empty line is needed to create first section
       (backward-char))
     (unless (plist-get plist :id)
       (org-id-get-create))
     (unless (plist-get plist :title)
       (let ((title (logseq-org-roam--buffer-title)))
         (goto-char (+ (or (plist-get plist :title-point) beg)
                       (- (buffer-size) start-size)
                       (if first-section-p 0 -1)))
         (unless (bolp) (throw 'update-error 'mismatch-before-title))
         (insert (concat "#+title: " title "\n"))))
     (when-let ((diff (seq-difference (plist-get plist :aliases)
                                      (plist-get plist :roam-aliases))))
       (goto-char beg)
       (dolist (alias diff)
         (org-roam-property-add "ROAM_ALIASES" alias)))
     (unless first-section-p
       (goto-char (+ beg (- (buffer-size) start-size) -1))
       (unless (looking-at "\n") (throw 'update-error 'mismatch-first-section))
       (delete-char 1)))))

(defun logseq-org-roam--update-links (links inventory file-dict fuzzy-dict)
  "Convert LINKS in current buffer to a target in INVENTORY.
This function returns t or an error code if there was an issue
updating the buffer.

The argument FILE-DICT is a hash-table that maps a normalized
file path to a key in inventory (a file path).  When dealing only
with fuzzy links, this hashtable is not used.

The argument FUZZY-DICT is a hash-table that maps a normalized
fuzzy link to a key in inventory (a file path).  When dealing
only with file links, this hashtable is not used."
  ;; `secure-hash' has a small chance of collision
  (pcase-dolist (`(_ ,beg ,end _ _ ,raw) links)
    (unless (string= (buffer-substring-no-properties beg end) raw)
      (throw 'update-error 'mismatch-link)))
  ;; Avoid offset calculations with buffer updates
  (sort links (lambda (a b) (> (nth 1 a) (nth 1 b))))
  (pcase-dolist (`(,type ,beg ,end ,path ,descr _) links)
    (when-let ((id (plist-get
                    (gethash
                     ;; file-dict and fuzzy-dict key can be `consp' (conflict)
                     (if (eq 'file type)
                         (gethash (logseq-org-roam--normalize-path
                                   (expand-file-name path)) file-dict)
                       (gethash (logseq-org-roam--normalize-text path) fuzzy-dict))
                     inventory) :id)))
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (goto-char beg)
          (delete-region beg end)
          ;; TODO: log link updates
          (if descr
              (insert (concat "[[id:" id "][" descr "]]"))
            (insert (concat "[[id:" id "][" path "]]"))))))))

(defun logseq-org-roam--update-all (files inventory &optional link-p file-dict fuzzy-dict)
  "Update all FILES according to INVENTORY.
By default only the first section is updated, but if LINK-P is
non-nil, links are updated instead taking into account
FILE-DICT and FUZZY-DICT.

First sections and links are never updated in the same pass,
since to update the links, all first sections must be inventoried
first."
  (let (updated-files log-p)
    (princ (concat "** Updating files:\n"))
    (dolist (file files)
      (when-let ((plist (gethash file inventory)))
        (unless (or (plist-get plist :modified-p)
                    (plist-get plist :external-p)
                    (plist-get plist :parse-error)
                    (plist-get plist :cache-p)
                    (plist-get plist :update-error)
                    (if link-p
                        (not (plist-get plist :links))
                      (and (plist-get plist :title)
                           (plist-get plist :id)
                           ;; TODO store diff and union instead?
                           (not (seq-difference (plist-get plist :aliases)
                                                (plist-get plist :roam-aliases))))))
          (logseq-org-roam--catch-fun
              'update-error '(mismatch-before-title
                              mismatch-first-section
                              mismatch-link
                              hash-mismatch)
              (lambda (err)
                (princ (format "- Error updating %s of %s\n"
                               (if link-p "links" "first section")
                               (logseq-org-roam--fl file)))
                (puthash file
                         (plist-put plist :update-error err)
                         inventory))
            (logseq-org-roam--with-edit-buffer file
              (unless (string= (plist-get plist :hash)
                               (secure-hash
                                'sha256 (current-buffer)))
                (throw 'update-error 'hash-mismatch))
              (if link-p
                  (logseq-org-roam--update-links (plist-get plist :links)
                                                 inventory file-dict fuzzy-dict)
                (logseq-org-roam--update-first-section plist))
              (when (buffer-modified-p)
                (save-buffer)  ;; NOTE: runs org-roam hook and formatters
                (push file updated-files)
                (princ (concat "- Updated " (if link-p "links" "first section")
                               " of " (logseq-org-roam--fl file) "\n"))
                (setq log-p t)))))))
    (unless log-p
      (princ "No updates found\n"))
    updated-files))

(defun logseq-org-roam-create-translate-default (fuzzy)
  "Transform FUZZY into an expanded file path.
If the fuzzy link represents a date, it will translate the link
to a path under the journal directory (see
`logseq-org-roam-journals-directory') otherwise translate the
link to a file path under the pages directory (See
`logseq-org-roam-pages-directory').

To tell apart dates from other strings, it uses
`logseq-org-roam-maybe-date-func'.  Special characters in the
link path are also replaced, see:
`logseq-org-roam-create-replace'."
  (let ((time (condition-case ()
                  (funcall logseq-org-roam-maybe-date-func
                           logseq-org-roam-journals-title-format
                           fuzzy)
                (error 0)))
        (normalized fuzzy))
    (if (and time (not (time-equal-p 0 time)))
        (setq normalized
              (format-time-string logseq-org-roam-journals-file-name-format
                                  time))
      (pcase-dolist (`(,regex . ,rep) logseq-org-roam-create-replace)
        (setq normalized (replace-regexp-in-string regex rep normalized))))
    (concat
     (expand-file-name
      normalized (expand-file-name
                  (if (and time (not (time-equal-p 0 time)))
                      logseq-org-roam-journals-directory
                    logseq-org-roam-pages-directory)
                  org-roam-directory))
     ".org")))

(defun logseq-org-roam--create-path-fuzzy (fuzzy fuzzy-dict)
  "Return a path from FUZZY link.
Uses FUZZY-DICT to ensure this is a brand new entry."
  (if (eq 'not-found (gethash (logseq-org-roam--normalize-text fuzzy)
                              fuzzy-dict 'not-found))
      (let ((translated
             (condition-case ()
                 (funcall logseq-org-roam-create-translate-func fuzzy)
               (error nil))))
        (if (file-name-absolute-p translated) translated))))

(defun logseq-org-roam--create-path-file (path file file-dict)
  "Return a path from the PATH in a FILE link.
The path needs to be expanded first before being checked against
FILE-DICT, as it is normally relative to the FILE it is located
in."
  (let ((expanded
         (expand-file-name path
                           (file-name-directory file))))
    (if (eq 'not-found (gethash (logseq-org-roam--normalize-path expanded)
                                file-dict 'not-found))
        expanded)))

(defun logseq-org-roam--create-from (files inventory file-dict fuzzy-dict)
  "Author new files for dead links of each FILES in INVENTORY.
FILE-DICT and FUZZY-DICT are used for dead links detection with
file links and fuzzy links respectively.  Any dead links is
considered a candidate for creation of new files.

Fuzzy links are first transformed to an expended path by calling
`logseq-org-roam-create-translate-func' with the link
path as an argument.

If file links are expanded against the parent directory of the
file containing them.

The path is given as the first argument to
`logseq-org-roam-create-accept-func' when it is
non-nil.

The resulting path must match `org-roam-file-p', it's
parent directory must exist, and the file must not exist.

Finally, for file links only, the description is used as the
title for the new file.

When the file is created, it is inserted an Org ID and a title,
then saved.  There is no support for templates at the moment.

Return the list of new files created."
  (let (created-files)
    (princ "** Creating new files:\n")
    (dolist (file files)
      (when-let ((plist (gethash file inventory)))
        (unless
            (or (plist-get plist :modified-p)
                (plist-get plist :external-p)
                (plist-get plist :parse-error)
                (plist-get plist :cache-p)
                (plist-get plist :update-error))
          (pcase-dolist (`(,type _ _ ,path ,descr _) (plist-get plist :links))
            (let (new-path new-title)
              (cond
               ((eq type 'file)
                (setq new-path (logseq-org-roam--create-path-file
                                path file file-dict))
                (setq new-title descr))
               (t
                (setq new-path (logseq-org-roam--create-path-fuzzy
                                path fuzzy-dict))
                (setq new-title path)))
              (cond ((not new-path) t) ;; link to existing entry
                    ((not (file-exists-p
                           (directory-file-name
                            (file-name-directory new-path))))
                     (princ (format "- For %s link %s in %s: parent directory of %s does not exists\n"
                                    (if (eq type 'file) "file" "fuzzy")
                                    path (logseq-org-roam--fl file) new-path)))
                    ((file-exists-p new-path)
                     (princ (format "- For %s link %s in %s: file %s already exists\n"
                                    (if (eq type 'file) "file" "fuzzy")
                                    path (logseq-org-roam--fl file) new-path)))
                    ((not (org-roam-file-p new-path))
                     (princ (format "- For %s link %s in %s: %s is not an org-roam file\n"
                                    (if (eq type 'file) "file" "fuzzy")
                                    path (logseq-org-roam--fl file) new-path)))
                    ((not (condition-case ()
                              (funcall logseq-org-roam-create-accept-func
                                       new-path)
                            (error nil)))
                     (princ (format "- For %s link %s in %s: creation of %s is rejected\n"
                                    (if (eq type 'file) "file" "fuzzy")
                                    path (logseq-org-roam--fl file) new-path)))
                    (t
                     (logseq-org-roam--with-edit-buffer new-path
                       ;; TODO: Use capture templates instead
                       (org-id-get-create)
                       (goto-char (point-max))
                       (insert (concat "#+title: " new-title "\n"))
                       (save-buffer))
                     (princ (concat "- Created " (logseq-org-roam--fl new-path)
                                    " from the " (if (eq type 'file) "file" "fuzzy")
                                    " link in " (logseq-org-roam--fl file) "\n"))
                     (push new-path created-files))))))))
    (unless created-files
      (princ "No new files created\n"))
    created-files))

(defun logseq-org-roam--log-start (force create)
  "Log start of execution and state of FORCE and CREATE flags."
  ;; TODO: log other settings
  (princ
   (concat
    (format "* Ran on %s\n" (format-time-string "%x at %X"))
    (format "Using Org-roam directory: %s\n" org-roam-directory)
    (format "Logseq pages directory is: %s\n" logseq-org-roam-pages-directory)
    (format "Logseq journal directory is: %s\n" logseq-org-roam-journals-directory)
    "With flags:\n"
    (format "- ~force~ was: %s\n" force)
    (format "- ~create~ was: %s\n" create)
    "With settings:\n"
    (format "- ~logseq-org-roam-link-types~: %S\n" logseq-org-roam-link-types))))

;; TODO: test
(defun logseq-org-roam--check-errors (files inventory)
  "Log parsing issues with FILES in INVENTORY.
Return non-nil if issues where found."
  (princ "** Verifing files:\n")
  (let (error-p)
    (dolist (file files)
      (let ((plist (gethash file inventory)))
        (if (not plist)
            (progn
              (setq error-p t)
              (princ (concat "- File " (logseq-org-roam--fl file)
                             "blank or not parsed\n")))
          (when (plist-get plist :modified-p)
            (setq error-p t)
            (princ (concat "- Save the buffer visiting " (logseq-org-roam--fl file) "\n")))
          (when-let ((err (plist-get plist :parse-error)))
            (setq error-p t)
            (princ (concat "- Error " (format "%s" err)
                           " parsing " (logseq-org-roam--fl file) ", skipped\n")))
          (when-let ((err (plist-get plist :update-error)))
            (setq error-p t)
            (princ (concat "- Error " (format "%s" err)
                           " updating " (logseq-org-roam--fl file)
                           ", left as-is\n")))
          (when (not (plist-get plist :id))
            (setq error-p t)
            (princ (concat "- No id found for " (logseq-org-roam--fl file) "\n")))
          (when (not (plist-get plist :title))
            (setq error-p t)
            (princ (concat "- No title found for " (logseq-org-roam--fl file) "\n"))))))
    (if (not error-p)
        (princ "No errors.\n")
      (princ "Errors found:\nPlease save any work in progress or fix syntax issues in the files mentionned above before re-running the function again. Errors can impact the accuracy of file creation or link conversion.\n")
      (throw 'stop 'error-encountered))))

(defun logseq-org-roam--sanity-check ()
  "Check that `org-roam' is installed and configured."
  (cond
   ((not (featurep 'org-roam))
    (user-error "logseq-org-roam: `org-roam' is not installed"))
   ((not org-roam-directory)
    (user-error "logseq-org-roam: `org-roam-directory' is not set"))
   ((not (file-directory-p org-roam-directory))
    (user-error "logseq-org-roam: `org-roam-directory' is not a directory"))
   ((not (file-exists-p (expand-file-name
                         logseq-org-roam-pages-directory
                         org-roam-directory)))
    (user-error "logseq-org-roam: Logseq pages must be found directly under `org-roam-directory'"))
   ((not (file-exists-p (expand-file-name
                         logseq-org-roam-journals-directory
                         org-roam-directory)))
    (user-error "logseq-org-roam: Logseq journals must be found directly under `org-roam-directory'"))
   (t)))

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
  (when (logseq-org-roam--sanity-check)
    (let ((start (current-time))
          elapsed
          force_flag
          create_flag)
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
       ;;
       ;; - Creation adds complexity since we must parse the entire
       ;; content for all files to discover dead links first.
       (let* ((files (org-roam-list-files))
              (link-parts (cond ((eq logseq-org-roam-link-types 'files)
                                 '(file-links))
                                ((eq logseq-org-roam-link-types 'fuzzy)
                                 '(fuzzy-links))
                                (t '(file-links fuzzy-links))))
              (fuzzy-dict (make-hash-table :test #'equal))
              (file-dict (make-hash-table :test #'equal))
              inventory
              modified-files
              created-files
              not-created-files)
         (logseq-org-roam--catch-fun
             'stop '(error-encountered)
             (lambda (_)
               (display-warning 'logseq-org-roam
                                (concat "Stopped with errors, see "
                                        (format logseq-org-roam--log-buffer-name
                                                org-roam-directory)
                                        " buffer")
                                :error))
           (setq inventory (logseq-org-roam--inventory-init files))
           (if (= 0 (logseq-org-roam--inventory-all
                     files inventory force_flag (append '(first-section) link-parts)))
               ;; TODO: calcuate left-over files only
               (progn
                 (princ "No updates to perform\n")
                 ;; Check errors in inventory none-the-less
                 ;; TODO: write dedicated interactive function for this
                 (logseq-org-roam--check-errors files inventory))
             ;; Compute the subset of logseq files instead of using files
             (setq modified-files
                   (logseq-org-roam--update-all files inventory))
             (if modified-files
                 (logseq-org-roam--inventory-update modified-files inventory
                                                    (append '(first-section)
                                                            link-parts)))
             (if (memq 'fuzzy-links link-parts)
                 (logseq-org-roam--fill-fuzzy-dict fuzzy-dict files inventory))
             (if (memq 'file-links link-parts)
                 (logseq-org-roam--fill-file-dict file-dict files))
             ;; Do as much work as possible, but beyond this point, the errors
             ;; (modified files, parse or update errors) could affect accuracy
             ;; of the changes
             (logseq-org-roam--check-errors files inventory)
             (setq not-created-files files)
             (when create_flag
               (setq created-files
                     (logseq-org-roam--create-from files inventory
                                                   file-dict fuzzy-dict))
               (logseq-org-roam--inventory-update created-files inventory
                                                  ;; No links added to new files
                                                  '(first-section))
               (if (memq 'fuzzy-links link-parts)
                   (logseq-org-roam--fill-fuzzy-dict fuzzy-dict created-files inventory))
               (if (memq 'file-links link-parts)
                   (logseq-org-roam--fill-file-dict file-dict created-files))
               (logseq-org-roam--check-errors created-files inventory))
             (if (and (logseq-org-roam--update-all not-created-files
                                                   inventory 'links file-dict fuzzy-dict)
                      modified-files)
                 (run-hooks 'logseq-org-roam-updated-hook)))
           ;; TODO: Add summary of results
           ;; TODO: timing should be `unwind-protect'
           (setq elapsed (float-time (time-subtract (current-time) start)))
           (princ (format "Completed in %.3f seconds\n" elapsed))))))))

(provide 'logseq-org-roam)
;;; logseq-org-roam.el ends here
