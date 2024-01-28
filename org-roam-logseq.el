;;; org-roam-logseq.el --- Org-roam Logseq converter -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2024, Sylvain Bougerel

;; Author: Sylvain Bougerel
;; URL: https://github.com/idanov/org-roam-logseq.el/
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
;; This package features a single point of entry: `org-roam-logseq-import' which
;; examines the notes in your org-roam directory and prompts to import into
;; org-roam all notes that were updated or captured in Logseq.
;; `org-roam-logseq-import' can run upon visiting a file and it will do a fine
;; job, however it's best to run it a single time, after the notes from Logseq
;; are added to org-roam.
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
;;    https://github.com/idanov/org-roam-logseq/ which is
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

(defgroup org-roam-logseq nil
  "Convert Logseq files to `org-roam' files."
  :group 'org-roam)

;;;###autoload (put 'org-roam-logseq-link-types 'safe-local-variable #'symbolp)
(defcustom org-roam-logseq-link-types nil
  "The kind of links `org-roam-logseq' should convert.
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

When dealing with file links, `org-roam-logseq' ignores links
that do not contain a description since Logseq always populates
it when referencing another page.  It also ignores links that
contain search options since Logseq does not create those.  And
finally it discards any links that is not a link to an `org-roam'
file (since these are not convertible to ID links), taking into
account that file paths are downcased.

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

On the other hand `org-roam-logseq' cares to implement the
complex matching rules set by `org-roam' to convert the right
fuzzy links, making Logseq and `org-roam' mostly interoperable
even when using fuzzy links in Logseq."
  :type 'symbol
  :options '(fuzzy file both)
  :group 'org-roam-logseq)

;;;###autoload (put 'org-roam-logseq-capture-template 'safe-local-variable #'stringp)
(defcustom org-roam-logseq-capture-template "d"
  "Key of the `org-roam' capture template to use."
  :type 'string
  :group 'org-roam-logseq)

;;;###autoload
(defcustom org-roam-logseq-updated-hook nil
  "Hook called  by `org-roam-logseq' if any files was updated."
  :type 'hook
  :group 'org-roam-logseq)

(defconst org-roam-logseq--named
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

(defconst org-roam-logseq--log-buffer-name "*Org-roam Logseq*"
  "Name for the log buffer.")

(defmacro org-roam-logseq--with-log-buffer (&rest body)
  "Bind standard output to a dedicated buffer for the duration of BODY."
  (declare (debug t))
  `(let* ((standard-output
           (with-current-buffer
               (get-buffer-create org-roam-logseq--log-buffer-name)
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

(defmacro org-roam-logseq--with-edit-buffer (file &rest body)
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
                 (find-file-noselect ,file)))))
     (unwind-protect
         (with-current-buffer buf
           (unless (derived-mode-p 'org-mode)
             (delay-mode-hooks
               (let ((org-inhibit-startup t)) (org-mode))))
           ,@body)
       (unless existing-buffer (kill-buffer buf)))))

(defmacro org-roam-logseq--with-temp-buffer (file &rest body)
  "Visit FILE into an `org-mode' temp buffer and execute BODY.
If the buffer is new, `org-mode' startup is inhibited.  This
macro does not save the file, but will kill the buffer if it was
previously created."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (delay-mode-hooks
       (let ((org-inhibit-startup t)) (org-mode)))
     (insert-file-contents ,file)
     ,@body))


(defmacro org-roam-logseq--catch-fun (sym fun &rest body)
  "Catch SYM during BODY's execution and pass it to FUN."
  (declare (indent 2) (debug t))
  (let ((result (make-symbol "result")))
    `(let ((,result (catch ',sym ,@body)))
       (when (symbolp ,result)
         (,fun ,result)))))

;; Aliased for mocking: remapping `expand-file-name' is dangerous
(defalias 'org-roam-logseq--expand-file #'expand-file-name)
(defalias 'org-roam-logseq--file-p #'org-roam-file-p)

(defun org-roam-logseq--fl (file)
  "Make an org link to FILE relative to `org-roam-directory'."
  (format "[[file:%s][%s]]"
          file
          (file-relative-name file org-roam-directory)))

(defun org-roam-logseq--image-file-p (file)
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

(defun org-roam-logseq--value-string-p (element)
  "Return non-nil if ELEMENT has a string value that is not empty."
  (and (org-element-property :value element)
       (not (string-empty-p (org-element-property :value element)))))

(defun org-roam-logseq--inventory-init (files)
  "Initialise inventory with FILES.
The path of each file is downcased, since both `org-mode' and
Logseq are case-insensitive when performing file search."
  (let ((inventory (make-hash-table :test #'equal)))
    (mapc (lambda (elem) (puthash (downcase elem) nil inventory))
          files)
    inventory))

(defun org-roam-logseq--inventory-from-cache (inventory)
  "Populate INVENTORY with `org-roam' cache for FILES.
Return the number of files whose metadata was retreived from the
cache."
  (let ((count 0)
        (data-cached (org-roam-db-query [:select [file id title]
                                         :from nodes
                                         :where (= 0 level)])))
    (pcase-dolist (`(,file ,id ,title) data-cached)
      ;; TODO: Consider throwing an error if cache is not updated
      (unless (eq 'not-found (gethash (downcase file) inventory 'not-found))
        (setq count (1+ count))
        (let ((aliases (mapcar #'car
                               (org-roam-db-query [:select [alias] :from aliases
                                                   :where (= node-id $s1)]
                                                  id))))
          (puthash (downcase file)
                   (append (list :cache-p t :id-p t :id id)
                           (if (and title (not (string-empty-p title)))
                               (list :title-p t :title title))
                           (if aliases
                               (list :aliases aliases)))
                   inventory))))
    count))

(defun org-roam-logseq--inventory-mark-modified (files inventory)
  "Update INVENTORY with modified buffer visiting any FILES.
Return the number of files from INVENTORY that are currently
being modified in a buffer."
  (let ((count 0))
    (dolist (file files)
      (when-let* ((existing_buf (find-buffer-visiting file))
                  (mod-p (buffer-modified-p existing_buf)))
        (setq count (1+ count))
        (let* ((plist (gethash (downcase file) inventory))
               (new_plist (plist-put plist :modified-p t)))
          (puthash (downcase file) new_plist inventory))))
    count))

(defun org-roam-logseq--parse-first-section-properties (section plist)
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
                   (org-roam-logseq--value-string-p node-property))
              (setq plist (plist-put plist :id
                                     (org-element-property :value
                                                           node-property))))
             ((and (equal "ROAM_ALIASES" key)
                   (org-roam-logseq--value-string-p node-property))
              (setq plist (plist-put plist :roam-aliases
                                     (split-string-and-unquote
                                      (downcase
                                       (org-element-property
                                        :value
                                        node-property))))))))))))
  plist)

(defun org-roam-logseq--parse-first-section-keywords (section plist)
  "Return updated PLIST based on first SECTION keywords."
  (org-element-map section 'keyword
    (lambda (keyword)
      (let ((key (org-element-property :key keyword)))
        (cond
         ((equal "TITLE" key)
          (setq plist (plist-put plist :title-point
                                 (org-element-property :begin keyword)))
          (when (org-roam-logseq--value-string-p keyword)
            (setq plist (plist-put plist :title
                                   (org-element-property :value keyword)))))
         ((and (equal "ALIAS" key)
               (org-roam-logseq--value-string-p keyword))
          ;; TODO: support anycase aliases, to ensure they appear in the same
          ;; case in ROAM_ALIASES.  This should be done with computing union /
          ;; diff.
          (setq plist (plist-put plist :aliases
                                 (split-string
                                  (downcase
                                   (org-element-property :value keyword))
                                  "\\s-*,\\s-*"))))))))
  plist)

(defun org-roam-logseq--parse-first-section (data plist)
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
      (setq plist (org-roam-logseq--parse-first-section-properties section plist))
      (setq plist (org-roam-logseq--parse-first-section-keywords section plist)))
    plist)))

(defun org-roam-logseq--parse-file-links (data plist)
  "Return updated PLIST based on file links in DATA."
  (let ((links (plist-get plist :links)))
    (org-element-map data 'link
      (lambda (link)
        (when (and (equal (org-element-property :type link) "file")
                   (org-element-property :contents-begin link)
                   (not (org-element-property :search-option link)))
          (let* ((path (org-element-property :path link))
                 (true-path (org-roam-logseq--expand-file path)))
            ;; Convert only links that point to org-roam files
            (when (org-roam-logseq--file-p true-path)
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

(defun org-roam-logseq--parse-fuzzy-links (data plist)
  "Return updated PLIST based on fuzzy links in DATA.
This function ensures that we do not convert fuzzy links that
already match internal links; they take precedence over external
ID links."
  (let ((links (plist-get plist :links))
        (text-targets (make-hash-table :test #'equal)))
    (org-element-map data (append '(link target headline)
                                  org-roam-logseq--named)
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
                            (org-roam-logseq--image-file-p path)))
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

(defun org-roam-logseq--parse-buffer (plist parts)
  "Return updated PLIST based on current buffer's content.
This function updates PLIST with based on selected PARTS, a list
of keywords which defaults to \\='(first-section file-links
fuzzy-links)."
  (org-with-wide-buffer
   (let* ((data (org-element-parse-buffer)))
     (when (memq 'first-section parts)
       (setq plist (org-roam-logseq--parse-first-section data plist)))
     (when (memq 'file-links parts)
       (setq plist (org-roam-logseq--parse-file-links data plist)))
     (when (memq 'fuzzy-links parts)
       (setq plist (org-roam-logseq--parse-fuzzy-links data plist)))))
  plist)

(defun org-roam-logseq--parse-files (files inventory parts)
  "Populate INVENTORY by parsing content of FILES.
Restrict parsing to PARTS if provided.  Return the number of files
that were parsed."
  (let ((count 0))
    (dolist (file files)
      (let* ((key (downcase file))
             (plist (gethash key inventory)))
        (unless (or (plist-get plist :modified-p)
                    (plist-get plist :cache-p)
                    (plist-get plist :parse-error)
                    (plist-get plist :update-error))
          (setq count (1+ count))
          (org-roam-logseq--catch-fun
              'fault (lambda (err)
                       (puthash key
                                (plist-put plist :parse-error err)
                                inventory)))
          (org-roam-logseq--with-temp-buffer file
            (let ((new_plist (plist-put plist :hash
                                        (secure-hash 'sha256
                                                     (current-buffer)))))
              (setq new_plist
                    (org-roam-logseq--parse-buffer new_plist parts))
              (puthash key new_plist inventory))))))
    count))

(defun org-roam-logseq--inventory-all (files force parts)
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
         (inventory (org-roam-logseq--inventory-init files))
         count_cached
         count_modified
         count_parsed
         elapsed)
    (setq count_cached
          (if force
              (org-roam-logseq--inventory-from-cache inventory)
            0))
    (setq count_modified
          (org-roam-logseq--inventory-mark-modified files inventory))
    (setq count_parsed
          (org-roam-logseq--parse-files files inventory parts))
    (setq elapsed (float-time (time-subtract (current-time) start)))
    (princ
     (concat
      (format "%s files found in org-roam directory\n"
              (hash-table-count inventory))
      (format "%s files' metadata retrieved from cache\n"
              count_cached)
      (format "%s files being visited in a modified buffer will be skipped\n"
              count_modified)
      (format "%s remaining files have been parsed\n"
              count_parsed)
      (format "This took %.3f seconds\n" elapsed)))
    inventory))

(defun org-roam-logseq--inventory-update (files inventory parts)
  "Update INVENTORY by reparsing FILES."
  (princ "** Inventory update:\n")
  (let* ((start (current-time))
         count_parsed
         elapsed)
    (setq count_parsed
          (org-roam-logseq--parse-files files inventory parts))
    (setq elapsed (float-time (time-subtract (current-time) start)))
    (princ
     (concat
      (format "%s updated files have been parsed\n"
              count_parsed)
      (format "This took %.3f seconds\n" elapsed)))
    inventory))

;; TODO HERE files links won't work without FILES
(defun org-roam-logseq--calculate-fuzzy-dict (files inventory)
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
      (let* ((key (downcase file))
             (plist (gethash key inventory)))
        (unless (or (plist-get plist :modified-p)
                    (plist-get plist :parse-error))
          ;; Aliases and title from the same file don't conflict
          (let ((merged (seq-uniq
                         (append
                          (list (downcase (plist-get plist :title)))
                          (plist-get plist :aliases)
                          (plist-get plist :roam-aliases)))))
            (dolist (target merged)
              (if (and (not (gethash target conflicts))
                       (not (gethash target dict)))
                  (puthash target key dict)
                (unless (gethash target conflicts)
                  (puthash target (gethash target dict) conflicts)
                  (remhash target dict))
                ;; Log the conflict
                (let* ((this-file key)
                       (this-title-p (equal target (downcase (plist-get plist :title))))
                       (other-file (gethash target conflicts))
                       (other-plist (gethash other-file inventory))
                       (other-title-p (equal target (downcase (plist-get other-plist :title)))))
                  (unless conflicts-p
                    (princ "** Conflicts found in fuzzy links:\n")
                    (setq conflicts-p t))
                  (princ (concat "- The "
                                 (if this-title-p "title" "alias")
                                 " \"" target "\" in "
                                 (org-roam-logseq--fl this-file)
                                 " conflicts with the "
                                 (if other-title-p "title" "alias")
                                 " in "
                                 (org-roam-logseq--fl other-file)
                                 ", links to \"" target
                                 "\" will not be converted.\n")))))))))
    (if conflicts-p
        ;; TODO: standardize on error codes
        (throw 'stop 'conflits-encountered)
      dict)))

(defun org-roam-logseq--buffer-title ()
  "Return a title based on current buffer's file name."
  (file-name-base (buffer-file-name)))

(defun org-roam-logseq--update-first-section (plist)
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
       (let ((title (org-roam-logseq--buffer-title)))
         (goto-char (+ (or (plist-get plist :title-point) beg)
                       (- (buffer-size) start-size)
                       (if first-section-p 0 -1)))
         (unless (looking-at "\n") (throw 'fault 'mismatch-before-title))
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

(defun org-roam-logseq--update-links (links inventory fuzzy-dict)
  "Convert LINKS in current buffer to a target in INVENTORY.
This function returns t or an error code if there was an issue
updating the buffer.

The argument FUZZY-DICT is a hash-tables needed to map a fuzzy
link target to a key in inventory (downcased path).  When dealing
only with file links, this hashtable is nil."
  ;; Even hash has a small chance of collision
  (catch 'fault
    (pcase-dolist (`(_ ,beg ,end _ _ ,raw) links)
      (unless (equal (buffer-substring-no-properties beg end) raw)
        (throw 'fault 'mismatch-link)))
    ;; Avoid offset calculations with buffer updates
    (sort links (lambda (a b) (> (nth 1 a) (nth 1 b))))
    (pcase-dolist (`(,type ,beg ,end ,path ,descr _) links)
      (when-let ((id (if (eq 'file type)
                         (plist-get (gethash (downcase
                                              (org-roam-logseq--expand-file path))
                                             inventory)
                                    :id)
                       ;; title type
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

(defun org-roam-logseq--update-all-first-sections (files inventory)
  "Update the first sections of all FILES according to INVENTORY.
After the update is completed, we update inventory with the new information."
  (let (updated-files log-p)
    (princ "** First sections of the following files are updated:\n")
    (dolist (file files)
      (let ((plist (gethash (downcase file) inventory)))
        (unless (or (plist-get plist :modified-p)
                    (plist-get plist :parse-error)
                    (plist-get plist :cache-p)
                    (plist-get plist :update-error)
                    (and (plist-get plist :title)
                         (plist-get plist :id)
                         ;; TODO store diff and union instead?
                         (not (seq-difference (plist-get plist :aliases)
                                              (plist-get plist :roam-aliases)))))
          (org-roam-logseq--catch-fun
              'fault (lambda (err)
                       (puthash (downcase file)
                                (plist-put plist :update-error err)
                                inventory))
            (org-roam-logseq--with-edit-buffer file
              (unless (equal (plist-get plist :hash)
                             (secure-hash 'sha256 (current-buffer)))
                (throw 'fault 'hash-mismatch))
              (org-roam-logseq--update-first-section plist)
              (when (buffer-modified-p)
                (save-buffer)  ;; NOTE: runs org-roam hook and formatters
                (push file updated-files)
                (setq log-p t)
                (princ (concat "- Updated " (org-roam-logseq--fl file) "\n"))))))))
    (unless log-p
      (princ "No updates found\n"))
    updated-files))

(defun org-roam-logseq--update-all-links (files inventory fuzzy-dict)
  "Update the links in FILES according to INVENTORY.
Return non-nil if any file was updated.

The argument FUZZY-DICT is a hash-tables needed to map a fuzzy
link target to a key in inventory (downcased path).  When dealing
only with file links, this hashtable is nil."
  (let (log-p updates-p)
    (princ "** Links of the following files are updated:\n")
    (dolist (file files)
      (let ((plist (gethash file inventory)))
        (unless (or (plist-get plist :modified-p)
                    (plist-get plist :parse-error)
                    (plist-get plist :cache-p)
                    (plist-get plist :update-error)
                    (not (plist-get plist :links))) ;; minimal check
          (org-roam-logseq--catch-fun
              'fault (lambda (err)
                       (puthash (downcase file)
                                (plist-put plist :update-error err)
                                inventory))
            (org-roam-logseq--with-edit-buffer file
              (unless (equal (plist-get plist :hash)
                             (secure-hash 'sha256 (current-buffer)))
                (throw 'fault 'hash-mismatch))
              (org-roam-logseq--update-links plist inventory fuzzy-dict)
              (when (buffer-modified-p)
                (save-buffer) ;; NOTE: important to run org-roam hook
                (setq updates-p t)
                (setq log-p t)
                (princ (concat "- Updated " (org-roam-logseq--fl file) "\n"))))))))
    (unless log-p
      (princ "No links to update\n"))
    updates-p))

(defcustom org-roam-logseq-date-like
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
`org-roam-logseq-create-translate-default' will still attempt to
parse it with `parse-time-string' and verify that it is actually
a real date (e.g. 2023-02-31 won't be accepted as a date)."
  :type 'string
  :group 'org-roam-logseq)

;;;###autoload (put 'org-roam-logseq-create-translate-link 'safe-local-variable #'symbolp)
(defcustom org-roam-logseq-create-translate
  #'org-roam-logseq-create-translate-default
  "Function translating a fuzzy link to a file path.

By default, it will attempt to translate a fuzzy link path to a
file path under the pages directory (see
`org-roam-logseq-pages-directory').  If the fuzzy link represents
a date, it will translate the link to a path under the journal
directory (see `org-roam-logseq-journals-directory').  To tell
apart dates from other strings, it uses `org-roam-logseq-date-p'.

Special characters in the link path are also replaced, see:
`org-roam-logseq-create-replace'.

Setting this value to nil disables creation of pages for fuzzy
links"
  :type 'symbol
  :group 'org-roam-logseq)

;; TODO: filter files futher from `org-roam-list-files'
;;;###autoload (put 'org-roam-logseq-pages-directory 'safe-local-variable #'string)
(defcustom org-roam-logseq-pages-directory "pages"
  "Set this variable to mirror Logseq :pages-directory setting."
  :type 'string
  :group 'org-roam-logseq)

;;;###autoload (put 'org-roam-logseq-journals-directory 'safe-local-variable #'string)
(defcustom org-roam-logseq-journals-directory "journals"
  "Set this variable to mirror Logseq :journals-directory setting."
  :type 'string
  :group 'org-roam-logseq)

;;;###autoload (put 'org-roam-logseq-create-accept 'safe-local-variable #'symbol)
(defcustom org-roam-logseq-create-accept #'org-roam-logseq-create-pages-only
  "Tells aparts paths that should be created from paths that should not.
When non-nil, it is called as a function with a single argument:
the path.  It is expected to return a boolean value.  If non-nil,
the path is accepted and the file is created.

The default value (`org-roam-logseq-creat-pages-only') will not
create journal entires.  If you want journal entries to be
created too, you can simply set this to
`org-roam-logseq-create-logseq'.  Note that if you set it to nil,
any non-existant Org file under `org-roam-directory' can be
created."
  :type 'symbol
  :group 'org-roam-logseq)

(defun org-roam-logseq-date-p (maybe-date)
  "Return non-nil if MAYBE-DATE is an actual date string.

Used in `org-roam-logseq-create-translate-default'."
  (if-let ((date-like (string-match-p org-roam-logseq-date-like
                                      maybe-date))
           (time (parse-time-string maybe-date))
           (year (nth 5 time))
           (month (nth 4 time))
           (day (nth 3 time))
           (date-string (format "%d-%02d-%02d" year month day))
           (real-date (format-time-string "%Y-%m-%d"
                                          (date-to-time date-string))))
      (equal date-string real-date)))

(defcustom org-roam-logseq-create-replace '(("[\\/-.,]" . "_"))
  "Alist specifying replacements for fuzzy links.

Car and cdr of each cons will be given as arguments to
`replace-regexp-in-string' when converting fuzzy links to paths
in `org-roam-logseq-create-translate-default'."
  :type 'alist
  :group 'org-roam-logseq)

(defun org-roam-logseq-create-translate-default (slug)
  "Transform SLUG into a file path."
  (let ((normalized slug))
    (pcase-dolist (`(,regex . ,rep) org-roam-logseq-create-replace)
      (setq normalized (replace-regexp-in-string regex rep normalized)))
    (concat
     (org-roam-logseq--expand-file
      normalized (org-roam-logseq--expand-file
                  (if (org-roam-logseq-date-p slug)
                      org-roam-logseq-journals-directory
                    org-roam-logseq-pages-directory)
                  org-roam-directory))
     ".org")))

(defun org-roam-logseq-create-pages-only (file)
  "Return non-nil if FILE path is under the Logseq pages directory."
  (file-in-directory-p file
                       (expand-file-name org-roam-logseq-pages-directory
                                         org-roam-directory)))

(defun org-roam-logseq-create-logseq (file)
  "Return non-nil if FILE path is under the Logseq pages directory."
  (or (file-in-directory-p file
                           (expand-file-name org-roam-logseq-pages-directory
                                             org-roam-directory))
      (file-in-directory-p file
                           (expand-file-name org-roam-logseq-journals-directory
                                             org-roam-directory))))

(defun org-roam-logseq--create-from (files inventory fuzzy-dict)
  "Author new files for dead links present in FILES.
INVENTORY and FUZZY-DICT are used for dead links detection with
file links and fuzzy links respectively.  Any dead links is
considered a candidate for creation of new files.

Fuzzy links are first transformed to an expended path by calling
`org-roam-logseq-create-translate' (via `funcall') with the link
path as an argument.

If file links are expanded against the parent directory of the
file containing them.

The path is given as the first argument to
`org-roam-logseq-create-accept' (via `funcall') when it is
non-nil.  Note that by default, journal entries will not be
created (see `org-roam-logseq-create-accept'), only pages will.

The resulting path must match `org-roam-file-p', it's
parent directory must exist, and the file must not exist.

Finally, for file links only, the description is used as the
title for the new file.

When the file is created, it is inserted an Org ID and a title,
then saved.  There is no support for templates at the moment.

Return the list of new files created."
  ;; TODO: seems I was fooled: path links are not case insensitive
  ;; Need to review the whole code for file links!
  ;; TODO: when updating links, I must expand file links first, and thus I must care to the directory is set properly!
  ;; TODO: add more logs to explain why links are not created.
  (let (created-files)
    (princ "** The following files are created:\n")
    (dolist (file files)
      (let ((plist (gethash file inventory)))
        (unless
            (or (not plist)
                (plist-get plist :modified-p)
                (plist-get plist :parse-error)
                (plist-get plist :cache-p)
                (plist-get plist :update-error)
                (not (plist-get plist :links)))
          (pcase-dolist (`(,type _ _ ,path ,descr _) (plist-get plist :links))
            ;; TODO: consider making 2 different section for file and fuzzy
            (when (if (eq type 'file)
                      (eq 'not-found (gethash
                                      (expand-file-name path
                                                        (file-name-directory file))
                                      inventory 'not-found))
                    (and
                     (eq 'not-found (gethash (downcase path)
                                             fuzzy-dict 'not-found))
                     org-roam-logseq-create-translate))
              (let ((new-path
                     (if (eq type 'file)
                         (expand-file-name path
                                           (file-name-directory file))
                       ;; TODO: expand the result of file-name too
                       (funcall org-roam-logseq-create-translate path)))
                    (new-title (if (eq type 'file) descr path)))
                (unless (or (file-exists-p new-path)
                            (not (org-roam-file-p new-path)))
                  (org-roam-logseq--with-edit-buffer new-path
                    (org-id-get-create)
                    (goto-char (point-max))
                    (insert (concat "#+title: " new-title "\n"))
                    (save-buffer))
                  (princ (concat "- Created " (org-roam-logseq--fl new-path)
                                 " from the " (if (eq type 'file) "file" "fuzzy")
                                 " link in " (org-roam-logseq--fl new-path) "\n"))
                  (push new-path created-files))))))))
    (unless created-files
      "No files were created\n")
    created-files))

(defun org-roam-logseq--log-start (force create)
  "Log start of execution and state of FORCE and CREATE flags."
  (princ
   (concat
    (format "* Ran %s\n" (format-time-string "%x at %X"))
    (format "Using Org-roam directory: %s\n" org-roam-directory)
    "With flags:\n"
    (format "- ~force~ was: %s\n" force)
    (format "- ~create~ was: %s\n" create)
    "With settings:\n"
    (format "- ~org-roam-logseq-link-types~: %S\n" org-roam-logseq-link-types)
    (format "- ~org-roam-logseq-capture-template~: %S\n" org-roam-logseq-capture-template)
    "\n")))

(defun org-roam-logseq--check-errors (files inventory)
  "Throw when an error with FILES in INVENTORY is encountered."
  (dolist (file files)
    (let ((plist (gethash (downcase file) inventory)))
      (when (or (plist-get plist :modified-p)
                (plist-get plist :parse-error)
                (plist-get plist :update-error))
        (throw 'stop 'error-encountered)))))

(defun org-roam-logseq--log-errors (files inventory)
  "Log parsing issues with FILES in INVENTORY.
Return non-nil if issues where found."
  (let (error-p modified)
    (dolist (file files)
      (let ((plist (gethash (downcase file) inventory)))
        (when (plist-get plist :modified-p)
          (push file modified))
        (when-let ((err (plist-get plist :parse-error)))
          (unless error-p
            (princ "** Verify the syntax in the following files:\n")
            (setq error-p t))
          (princ (concat "- Error " (format "%s" err)
                         " parsing " (org-roam-logseq--fl file) ", skipped\n")))
        (when-let ((err (plist-get plist :update-error)))
          (unless error-p
            (princ "** Verify the syntax in the following files:\n")
            (setq error-p t))
          (princ (concat "- Error " (format "%s" err)
                         " updating " (org-roam-logseq--fl file)
                         ", left as-is\n")))))
    (when modified
      (princ "** Save the following files before continuing:\n")
      (dolist (file modified)
        (princ (concat "- " (org-roam-logseq--fl file)))))
    (when (or modified error-p)
      (princ (concat "** Stopped with errors:
Please save any work in progress or fix syntax issues in the
files mentionned above before re-running the function again.
Errors can impact the accuracy of file creation or link
conversion.\n\n")))))

(defun org-roam-logseq--sanity-check ()
  "Check that `org-roam' is installed and configured."
  (cond
   ((not (featurep 'org-roam))
    (display-warning 'org-roam-logseq
                     "`org-roam' is not installed" :error) nil)
   (org-roam-directory
    (display-warning 'org-roam-logseq
                     "`org-roam-directory' is not set" :error) nil)
   ((not (file-directory-p org-roam-directory))
    (display-warning 'org-roam-logseq
                     "`org-roam-directory' is not a directory" :error) nil)
   (t))

;;;###autoload
  (defun org-roam-logseq (&optional mode)
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

When `org-roam-logseq' encounters a link created with Logseq to a
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

When calling `org-roam-logseq' programmatically it accepts the
following arguments:

nil: parse only files that are not yet indexed (by `org-roam')
  and does not create any new files (when it encounters a link
  created by Logseq without an existing target).

\\='(4) or 4 or \\='force: parse all files (even those already
  indexed) and does not create any new files.  Equivalent to
  \\[universal-argument] \\[org-roam-logseq].

\\='(16) or 16 or \\='create: parse only files that are not yet
  indexed and create new files using your capture templates (when
  it encounters a Logseq link without target).  Equivalent to
  \\[universal-argument] \\[universal-argument]
  \\[org-roam-logseq].

\\='(64) or 64 or \\='force-create: parse all files and create new
  files using your capture templates.  Equivalent to
  \\[universal-argument] \\[universal-argument]
  \\[universal-argument] \\[org-roam-logseq].

To find out how `org-roam-logseq' dectect Logseq links, read the
documentation string of `org-roam-logseq-link-types'.  To find
out how `org-roam-logseq' uses your own capture templates, read
the documentation string of `org-roam-logseq-capture'."
    (interactive "P")
    (unless (org-roam-logseq--sanity-check)
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
        (org-roam-logseq--with-log-buffer
         (org-roam-logseq--log-start force_flag create_flag)
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
                (link-parts (cond ((eq org-roam-logseq-link-types 'files)
                                   '(file-links))
                                  ((eq org-roam-logseq-link-types 'fuzzy)
                                   '(fuzzy-links))
                                  (t '(file-links fuzzy-links))))
                inventory
                modified-files
                created-files
                not-created-files
                fuzzy-dict)
           (org-roam-logseq--catch-fun 'stop
               (lambda (_)
                 (org-roam-logseq--log-errors files inventory)
                 (display-warning 'org-roam-logseq
                                  (concat "Stopped with errors, see "
                                          org-roam-logseq--log-buffer-name
                                          " buffer")
                                  :error))
             (setq inventory
                   (org-roam-logseq--inventory-all
                    files force_flag (append '(first-section) link-parts)))
             (setq modified-files
                   (org-roam-logseq--update-all-first-sections files inventory))
             (org-roam-logseq--inventory-update modified-files inventory
                                                (append '(first-section) link-parts))
             (when (memq 'fuzzy-links link-parts)
               (setq fuzzy-dict (org-roam-logseq--calculate-fuzzy-dict files inventory)))
             ;; Do as much work as possible, but beyond this point it could
             ;; affect accuracy of the changes
             (org-roam-logseq--check-errors files inventory)
             (setq not-created-files files)
             (when create_flag
               (setq created-files
                     (org-roam-logseq--create-from files inventory
                                                   fuzzy-dict))
               (org-roam-logseq--inventory-update created-files inventory
                                                  ;; Assume no links in templates
                                                  '(first-section))
               (org-roam-logseq--check-errors created-files inventory)
               (setq files (append created-files files)))
             (when (org-roam-logseq--update-all-links not-created-files
                                                      inventory fuzzy-dict)
               (run-hooks 'org-roam-logseq-updated-hook))
             (org-roam-logseq--check-errors files inventory)
             ;; TODO: Add summary of results
             )))))))

(provide 'org-roam-logseq)
;;; org-roam-logseq.el ends here
