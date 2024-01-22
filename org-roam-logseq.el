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
(eval-when-compile
  (require 'cl-lib))
(require 'org)
(require 'org-roam)

(defgroup org-roam-logseq nil
  "Convert Logseq files to `org-roam' files."
  :group 'org-roam)

;;;###autoload (put 'org-roam-logseq-link-types 'safe-local-variable #'symbolp)
(defcustom org-roam-logseq-link-types 'both
  "The kind of links `org-roam-logseq' should convert.
Value is a symbol, only the following are recognized:
- \\='files
- \\='fuzzy
- \\='both (default, if unrecognized)

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

;; Aliased for mocking: remapping `expand-file-name' is dangerous
(defalias 'org-roam-logseq--expand-file #'expand-file-name)
(defalias 'org-roam-logseq--file-p #'org-roam-file-p)

(defun org-roam-logseq--inventory-init (files)
  "Initialise inventory with FILES.
The path of each file is downcased, since Logseq is
case-insensitive when performing file search."
  (let ((inventory (make-hash-table :test #'equal)))
    (mapc (lambda (elem) (puthash (downcase elem) nil inventory))
          files)
    inventory))

(defun org-roam-logseq--inventory-from-cache (inventory)
  "Populate INVENTORY with `org-roam' cache information.
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

(defun org-roam-logseq--inventory-mark-modified (inventory)
  "Mark if any buffer visiting a file in INVENTORY is modified.
Return the number of files from INVENTORY that are currently
being modified in a buffer."
  (let ((count 0) alist)
    (maphash (lambda (file _)
               (push (cons file
                           (if-let* ((existing_buf (find-buffer-visiting file)))
                               (buffer-modified-p existing_buf)))
                     alist))
             inventory)
    (mapc (lambda (file_mod)
            (when (cdr file_mod)
              (setq count (1+ count))
              (let* ((plist (gethash (car file_mod) inventory))
                     (new_plist (plist-put plist :modified-p t)))
                (puthash (car file_mod) new_plist inventory))))
          alist)
    count))

(defun org-roam-logseq--parse-files (files inventory)
  "Populate INVENTORY by parsing content of FILES.
Return the number of files that were parsed."
  (let ((count 0))
    (dolist (file files)
      (let* ((downcased (downcase file))
             (plist (gethash downcased inventory)))
        (unless (or (plist-get plist :modified-p)
                    (plist-get plist :cache-p))
          (setq count (1+ count))
          (org-roam-logseq--with-temp-buffer file
            (let ((new_plist (plist-put plist :hash
                                        (secure-hash 'sha256 (current-buffer)))))
              (setq new_plist
                    (org-roam-logseq--parse-buffer new_plist))
              (puthash downcased new_plist inventory))))))
    count))

(defun org-roam-logseq--inventory-all (&optional force)
  "Build inventory of `org-roam' files metadata.
Returns a hashtable mapping absolute paths to a plist describing
relevant metadata to convert Logseq files to `org-roam'.  The
absolute paths point to existing and new `org-roam'
files (presumably created with Logseq).

The argument FORCE ensure that all files are parsed, instead of
relying on information from the `org-roam' cache (in which case,
files already indexed are ever modififed).

The plist contains the following properties:

- `:cache-p' is t if the file is already in `org-roam' cache
- `:hash' is the sha256 hash of the file to ensure consistency
- `:modified-p' is t if a modified buffer visits the file
- `:title-p' is t if the file already has a title
- `:title' contains the title for the file
      (generated from the file's base name if `:title-p' is nil)
- `:id-p' is t if the file already has an ID
- `:id' contains the org ID for the file
      (generated by `org-id-new' if `:id-p' is nil)
- `:aliases' contains the value of \"#+ALIAS\" or nil
- `:links' is the list of Logseq links present in the file"
  (princ "** Inventory:\n")
  (let* ((files (org-roam-list-files))
         (start (current-time))
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
          (org-roam-logseq--inventory-mark-modified inventory))
    (setq count_parsed
          (org-roam-logseq--parse-files files inventory))
    (setq elapsed (float-time (time-subtract (current-time) start)))
    (princ
     (concat
      (format "%s files found in org-roam directory\n"
              (hash-table-count inventory))
      (format "%s files' metadata retrieved from cache\n"
              count_cached)
      (format "%s files currently modified in a buffer\n"
              count_modified)
      (format "%s remaining files have been parsed\n"
              count_parsed)
      (format "This took %.3f seconds\n\n" elapsed)))
    inventory))

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

(defun org-roam-logseq--parse-first-section (data plist)
  "Return updated PLIST based on first section of DATA."
  (declare (pure t) (side-effect-free t))
  (cond
   ((or (not (consp data))
        (not (eq 'org-data (car data))))
    (throw 'failure 'invalid-ast))
   ((or (not (cddr data))
        (not (consp (caddr data)))
        (not (eq 'section (caaddr data))))
    ;; no content or no first section
    nil)
   (t
    (let ((section (caddr data)))
      ;; Search for properties drawer
      (org-element-map section 'property-drawer
        (lambda (property-drawer)
          (setq plist (plist-put plist :property-p t))
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
                 ;; TODO: handle case where the property is present but empty
                 ((equal "ID" key)
                  (setq plist (plist-put plist :id-p t))
                  (setq plist (plist-put plist :id
                                         (org-element-property :value
                                                               node-property))))
                 ((equal "ROAM_ALIASES" key)
                  (setq plist (plist-put plist :roam-aliases
                                         (split-string-and-unquote
                                          (downcase
                                           (org-element-property
                                            :value
                                            node-property))))))))))
          ;; Record point where ID should be inserted
          (unless (plist-member plist :id-p)
            (setq plist (plist-put plist :id-point
                                   (progn
                                     (goto-char
                                      (org-element-property :begin property-drawer))
                                     (forward-line)
                                     (beginning-of-line)
                                     (point)))))))
      (org-element-map section 'keyword
        (lambda (keyword)
          (let ((key (org-element-property :key keyword)))
            (cond
             ;; TODO: handle case where the keyword is present but empty
             ((equal "TITLE" key)
              (setq plist (plist-put plist :title-p t))
              (setq plist (plist-put plist :title (org-element-property :value keyword))))
             ((equal "ALIAS" key)
              (setq plist (plist-put plist :aliases
                                     (split-string
                                      (downcase
                                       (org-element-property :value keyword))
                                      "\\s-*,\\s-*")))))))))))
  ;; Settle missing values
  (unless (plist-member plist :id)
    (setq plist (plist-put plist :id (org-id-new))))
  (unless (plist-member plist :title)
    (setq plist (plist-put plist :title (file-name-sans-extension
                                         (file-name-base
                                          (buffer-file-name (buffer-base-buffer)))))))
  plist)

(defun org-roam-logseq--parse-buffer (plist)
  "Return updated PLIST based on current buffer's content.
This function updates PLIST with the following properties:

- `:tofp-p' t if top of file has properties
- `:id-p' t if top of file has an ID property
- `:id' the ID found in the file or gotten from `org-id-new'
- `:title-p' t if the file has a title
- `:title' the title of the file or from file's name
- `:aliases' the list of (top-level) Logseq aliases '#+ALIAS': ...
- `:links' lists all Logseq links candidates for conversion
- (optional) `:id-point' the point where id should be inserted
- (optional) `:title-point' the point where title should be inserted

Only Logseq #+ALIAS keywords that do not have an equivalent in
:ROAM_ALIASES: are inventoried.  The program will attempt to
create corresponding :ROAM_ALIASES: for every #+ALIAS (but not
the reverse).

The last property `:links' is a list ((TYPE BEG END TITLE MATCH)
...) of potential Logseq links candidates to be translasted to
external ID links."
  (org-with-wide-buffer
   (let ((data (org-element-parse-buffer))
         (text-targets (make-hash-table :test #'equal))
         links)
     (catch 'failure
       (setq plist (org-roam-logseq--parse-first-section data plist)))
     ;; Gather relevant information
     (org-element-map data (append '(link target keyword node-property headline) org-roam-logseq--named)
       (lambda (element)
         ;; TODO: The new way of doing, with catch, and each element updating plist or others as they go!
         ;; Start by initializing element types with corresponding variables, then:
         ;; (catch 'processed
         ;;   (setq plist (org-roam-logseq--parse-common element plist))
         ;;   (org-roam-logseq--parse-for))
         ;;   )
         ;; Finally run with element "nil" to signal the end of the parsing
         ;;   (setq plist (org-roam-logseq--parse-common nil plist))
         ;; Use plist to store states in between calls, until the nil call.
         (let ((type (org-element-type element)))
           (if-let ((name (org-element-property :name element)))
               ;; Org-mode searches are case incensitive
               (puthash (downcase name) t text-targets))
           (cond
            ;; See org-link-search to understand what fuzzy link point to
            ;; TODO: simplify fragment with chaining/dispatch?
            ;; TODO: split FILE and TITLE processing
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
            ((and (eq type 'link)
                  (equal (org-element-property :type element) "file")
                  (org-element-property :contents-begin element)
                  (not (org-element-property :search-option element)))
             (let* ((path (org-element-property :path element))
                    (true-path (org-roam-logseq--expand-file path)))
               (when (org-roam-logseq--file-p true-path)
                 (let* ((descr (buffer-substring-no-properties
                                (org-element-property :contents-begin element)
                                (org-element-property :contents-end element)))
                        (begin (org-element-property :begin element))
                        (end (- (org-element-property :end element)
                                (org-element-property :post-blank element)))
                        (raw (buffer-substring-no-properties begin end)))
                   (push `(file ,begin ,end ,path ,descr ,raw) links)))))
            ((eq type 'target)
             (puthash (downcase (org-element-property :value element)) t text-targets))))))
     ;; Filter out link that match targets, headlines or named elements
     (setq plist (plist-put plist :links
                            (seq-filter (lambda (link)
                                          (not (gethash (nth 3 link)
                                                        text-targets)))
                                        links))))))

(defun org-roam-logseq--reverse-map (inventory)
  "Map INVENTORY in reverse and reports conflicts.
This function ensures that titles and aliases found across all
notes are unique.  If any 2 titles or alias conflicts with each
other, there is no unique target for titled links to these files.
This will get reported.

The function returns a reverse map of inventory."
  (let ((reverse_map (make-hash-table :test #'equal))
        (conflicts_map (make-hash-table :test #'equal))
        conflicts)
    ;; TODO: take ROAM_ALIASES into account
    (maphash (lambda (key val)
               (let ((merged (append (list (downcase (plist-get val :title)))
                                     (plist-get val :aliases))))
                 (dolist (target merged)
                   (if (and (not (gethash target conflicts_map))
                            (not (gethash target reverse_map)))
                       (puthash target key reverse_map)
                     (unless (gethash target conflicts_map)
                       (puthash target (gethash target reverse_map) conflicts_map)
                       (remhash target reverse_map))
                     ;; Report the conflict
                     (let* ((this_file key)
                            (this_titled (plist-get val :title-p))
                            (this_titlep (equal target (downcase (plist-get val :title))))
                            (other_file (gethash target conflicts_map))
                            (other_val (gethash other_file inventory))
                            (other_titled (plist-get other_val :title-p))
                            (other_titlep (equal target (downcase (plist-get other_val :title)))))
                       (push (list target
                                   this_file this_titled this_titlep
                                   other_file other_titled other_titlep)
                             conflicts))))))
             inventory)
    (list reverse_map conflicts)))

(defun org-roam-logseq--filter-links (links inventory reverse_map)
  "Using INVENTORY and REVERSE_MAP, keep items in LINKS to update.
Find each of the links that will be converted to an external ID,
return a list sorted by reverse position: last links will be
converted first to avoid computing point movements between
updates."
  (let (convert)
    (pcase-dolist (`(,type ,beg ,end ,tgt ,descr ,raw) links)
      (if-let* ((path (if (eq 'file type) tgt
                        (gethash (downcase tgt) reverse_map)))
                (props (gethash path inventory)))
          ;; This is a link to a known file, convert it
          (let ((id (plist-get props :id)))
            (push `(,type ,beg ,end ,tgt ,descr ,raw ,id) convert))))
    (sort convert (lambda (a b) (> (nth 1 a) (nth 1 b))))))

(defun org-roam-logseq--update-links (links)
  "Update all LINKS in current buffer.
This function returns t or an error code if there was an issue
updating the buffer."
  ;; First ensure that `raw' matches with the buffer's content for all links
  ;; (i.e. minimize chance that file was modified since our last
  ;; visit.)
  (catch 'fault
    ;; ...There's a tiny chance of hash collision.
    (pcase-dolist (`(_ ,beg ,end _ _ ,raw _) links)
      (unless (equal (buffer-substring-no-properties beg end) raw)
        (throw 'fault 'mismatch)))
    ;; Past this point, attempt to update all links
    (pcase-dolist (`(_ ,beg ,end ,path ,descr _ ,id) links)
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (goto-char beg)
          (save-match-data
            ;; We already confirmed it matches... let's destroy:
            (delete-region beg end)
            (if descr
                (insert (concat "[[id:" id "][" descr "]]"))
              ;; Path links always have a description, path below is necessary a title
              (insert (concat "[[id:" id "][" path "]]")))))))
    t))

(defun org-roam-logseq--update-top (id title aliases)
  "Update ID, TITLE, ALIASES for current buffer.
If any of the argument is nil, no work is performed.  This
function should be used only when buffer content was previously
parsed to ensure correctness and uniqueness of each arguments."
  ;; TODO: Collect more info from Org-element AST earlier to save unnecessary
  ;; work.
  (org-with-wide-buffer
   (goto-char (point-min))
   (save-match-data
     (re-search-forward org-heading-regexp)
     (when (eq (match-beginning 0) (point-min))
       ;; File has no first section, create one
       (goto-char (point-min))
       (insert "\n")
       (backward-char)))
   (when id (org-entry-put (point) "ID" id))
   (when aliases
     (dolist (alias aliases)
       (org-roam-property-add "ROAM_ALIASES" alias)))
   (when title
     (goto-char (point-min))
     (save-match-data
       ;; Puts (point) at the end of the first drawer
       ;; (Assuming it's a property drawer)
       ;; TODO: Should use AST data here especially
       (re-search-forward "^\\s-*:END:\\s-*$" nil t))
     (backward-char)
     (if (looking-at-p ":")
         (forward-char))
     (insert (concat "\n#+title: " title))))
  t)

(defun org-roam-logseq--update-all (inventory)
  "Update all files in INVENTORY as needed.
TODO: better descriptions."
  (pcase-let ((`(,reverse_map ,_)
               (org-roam-logseq--reverse-map inventory)))
    (maphash
     (lambda (file plist)
       (let ((update_links (org-roam-logseq--filter-links
                            (plist-get :links plist) inventory reverse_map)))
         (when (or update_links
                   (plist-get :aliases plist)
                   (not (plist-get :title-p plist))
                   (not (plist-get :id-p plist)))
           ;; There's updates to perform, load the file in an org buffer
           (org-roam-logseq--with-edit-buffer file
             (when (equal (plist-get :hash plist)
                          (secure-hash 'sha256 (current-buffer)))
               ;; TODO perform all verifications first
               (org-roam-logseq--update-links update_links)
               (org-roam-logseq--update-top
                (unless (plist-get :id-p plist) (plist-get :id plist))
                (unless (plist-get :title-ed plist) (plist-get :title plist))
                (plist-get :aliases plist)))
             ;; Updates done, time to save the buffer...
             (save-buffer)))))
     inventory)))

(defun org-roam-logseq--start (force create)
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

(defun org-roam-logseq--sanity-check ()
  "Check that `org-roam' is installed and configured."
  (unless (featurep 'org-roam)
    (error "`org-roam' is not installed"))
  (unless org-roam-directory
    (error "`org-roam-directory' is not set"))
  (unless (file-directory-p org-roam-directory)
    (error "`org-roam-directory' is not a directory")))

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
  (org-roam-logseq--sanity-check)
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
     (org-roam-logseq--start force_flag create_flag)
     ;;(let ((inventory (org-roam-logseq--inventory-all force_flag)))
     ;;    (org-roam-logseq--update-all inventory))
     )
    ))

(provide 'org-roam-logseq)
;;; org-roam-logseq.el ends here
