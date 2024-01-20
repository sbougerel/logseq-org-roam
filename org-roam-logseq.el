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

(defvar org-roam-logseq-link-types 'both
  "Types of links `org-roam-logseq' should convert.
Valid values are:
- \\='files
- \\='fuzzy
- \\='both")

(defvar org-roam-logseq-capture-template "d"
  "Key of the `org-roam' capture template to use.")

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

(defun org-roam-logseq--inventory-init (files)
  "Initialise inventory with FILES.
The path of each file is downcased, since Logseq is
case-insensitive when performing file search."
  (let ((inventory (make-hash-table :test #'equal)))
    (mapc (lambda (elem) (puthash (downcase elem)
                                  (list :cache-p nil) inventory))
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
      (when (gethash (downcase file) inventory)
        (setq count (1+ count))
        (let ((aliases (mapcar #'car
                               (org-roam-db-query [:select [alias] :from aliases
                                                   :where (= node-id $s1)]
                                                  id))))
          (puthash (downcase file)
                   (list :cache-p t
                         :id-p t
                         :id id
                         :title-p t
                         :title title
                         :aliases aliases)
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
                                                     (org-roam-logseq--parse-buffer new_plist inventory))
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

(defun org-roam-logseq--parse-buffer (plist inventory)
  "Update PLIST based on INVENTORY and current buffer's content.
This function updates PLIST with the following properties:

- `:title-p' t if the file already has a title
- `:id-p' t if the file already has an ID
- `:id' the org ID for the file (`org-id-new' otherwise)
- `:title' the title for the file (from file's name otherwise)
- `:aliases' the list of (top-level) *Logseq* aliases #+ALIAS: ...
- `:links' lists Logseq links present in the file

Only Logseq #+ALIAS keywords that do not have an equivalent in
:ROAM_ALIASES: are inventoried.  The program will attempt to
create corresponding :ROAM_ALIASES: for every #+ALIAS (but not
the reverse).

The last property `:links' is a list ((TYPE BEG END TITLE MATCH)
...) of Logseq-like links (equivalant to internal fuzzy links in
`org-mode').  These links are candidates to be translasted to
external ID links, if they match an `org-roam' file (not done in
this function).

Link extracted are generally, of 2 types:

- File links without search options (Logseq does not create
  these) such as: [[file:path/to/org/roam][DESCRIPTION]]

- Fuzzy links that do not match with an internal target (read
  below for details)

`org-mode' internal links have complex matching rules (Read
https://orgmode.org/manual/Hyperlinks.html for a details on the
different types of links).  This is why it is usually recommended
to set \":org-mode/insert-file-link?\" to true in Logseq.

Unfortunately, Logseq does not always provide a correct path (as
of version 0.10.3) on platforms tested (Android, Linux).  When a
note does not exist yet or is aliased, the path provided by
Logseq is a grabled concatenation of a relative parent '../' to
an absolute path.  Logseq does not fix this path, even after the
page is created.  Since this use case is common, using this
feature makes things worse for `org-mode'.  When the path is
correctly provided, it is always downcased in the link (not on
disk).  This is something we have to take into consideration
therefore, when matching to another file.

The heurisitc used in this function is designed to make Logseq
and `org-roam' mostly interoperable, without compromising much on
each of their functionalities, *even when using internal
links* (i.e. keeping \":org-mode/insert-file-link? false\") in
Logseq which is what the author currently reverted to.


When dealing with file links, it ignores links that do not
contain a description, since Logseq always populates it when
referencing another file.

When dealing with internal links, it ignores dedicated internal
link formats that have specific meaning in `org-mode' (even if
they are broken):

- [[#custom-id]] links,
- [[*heading]] links,
- [[(coderef)]] links,
- [[image.jpg]] inline links to images,

Of the remaining fuzzy links, it discards links that match in the
buffer with:

 - <<targets>> or,
 - #+name: named elements or,
 - a headline by text search,

The leftover links are the candidates to be converted to
`org-roam' external ID links."
  (org-with-wide-buffer
   (let ((data (org-element-parse-buffer))
         (text-targets (make-hash-table :test #'equal))
         id title roam_aliases aliases links)
     ;; Gather relevant information
     (org-element-map data (append '(link target keyword node-property headline) org-roam-logseq--named)
       (lambda (element)
         (let ((type (org-element-type element))
               (begin (org-element-property :begin element))
               (end (org-element-property :end element)))
           (if-let ((name (org-element-property :name element)))
               ;; Org-mode searches are case incensitive
               (puthash (downcase name) t text-targets))
           (cond
            ;; See org-link-search to understand what fuzzy link point to
            ;; TODO: simplify fragment with chaining/dispatch?
            ;; TODO: inline parent functions?
            ;; TODO: don't gather data from elements when not needed, e.g. begin/end
            ;; TODO: Use cl-struct over plist?
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
                 (push `(title ,begin ,end ,(downcase path)
                         ,(buffer-substring-no-properties begin end))
                       links))))
            ((and (eq type 'link)
                  (equal (org-element-property :type element) "file")
                  (org-element-property :contents-begin element)
                  (not (org-element-property :search-option element)))
             (let* ((path (org-element-property :path element))
                    (true-path (expand-file-name path)))
               (when (gethash true-path inventory)
                 (push `(file ,begin ,end
                         ;; No need to downcase path here, either it's created
                         ;; by Logseq, and it's downcased, OR it's not, in which
                         ;; case we want to keep that link as it is.
                         ,path
                         ,(buffer-substring-no-properties begin end))
                       links))))
            ((eq type 'target)
             (puthash (downcase (org-element-property :value element)) t text-targets))
            ;; Deal with top-level keywords
            ((and (eq type 'keyword)
                  (eq (org-element-type (org-element-property :parent element)) 'section)
                  (eq (org-element-property :mode (org-element-property :parent element)) 'first-section))
             (cond
              ((equal (org-element-property :key element) "TITLE")
               (setq title (org-element-property :value element)))
              ((equal (org-element-property :key element) "ALIAS")
               (setq aliases
                     (split-string (downcase (org-element-property :value element)) "\\s-*,\\s-*")))))
            ;; Deal with top-level node-properties
            ((and (eq type 'node-property)
                  (eq (org-element-type (org-element-property :parent element)) 'property-drawer)
                  (eq (org-element-type (org-element-property :parent (org-element-property :parent element))) 'section)
                  (eq (org-element-property :mode (org-element-property :parent (org-element-property :parent element))) 'first-section))
             (if (equal "ID" (org-element-property :key element))
                 (setq id (org-element-property :value element)))
             (if (equal "ROAM_ALIASES" (org-element-property :key element))
                 (setq roam_aliases (split-string-and-unquote (downcase (org-element-property :value element))))))))))
     ;; populate plist
     (if (and id (not (string= id "")))
         (progn
           (setq plist (plist-put plist :id-p t))
           (setq plist (plist-put plist :id id)))
       (setq plist (plist-put plist :id-p nil))
       (setq plist (plist-put plist :id (org-id-new))))
     (if (and title (not (string= title "")))
         (progn
           (setq plist (plist-put plist :title-p t))
           (setq plist (plist-put plist :title title)))
       (setq plist (plist-put plist :title-p nil))
       (setq plist (plist-put plist :title
                              (file-name-sans-extension
                               (file-name-base
                                (buffer-file-name (buffer-base-buffer)))))))
     (if-let ((logseq_aliases (seq-difference aliases roam_aliases)))
         (setq plist (plist-put plist :aliases logseq_aliases)))
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
    (pcase-dolist (`(,type ,beg ,end ,tgt ,raw) links)
      (if-let* ((path (if (eq 'file type) tgt
                        (gethash tgt reverse_map)))
                (props (gethash path inventory)))
          ;; This is a link to a known file, convert it
          (let ((id (plist-get props :id)))
            (push `(,type ,beg ,end ,tgt ,raw ,id) convert))))
    (sort convert (lambda (a b) (> (nth 1 a) (nth 1 b))))))

(defun org-roam-logseq--update-links (links)
  "Update all LINKS in current buffer.
This function returns t or an error code if there was an issue
updating the buffer."
  ;; First ensure that `raw' matches with the buffer's content for all links
  ;; (i.e. minimize chance that file was modified since our last
  ;; visit.)
  ;; TODO: Compute buffer secure hash instead.
  (catch 'fault
    (pcase-dolist (`(,_ ,beg ,end ,_ ,raw, _) links)
      (unless (equal (buffer-substring-no-properties beg end) raw)
        (throw 'fault 'mismatch)))
    ;; Past this point, attempt to update all links
    (pcase-dolist (`(,_ ,beg ,end ,_ ,_, id) links)
      (save-excursion
        (goto-char beg)
        (save-restriction
          (narrow-to-region beg end)
          (save-match-data
            ;; If it fails here, buffer is killed/reverted without saving
            ;; TODO: use element's data instead: it's already parsed!
            (if (not (re-search-forward org-link-bracket-re nil t))
                (throw 'fault 'link-re-mismatch)
              (goto-char (match-beginning 1))
              ;; 'file links are matched only when description is not empty, so
              ;; we can be sure we only need to replace the first match group.
              ;; 'title links may or may not have a description.  If there is a
              ;; description, we replace the first match group; if there isn't
              ;; we insert '[id:UUID]' just before the first match group.
              (if (match-beginning 2)
                  (progn
                    (delete-region (match-beginning 1) (match-end 1))
                    (insert (concat "id:" id)))
                (backward-char) ;; at "["
                (insert (concat "[id:" id "]"))))))))
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
     (lambda (file props)
       (let ((update_links (org-roam-logseq--filter-links
                            (plist-get :links props) inventory reverse_map)))
         (when (or update_links
                   (plist-get :aliases props)
                   (not (plist-get :title-p props))
                   (not (plist-get :id-p props)))
           ;; There's updates to perform, load the file in an org buffer
           (org-roam-logseq--with-edit-buffer file
                                              (org-roam-logseq--update-links update_links)
                                              (org-roam-logseq--update-top
                                               (unless (plist-get :id-p props) (plist-get :id props))
                                               (unless (plist-get :title-ed props) (plist-get :title props))
                                               (plist-get :aliases props))
                                              ;; Updates done, time to save the buffer
                                              ))))
     inventory)))

(defun org-roam-logseq--start (force create)
  "Log start of execution and state of FORCE and CREATE flags."
  (princ
   (concat
    (format "* Ran %s\n" (format-time-string "%x at %X"))
    "With flags:\n"
    (format "- ~force~ was: %s\n" force)
    (format "- ~create~ was: %s\n" create)
    "With settings:\n"
    (format "- ~org-roam-logseq-link-types~: %S\n" org-roam-logseq-link-types)
    (format "- ~org-roam-logseq-capture-template~: %S\n" org-roam-logseq-capture-template)
    "\n")))

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
