<a href="https://github.com/sbougerel/logseq-org-roam"><img src="https://www.gnu.org/software/emacs/images/emacs.png" alt="Emacs Logo" width="80" height="80" align="right"></a>
## logseq-org-roam.el
*Logseq Org-roam converter*

---

[![License GPLv3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.html)
[![CI](https://github.com/sbougerel/logseq-org-roam/actions/workflows/makefile.yml/badge.svg)](https://github.com/sbougerel/logseq-org-roam/actions)

This package provide facilities to convert Logseq files to `org-roam` files
and author missing `org-roam` files when necessary.  It should be used on the
entire `org-roam-directory` at once.  By default, it uses the `org-roam` cache
to ensure that only the necessary files are parsed.  Finally, it provides
clear descriptions of the changes it performs.  Overall, it makes Logseq and
`org-roam` interoperable to some extent.

### Usage


Call `logseq-org-roam` to convert Logseq files to `org-roam` files.  This
function is the single entry point to this package.  Its behaviour can be
modified with univeral arguments.  See `logseq-org-roam` for more
information.

*You should keep a backup of your `org-roam-directory` before using this
package.*

### Installation


With `use-package.el` and `straight.el`, you can grab this package from
Github with:

    (use-package logseq-org-roam
     :straight (:host github
                :repo "sbougerel/logseq-org-roam"
                :files ("*.el")))

### Interoperability with Logseq


This package assumes that the `org-roam-directory` shares its location with
the Logseq graph.  The expected file structure should be similar to:

    .
    └── org-roam-directory/
        ├── assets/          ;; attachements
        ├── journals/        ;; journals in Logseq, dailies in `org-roam`
        ├── logseq/          ;; private to Logseq, ignored by `org-roam`
        └── pages/           ;; Logseq pages, `org-roam` files

Sample `org-roam` configuration for interoperability with Logseq:

    (setq org-roam-directory "/path/to/org-roam-directory")
    (setq org-roam-dailies-directory "journals/")
    (setq org-roam-file-exclude-regexp "logseq/.*$")  ;; exclude Logseq files
    (setq org-roam-capture-templates
          '(("d" "default" plain "%?"
             ;; Accomodates for the fact that Logseq uses the "pages" directory
             :target (file+head "pages/${slug}.org" "#+title: ${title}\n")
             :unnarrowed t))
          org-roam-dailies-capture-templates
          '(("d" "default" entry "* %?"
             :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

Corresponding Logseq configuration for interoperability with `org-roam`:

    :preferred-format :org            ;; required
    :pages-directory "pages"          ;; default
    :journals-directory "journals"    ;; default, must match `org-roam-dailies-directory`
    :journal/page-title-format "yyyy-MM-dd"   ;; match with `org-roam-dailies-capture-templates`
    :journal/file-name-format "yyyy-MM-dd"    ;; match with `org-roam-dailies-capture-templates`
    :preferred-workflow :todo          ;; recommended
    :property-pages/enabled? false     ;; recommended, disable property pages

Finally, provide the following settings for `logseq-org-roam`:

    (setq logseq-org-roam-link-types 'fuzzy) ;; or 'files, depending on the
                                             ;; setting ":org-mode/insert-file-link?"
                                             ;; See `logseq-org-roam-link-types`
    (setq logseq-org-roam-pages-directory "pages")
    (setq logseq-org-roam-journals-directory "journals")
    (setq logseq-org-roam-journals-file-name-format "%Y-%m-%d")
    (setq logseq-org-roam-journals-title-format "%Y-%m-%d")

With the configurations above, this package makes it possible to use Logseq
and `org-roam` together.

### Caveats


This package assumes that you prefer to use `org-roam` as your primary note
taking tool; and Logseq as a companion.  When it converts a file from Logseq
to `org-roam`, it can update it link to use ID links.  Logseq recognizes ID
links for navigation, but does not reference them properly as of version
0.10.3: this means that once converted to an ID link, the backlink will show
under "Unlinked references" in Logseq.  This limitation extends to the graph;
the link will not be shown in the graph.  In fact, a fully converted set of
notes (where all links are ID links) will result in an empty graph.  However
all files are still searchable and navigable in Logseq.  If you care about
the graph in Logseq, this package is not for you.

Logseq's workflow often results in leftovers "*" at the bottom files.  In
some versions of `org`, the `org-element` parser may throw errors
(e.g. "wrong-side-of-point").  These errors prevent `org-roam` from parsing
the buffer properly and updating the database.  Try to keep your files tidy
in Logseq and not leave trailing stars.

This package only deal with updating a file's first section (top properties,
title, aliases) and its links.  Converting timestamps, tags, asset location
is not supported.  In some cases, interoperability can be maintained by
keeping some discipline or aligning configurations (e.g. for templates).

Finally, the changes done by the package are destructive, and backup copies
are not kept by this package.  The author uses version control.

### History


This package is based on an original [gist by William R Burdick
Jr](https://gist.github.com/zot/ddf1a89a567fea73bc3c8a209d48f527) and its port
to a [package by Ivan Danov](https://github.com/idanov/org-roam-logseq.el).  It
is a complete rewrite.

Some limitations in the orignal works above motivated me to write a new
package:

- `logseq-org-roam` is the single entry point

- `logseq-org-roam` supports multiple `org-roam` directories (Logseq graphs)

- `logseq-org-roam` has a more robust logic to match Logseq links to
  `org-roam` nodes.  For example, it does not confuse internal links, it
  supports aliases, disregards case-sensitivity for the file's base name, and
  more.

- `logseq-org-roam` provides better support for journal dates.

- `logseq-org-roam` is able to add new entries from new (dead) links created
  in Logseq, as you normally would in the `org-roam` workflow, since Logseq
  does not do it.  Which in turn makes `org-roam` cache usage even more
  relevant.

- `logseq-org-roam` is verbose about the changes it makes.

- Finally, `logseq-org-roam` should be rather fast; it should be able to
  handle 500 files in about 10 seconds (when ignoring the cache)

It's not all better.  `logseq-org-roam` has some drawbacks compared to the
original works above:

- No single-file update is provided, as in `org-roam-logseq.el`.

- `logseq-org-roam` is considerably more complex (more than the author
  anticipated).  Since it is a larger package, it should pay off to lazy-load
  it.  To this end, autoload cookies are assigned to `logseq-org-roam` as
  well as customization options.



### Customization Documentation

#### `logseq-org-roam-link-types`

The kind of links `logseq-org-roam` should convert.
Value is a symbol, only the following are recognized:
- `'files`
- `'fuzzy`
- nil (default, if unrecognized)

You should customize this value based on your
`:org-mode/insert-file-link?` setting in Logseq.  Values other
than nil save some processing time.

Links considered as candidates to be converted to `org-roam`
ID links are of 2 types:

- File links such as: `[[file:path/to/pages/page.org][DESCRIPTION]]`.
- Fuzzy links such as `[[TITLE-OR-ALIAS][DESCRIPTION]]`.


Matching rules for each kind of links are as follows.

When dealing with file links, `logseq-org-roam` ignores links
that do not contain a description since Logseq always populates
it when referencing another page.  It also ignores links that
contain search options since Logseq does not create those.  And
finally it discards any links that is not a link to an `org-roam`
file (since these are not convertible to ID links).

When dealing with fuzzy links, it first ignores dedicated internal
link formats that have specific meaning in `org-mode` (even if
they are broken):

- `[[#custom-id]]` links,
- `[[*heading]]` links,
- `[[(coderef)]]` links,
- `[[image.jpg]]` inline links to images,

Of the remaining fuzzy links, it discards links that match
internally (as per `org-mode` rules) with:

- `<<targets>>` or,
- `#+name`: named elements or,
- a headline by text search,

The leftover links are the candidates to be converted to
`org-roam` external ID links.


Notes on using file links in Logseq.

It is usually recommended to set `:org-mode/insert-file-link?`
to true in Logseq, presumably to ensure the correct target is
being pointed to.

Unfortunately, Logseq does not always provide a correct path (as
of version 0.10.3) on platforms tested (Android, Linux). As of
version 0.10.3, when a note does not exist yet (or when it is
aliased, see `https://github.com/logseq/logseq/issues/9342`), the
path provided by Logseq is incorrect.  (TODO: test in newer
versions.)

On the other hand `logseq-org-roam` cares to implement the
complex matching rules set by `org-roam` to convert the right
fuzzy links, making Logseq and `org-roam` mostly interoperable
even when using fuzzy links in Logseq.

#### `logseq-org-roam-pages-directory`

Set this variable to mirror Logseq :pages-directory setting.

#### `logseq-org-roam-journals-directory`

Set this variable to mirror Logseq :journals-directory setting.

#### `logseq-org-roam-journals-file-name-format`

Set this variable to mirror Logseq :journal/file-name-format setting.
You should pick a format that `logseq-org-roam-maybe-date-func`
can use.  Otherwise, titles for journal entries will not be
formated correctly: `logseq-org-roam` first parses the file name
into a time before feeding it back to `format-time-string` to
create the title (See: `logseq-org-roam-jounals-title-format`).

#### `logseq-org-roam-journals-title-format`

Set this variable to mirror Logseq :journal/file-name-format setting.
This is used to create a title for journal entires and to find
out which fuzzy links point to journal entries (See
`logseq-org-roam-maybe-date-func`).

You can set this to any format that `format-time-string` accepts.
However, you should only use it to create date strings, and not
time strings.  Having hours and seconds in the format will
make it impossible to find out journal entries from fuzzy links.

#### `logseq-org-roam-maybe-date-func`

Try parsing a string into a date and return time when successful.
When non-nil, this variable is called with `funcall`.  It is
given 2 arguments: the first is a time format for
`format-time-string`, the second is the string to evaluate.  It
is expected to return a time, like `date-to-time` or
`encode-time`.  If the time returned is 0, it assumes that the
string is not a date.  See `logseq-org-roam-maybe-date-default`
for a description of the default behaviour.

If nil, date parsing is disabled.

#### `logseq-org-roam-create-replace`

Alist specifying replacements for fuzzy links.
Car and cdr of each cons will be given as arguments to
`replace-regexp-in-string` when converting fuzzy links to paths
in `logseq-org-roam-create-translate-default`.

#### `logseq-org-roam-create-translate-func`

Function translating a fuzzy link to a file path.
When non-nil, it is called with `funcall` and a single argument,
the fuzzy link.  It is expected to return an absolute file path.
This variable provide complete control over how fuzzy links are
translated to file paths.

Default to `logseq-org-roam-create-translate-default`.  Setting
this value to nil disables creation of pages for fuzzy links.

#### `logseq-org-roam-create-accept-func`

Tells aparts paths that should be created from paths that should not.
When non-nil, it is called as a function with a single argument:
the path.  When the return value is non-nil, the path is accepted
and the file is created.

The default value (`logseq-org-roam-pages-p`) will not create
journal entires.  If you want journal entries to be created too,
you can set this to `logseq-org-roam-logseq-p`.  If you want to
allow files to be created anywhere, you can set this to `always`.

When set to nil, file creation is disabled.

#### `logseq-org-roam-updated-hook`

Hook called  by `logseq-org-roam` if any files was updated.

### Function and Macro Documentation

#### `(logseq-org-roam-pages-p FILE)`

Return non-nil if FILE path is under the Logseq pages directory.

#### `(logseq-org-roam-journals-p FILE)`

Return non-nil if FILE path is under the Logseq journal directory.

#### `(logseq-org-roam-logseq-p FILE)`

Return non-nil if FILE path is under the Logseq journal or pages directory.

#### `(logseq-org-roam-maybe-date-default DATE-FORMAT MAYBE-DATE)`

Turn MAYBE-DATE into time if it matches DATE-FORMAT.
Attempts to parse MAYBE-DATE with `parse-time-string` first and
convert it back to a string with `format-time-string` using
DATE-FORMAT.  If both string match, it is taken as a journal
date, and the corresponding time is returned.

#### `(logseq-org-roam-create-translate-default FUZZY)`

Transform FUZZY into an expanded file path.
If the fuzzy link represents a date, it will translate the link
to a path under the journal directory (see
`logseq-org-roam-journals-directory`) otherwise translate the
link to a file path under the pages directory (See
`logseq-org-roam-pages-directory`).
To tell apart dates from other strings, it uses
`logseq-org-roam-maybe-date-func`.  Special characters in the
link path are also replaced, see:
`logseq-org-roam-create-replace`.

#### `(logseq-org-roam &optional MODE)`

Migrate files edited with Logseq to `org-roam`.
Parse files returned by `org-roam-list-files` that are not part
of the `org-roam` cache, and if it finds files that were created
by Logseq, it updates these files to set ID, title, aliases, or
convert links to other files by using ID-links.

Please note that while care was taken to ensure its reliability,
you should have a mean to revert the changes it makes on your
files (e.g. backup or version control) before running this
function.

While using this facility regularly makes `org-roam` and Logseq
mostly interoperable; ID-links in Logseq show up as "Unlinked
Reference" and thus break the connection in your Logseq graph.
If you primarily rely on Logseq, this could be a deal-breaker.
When called with `C-u` or MODE is not nil, it

parses any files returned by `org-roam-list-files` (even if they
are already indexed by `org-roam`).  This is useful if you've had
a lot of edits with Logseq (or other), and some of the files that
are in `org-roam` may still contain links to other files that are
not converted to ID-links yet.

When `logseq-org-roam` encounters a link created with Logseq to a
page that does not exists yet, it can create that page for you,
leveraging your `org-roam-capture-templates`, as you would if you
were using the normal `org-roam` workflow.  This mode of
operation is enabled when `logseg-to-roam-create` is t or when
calling this function with (double) `C-u C-u`.

To active both creation and forcing to parse all files returned
by `org-roam-list-files` call this function with (triple)
`C-u C-u C-u`.

When calling `logseq-org-roam` programmatically it accepts the
following arguments:

- nil: parse only files that are not yet indexed (by `org-roam`)
  and does not create any new files (when it encounters a link
  created by Logseq without an existing target).
- '(4) or 4 or 'force: parse all files (even those already
  indexed) and does not create any new files.  Equivalent to
  `C-u M-x logseq-org-roam`.
- '(16) or 16 or 'create: parse only files that are not yet
  indexed and create new files using your capture templates (when
  it encounters a Logseq link without target).  Equivalent to
  `C-u C-u M-x logseq-org-roam`.
- '(64) or 64 or 'force-create: parse all files and create new
  files using your capture templates.  Equivalent to
  `C-u C-u C-u M-x logseq-org-roam`.
  
To find out how `logseq-org-roam` detects Logseq links, read the
documentation string of `logseq-org-roam-link-types`.  To find
out how `logseq-org-roam` uses your own capture templates, read
the documentation string of `logseq-org-roam-capture`.

-----
<div style="padding-top:15px;color: #d0d0d0;">
Markdown README file generated by
<a href="https://github.com/mgalgs/make-readme-markdown">make-readme-markdown.el</a>
</div>
