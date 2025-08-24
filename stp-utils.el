;;; -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 David J. Rosenbaum <djr7c4@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of version 3 of the GNU General Public License, as
;; published by the Free Software Foundation.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl-lib)
(require 'crm)
(require 'f)
(require 'map)
(require 'pp)
(require 'rem)
(require 'rem-abbrev)
(require 's)
(require 'seq)
(require 'stp-locked)

;; This is required for Emacs 30+. The fix is merged upstream in the development
;; version and should be remove here once there is a tagged release.
(defvar stp-async-inject-variables-exclude-regexp "-abbrev-table\\'")

(defvar stp-async-inject-large-variables-exclude-regexp "\\|-history$\\|^stp-package-info$\\|^stp-latest-versions-cache$")

(defvar stp-ellipsis (if (char-displayable-p ?…) "…" "..."))

(defvar stp-no-break-space (propertize " " 'display " "))

(defvar stp-package-info nil)

(defvar stp-log-buffer-name "*STP Log*"
  "All messages from STP are logged in this buffer.")

(defvar stp-log-max message-log-max
  "The maximum number of lines of STP log messages to keep in
`stp-log-buffer-name'. When it is nil, logging is disabled. When
it is t, the buffer is never truncated.")

(defun stp-msg (&rest args)
  (with-current-buffer (get-buffer-create stp-log-buffer-name)
    (let ((msg (apply #'message args)))
      (when stp-log-max
        (insert msg "\n"))))
  (stp-truncate-log))

;; Based on `comint-truncate-buffer'.
(defun stp-truncate-log ()
  (awhen (and (integerp stp-log-max) (get-buffer stp-log-buffer-name))
    (with-current-buffer it
      (save-excursion
        (goto-char (point-max))
        (forward-line (- stp-log-max))
        (beginning-of-line)
        (let ((inhibit-read-only t))
          (delete-region (point-min) (point)))))))

(defun stp-negate (value)
  "If VALUE is a function, return a function that returns the
negation of that function. Otherwise, return the negation of
VALUE."
  (if (functionp value)
      (fn (not (apply value &*)))
    (not value)))

(defun stp-and (&rest values)
  "If no value is a function, return the non-nil if all values
are non-nil. Otherwise, return a function that calls each value
that is a function and returns non-nil if all of the results are
non-nil."
  (if (cl-some #'functionp values)
      (lambda (&rest args)
        (cl-every (lambda (value)
                    (if (functionp value)
                        (apply value args)
                      value))
                  args))
    (cl-every #'identity values)))

(defun stp-maybe-call (value &rest args)
  "If VALUE is a function with ARGS, call it and return the result.
Otherwise, return VALUE."
  (if (functionp value)
      (apply value args)
    value))

(defun stp-short-format-seconds (seconds)
  (concat (if (< seconds 0) "-" "")
          (progn
            (setq seconds (abs seconds))
            (cond
             ((= seconds 0)
              "now")
             ((< seconds 60)
              (format-seconds "%ss" seconds))
             ((< seconds (* 24 60 60))
              (format-seconds "%hh%mm%z" seconds))
             (t
              (format-seconds "%yy%dd%z" seconds))))))

(defun stp-short-format-date (timestamp)
  (format-time-string "%d/%m/%y" timestamp))

(defvar stp-annotated-version-type 'time-delta
  "This specifies how versions are annotated with timestamps.

When it is \\='timestamp, the timestamp for the version is used.
When it is \\='time-delta, the amount of time from the installed
version is used.")

(defun stp-latest-version-annotation (count version-timestamp latest-timestamp)
  (let* ((count-string (cond
                        ((consp count)
                         (db (m n)
                             count
                           (format "%+d%d" m (- n))))
                        ((and (integerp count) (/= count 0))
                         (format "%d" count))
                        (t
                         "")))
         (time-string (if (and version-timestamp latest-timestamp)
                          (cl-ecase stp-annotated-version-type
                            (time-delta
                             (let ((seconds (- latest-timestamp version-timestamp)))
                               (if (/= (round seconds) 0)
                                   (stp-short-format-seconds seconds)
                                 "")))
                            (timestamp
                             (stp-short-format-date latest-timestamp)))
                        ""))
         (separator (if (and (not (string= count-string ""))
                             (not (string= time-string "")))
                        ";"
                      ""))
         (combined-string (concat count-string separator time-string)))
    (if (not (string= combined-string ""))
        (format "(%s)" combined-string)
      "")))

(defun stp-min-version-annotation (min-version enforce-min-version)
  (if (and (not enforce-min-version) min-version)
      (format " (>= %s required)" min-version)
    ""))

(defun stp-prefix-prompt (prompt-prefix prompt)
  (if (or (not prompt-prefix) (string= prompt-prefix ""))
      prompt
    (concat prompt-prefix
            (s-downcase (substring prompt 0 1))
            (substring prompt 1))))

(defun stp-sort-paths-top-down (paths)
  (-sort (lambda (path path2)
           (or (< (rem-path-length path)
                  (rem-path-length path2))
               (and (= (rem-path-length path)
                       (rem-path-length path2))
                    (string< path path2))))
         paths))

(defun stp-delete-load-path (pkg-name)
  (setq load-path (cl-delete-if (lambda (path)
                                  (f-same-p (stp-canonical-path pkg-name)
                                            (f-slash path)))
                                load-path)))

(defun stp-canonical-path (pkg-name)
  "Return the canonical path to pkg-name. The return value always
ends with a slash."
  (stp-full-path pkg-name t))

(defun stp-full-path (pkg-name &optional canonical)
  "Return the full path to pkg-name. The return value always ends
with a slash."
  (let ((path (if (f-absolute-p pkg-name)
                  pkg-name
                (f-join stp-source-directory pkg-name))))
    (when canonical
      (setq path (f-canonical path)))
    (f-slash path)))

(defun stp-symbol-package-name (sym)
  (if (symbolp sym)
      (symbol-name sym)
    sym))

(defun stp-name (pkg-path)
  "Returns the name of the package. Unlike `stp-relative-path', this
never ends with a slash (nor does it contain any slashes)."
  (f-filename pkg-path))

(defmacro stp-with-package-source-directory (&rest body)
  (declare (indent 0))
  `(let ((default-directory stp-source-directory))
     ,@body))

(def-edebug-spec stp-with-package-source-directory t)

(defun stp-filesystem-names ()
  "Return a list of packages installed in `stp-source-directory'."
  (stp-with-package-source-directory
    (-filter #'f-dir-p (directory-files stp-source-directory nil "^[^.]"))))

(defun stp-get-info-groups ()
  (map-elt stp-package-info 'groups))

(defun stp-set-info-groups (groups)
  (setf (map-elt stp-package-info 'groups) groups))

(defun stp-get-info-group-names ()
  (mapcar #'car (stp-get-info-groups)))

(defun stp-get-info-group (group-name)
  (map-elt (stp-get-info-groups) group-name))

(defun stp-set-info-group (group-name pkg-names)
  (let ((groups (stp-get-info-groups)))
    (setf (map-elt groups group-name) pkg-names)
    (stp-set-info-groups groups)
    pkg-names))

(defun stp-delete-info-group (group-name)
  (let ((groups (stp-get-info-groups)))
    (setq groups (map-delete groups group-name))
    (stp-set-info-groups groups)
    groups))

(defun stp-get-info-packages ()
  (map-elt stp-package-info 'packages))

(defun stp-set-info-packages (packages)
  (setf (map-elt stp-package-info 'packages) packages))

(defun stp-info-names (&optional method)
  "Return a list of packages stored in `stp-package-info'."
  (sort (mapcar #'car
                (-filter (lambda (pkg)
                           (or (not method)
                               (let ((pkg-alist (cdr pkg)))
                                 (eq (map-elt pkg-alist 'method) method))))
                         (stp-get-info-packages)))
        #'string<))

(defvar stp-read-name-history nil)

(defun stp-default-name (remote)
  ;; Remove .git and then the extension. This causes the default name for a
  ;; repository like "abc.el.git" to be "abc".
  (f-no-ext (s-chop-suffix ".git" (f-filename remote))))

(defvar stp-candidate-separator "  ")

(defun stp-toggle-plist (prompt plist)
  (cl-labels ((make-cand (group width)
                (format "%s%s%S"
                        (string-pad (car group) width)
                        stp-candidate-separator
                        (cadr group)))
              (read-toggle ()
                (let* ((groups (mapcar (lambda (group)
                                         (list (s-chop-prefix ":do-" (symbol-name (car group))) (cadr group)))
                                       (-partition 2 plist)))
                       (width (apply #'max (mapcar (-compose #'length #'car) groups)))
                       (candidates (append (list "done")
                                           (mapcar (-rpartial #'make-cand width) groups))))
                  (rem-comp-read prompt
                                 candidates
                                 :require-match t
                                 :sort-fun #'identity))))
    (let (choice)
      (setq plist (cl-copy-list plist))
      (while (not (string= (setq choice (read-toggle)) "done"))
        (let ((kwd (intern (format ":do-%s" (car (s-split " " choice))))))
          (setf (plist-get plist kwd) (not (plist-get plist kwd)))))
      plist)))

(defun stp-skip-package ()
  "Skip installing or upgrading this package."
  (interactive)
  (throw 'stp-skip 'skip))

(defvar-keymap stp-skip-map
  "C-c C-k" #'stp-skip-package)

(cl-defmacro stp-maybe-allow-skip ((skip-sym &optional skip-form) &rest body)
  (declare (indent 1))
  (with-gensyms (result)
    `(cl-flet ((setup-keymap ()
                 (when ,skip-sym
                   (use-local-map (make-composed-keymap (list stp-skip-map) (current-local-map))))))
       (let ((,result (minibuffer-with-setup-hook (:append #'setup-keymap)
                        (catch 'stp-skip
                          ,@body))))
         (when (eq ,result 'skip)
           ,skip-form)
         ,result))))

(def-edebug-spec stp-maybe-allow-skip ((form &optional form) body))

(cl-defun stp-read-name (prompt &key default)
  "Read the name of a package."
  (rem-read-from-mini prompt :history 'stp-read-name-history :initial-contents default))

(cl-defun stp-read-existing-name (prompt &key (require-match t) multiple (table (stp-info-names)))
  "Read the name of a package that is already installed."
  (rem-comp-read prompt
                 table
                 :require-match require-match
                 :history 'stp-read-name-history
                 :multiple multiple))

(defvar stp-read-group-name-history nil)

(defun stp-read-group-name (prompt)
  (rem-comp-read prompt (stp-get-info-group-names) :require-match nil :history 'stp-read-group-history))

(defun stp-expand-groups (group-names)
  "Return a list of all packages specified by GROUP-NAMES. A group
name may also be the name of a package."
  (let ((groups (stp-get-info-groups)))
    (-uniq (mapcan (fn (or (cdr (assoc % groups)) (list %))) group-names))))

(defvar stp-attribute-order '(method remote other-remotes last-remote version update branch dependency requirements)
  "The order in which package attributes should be sorted before being written
  to disk.")

(defvar stp-remote-valid-alist '((git . stp-git-valid-remote-p)
                                 (elpa . stp-elpa-valid-remote-p)
                                 (archive . stp-archive-valid-remote-p)
                                 (url . stp-url-valid-remote-p))
  "This alist maps predicates for testing if a remote is valid for a
given method to methods.")

(cl-defun stp-remote-method (remote &key noerror ignored-methods)
  "Determine the method for REMOTE. If NOERROR is non-nil, then do
not signal an error when REMOTE is invalid. Methods in
IGNORED-METHODS are not considered."
  (or (car (cl-find-if (lambda (cell)
                         (and (not (memq (car cell) ignored-methods))
                              (funcall (cdr cell) remote)))
                       stp-remote-valid-alist))
      (unless noerror
        (error "Invalid remote: %s" remote))))

(defvar stp-methods-order (mapcar #'car stp-remote-valid-alist)
  "Valid values for the METHOD attribute.")

(defun stp-sort-remotes (remotes)
  "Sort the alist REMOTES that maps remotes to methods by method
according to the order in `stp-methods-order'. REMOTES may also
contain strings that map to remotes symbols representing
archives."
  (seq-sort-by (lambda (remote)
                 (let ((method-or-archive (cdr remote)))
                   (cl-position (if (not (memq method-or-archive stp-methods-order))
                                    'archive
                                  method-or-archive)
                                stp-methods-order)))
               #'<
               remotes))

(defun stp-read-method (prompt &optional default)
  (when (and default (symbolp default))
    (setq default (symbol-name default)))
  (intern (rem-comp-read prompt
                         (mapcar #'symbol-name stp-methods-order)
                         :require-match t
                         :default default
                         :sort-fun #'identity)))

(defvar stp-development-directory nil
  "Set this to the directory that contains local elisp repositories
for packages that you develop.")

(defun stp-normalize-remote (remote)
  ;; Use absolute paths for local repositories.
  (let ((default-directory stp-development-directory))
    (if (f-exists-p remote)
        (setq remote (f-slash (f-full remote)))
      remote)))

(defun stp-read-remote-with-predicate (prompt valid-remote-p &optional default history)
  (let (remote)
    (while (or (not remote)
               (consp remote)
               (not (funcall valid-remote-p remote)))
      (when remote
        (minibuffer-message "%s is invalid" remote))
      (setq remote
            (let ((default-directory (or stp-development-directory
                                         default-directory)))
              (-> (rem-comp-read prompt
                                 #'completion-file-name-table
                                 :default default
                                 :history history
                                 :metadata '((category . nil)))
                  stp-normalize-remote))))
    remote))

(defvar stp-use-other-remotes t
  "If this variable is non-nil, allow the user to choose from the
other-remotes attribute in `stp-info-file' when a remote needs to
be selected.")

(defvar stp-remote-history nil)

(cl-defun stp-comp-read-remote (prompt known-remotes &key default multiple (normalize t))
  (cl-labels ((valid-candidate-p (candidate)
                (or (member candidate known-remotes)
                    (f-dir-p candidate)))
              (valid-candidates-p (candidates)
                (if multiple
                    (cl-every #'valid-candidate-p (s-split crm-separator candidates))
                  (valid-candidate-p candidates))))
    (let* ((default-directory (or stp-development-directory
                                  default-directory))
           (new-remotes (rem-comp-read prompt
                                       (completion-table-in-turn known-remotes
                                                                 #'completion-file-name-table)
                                       :predicate #'valid-candidates-p
                                       :default default
                                       :history 'stp-remote-history
                                       :sort-fun #'identity
                                       ;; Disable the category. By default, it
                                       ;; will be 'file which will cause https://
                                       ;; to be replaced with / during completion.
                                       :metadata '((category . nil))
                                       :multiple multiple)))
      (cond
       ((null normalize)
        new-remotes)
       ;; When multiple is nil, new-remotes will just be a single remote rather
       ;; than a list.
       (multiple
        (mapcar #'stp-normalize-remote new-remotes))
       (t
        (stp-normalize-remote new-remotes))))))

(defun stp-choose-remote (prompt remote &optional other-remotes)
  (if stp-use-other-remotes
      ;; A match is not required. This way, a new remote can be added
      ;; interactively by the user. Type ./ to complete in the current
      ;; directory.
      (stp-comp-read-remote prompt (cons remote other-remotes) :default remote)
    remote))

(defvar stp-prefer-chosen-remote nil
  "When non-nil, always put the last chosen remote as the \\='remote
attribute and relegate the others to \\'other-remotes. Otherwise,
do not reorder the remotes based on which was chosen.")

(defun stp-update-remotes (pkg-name chosen-remote remote other-remotes)
  ;; Remote should always be chosen-remote since that is where the package was
  ;; just installed or upgraded from. (See the documentation of
  ;; `stp-info-file'.) Other-remotes is whatever other remotes exist that were
  ;; not chosen.
  (stp-set-attribute pkg-name 'last-remote chosen-remote)
  (when stp-prefer-chosen-remote
    (stp-set-attribute pkg-name 'remote chosen-remote)
    (when (or other-remotes (not (string= chosen-remote remote)))
      (->> (cons remote other-remotes)
           (remove chosen-remote)
           (stp-set-attribute pkg-name 'other-remotes)))))

(defun stp-github-io-transformer (remote)
  "Transform github.io pages to git repositories."
  (format "https://github.com/%s/%s" (match-string 1 remote) (match-string 2 remote)))

(defun stp-github-no-extension-transformer (remote)
  "Remove .git extensions from github repositories."
  (match-string 1 remote))

(defvar stp-remote-transformers
  '(("\\(?:http[s]?://\\)\\(?:www\\.\\)?\\(.+\\)\\.github\\.io/\\(.+\\)/?" . stp-github-io-transformer)
    ("\\(\\(?:http[s]?://\\)\\(?:www\\.\\)?github\\.com/\\(?:.+\\)/\\(?:.+\\)\\)\\.git" . stp-github-no-extension-transformer))
  "This is an alist that maps regular expressions to functions that
transform the :url fields of the extras slot of `package-desc'
objects. A function is applied with the remote as the argument if
the corresponding regular expression matches. Only the first
function that applies is used to transform the remote. When
functions are applied, the match data from their regular
expression is active.")

(defun stp-transform-remote (remote)
  "Transform REMOTE by applying `stp-remote-transformers'."
  ;; `cl-dolist' is used because of the use of `cl-return'. `dolist' only works
  ;; with `cl-return' when the deprecated `cl' library is loaded.
  (or (cl-dolist (cell stp-remote-transformers)
        (db (regexp . transformer)
            cell
          (when (string-match regexp remote)
            (cl-return (funcall transformer remote)))))
      remote))

(defun stp-elisp-files (directories)
  (-flatten (mapcar (lambda (dir)
                      (-filter (lambda (path)
                                 (member (f-ext path) '("el" "el.gz")))
                               (f-entries dir)))
                    directories)))

(defun stp-requirements-to-names (requirements)
  (mapcar (-compose #'symbol-name #'car) requirements))

(defun stp-required-by (pkg-name)
  "Return the list of packages that require PKG-NAME."
  (let ((pkg-sym (intern pkg-name)))
    (mapcar #'car
            (-filter (lambda (cell)
                       (db (_pkg-name2 . pkg-alist2)
                           cell
                         (let-alist pkg-alist2
                           (cl-find-if (lambda (requirement)
                                         (eq (car requirement) pkg-sym))
                                       .requirements))))
                     (stp-get-info-packages)))))

(defun stp-no-leading-zeros (string)
  (save-match-data
    (if (and (string-match "^0+" string))
        (or (rem-empty-nil (substring string (match-end 0)))
            "0")
      string)))

(defun stp-number-string-p (string)
  (string-match-p "[+-]?[0-9]+" string))

(defun stp-version-component= (v1 v2)
  (let ((v1-nz (stp-no-leading-zeros v1))
        (v2-nz (stp-no-leading-zeros v2)))
    (string= v1-nz v2-nz)))

(defun stp-version-component< (v1 v2)
  ;; Remove leading zeros.
  (let ((v1-nz (stp-no-leading-zeros v1))
        (v2-nz (stp-no-leading-zeros v2)))
    ;; Compare string as numbers when possible.
    (if (and (stp-number-string-p v1)
             (stp-number-string-p v2))
        (< (string-to-number v1) (string-to-number v2))
      ;; When strings are the same without leading zeros, consider the longer
      ;; string to be a newer version.
      (or (and (string= v1-nz v2-nz)
               (< (length v1) (length v2)))
          ;; Compare strings after removing leading zeros.
          (rem-shortlex-string< v1-nz v2-nz)))))

(defun stp-version-list< (v1 v2)
  (and (not (and (null v1) (null v2)))
       ;; Missing elements default to 0 as in `version-to-list'.
       (let ((v1-comp (if v1 (car v1) "0"))
             (v2-comp (if v2 (car v2) "0")))
         (or (stp-version-component< v1-comp v2-comp)
             (and (stp-version-component= v1-comp v2-comp)
                  (stp-version-list< (cdr v1) (cdr v2)))))))

(defun stp-version< (v1 v2)
  "Determine if v2 of the package is newer than v1."
  (stp-version-list< (stp-version-extract v1)
                     (stp-version-extract v2)))

(defun stp-version= (v1 v2)
  "Determine if v1 of the package is equal to v2."
  (equal (stp-version-extract v1) (stp-version-extract v2)))

(defun stp-version<= (v1 v2)
  "Determine if v1 of the package is less than or equal to v2."
  (let ((v1e (stp-version-extract v1))
        (v2e (stp-version-extract v2)))
    (or (equal v1e v2e)
        (stp-version-list< v1e v2e))))

(defun stp-filter-by-min-version (min-version versions)
  (-filter (lambda (version)
             (or (not (stp-version-extract version))
                 (stp-version<= min-version version)))
           versions))

(defvar stp-info-file (f-join user-emacs-directory "stp-pkg-info.eld")
  "The name of the file that stores the package information.

This consists of an alist that maps the name of each package to
an alist that stores information for each package. Each of These
alists is of the form

  \\='((method . ...)
    (remote . ...)
    (other-remotes ...)
    (last-remote ...)
    (version . ...)
    (update . ...)
    (branch . ...)
    (dependency ...)
    (requirements ...))

Method should be one of the symbols \\='git or \\='url. Remote
should be a \\='url indicating the location that the package was
installed from. If method is \\='git, it should be a git
repository and if it is \\='url it should be the URL it was
downloaded from. other-remotes should be a list of alternative
remotes that the user may select to use instead of remote.
last-remote should be the most remote that was most recently used
to install or upgrade the package. Version should be a string
that indicates the version. If method is \\='git, this should be
a ref. If method is \\='url, it can be any string that the user
cares to use to describe the current version. If method is
\\='git, then update indicates the update mechanism. \\='stable
means that stable versions are being used and \\='unstable means
that the value associated with the branch attribute is being
used. If the method is \\='url, the \\='update and \\='branch
attributes should not be present. dependency should be non-nil
when the package was installed as a dependency of another package
rather than explicitly by the user. requirements is a list of the
requirements for the package as they would appear in the
Package-Requires header of an elisp file.")

(defun stp-attribute< (attribute attribute2)
  (< (cl-position attribute stp-attribute-order)
     (cl-position attribute2 stp-attribute-order)))

(defun stp-sort-info (pkg-info)
  "Sort the groups and packages in PKG-INFO."
  (let ((groups (map-elt pkg-info 'groups))
        (packages (map-elt pkg-info 'packages)))
    (rem-maybe-args (cons 'groups (stp-sort-info-groups groups)) groups
                    (cons 'packages (stp-sort-info-packages packages)) packages)))

(defun stp-sort-info-groups (groups)
  "Sort package groups alphabetically by name."
  (-sort (fn (string< (car %1) (car %2))) groups))

(defun stp-sort-info-packages (packages)
  "Sort packages alphabetically by name and sort their attributes
according to `stp-attribute-order'."
  (mapcar (lambda (cell)
            (cons (car cell)
                  (-sort (fn (stp-attribute< (car %1) (car %2)))
                         (cdr cell))))
          (-sort (fn (string< (car %1) (car %2)))
                 packages)))

(defun stp-read-info ()
  ;; We don't signal an error when stp-info-file doesn't exist. This just means
  ;; that STP is being run for the first time.
  (when (file-readable-p stp-info-file)
    (with-temp-buffer
      (insert-file-contents stp-info-file)
      (read (buffer-string)))))

(defun stp-refresh-info ()
  (stp-force-refresh-info))

;; This version exists for when memoization should not be used.
(defun stp-force-refresh-info ()
  (setq stp-package-info (stp-read-info)))

(defun stp-write-info ()
  (with-temp-buffer
    (insert ";;; -*- buffer-read-only: t; no-byte-compile: t; -*-\n\n")
    (pp (stp-sort-info stp-package-info) (current-buffer))
    (f-write (buffer-string) 'utf-8 stp-info-file)))

(defun stp-get-attribute (pkg-name attr)
  "Get the attribute attr in the alist with the key corresponding to
pkg-name."
  (let ((pkg-name (stp-name pkg-name)))
    (map-elt (map-elt (stp-get-info-packages) pkg-name) attr)))

(defun stp-set-attribute (pkg-name attr val)
  "Set the attribute attr to val in the alist with the key
corresponding to pkg-name."
  (let* ((pkg-name (stp-name pkg-name))
         (packages (stp-get-info-packages))
         (alist (map-elt packages pkg-name)))
    (if alist
        (setf (map-elt alist attr) val
              (map-elt packages pkg-name) alist)
      (setq packages (cons `(,pkg-name . ((,attr . ,val))) packages))
      (stp-set-info-packages packages))
    val))

(defun stp-delete-attribute (pkg-name attr)
  "Remove attr from the alist with the key corresponding to
pkg-name."
  (let ((alist (stp-get-alist pkg-name)))
    (stp-set-alist pkg-name (remq (assoc attr alist) alist))))

(defun stp-get-alist (pkg-name)
  "Get the alist that contains information corresponding to
pkg-name."
  (let ((pkg-name (stp-name pkg-name)))
    (map-elt (stp-get-info-packages) pkg-name)))

(defun stp-set-alist (pkg-name alist)
  "Set the alist that contains information corresponding to pkg-name
to alist."
  (let* ((pkg-name (stp-name pkg-name))
         (packages (stp-get-info-packages)))
    (setf (map-elt packages pkg-name) alist)
    (stp-set-info-packages packages)
    alist))

(defun stp-delete-alist (pkg-name)
  "Remove the alist that contains information corresponding to
PKG-NAME."
  (let ((packages (stp-get-info-packages)))
    (setq packages (map-delete packages pkg-name))
    (stp-set-info-packages packages)))

(defun stp-find-unnecessary-dependencies ()
  "Return a list of the names of all packages that are no longer
required by another package but were installed as dependencies."
  (mapcar #'car
          (-filter (fn (map-elt (cdr %) 'dependency))
                   (stp-get-info-packages))))

(defvar stp-version-regexp "^\\(?:\\(?:v\\|V\\|release\\|Release\\|version\\|Version\\)\\(?:[-_./]?\\)\\)?\\([0-9]+[a-zA-Z]?\\(\\([-_./]\\)[0-9]+[a-zA-Z]?\\)*\\)[-_./]?$")

(defun stp-default-extractor (main-version)
  (mapcan (lambda (s)
            (if (string-match "^\\([0-9]+\\)\\([A-Za-z]+\\)$" s)
                (list (match-string 1 s) (match-string 2 s))
              (list s)))
          (s-split "[-_.]" (match-string 1 main-version) t)))

(defun stp-haskell-extractor (main-version)
  (->> (match-string 1 main-version)
       (s-chop-prefix "haskell-mode-")
       (s-chop-suffix "_")
       (s-split "-")))

(defun stp-auctex-extractor (main-version)
  (let* ((vs (s-split "_\\|-" (match-string 1 main-version)))
         (v-butlast (butlast vs))
         (v-last (car (last vs))))
    ;; Any trailing letters or +'s need to be separate elements of the list
    ;; for the version comparison to work correctly. This is because
    ;; otherwise, for example, 6+ would be treated as newer than 10.
    (append v-butlast
            (save-match-data
              (if (string-match "^\\([0-9]+\\)\\([a-zA-Z]?\\)\\(\\+?\\)$"
                                v-last)
                  (list (match-string 1 v-last)
                        (match-string 2 v-last)
                        (match-string 3 v-last))
                (list v-last))))))

(defvar stp-version-suffix-regexp "[-_./]?\\(?:\\(snapshot\\|git\\|hg\\|darcs\\|bzr\\|svn\\|cvs\\)\\|\\(alpha\\)\\|\\(beta\\)\\|\\(pre\\|rc\\)\\)[-_./]?\\(?:\\([0-9]+\\)[-_./]?\\([a-zA-Z]?\\)\\)?$")

(defun stp-version-default-suffix-extractor (version)
  (list (cons 'main-version
              (substring version 0 (match-beginning 0)))
        (cons 'version-suffix-alist
              (append
               (list (cond
                      ((rem-empty-nil (match-string 1 version)) "-4") ; git
                      ((rem-empty-nil (match-string 2 version)) "-3") ; alpha
                      ((rem-empty-nil (match-string 3 version)) "-2") ; beta
                      ((rem-empty-nil (match-string 4 version)) "-1") ; rc
                      ;; This should be impossible.
                      (t (error "Unknown suffix type"))))
               (rem-empty-nil (match-string 5 version) #'list) ; number
               (rem-empty-nil (match-string 6 version) #'list))))) ; letter

(defvar stp-version-suffix-extractor #'stp-version-default-suffix-extractor)

(defvar stp-version-extractor-alist
  ;; This matches the versions for most emacs packages.
  `((,stp-version-regexp . stp-default-extractor)
    ;; haskell-mode
    ("^\\(?:haskell-mode-\\)\\(1-44_\\|1-45_\\)$" . stp-haskell-extractor)
    ;; auctex
    ("^\\(?:auctex_release\\|auctex\\|release\\|rel\\)\\(?:[-_./]\\)\\([0-9]+\\([-_.][0-9]+\\)*[a-zA-Z]?\\+?\\)$" . stp-auctex-extractor))
  "An list of regexps to match to package versions (after the suffix
is removed) and functions to extract a key from the text that
matches the first group of the regexp. The key should be a list
of strings which are the components of the version string. For
example, for v1.2.3a the key would be (\"1\" \"2\" \"3\" \"a\").

Functions are called with the match data from matching the
package version and take the version with the suffix removed as
an argument.")

(defun stp-version-extract (version)
  (save-match-data
    (cl-dolist (cell stp-version-extractor-alist)
      (db (regexp . extractor)
          cell
        (let (version-suffix-alist
              (main-version version))
          (when (string-match stp-version-suffix-regexp version)
            (let-alist (funcall stp-version-suffix-extractor version)
              (setq main-version .main-version
                    version-suffix-alist .version-suffix-alist)))
          (when (string-match regexp main-version)
            (cl-return (append (funcall extractor main-version) version-suffix-alist))))))))

(defun stp-download-elisp (dir pkg-name remote)
  "Download the elisp file or archive at REMOTE and copy it to DIR.
DIR will be created if it does not already exist. If REMOTE
contains a single elisp file, it will be renamed as PKG-NAME with a
.el extension added if necessary."
  (unless (f-dir-p dir)
    (f-mkdir-full-path dir))
  ;; Without this, `f-move' will not work below when dir is the target.
  (setq dir (f-slash dir))
  ;; Check for ordinary elisp files.
  (if (string= (f-ext remote) "el")
      ;; Handle local remotes as well.
      (let ((target (f-join dir (f-swap-ext pkg-name "el"))))
        (if (f-exists-p remote)
            (f-copy remote target)
          ;; Ordinary elisp files can simply be downloaded and copied to dir.
          (or (url-copy-file remote (f-join dir (f-swap-ext pkg-name "el")))
              (error "Failed to download %s" remote))))
    ;; Archives are downloaded, extracted and then copied to dir.
    (let* ((temp-dir (make-temp-file pkg-name t))
           (archive-path (f-join temp-dir (f-filename remote))))
      (unwind-protect
          (progn
            (if (f-exists-p remote)
                (f-copy remote archive-path)
              (or (url-copy-file remote archive-path)
                  (error "Failed to download %s" remote)))
            (rem-extract-archive archive-path t)
            (f-delete archive-path)
            ;; Ignore directories that only contain a single directory.
            (let (files
                  (extract-path temp-dir))
              (while (and (= (length (setq files (f-entries extract-path))) 1)
                          (f-dir-p (car files)))
                (setq extract-path (car files)))
              ;; Correct the name of the file if necessary. This is needed
              ;; because sometimes the filename includes the version (for
              ;; example with some ELPA packages such as older versions of
              ;; adaptive-wrap).
              (let* ((files (f-entries extract-path))
                     (file (car files)))
                (when (= (length files) 1)
                  (f-move file (f-join (f-dirname file) (f-swap-ext pkg-name "el")))))
              ;; We don't need to handle tarbombs and archives that are a single
              ;; compressed elisp file (e.g. file.el.lz) because
              ;; `rem-extract-archive' already handles them by creating a
              ;; subdirectory even if the archive doesn't contain one.
              (cl-dolist (file (f-entries extract-path))
                (f-move file dir))))
        (f-delete temp-dir t)))))

(defun stp-invert-update (update)
  (cl-ecase update
    (unstable 'stable)
    (stable 'unstable)))

(defvar stp-gnu-makefile-names '("GNUmakefile" "makefile" "Makefile"))

;; Bash magic copied from
;; https://unix.stackexchange.com/questions/230047/how-to-list-all-targets-in-make/230050.
;; It is from the bash completion function for make. This is no longer needed
;; for versions of make after 4.4.1 as there is a --print-targets option.
;; However, 4.4.1 is the latest stable version as of 5/22/2025.
(defvar stp-make-target-command "make -qp | awk -F':' '/^[a-zA-Z0-9][^$#\\/\t=]*:([^=]|$)/ {split($1,A,/ /);for(i in A)print A[i]}' | sort -u")

(defun stp-make-targets (&optional makefile)
  (let* ((directory (or (and makefile
                             (f-dirname makefile))
                        default-directory))
         (default-directory directory))
    (s-split rem-positive-whitespace-regexp
             (rem-run-command stp-make-target-command :error t)
             t)))

(defun stp-before-build-command (cmd buf)
  ;; Using `with-current-buffer' can change the default directory.
  (let ((dir default-directory))
    (with-current-buffer (get-buffer-create buf)
      (read-only-mode 0)
      (insert "\n\n")
      (insert (format "Current directory: %s\n" dir))
      (insert (rem-as-shell-command cmd)))))

(cl-defun stp-reload-once (pkg-name)
  "Reload all files for PKG-NAME."
  (let* ((pkg-path (stp-canonical-path pkg-name))
         ;; Reload those features that were already loaded and correspond to
         ;; files in the package.
         (files (mapcar #'f-no-ext (rem-elisp-files-to-load pkg-path :recursive t))))
    (cl-dolist (f files)
      (load f))))

(provide 'stp-utils)
;;; stp-utils.el ends here
