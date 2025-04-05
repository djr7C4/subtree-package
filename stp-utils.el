;;; -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025, David J. Rosenbaum <djr7c4@gmail.com>
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
(require 'memoize)
(require 'rem)
(require 'rem-abbrev)
(require 's)

(defvar stp-ellipsis (if (char-displayable-p ?…) "…" "..."))

(defvar stp-memoized-functions '(stp-refresh-info stp-git-remote-hash-alist stp-elpa-version-url-alist))

(defvar stp-package-info nil)

(defvar stp-memoization-active nil)

(defmacro stp-with-memoization (&rest body)
  "Evaluate BODY with memoization active for expensive functions.
Cached results are only retained while within the scope of this
macro. This allows functions that would otherwise make many
duplicate queries to remote Git repositories to only make one of
each type per interactive command."
  (declare (indent 0))
  (with-gensyms (memoization-active-orig)
    `(let ((,memoization-active-orig stp-memoization-active)
           (stp-memoization-active t))
       (unwind-protect
           (progn
             (unless ,memoization-active-orig
               (mapc (-rpartial #'memoize nil) stp-memoized-functions))
             ,@body)
         (unless ,memoization-active-orig
           (mapc #'memoize-restore stp-memoized-functions))))))

(def-edebug-spec stp-with-memoization t)

(defun stp-prefix-prompt (prompt-prefix prompt)
  (if (or (not prompt-prefix) (string= prompt-prefix ""))
      prompt
    (concat prompt-prefix
            (s-downcase (substring prompt 0 1))
            (substring prompt 1))))

(defvar stp-source-directory (f-join user-emacs-directory "package-source"))

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
  "Return the canonical path to pkg-name. The return value always
ends with a slash."
  (let ((path (if (f-absolute-p pkg-name)
                  pkg-name
                (f-join stp-source-directory pkg-name))))
    (when canonical
      (setq path (f-canonical path)))
    (f-slash path)))

(defun stp-relative-path (pkg-name)
  "Return the path to pkg-name relative to `stp-source-directory'.
The return value always ends with a slash."
  (rem-slash pkg-name))

(defun stp-name (pkg-path)
  "Returns the name of the package. Unlike `stp-relative-path', this
never ends with a slash (nor does it contain any slashes)."
  (f-filename pkg-path))

(defun stp-main-package-file (pkg-path)
  (let* ((pkg-name (stp-name pkg-path))
         (pkg-file (concat pkg-name ".el"))
         (paths (-sort (lambda (path path2)
                         (or (< (rem-path-length path)
                                (rem-path-length path2))
                             (and (= (rem-path-length path)
                                     (rem-path-length path2))
                                  (string< path path2))))
                       (directory-files-recursively pkg-path (regexp-quote pkg-file))))
         (path (car paths)))
    (or path pkg-path)))

(defun stp-split-current-package ()
  "Return a list containing the name of the package for the current
file and the relative path to the current file or directory
within that package."
  (stp-refresh-info)
  (let ((path (or buffer-file-name default-directory)))
    (db (pkg-name k)
        (->> (stp-info-names)
             (mapcar (lambda (pkg-name)
                       (list pkg-name
                             (->> path
                                  (s-matched-positions-all (regexp-quote (concat "/" (stp-relative-path pkg-name))))
                                  last
                                  caar))))
             (cl-find-if #'cadr))
      (list pkg-name (apply #'f-join (cddr (f-split (substring path k))))))))

(defmacro stp-with-package-source-directory (&rest body)
  (declare (indent 0))
  `(let ((default-directory stp-source-directory))
     ,@body))

(def-edebug-spec stp-with-package-source-directory t)

(defun stp-filesystem-names ()
  "Return a list of packages installed in `stp-source-directory'."
  (stp-with-package-source-directory
    (-filter 'f-dir-p (directory-files stp-source-directory nil "^[^.]"))))

(defun stp-info-names (&optional method)
  "Return a list of packages stored in `stp-info-file'."
  (sort (mapcar 'car
                (-filter (lambda (pkg)
                           (or (not method)
                               (let ((pkg-alist (cdr pkg)))
                                 (eq (map-elt pkg-alist 'method) method))))
                         stp-package-info))
        'string<))

(defvar stp-methods-order '(git elpa url)
  "Valid values for the METHOD attribute.")

(defvar stp-attribute-order '(method remote other-remotes version update branch)
  "The order in which package attributes should be sorted before being written
  to disk.")

(defvar stp-read-name-history nil)

(defun stp-default-name (remote)
  ;; Remove .git and then the extension. This causes the default name for a
  ;; repository like "abc.el.git" to be "abc".
  (f-no-ext (s-chop-suffix ".git" (f-filename remote))))

(defun stp-read-name (prompt &optional default)
  "Read the name of a package."
  (rem-read-from-mini prompt :history 'stp-read-name-history :initial-contents default))

(defun stp-read-existing-name (prompt)
  "Read the name of a package that is already installed."
  (rem-comp-read prompt
                 (stp-info-names)
                 :require-match t
                 :history 'stp-read-name-history))

(defun stp-read-method (prompt &optional default)
  (when (and default (symbolp default))
    (setq default (symbol-name default)))
  (intern (rem-comp-read prompt
                         (mapcar #'symbol-name stp-methods-order)
                         :require-match t
                         :default default
                         :sort-fun #'identity)))

(defun stp-normalize-remote (remote)
  ;; Use absolute paths for local repositories.
  (if (f-exists-p remote)
      (setq remote (f-slash (f-full remote)))
    remote))

(defvar stp-read-remote-default-directory nil)

(defun stp-read-remote-with-predicate (prompt valid-remote-p &optional default history)
  (let (remote)
    (while (or (not remote)
               (consp remote)
               (not (funcall valid-remote-p remote)))
      (when remote
        (minibuffer-message "%s is invalid" remote))
      (setq remote
            (let ((default-directory (or stp-read-remote-default-directory
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

(defun stp-comp-read-remote (prompt remote known-remotes &optional multiple)
  (cl-labels ((valid-candidate-p (candidate)
                (or (member candidate known-remotes)
                    (f-dir-p candidate)))
              (valid-candidates-p (candidates)
                (if multiple
                    (cl-every #'valid-candidate-p (s-split crm-separator candidates))
                  (valid-candidate-p candidates))))
    (let* ((default-directory (or stp-read-remote-default-directory
                                  default-directory))
           (new-remotes (rem-comp-read prompt
                                       (completion-table-in-turn known-remotes
                                                                 #'completion-file-name-table)
                                       :predicate #'valid-candidates-p
                                       :default remote
                                       :history 'stp-remote-history
                                       :sort-fun #'identity
                                       ;; Disable the category. By default, it
                                       ;; will be 'file which will cause https://
                                       ;; to be replaced with / during completion.
                                       :metadata '((category . nil))
                                       :multiple multiple)))
      ;; When multiple is nil, new-remotes will just be a single remote rather
      ;; than a list.
      (if multiple
          (mapcar #'stp-normalize-remote new-remotes)
        (stp-normalize-remote new-remotes)))))

(defun stp-choose-remote (prompt remote &optional other-remotes)
  (if stp-use-other-remotes
      ;; A match is not required. This way, a new remote can be added
      ;; interactively by the user. Type ./ to complete in the current
      ;; directory.
      (stp-comp-read-remote prompt remote (cons remote other-remotes))
    remote))

(defun stp-update-remotes (pkg-name chosen-remote remote other-remotes)
  ;; Remote should always be chosen-remote since that is where the package was
  ;; just installed or upgraded from. (See the documentation of
  ;; `stp-info-file'.) Other-remotes is whatever other remotes exist that were
  ;; not chosen.
  (stp-set-attribute pkg-name 'remote chosen-remote)
  (when (or other-remotes (not (string= chosen-remote remote)))
    (->> (cons remote other-remotes)
         (remove chosen-remote)
         (stp-set-attribute pkg-name 'other-remotes))))

(defun stp-version< (v1 v2)
  "Determine if v2 of the package is newer than v1."
  (rem-version< (stp-version-extract v1)
                (stp-version-extract v2)))

(defvar stp-info-file (f-join user-emacs-directory "pkg-info.eld")
  "The name of the file that stores the package information.

This consists of an alist that maps the name of each package to
an alist that stores information for each package. Each of These
alists is of the form

  \\='((method . ...)
    (remote . ...)
    (other-remotes ...)
    (version . ...)
    (update . ...)
    (branch . ...))

Method should be one of the symbols \\='git or \\='url. Remote
should be a \\='url indicating the location that the package was
installed from. If method is \\='git, it should be a git
repository and if it is \\='url it should be the URL it was
downloaded from. Other-remotes should be a list of alternative
remotes that the user may select to use instead of remote.
Version should be a string that indicates the version. If method
is \\='git, this should be a ref. If method is \\='url, it can be
any string that the user cares to use to describe the current
version. If method is \\='git, then update indicates the update
mechanism. \\='stable means that stable versions are being used
and \\='unstable means that the value associated with the branch
attribute is being used. If the method is \\='url, the \\='update
and \\='branch attributes should not be present.")

(defun stp-attribute< (attribute attribute2)
  (< (cl-position attribute stp-attribute-order)
     (cl-position attribute2 stp-attribute-order)))

(defun stp-sort-info (pkg-info)
  "Sort packages alphabetically by name and sort their attributes according to
  `stp-attribute-order'."
  (mapcar (lambda (cell)
            (cons (car cell)
                  (-sort (lambda (x y)
                           (stp-attribute< (car x) (car y)))
                         (cdr cell))))
          (-sort (lambda (x y)
                   (string< (car x) (car y)))
                 pkg-info)))

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
    (insert ";;; mode: read-only; -*- no-byte-compile: t; -*-\n\n")
    (pp (stp-sort-info stp-package-info) (current-buffer))
    (write-file stp-info-file)))

(defun stp-get-attribute (pkg-name attr)
  "Get the attribute attr in the alist with the key corresponding to
pkg-name."
  (let ((pkg-name (stp-name pkg-name)))
    (map-elt (map-elt stp-package-info pkg-name) attr)))

(defun stp-set-attribute (pkg-name attr val)
  "Set the attribute attr to val in the alist with the key
corresponding to pkg-name."
  (let* ((pkg-name (stp-name pkg-name))
         (alist (map-elt stp-package-info pkg-name)))
    (if alist
        (setf (map-elt alist attr) val
              (map-elt stp-package-info pkg-name) alist)
      (setq stp-package-info (cons `(,pkg-name . ((,attr . ,val))) stp-package-info)))))

(defun stp-delete-attribute (pkg-name attr)
  "Remove attr from the alist with the key corresponding to
pkg-name."
  (let ((alist (stp-get-alist pkg-name)))
    (stp-set-alist pkg-name (remq (assoc attr alist) alist))))

(defun stp-get-alist (pkg-name)
  "Get the alist that contains information corresponding to
pkg-name."
  (let ((pkg-name (stp-name pkg-name)))
    (map-elt stp-package-info pkg-name)))

(defun stp-set-alist (pkg-name alist)
  "Set the alist that contains information corresponding to pkg-name
to alist."
  (let ((pkg-name (stp-name pkg-name)))
    (setf (map-elt stp-package-info pkg-name) alist)))

(defun stp-delete-alist (pkg-name)
  "Remove the alist that contains information corresponding to
PKG-NAME."
  (setq stp-package-info (map-delete stp-package-info pkg-name)))

(defvar stp-version-regexp "^\\(?:\\(?:v\\|V\\|release\\|Release\\|version\\|Version\\)\\(?:[-_./]?\\)\\)?\\([0-9]+[a-zA-Z]?\\(\\([-_./]\\)[0-9]+[a-zA-Z]?[-_./]?\\)*\\)$")

(defun stp-default-extractor (v)
  (mapcan (lambda (s)
            (save-match-data
              (if (string-match "^\\([0-9]+\\)\\([A-Za-z]+\\)$" s)
                  (list (match-string 1 s) (match-string 2 s))
                (list s))))
          (s-split "[-_.]" v)))

(defun stp-haskell-extractor (v)
  (s-split "-" (s-chop-prefix "haskell-mode-" v)))

(defun stp-auctex-extractor (v)
  (let* ((vs (s-split "_\\|-" v))
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

(defvar stp-version-extractor-alist
  ;; This matches the versions for most emacs packages.
  `((,stp-version-regexp . stp-default-extractor)
    ;; haskell-mode
    ("^\\(?:haskell-mode-\\)\\(1-44_\\|1-45_\\)$" . stp-haskell-extractor)
    ;; auctex
    ("^\\(?:auctex_release\\|auctex\\|release\\|rel\\)\\(?:[-_./]\\)\\([0-9]+\\([-_.][0-9]+\\)*[a-zA-Z]?\\+?\\)$" . stp-auctex-extractor))
  "An list of regexps to match to package versions and functions to
extract a key from the text that matches the first group of the
regexp. The key should be a list of strings which are the
components of the version string. For example, for v1.2.3a the
key would be (\"1\" \"2\" \"3\" \"a\").")

(defun stp-version-extract (version)
  (dolist (cell stp-version-extractor-alist)
    (db (regexp . extractor)
        cell
      (save-match-data
        (when (string-match regexp version)
          (cl-return (funcall extractor (match-string 1 version))))))))

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
              (dolist (file (f-entries extract-path))
                (f-move file dir))))
        (f-delete temp-dir t)))))

(defvar stp-url-unsafe-regexps '("emacswiki\\.org")
  "The user should be warned before downloading from an unsafe URL.")

(defun stp-url-safe-remote-p (remote)
  (or (not remote)
      (let ((domain (if (f-exists-p remote)
                        ;; Handle local paths which cannot be parsed by
                        ;; `url-generic-parse-url'.
                        remote
                      (url-domain (url-generic-parse-url remote)))))
        (not (-any (lambda (regexp)
                     (and (string-match-p regexp domain) t))
                   stp-url-unsafe-regexps)))
      (yes-or-no-p (format "The remote %s is unsafe. Continue anyway? " remote))))

(defun stp-invert-update (update)
  (cl-ecase update
    (unstable 'stable)
    (stable 'unstable)))

(defvar stp-gnu-makefile-names '("GNUmakefile" "makefile" "Makefile"))

;; Bash magic copied from
;; https://unix.stackexchange.com/questions/230047/how-to-list-all-targets-in-make/230050.
;; It is from the bash completion function for make. This is no longer needed
;; for versions of make after 4.4.1 as there is a --print-targets option.
;; However, 4.4.1 is the latest stable version as of 9/27/2024.
(defvar stp-make-target-command "make -qp | awk -F':' '/^[a-zA-Z0-9][^$#\\/\t=]*:([^=]|$)/ {split($1,A,/ /);for(i in A)print A[i]}' | sort -u")

(defun stp-make-targets (&optional makefile)
  (let ((directory (or (and makefile
                            (f-dirname makefile))
                       default-directory)))
    (let ((default-directory directory))
      (db (exit-code output)
          (rem-call-process-shell-command stp-make-target-command)
        (when (/= exit-code 0)
          (error "Failed to obtain make targets in %s" directory))
        (s-split rem-positive-whitespace-regexp output t)))))

(defun stp-before-build-command (cmd buf)
  ;; Using `with-current-buffer' can change the default directory.
  (let ((dir default-directory))
    (with-current-buffer buf
      (read-only-mode 0)
      (insert "\n\n")
      (insert (format "Current directory: %s\n" dir))
      (insert cmd))))

(defvar stp-load-blacklist (list "-pkg\\.\\(el\\|elc\\)$" (format "\\(^\\|/\\)%s$" dir-locals-file) (format "\\(^\\|/\\)%s-2.el$" (f-no-ext dir-locals-file))))

(cl-defun stp-reload-once (pkg-name)
  "Reload all features for PKG-NAME that have already been loaded
according to `features'. When ALL is non-nil, load all features
for PKG-NAME even if they were not previously loaded."
  (let* ((pkg-path (stp-canonical-path pkg-name))
         ;; Reload those features that were already loaded and correspond to
         ;; files in the package.
         (files (directory-files-recursively pkg-path
                                             (concat "\\.\\("
                                                     rem-elisp-file-regexp
                                                     "\\)$"))))
    (dolist (f files)
      (unless (cl-some (-rpartial #'string-match-p f) stp-load-blacklist)
        (load f)))))

(provide 'stp-utils)
;;; stp-utils.el ends here
