;;; stp-headers.el --- -*- lexical-binding: t; -*-
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

(require 'lisp-mnt)
(require 'rem)
(require 'stp-git)
(require 'stp-git-utils)
(require 'stp-utils)

(defun stp-headers-normalize-version (version)
  (when (ignore-errors (version-to-list version))
    version))

(defun stp-headers-elisp-requirements ()
  "Return the packages required by the current buffer.

These are determined according to the Package-Requires field."
  (let ((text (apply #'concat (lm-header-multiline "Package-Requires"))))
    (unless (string= (s-trim text) "")
      (db (reqs . index)
          (read-from-string text)
        (when (>= index (length text))
          (mapcar (lambda (entry)
                    (list (car entry) (stp-headers-normalize-version (cadr entry))))
                  reqs))))))

(cl-defmacro stp-headers-with-file-cache ((file cache) &rest body)
  "Return the result of evaluating BODY unless a valid entry in
CACHE can be used. An entry is valid unless the modification time
of FILE is different from the last time BODY was evaluated."
  (declare (indent 1))
  (with-gensyms (entry new-timestamp result)
    (once-only (file cache)
      `(let ((,entry (gethash ,file ,cache))
             (,new-timestamp (f-change-time ,file)))
         (if (and ,entry (time-equal-p ,new-timestamp (car ,entry)))
             (cadr ,entry)
           (let ((,result (progn ,@body)))
             (setf (gethash ,file ,cache) (list ,new-timestamp ,result))
             ,result))))))

(def-edebug-spec stp-headers-with-file-cache ((form form) body))

(defun stp-headers-version ()
  (or (lm-header "Package-Version")
      (lm-header "Version")))

(defun stp-headers-get-header ()
  (save-match-data
    (looking-back " \\([^ \t:]+\\):[ \t]*" nil)
    (match-string 1)))

;; Without caching, checking all the requirements for the load path is slow
;; (especially when dealing with compressed files). These caches are safe to add
;; to `savehist-additional-variables' since hash tables can be printed and read
;; in Emacs Lisp (unlike Common Lisp).
(defvar stp-headers-elisp-file-requirements-cache (make-hash-table :test #'equal))

(defun stp-headers-elisp-file-requirements (file)
  (stp-headers-with-file-cache (file stp-headers-elisp-file-requirements-cache)
    (with-temp-buffer
      (insert-file-contents file)
      (stp-headers-elisp-requirements))))

(defun stp-headers-elisp-feature (name)
  "Return requirements satisfied by the current buffer."
  (let ((version (save-excursion (stp-headers-version))))
    (when version
      (list (intern name) (stp-headers-normalize-version version)))))

(defvar stp-headers-elisp-file-feature-cache (make-hash-table :test #'equal))

(cl-defun stp-headers-elisp-file-feature (file)
  (stp-headers-with-file-cache (file stp-headers-elisp-file-feature-cache)
    (with-temp-buffer
      (insert-file-contents file)
      (stp-headers-elisp-feature (rem-no-ext (f-filename file))))))

(cl-defun stp-headers-merge-elisp-requirements (requirements &optional (hash-table nil hash-table-provided-p))
  "Merge duplicate requirements in REQUIREMENTS. Only the most
recent version will be kept. If HASH-TABLE is a hash table then
requirements will be merged into it and the result will be
returned instead of a requirements list. Otherwise, if HASH-TABLE
is non-nil, then they will be merged into an empty hash table.
The result is an alist unless hash-table is provided."
  (let ((versions (if (hash-table-p hash-table)
                      hash-table
                    (make-hash-table :test #'eq))))
    (cl-dolist (requirement requirements)
      (db (pkg-sym version)
          requirement
        ;; Keep the newest version of each package.
        (let ((curr-version (gethash pkg-sym versions)))
          (when (or (not curr-version)
                    (and version
                         (version< curr-version version)))
            (setf (gethash pkg-sym versions) version)))))
    (if hash-table-provided-p
        versions
      (cl-loop
       for pkg-sym being the hash-keys of versions using (hash-values version)
       collect (list pkg-sym version)))))

(cl-defun stp-headers-directory-requirements (dir &key (fun #'stp-headers-elisp-file-requirements) recursive)
  "Find all packages that are required by DIR.

There are determined according to the Package-Requires field of
its elisp files."
  (let* (reqs
         (files (rem-elisp-files-to-load dir :compressed t :recursive recursive)))
    (cl-dolist (file files)
      (setq reqs (append reqs (funcall fun file))))
    (stp-headers-merge-elisp-requirements reqs)))

(cl-defun stp-headers-paths-requirements (paths &key (fun #'stp-headers-directory-requirements) recursive)
  "Find all requirements for the files in PATHS. PATHS may be either
a single path or a list of paths."
  (setq paths (ensure-list paths))
  (->> (-filter (fn (and % (f-directory-p %))) paths)
       (mapcar (-rpartial fun :recursive recursive))
       (apply #'append)
       stp-headers-merge-elisp-requirements))

(cl-defun stp-headers-directory-features (dir &key recursive)
  "Find all requirements that are satisfied by files in DIR."
  (stp-headers-directory-requirements dir
                                      :fun (fn (awhen (stp-headers-elisp-file-feature %)
                                                 (list it)))
                                      :recursive recursive))

(cl-defun stp-headers-paths-features (paths &key recursive)
  "Find all requirements that are satisfied by files in PATHS. PATHS
may be either a single path or a list of paths."
  (stp-headers-paths-requirements paths :fun #'stp-headers-directory-features :recursive recursive))

(defun stp-headers-requirements-hash-table (requirements)
  "Return a hash table mapping the symbol for each package in
REQUIREMENTS to its version."
  ;; Make sure that there are no duplicate packages.
  (stp-headers-merge-elisp-requirements requirements t))

(defvar stp-headers-installed-features nil)
(defvar stp-headers-uninstalled-features nil)
(defvar stp-headers-versions nil)

(defvar stp-headers-always-recompute-features nil
  "When non-nil, features are always recomputed by
`stp-headers-update-features' instead of using incremental updates. This
is slower but will detect packages installed with other package
managers.")

(defvar stp-headers-update-recompute-development-directory t
  "When non-nil, always recompute features for packages in
`stp-development-directory'.")

(defun stp-headers-update-features (&optional suppress-first)
  "Update `stp-headers-installed-features'. Add the new features from
packages that were installed or upgraded since this function was
last invoked. This is much faster than recomputing all features
which can take several seconds or more if many packages are
installed. The downside is that packages installed outside of STP
will not be detected."
  (when stp-headers-always-recompute-features
    (setq stp-headers-elisp-file-feature-cache (make-hash-table :test #'equal)))
  (if (and (not stp-headers-always-recompute-features)
           stp-headers-installed-features
           ;; If the installed version of Emacs has changed, recompute
           ;; everything since the built-in packages may have been upgraded.
           (string= (cadar stp-headers-versions) emacs-version))
      (let* ((new-versions (stp-headers-compute-versions))
             (modified-packages (mapcar #'car (cl-set-difference new-versions stp-headers-versions :test #'equal)))
             (new-paths (mapcan (-compose #'stp-compute-load-path #'stp-canonical-path)
                                modified-packages))
             ;; stp-headers-update-recompute-development-directory
             (dev-paths (and stp-headers-update-recompute-development-directory
                             (stp-compute-load-paths stp-development-directory)))
             (new-features (stp-headers-paths-features (append new-paths dev-paths))))
        (setq stp-headers-installed-features (-> stp-headers-installed-features
                                                 (cl-set-difference stp-headers-uninstalled-features :test #'equal)
                                                 (append new-features)
                                                 stp-headers-merge-elisp-requirements)
              stp-headers-uninstalled-features nil
              stp-headers-versions new-versions))
    (unless suppress-first
      (stp-msg "Installed features have not yet been computed. This will take a moment the first time"))
    (setq stp-headers-installed-features (stp-headers-paths-features load-path)
          stp-headers-versions (stp-headers-compute-versions))))

(defun stp-headers-recompute-features ()
  "Recompute all features in the load path. This may be necessary if
a package is installed outside of STP."
  (interactive)
  (stp-refresh-info)
  (setq stp-headers-installed-features nil
        stp-headers-elisp-file-feature-cache (make-hash-table :test #'equal))
  (stp-headers-update-features t))

(defun stp-headers-compute-versions ()
  (cons `(emacs ,emacs-version)
        (mapcar (fn (list (car %) (map-elt (cdr %) 'version)))
                (stp-get-info-packages))))

(defun stp-package-requirements (pkg-name)
  (let* ((pkg-path (stp-full-path pkg-name))
         (main-file (or (stp-main-package-file pkg-name :no-directory t)
                        (read-file-name (format "Main elisp file for %s: " pkg-name)
                                        pkg-path
                                        nil
                                        t
                                        nil
                                        (-compose (-partial #'string= "el") #'f-ext)))))
    (stp-headers-elisp-file-requirements main-file)))

(cl-defun stp-update-requirements (pkg-name &optional (requirements (stp-package-requirements pkg-name)))
  (if requirements
      (stp-set-attribute pkg-name 'requirements requirements)
    (stp-delete-attribute pkg-name 'requirements)))

;;; For archaic reasons, Emacs lisp packages require some redundant headers such
;;; as beginning and end of file headers. These are maintained automatically.
(defun stp-headers-bounds-of-bob-header ()
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (and (re-search-forward "^;+[ \t]*\\([^ ]*\\(\\.el\\)?\\)[ \t]*---" nil t)
           (cons (match-beginning 1) (match-end 1))))))

(defun stp-headers-bounds-of-eob-header ()
  (save-excursion
    (save-match-data
      (goto-char (point-max))
      (and (re-search-backward "^;+[ \t]*\\([^ ]*\\(\\.el\\)?\\)[ \t]+ends[ \t]+here" nil t)
           (cons (match-beginning 1) (match-end 1))))))

(defun stp-headers-update-elisp-filename-headers (&optional insert)
  "Update the elisp headers at the beginning and end of the buffer.

These contain the filename. If they do not already exist and
INSERT is non-nil then insert them. Return non-nil if one of the
headers did not exist and was inserted."
  (interactive)
  (let (inserted
        (filename (or (f-filename buffer-file-name)
                      (f-swap-ext (buffer-name) "el"))))
    (acond
     ((stp-headers-bounds-of-bob-header)
      (save-excursion
        (db (bobh-beg . bobh-end)
            it
          (rem-replace-region bobh-beg bobh-end filename 'after)
          ;; Fix spacing after the filename. The rest of the line after --- is
          ;; unchanged since it might contain file local variables.
          (just-one-space)
          ;; Fix spacing and the comment at the beginning of the line.
          (beginning-of-line)
          (skip-chars-forward "; \t")
          (delete-region (line-beginning-position) (point))
          (beginning-of-line)
          (insert ";;; "))))
     (insert
      (ignore it)
      (setq inserted t)
      ;; We make sure that there is a prop line because package.el requires
      ;; it. See `package-buffer-info'.
      (rem-ensure-prop-line)
      (beginning-of-line)
      (skip-chars-forward "; \t")
      (delete-region (line-beginning-position) (point))
      (beginning-of-line)
      (insert (format ";;; %s --- " filename))
      (end-of-line)
      (insert "\n")))
    (acond
     ((stp-headers-bounds-of-eob-header)
      (save-excursion
        (db (eobh-beg . eobh-end)
            it
          (goto-char eobh-beg)
          (beginning-of-line)
          (delete-region (point) (line-end-position))
          (insert (format ";;; %s ends here" filename)))))
     ;; Insert the end of buffer header if it is missing.
     (insert
      (ignore it)
      (setq inserted t)
      (goto-char (point-max))
      (skip-chars-backward rem-whitespace)
      (delete-region (point) (point-max))
      (insert (format "\n\n;;; %s ends here" filename))))
    inserted))

(defun stp-headers-update-copyright-header (&optional insert)
  "Update the years for the copyright header.

If it does not exist and INSERT is non-nil, then insert a
copyright header. Return non-nil if the header did not exist and
was inserted."
  (interactive)
  (save-excursion
    (save-match-data
      ;; Move point to just before the years in the copyright notice if it
      ;; exists.
      (let ((current-year (format-time-string "%Y")))
        (aif (lm-copyright-mark)
            (progn
              (goto-char it)
              ;; Check for the copyright years.
              (when (looking-at "\\([0-9]+\\)\\([ \t]*[-,][ \t]*\\([0-9]+\\)\\)*")
                (let ((digits "0123456789"))
                  (goto-char (match-end 0))
                  ;; Find the last year in the copyright notice.
                  (skip-chars-backward digits)
                  (let* ((last-year-end (match-end 0))
                         (last-year (buffer-substring-no-properties (point) last-year-end)))
                    (unless (string= current-year last-year)
                      ;; Detect ranges and change the last year.
                      (if (looking-back "-[ \t]*" nil)
                          (rem-replace-region (point) last-year-end current-year)
                        ;; When the last separator was a comma (e.g. for the years
                        ;; 2020, 2022), we append the current year as a range.
                        (goto-char last-year-end)
                        (insert "-" current-year))))))
              nil)
          (when insert
            (let ((pt (point)))
              (insert (format "Copyright (C) %s %s" current-year user-full-name))
              (comment-region pt (point))
              t)))))))

(defun stp-headers-update-version-header (&optional insert)
  (let ((header "Version: "))
    (cl-flet ((insert-version (value)
                (insert (format ";; %s: %s\n"
                                header
                                (or (->> (stp-git-root)
                                         stp-git-latest-stable-version
                                         stp-version-extract
                                         (s-join "."))
                                    "TODO")))
                value))
      (if (save-excursion (stp-headers-version))
          (save-excursion
            ;; Move point to the line with the version.
            (stp-headers-version)
            (setq header (stp-headers-get-header))
            (delete-line)
            (insert-version nil))
        (when insert
          (insert-version t))))))

(defun stp-headers-package-requirements-multiline (requirements)
  (if (<= (length requirements) 1)
      (prin1-to-string requirements)
    (let ((prefix ";;   "))
      (--> requirements
           (mapcar (fn (format "%s%S" prefix %)) it)
           (s-join "\n" it)
           (concat "(\n" it ")")))))

(defvar stp-headers-ignored-requirements nil)

(defun stp-headers-update-requirements-header (&optional insert)
  (interactive (list t))
  (stp-refresh-info)
  (stp-headers-update-features)
  ;; Update each requirement to the latest installed version.
  (let* ((new-requirements (->> (mapcar (lambda (requirement)
                                          (aif (assoc (car requirement) stp-headers-installed-features)
                                              it
                                            requirement))
                                        (stp-headers-elisp-requirements))
                                (-sort (fn (string< (car %1) (car %2))))))
         (requirements-string (stp-headers-package-requirements-multiline new-requirements)))
    (if (save-excursion (lm-header "Package-Requires"))
        (save-excursion
          (lm-header "Package-Requires")
          (progn
            (delete-region (point)
                           ;; Don't ignore comments because otherwise
                           ;; `forward-sexp' won't work on multiline headers.
                           (let ((parse-sexp-ignore-comments nil))
                             (forward-sexp)
                             (point)))
            (insert requirements-string)
            nil))
      (when insert
        (insert (format ";; Package-Requires: %s" requirements-string))))))

(defun stp-emacs-requirement-satisfied-p (pkg-name &optional version)
  (and (string= pkg-name "emacs")
       (version< version (format "%d.%d" emacs-major-version emacs-minor-version))))

(defun stp-installed-version (pkg-name)
  (cadr (cl-find-if (fn (eq (car %) (intern pkg-name)))
                    (stp-headers-directory-features (stp-full-path pkg-name) :recursive t))))

(defun stp-package-requirement-satisfied-p (pkg-name &optional version search-load-path)
  (let ((pkg-sym (intern pkg-name)))
    (or (member pkg-name stp-headers-ignored-requirements)
        (if search-load-path
            (aand (cl-find-if (fn (eq pkg-sym (car %))) stp-headers-installed-features)
                  (and version
                       ;; Check the newest version found in the load path.
                       (version<= version (cadr it))
                       ;; If the version installed in STP is older we upgrade
                       ;; anyway. This can be the case if the package was
                       ;; installed outside STP or the source code is included
                       ;; in the load path. `stp-installed-version' is used
                       ;; instead of `stp-get-attribute' because the latter
                       ;; might return a hash and those can't be compared by
                       ;; `stp-version<'.
                       (not (aand (member pkg-name (stp-info-names))
                                  (stp-installed-version pkg-name)
                                  (stp-version< it version)))))
          (aand (stp-installed-version pkg-name)
                (stp-version<= version it))))))

(defun stp-requirement-satisfied-p (pkg-name &optional version search-load-path)
  (or (stp-emacs-requirement-satisfied-p pkg-name version)
      (stp-package-requirement-satisfied-p pkg-name version search-load-path)))

(defvar stp-headers-bootstrap-requirements-file "bootstrap-requirements")

(defun stp-headers-write-bootstrap-requirements ()
  (let ((requirements (stp-headers-elisp-requirements)))
    (with-temp-buffer
      (cl-dolist (requirement requirements)
        (db (pkg-sym version)
            requirement
          (insert (format "%s %s\n" pkg-sym version))))
      (write-file stp-headers-bootstrap-requirements-file))))

(defvar stp-headers-update-hook nil
  "Hook to run after headers are updated.")

(defun stp-headers-update-elisp-headers (&optional insert)
  "Update the elisp headers.

When INSERT is non-nil, insert the headers if they are not
present. Return non-nil if a header that was not there before was
inserted."
  (interactive (list t))
  (save-excursion
    (stp-headers-update-elisp-filename-headers insert)
    ;; Go to the prop line.
    (rem-ensure-prop-line)
    (beginning-of-line)
    (forward-comment 1)
    (when (stp-headers-update-copyright-header insert)
      ;; When a copyright header was added, make sure there is a blank line
      ;; before it.
      (beginning-of-line)
      (unless (rem-looking-back-p (format "\\([%s]*[%s][%s]*[%s]\\)\\{0,2\\}"
                                          rem-spaces
                                          rem-newlines
                                          rem-spaces
                                          rem-newlines)
                                  nil
                                  t)
        (replace-match "\n\n")))
    (let (inserted)
      (when insert
        (unless (lm-header "Author")
          (setq inserted t)
          (goto-char (lm-copyright-mark))
          (end-of-line)
          (insert (format "\n\n;; Author: %s <%s>\n" user-full-name user-mail-address)))
        (unless (save-excursion (lm-header "Keywords"))
          (setq inserted t)
          (insert ";; Keywords: TODO\n"))
        (unless (save-excursion (or (lm-header "URL") (lm-header "Website")))
          (awhen (ignore-errors
                   (-> (stp-git-push-target)
                       stp-git-remote-url
                       stp-transform-remote))
            (setq inserted t)
            (insert (format ";; URL: %s\n" it))))
        (when (stp-headers-update-version-header insert)
          (setq inserted t))
        (when (stp-headers-update-requirements-header insert)
          (setq inserted t))
        (unless (save-excursion (lm-header-multiline "Package-Requires"))
          (setq inserted t)
          (insert ";; Package-Requires: ()")))
      (run-hooks 'stp-headers-update-hook)
      (message "Update headers in %s" (or buffer-file-name (buffer-name)))
      inserted)))

(defvar stp-main-package-name-transform (fn (s-chop-suffix ".el" (s-chop-prefix "emacs-" %)))
  "The function to apply to transform the name of a package when
looking for the main file.")

(defvar stp-main-file nil
  "This variable overrides the heuristics in
`stp-main-package-file'. Generally, it should be set as a
file-local or directory-local variable.")

(cl-defun stp-main-package-file (pkg-name &key no-directory relative)
  (or stp-main-file
      (let* ((pkg-path (stp-full-path pkg-name))
             (pkg-file (concat (funcall stp-main-package-name-transform (stp-name pkg-name)) ".el"))
             (paths (->> (regexp-quote pkg-file)
                         (directory-files-recursively pkg-path)
                         stp-sort-paths-top-down))
             (path (car paths))
             (paths2 (->> (directory-files-recursively pkg-path ".*\\.el")
                          stp-sort-paths-top-down
                          (-filter #'stp-headers-elisp-file-requirements)))
             (path2 (and (= (length paths2) 1)
                         (car paths2)))
             (result (or path
                         path2
                         (and (not no-directory) pkg-path))))
        ;; First try the elisp file that has the same name as the package. If that
        ;; doesn't exist, use the file with headers at the top-level if there is
        ;; only one such file. Otherwise, fall back on the package directory.
        (if relative
            (->> (rem-relative-path result stp-source-directory)
                 f-split
                 cdr
                 (apply #'f-join))
          result))))

(provide 'stp-headers)
;;; stp-headers.el ends here
