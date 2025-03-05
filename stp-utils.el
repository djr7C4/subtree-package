;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'f)
(require 'rem)
(require 'rem-abbrev)
(require 's)

(defvar stp-source-directory (f-join user-emacs-directory "package-source"))

(defun stp-delete-load-path (pkg-name)
  (setq load-path (cl-delete-if (lambda (path)
                                  (f-same-p (stp-absolute-path pkg-name)
                                            (f-slash path)))
                                load-path)))

(defun stp-absolute-path (pkg-path)
  "Return the absolute path to pkg-path. pkg-path can be either an absolute path
or relative from `stp-source-directory'. The return value always ends
with a slash."
  (f-slash (f-canonical (if (f-absolute-p pkg-path)
                            pkg-path
                          (f-join stp-source-directory pkg-path)))))

(defun stp-name (pkg-path)
  "Returns the name of the package. Unlike `stp-relative-path', this
  never ends with a slash (nor does it contain any slashes)."
  (f-filename pkg-path))

(defmacro stp-with-package-source-directory (&rest body)
  (declare (indent 0))
  `(rem-with-directory stp-source-directory
     ,@body))

(def-edebug-spec stp-with-package-source-directory t)

(defun stp-filesystem-names ()
  "Return a list of packages installed in `stp-source-directory'."
  (stp-with-package-source-directory
    (-filter 'f-directory-p (directory-files stp-source-directory nil "^[^.]"))))

(defun stp-info-names ()
  "Return a list of packages stored in `stp-info-file'."
  (sort (mapcar 'car (stp-read-info)) 'string<))

(defvar stp-methods-order '(git elpa url)
  "Valid values for the METHOD attribute.")

(defvar stp-attribute-order '(method remote version update branch)
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

(defun stp-read-remote-with-predicate (prompt valid-remote-p &optional default history)
  (let (remote)
    (while (or (not remote) (not (funcall valid-remote-p remote)))
      (when remote
        (minibuffer-message "%s is invalid" remote))
      (setq remote (rem-read-from-mini prompt :default default :history history)))
    remote))

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
  (if (file-readable-p stp-info-file)
      (with-temp-buffer
        (insert-file-contents stp-info-file)
        (read (buffer-string)))
    (error "Could not read %s" stp-info-file)))

(defun stp-write-info (pkg-info)
  (with-temp-buffer
    (insert ";;; mode: read-only; -*- no-byte-compile: t; -*-\n\n")
    (insert (pp (stp-sort-info pkg-info) (current-buffer)))
    (write-file stp-info-file)))

(defun stp-get-attribute (pkg-info pkg-name attr)
  "Get the attribute attr in the alist with the key corresponding to
pkg-name."
  (let ((pkg-name (stp-name pkg-name)))
    (alist-get attr (alist-get pkg-name pkg-info nil nil 'equal))))

(defun stp-set-attribute (pkg-info pkg-name attr val)
  "Set the attribute attr to val in the alist with the key
corresponding to pkg-name."
  (let* ((pkg-name (stp-name pkg-name))
         (alist (alist-get pkg-name pkg-info nil nil 'equal)))
    (if alist
        (setf (alist-get attr alist) val
              (alist-get pkg-name pkg-info nil nil 'equal) alist)
      (setq pkg-info (cons `(,pkg-name . ((,attr . ,val))) pkg-info))))
  pkg-info)

(defun stp-delete-attribute (pkg-info pkg-name attr)
  "Remove attr from the alist with the key corresponding to
pkg-name."
  (let ((alist (stp-get-alist pkg-info pkg-name)))
    (stp-set-alist pkg-info pkg-name (remq (assoc attr alist) alist))))

(defun stp-get-alist (pkg-info pkg-name)
  "Get the alist the contains information corresponding to pkg-name."
  (let ((pkg-name (stp-name pkg-name)))
    (alist-get pkg-name pkg-info nil nil 'equal)))

(defun stp-set-alist (pkg-info pkg-name alist)
  "Set the alist the contains information corresponding to pkg-name to alist."
  (let ((pkg-name (stp-name pkg-name)))
    (setf (alist-get pkg-name pkg-info nil nil 'equal) alist))
  pkg-info)

(defvar stp-version-regexp "\\`\\(?:\\(?:v\\|V\\|release\\|Release\\|version\\|Version\\)\\(?:[-_./]?\\)\\)?\\([0-9]+[a-zA-Z]?\\(\\([-_./]\\)[0-9]+[a-zA-Z]?[-_./]?\\)*\\)\\'")

(defvar stp-version-extractor-alist
  ;; This matches the versions for most emacs packages.
  `((,stp-version-regexp .
                         (lambda (v)
                           (mapcan (lambda (s)
                                     (save-match-data
                                       (if (string-match "\\`\\([0-9]+\\)\\([A-Za-z]+\\)\\'" s)
                                           (list (match-string 1 s) (match-string 2 s))
                                         (list s))))
                                   (s-split "[-_.]" v))))
    ;; haskell-mode
    ("\\`\\(?:haskell-mode-\\)\\(1-44_\\|1-45_\\)\\'" .
     (lambda (v)
       (s-split "-" (s-chop-prefix "haskell-mode-" v))))
    ;; auctex
    ("\\`\\(?:auctex_release\\|auctex\\|release\\|rel\\)[-_./]\\([0-9]+\\([-_.][0-9]+\\)*[a-zA-Z]?\\+?\\)\\'" .
     (lambda (v)
       (let* ((vs (s-split "_\\|-" v))
              (v-butlast (butlast vs))
              (v-last (car (last vs))))
         ;; Any trailing letters or +'s need to be separate elements of the list
         ;; for the version comparison to work correctly. This is because
         ;; otherwise, for example, 6+ would be treated as newer than 10.
         (append v-butlast
                 (save-match-data
                   (if (string-match "\\`\\([0-9]+\\)\\([a-zA-Z]?\\)\\(\\+?\\)\\'"
                                     v-last)
                       (list (match-string 1 v-last)
                             (match-string 2 v-last)
                             (match-string 3 v-last))
                     (list v-last))))))))
  "An list of regexps to match to package versions and functions to extract a
  key to compare.")

(defun stp-version-extract (version)
  (dolist (cell stp-version-extractor-alist)
    (db (regexp . extractor)
        cell
      (save-match-data
        (when (string-match regexp version)
          (cl-return (funcall extractor (match-string 1 version))))))))

;; (defvar stp-memoized-functions '(stp-git-remote-hash-alist stp-elpa-version-url-alist))
;;
;; (defmacro stp-with-memoization (&rest body)
;;   "Memoize functions that make requests via HTTP or Git that are not likely to
;;   change during the body of the macro. Many stp functions that are
;;   wrapped in this macro will run significantly faster."
;;   `(unwind-protect
;;        (progn
;;          ,@(mapcar (lambda (fun)
;;                      `(memoize ',fun))
;;                    stp-memoized-functions)
;;          ,@body)
;;      ,@(mapcar (lambda (fun)
;;                  `(memoize-restore ',fun))
;;                stp-memoized-functions)))

(defun stp-download-elisp (pkg-name remote)
  "Download the elisp file or archive at url and copy it to pkg-name. pkg-name
should already exist."
  (let ((pkg-name (f-filename pkg-name))
        (pkg-path (stp-absolute-path pkg-name)))
    ;; Check for archives
    (if (string= (f-ext remote) "el")
        ;; Ordinary elisp files can simply be downloaded and copied to the
        ;; package directory.
        (url-copy-file remote (f-swap-ext (f-join pkg-path pkg-name) "el"))
      ;; Archives are downloaded, extracted and then copied to pkg-name.
      (let* ((temp-dir (make-temp-file pkg-name t))
             (archive-path (f-join temp-dir (f-filename remote)))
             (extract-path (file-name-sans-extension archive-path)))
        (unwind-protect
            (progn
              (url-copy-file remote archive-path)
              (rem-extract-archive archive-path)
              (copy-directory extract-path pkg-path nil nil t))
          (delete-directory temp-dir t))))))

(defvar stp-url-unsafe-regexps '("emacswiki\\.org")
  "The user should be warned before downloading from an unsafe URL.")

(defun stp-url-safe-remote-p (remote)
  (or (not remote)
      (progn
        (setq remote (url-generic-parse-url remote))
        (not (-any (lambda (regexp)
                     (and (string-match-p regexp (url-domain remote)) t))
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
    (rem-with-directory directory
      (db (exit-code output)
          (rem-call-process-shell-command stp-make-target-command)
        (when (/= exit-code 0)
          (error "Failed to obtain make targets in %s" directory))
        (s-split rem-positive-whitespace-regexp output t)))))

(defun stp-before-build-command (cmd buf)
  (with-current-buffer buf
    (read-only-mode 0)
    (insert "\n\n")
    (insert (format "Current directory: %s\n" default-directory))
    (insert cmd)))

(provide 'stp-utils)
;;; stp-utils.el ends here
