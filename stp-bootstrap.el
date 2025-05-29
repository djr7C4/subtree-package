;;; -*- lexical-binding: t; -*-

(defvar stp-pkg-source-directories '("clients" "contrib" "core" "elisp" "extensions" "lisp" "src"))

(defun stp-bootstrap ()
  ;; We need dash, f and s because they are used by stp.
  (setq load-path (append (mapcar (lambda (dir)
                                    ;; Don't use `f-join' as we do elsewhere in
                                    ;; STP as it is an external dependency that
                                    ;; will not be available during
                                    ;; bootstrapping.
                                    (expand-file-name (concat dir "/") stp-source-directory))
                                  '("dash"
                                    "f"
                                    "s" ;; s needs to be here because it is
                                        ;; required by f.
                                    ))
                          load-path)))

(defun stp-update-load-path (pkg-path &optional interactive-p)
  "Add all appropriate directories in PKG-PATH to the `load-path'."
  (interactive "DDirectory: \nd")
  (setq pkg-path (file-name-as-directory pkg-path))
  (unless (file-name-absolute-p pkg-path)
    (user-error "%s is not an absolute path" pkg-path))
  ;; Include package directories and some sub-directories.
  (add-to-list 'load-path pkg-path)
  (dolist (subdir stp-pkg-source-directories)
    (let ((path (expand-file-name subdir pkg-path)))
      (when (file-directory-p path)
        (add-to-list 'load-path path)
        (stp-update-load-path path))))
  (when interactive-p
    (message "Loaded package at %s" pkg-path)))

(defun stp-update-load-paths (&optional interactive-p)
  "Add all appropriate package directories to the `load-path'."
  ;; We do not use `directory-files-recursively' because it includes too much.
  ;; Not all elisp files are meant to be in the `load-path'.
  (interactive (list t))
  (dolist (pkg-path (directory-files stp-source-directory t))
    (when (and (file-directory-p pkg-path)
               ;; Do not consider ".", ".." or any path that ends with "/." or
               ;; "/..". The intent here is to prevent the current directory
               ;; and the parent directory from being considered. There are
               ;; certianly nicer ways to do this but this one does not require
               ;; dependencies such as f.
               (not (string= pkg-path "."))
               (not (string= pkg-path ".."))
               (not (string= (substring pkg-path -2) "/."))
               (not (string= (substring pkg-path -3) "/..")))
      (stp-update-load-path pkg-path)))
  (when interactive-p
    (message "Load paths updated")))

(provide 'stp-bootstrap)
;;; stp-bootstrap.el ends here
