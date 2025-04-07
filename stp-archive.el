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

(require 'async)
(require 'package)

(defun stp-archive-async-refresh ()
  "Refresh the package archive asynchronously in a separate process."
  (interactive)
  (async-start
   `(lambda ()
      (require 'package)
      ,(async-inject-variables "^package-archives$")
      (package-refresh-contents)
      nil)
   (lambda (_result)
     ;; Receiving `package-archive-contents' from the child process is very slow
     ;; because it is so large. It is much faster to just read the package
     ;; archive from disk instead.
     (package-read-all-archive-contents)
     (message "Asynchronous refresh package archive finished"))))

(defun stp-package-requirements (pkg-name)
  "Find all packages that are required by PKG-NAME."
  (let* (reqs
         (pkg-path (stp-canonical-path pkg-name))
         (files (rem-elisp-files-to-load pkg-path :keep-extensions t :extensions '("el"))))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (setq reqs (append reqs (awhen (ignore-errors (package-buffer-info))
                                  (package-desc-reqs it))))))
    (mapcar (lambda (cell)
              (db (pkg-name-sym . pkg-reqs)
                  cell
                (cons pkg-name-sym
                      ;; Select the most recent version of each package that is
                      ;; required by one of its files.
                      (car (-sort (lambda (v1 v2)
                                    (version-list-< v2 v1))
                                  (mapcar #'cdr pkg-reqs))))))
            (-group-by #'car reqs))))

(provide 'stp-archive)
;;; stp-archive.el ends here
