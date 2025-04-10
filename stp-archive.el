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

(require 'async)
(require 'package)
(require 'persist nil t)
(require 'rem)
(require 'stp-utils)

(defvar stp-archive-async-refresh-running nil)

(with-no-warnings
  (when (featurep 'persist)
    (persist-defvar stp-archive-last-refreshed most-negative-fixnum nil)))

(defvar stp-archive-last-refreshed most-negative-fixnum)

(defvar stp-archive-refresh-interval (timer-duration "1 day")
  "The number of seconds until the package archives are considered
stale and should be refreshed.")

(cl-defun stp-archive-async-refresh (&key force quiet)
  "Refresh the package archive asynchronously in a separate process.
When FORCE is non-nil or with a prefix argument interactively,
refresh even if the last refresh was less than
`stp-archive-refresh-interval' seconds ago."
  (interactive (list :force current-prefix-arg))
  (when stp-archive-async-refresh-running
    (user-error "`stp-archive-async-refresh' is already running"))
  ;; Only refresh when it has been at least `stp-archive-refresh-interval'
  ;; seconds since the last refresh.
  (if (or force
          (> (- (rem-seconds) stp-archive-last-refreshed)
             stp-archive-refresh-interval))
      (progn
        (setq stp-archive-async-refresh-running t
              stp-archive-last-refreshed (rem-seconds))
        (unless quiet
          (message "Refreshing package archives asynchronously"))
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
           (setq stp-archive-async-refresh-running nil)
           (unless quiet
             (message "Asynchronous refresh of the package archives finished")))))
    (unless quiet
      (message "The package archives were last refreshed on %s: no refresh is necessary"
               (format-time-string "%c" stp-archive-last-refreshed)))))

(defun stp-package-requirements (pkg-name)
  "Find all packages that are required by PKG-NAME according to the
Package-Requires field of its elisp files."
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
                                  (mapcar #'cadr pkg-reqs))))))
            (-group-by #'car reqs))))

(provide 'stp-archive)
;;; stp-archive.el ends here
