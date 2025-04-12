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
(require 'map)
(require 'package)
(require 'rem)
(require 'stp-headers)
(require 'stp-utils)

(defvar stp-archive-async-refresh-running nil)

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

(defun stp-archive-ensure-loaded ()
  (unless package-archive-contents
    (package-read-all-archive-contents)))

(defun stp-archive-package-names ()
  "Return a list of all packages in `package-archive-contents' as
 strings in alphabetical order."
  ;; `package-archive-contents' needs to be initialized in order for this
  ;; comment to work. Ideally, we would run `package-refresh-contents' but that
  ;; would make everything very slow.
  (->> package-archive-contents
       (mapcar (-compose #'symbol-name #'car))
       (-sort #'string<)))

(cl-defun stp-archive-find-remotes (pkg-name &key keep-methods)
  "Find remotes for PKG-NAME in `package-archive-contents'. The
result is returned as an alist that maps methods to valid
remotes."
  (--> (map-elt package-archive-contents (intern pkg-name))
       (mapcar (lambda (desc)
                 (map-elt (package-desc-extras desc) :url))
               it)
       (-filter #'identity it)
       (cl-remove-duplicates it :test #'equal)
       (mapcar (lambda (remote)
                 (when-let ((method (stp-remote-method remote)))
                   (cons method (stp-transform-remote remote))))
               it)
       (-filter #'identity it)
       (stp-sort-remotes it)
       (mapcar (if keep-methods #'identity #'cdr) it)))

(provide 'stp-archive)
;;; stp-archive.el ends here
