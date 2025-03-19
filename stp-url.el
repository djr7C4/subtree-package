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

(require 'url-parse)
(require 'stp-utils)

(defun stp-url-valid-remote-p (remote)
  (let ((url (url-generic-parse-url remote)))
    (and url remote)))

(defvar stp-url-remote-history nil)

(defun stp-url-read-remote (prompt &optional default)
  (stp-read-remote-with-predicate prompt #'stp-url-valid-remote-p default 'stp-url-remote-history))

(defvar stp-url-version-history nil)

(cl-defun stp-url-read-version (prompt &optional (default (format-time-string "%m/%d/%Y")))
  ;; Versions for URL packages are simply whatever the user chooses. Dates are
  ;; one possibility.
  (rem-read-from-mini prompt :default default :history stp-url-version-history))

(cl-defun stp-url-install-or-upgrade (pkg-info pkg-name remote version &key (type 'install))
  "Install or upgrade to the specified version of pkg-name from
remote into `stp-source-directory'. If the file fetched from
remote is an archive, it will be automatically extracted. type
should be either \\='install or upgrade depending on which
operation should be performed."
  (when (or (stp-url-safe-remote-p remote)
            (yes-or-no-p (format "The remote %s is unsafe. Proceed anyway?" remote)))
    (let ((pkg-path (stp-canonical-path pkg-name)))
      (if (eq type 'install)
          (when (f-exists-p pkg-path)
            (error "%s already exists" pkg-name))
        (unless (f-exists-p pkg-path)
          (error "%s does not exist" pkg-name)))
      (when (eq type 'install)
        (make-directory pkg-path))
      (stp-download-elisp pkg-path remote)
      (setq pkg-info (stp-set-attribute pkg-info pkg-name 'remote remote))
      (setq pkg-info (stp-set-attribute pkg-info pkg-name 'version version))
      (when (eq type 'install)
        (setq pkg-info (stp-set-attribute pkg-info pkg-name 'method 'url)))
      ;; Add any new files to the git index.
      (stp-git-add pkg-path)))
  pkg-info)

(defun stp-url-install (pkg-info pkg-name remote version)
  "Install the specified version of pkg-name from remote into
`stp-source-directory'."
  (stp-url-install-or-upgrade pkg-info pkg-name remote version :type 'install))

(defun stp-url-upgrade (pkg-info pkg-name remote version)
  "Upgrade the specified version of pkg-name from remote into
`stp-source-directory'."
  (stp-url-install-or-upgrade pkg-info pkg-name remote version :type 'upgrade))

(provide 'stp-url)
;;; stp-url.el ends here
