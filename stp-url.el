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

(require 'stp-git)
(require 'stp-git-utils)
(require 'stp-utils)
(require 'url-parse)

(defun stp-url-valid-remote-p (remote)
  (or (f-exists-p remote)
      (let ((url (url-generic-parse-url remote)))
        (and url remote))))

(defvar stp-url-remote-history nil)

(defun stp-url-read-remote (prompt &optional default)
  (stp-read-remote-with-predicate prompt #'stp-url-valid-remote-p default 'stp-url-remote-history))

(defvar stp-url-version-history nil)

(cl-defun stp-url-read-version (prompt &optional (default (format-time-string "%m/%d/%Y")))
  ;; Versions for URL packages are simply whatever the user chooses. Dates are
  ;; one possibility.
  (rem-read-from-mini prompt :default default :history stp-url-version-history))

(cl-defun stp-url-install-or-upgrade-basic (pkg-name remote version action)
  (let ((pkg-path (stp-canonical-path pkg-name)))
    (cond
     ((and (eq action 'install) (f-exists-p pkg-path))
      (error "%s already exists" pkg-name))
     ((and (not (eq action 'install)) (not (f-exists-p pkg-path)))
      (error "%s does not exist" pkg-name)))
    (let* ((repo (stp-git-download-as-synthetic-repo pkg-name remote))
           (branch (let ((default-directory repo))
                     (stp-git-current-branch))))
      (unwind-protect
          ;; Note that squashing is necessary because otherwise git will refuse
          ;; to merge unrelated histories.
          (if (eq action 'install)
              (stp-git-install pkg-name repo branch 'unstable  :squash t)
            (stp-git-upgrade pkg-name repo branch :squash t))
        (f-delete repo t)))
    (stp-set-attribute pkg-name 'remote remote)
    (stp-set-attribute pkg-name 'version version)))

(cl-defun stp-url-install-or-upgrade (pkg-name remote version action)
  "Install or upgrade to the specified version of PKG-NAME from
remote into `stp-source-directory'. If the file fetched from
remote is an archive, it will be automatically extracted. type
should be either \\='install or upgrade depending on which
operation should be performed."
  (when (or (stp-url-safe-remote-p remote)
            (yes-or-no-p (format "The remote %s is unsafe. Proceed anyway?" remote)))
    (stp-url-install-or-upgrade-basic pkg-name remote version action)
    (when (eq action 'install)
      (stp-set-attribute pkg-name 'method 'url))))

(defun stp-url-install (pkg-name remote version)
  "Install the specified version of pkg-name from remote into
`stp-source-directory'."
  (stp-url-install-or-upgrade pkg-name remote version 'install))

(defun stp-url-upgrade (pkg-name remote version)
  "Upgrade the specified version of pkg-name from remote into
`stp-source-directory'."
  (stp-url-install-or-upgrade pkg-name remote version 'upgrade))

(provide 'stp-url)
;;; stp-url.el ends here
