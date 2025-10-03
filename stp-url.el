;;; stp-url.el --- Support for URL packages -*- lexical-binding: t; -*-
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

;;; Code:

(require 'stp-git)
(require 'stp-git-utils)
(require 'stp-utils)
(require 'url-parse)

(defun stp-url-valid-remote-p (remote)
  (and (stringp remote)
       (or (f-exists-p remote)
           (let ((url (url-generic-parse-url remote)))
             (and url remote)))))

(defvar stp-url-unsafe-regexps '("emacswiki\\.org")
  "The user should be warned before downloading from an unsafe URL.")

(defun stp-url-safe-remote-p (remote)
  (or (not remote)
      (symbolp remote)
      (let ((domain (if (f-exists-p remote)
                        ;; Handle local paths which cannot be parsed by
                        ;; `url-generic-parse-url'.
                        remote
                      (ignore-errors (url-domain (url-generic-parse-url remote))))))
        (or (not domain)
            (not (-any (lambda (regexp)
                         (and (string-match-p regexp domain) t))
                       stp-url-unsafe-regexps))))
      (yes-or-no-p (format "The remote %s is unsafe. Continue anyway? " remote))))

(defvar stp-url-remote-history nil)

(defun stp-url-read-remote (prompt &optional default)
  (stp-read-remote-with-predicate prompt #'stp-url-valid-remote-p default 'stp-url-remote-history))

(defvar stp-url-version-history nil)

(defun stp-url-default-version ()
  (format-time-string "%m/%d/%Y-%R"))

(cl-defun stp-url-read-version (prompt &optional (default (stp-url-default-version)))
  ;; Versions for URL packages are simply whatever the user chooses. Dates are
  ;; one possibility.
  (rem-read-from-mini prompt :default default :history stp-url-version-history))

(cl-defun stp-url-install-or-upgrade-basic (controller pkg-name remote version action options &key (set-remote t))
  (let ((pkg-path (stp-canonical-path pkg-name)))
    (cond
     ((and (eq action 'install) (f-exists-p pkg-path))
      (error "%s already exists" pkg-name))
     ((and (not (eq action 'install)) (not (f-exists-p pkg-path)))
      (error "%s does not exist" pkg-name)))
    ;; Cleanup of repo is handled by `stp-with-memoization' in higher-level
    ;; commands.
    (let* ((repo (stp-git-download-as-synthetic-repo pkg-name remote))
           (branch (let ((default-directory repo))
                     (stp-git-current-branch))))
      ;; Note that squashing is necessary because otherwise git will refuse to
      ;; merge unrelated histories.
      (if (eq action 'install)
          (stp-git-install controller pkg-name repo branch 'unstable options :squash t :set-pkg-info nil)
        ;; fallback-version is needed for when a package has to be reinstalled.
        (stp-git-upgrade controller pkg-name repo branch options :squash t :set-pkg-info nil :fallback-version version)))
    (when set-remote
      (stp-set-attribute pkg-name 'remote remote))
    (stp-set-attribute pkg-name 'version version)))

(defun stp-url-install-or-upgrade (controller pkg-name remote version action options)
  "Install or upgrade PKG-NAME from REMOTE.

If the file fetched from REMOTE is an archive, it will be
automatically extracted. ACTION should be either \\='install or
upgrade depending on which operation should be performed."
  (when (or (stp-url-safe-remote-p remote)
            (yes-or-no-p (format "The remote %s is unsafe. Proceed anyway?" remote)))
    (stp-url-install-or-upgrade-basic controller pkg-name remote version action options)
    (when (eq action 'install)
      (stp-set-attribute pkg-name 'method 'url))))

(defun stp-url-install (controller pkg-name remote version options)
  "Install PKG-NAME from REMOTE."
  (stp-url-install-or-upgrade controller pkg-name remote version 'install options))

(defun stp-url-upgrade (controller pkg-name remote version options)
  "Upgrade PKG-NAME from REMOTE."
  (stp-url-install-or-upgrade controller pkg-name remote version 'upgrade options))

(provide 'stp-url)

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("dsb" . "cl-destructuring-bind")
;;   ("mvb" . "cl-multiple-value-bind")
;;   ("mvs" . "cl-multiple-value-setq")
;;   ("with-gensyms" . "cl-with-gensyms")
;;   ("once-only" . "cl-once-only")
;;   ("dflet" . "noflet")
;;   ("plet" . "pcase-let")
;;   ("plet*" . "pcase-let*")
;;   ("psetq*" . "pcase-setq")
;;   ("pdolist" . "pcase-dolist")
;;   ("plambda" . "pcase-lambda")
;;   ("pdefmacro" . "pcase-defmacro")
;;   ("epcase" . "pcase-exhaustive")
;;   ("fn" . "rem-fn")
;;   ("fn1" . "rem-fn1")
;;   ("fn2" . "rem-fn2")
;;   ("fn3" . "rem-fn3")
;;   ("fn4" . "rem-fn4")
;;   ("fn5" . "rem-fn5")
;;   ("fn6" . "rem-fn6")
;;   ("fn7" . "rem-fn7")
;;   ("fn8" . "rem-fn8")
;;   ("fn9" . "rem-fn9")
;;   ("fn10" . "rem-fn10")
;;   ("and$" . "cond-let--and$")
;;   ("and>" . "cond-let--and>")
;;   ("and-let" . "cond-let--and-let")
;;   ("if-let" . "cond-let--if-let")
;;   ("when-let" . "cond-let--when-let")
;;   ("while-let" . "cond-let--while-let"))
;; End:
;;; stp-url.el ends here
