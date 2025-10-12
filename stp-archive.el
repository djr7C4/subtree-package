;;; stp-archive.el --- Support for Emacs package archives -*- lexical-binding: t; -*-
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

(require 'async)
(require 'map)
(require 'package)
(require 'rem)
(require 'stp-headers)
(require 'stp-url)
(require 'stp-utils)

(defun stp-archive-valid-remote-p (remote)
  (and (symbolp remote)
       (map-elt package-archives (symbol-name remote))))

(defvar stp-archive-stable-archives '("gnu" "nongnu" "melpa-stable"))

(defun stp-archive-stable-p (remote)
  (when (symbolp remote)
    (setq remote (symbol-name remote)))
  (member remote stp-archive-stable-archives))

(defun stp-archive-latest-version (pkg-name remote)
  (when (symbolp remote)
    (setq remote (symbol-name remote)))
  (let ((desc (stp-archive-get-desc pkg-name remote)))
    (and desc (package-version-join (package-desc-version desc)))))

(defun stp-archive-latest-stable-version (pkg-name remote)
  (when (stp-archive-stable-p remote)
    (stp-archive-latest-version pkg-name remote)))

(defun stp-archive-latest-unstable-version (pkg-name remote)
  (unless (stp-archive-stable-p remote)
    (stp-archive-latest-version pkg-name remote)))

(defun stp-archive-version-upgradable-p (pkg-name remote)
  (let-alist (stp-get-alist pkg-name)
    (let ((latest-version (stp-archive-latest-version pkg-name remote)))
      (and latest-version (version< .version latest-version)))))

(defvar stp-archive-async-refresh-running nil)

(defvar stp-archive-last-refreshed most-negative-fixnum)

(defvar stp-archive-refresh-interval (timer-duration "1 day")
  "The number of seconds until the archives are stale.

After this, they should be refreshed.")

(cl-defun stp-archive-async-refresh (&key force quiet)
  "Refresh the package archive asynchronously in a separate process.

When FORCE is non-nil or with a prefix argument interactively,
refresh even if the last refresh was less than
`stp-archive-refresh-interval' seconds ago.

When QUIET is non-nil, suppress messages."
  (interactive (list :force current-prefix-arg))
  (when stp-archive-async-refresh-running
    (user-error "`stp-archive-async-refresh' is already running"))
  ;; Only refresh when it has been at least `stp-archive-refresh-interval'
  ;; seconds since the last refresh.
  (if (or force
          (> (- (float-time) stp-archive-last-refreshed)
             stp-archive-refresh-interval))
      (progn
        (setq stp-archive-async-refresh-running t
              stp-archive-last-refreshed (float-time))
        (unless quiet
          (stp-msg "Refreshing package archives asynchronously"))
        (async-start
         `(lambda ()
            (require 'package)
            ,(async-inject-variables "^\\(package-archives\\|user-emacs-directory\\|package-user-dir\\)$")
            (package-refresh-contents)
            nil)
         (lambda (_result)
           ;; Receiving `package-archive-contents' from the child process is very slow
           ;; because it is so large. It is much faster to just read the package
           ;; archive from disk instead.
           (package-read-all-archive-contents)
           (setq stp-archive-async-refresh-running nil)
           (unless quiet
             (stp-msg "Asynchronous refresh of the package archives finished")))))
    (unless quiet
      (stp-msg "The package archives were last refreshed on %s: no refresh is necessary"
               (format-time-string "%c" stp-archive-last-refreshed)))))

(defun stp-archive-ensure-loaded ()
  (unless package-archive-contents
    (package-read-all-archive-contents)))

(defun stp-archive-package-names ()
  "Return all names of packages in `package-archive-contents'.

Names are strings and are sorted in alphabetical order."
  (->> package-archive-contents
       (mapcar (-compose #'symbol-name #'car))
       (-sort #'string<)))

;; This function is useful due to memoization.
(stp-defmemoized stp-achive-get-descs (pkg-name)
  (map-elt package-archive-contents (intern pkg-name)))

(defun stp-archive-get-desc (pkg-name archive)
  "Find the `package-desc' for PKG-NAME in ARCHIVE."
  (when (symbolp archive)
    (setq archive (symbol-name archive)))
  (cl-find-if (lambda (desc)
                (string= (package-desc-archive desc) archive))
              (stp-achive-get-descs pkg-name)))

(defun stp-archive-find-remotes (pkg-name)
  "Find remotes for PKG-NAME in `package-archive-contents'.

The result is returned as an alist that maps valid remotes to
methods."
  (--> (stp-achive-get-descs pkg-name)
       (mapcar (lambda (desc)
                 (map-elt (package-desc-extras desc) :url))
               it)
       (-filter #'identity it)
       (cl-remove-duplicates it :test #'equal)
       (mapcar (lambda (remote)
                 (when (stp-valid-remote-p remote)
                   (setq remote (stp-transform-remote remote))
                   (cons remote (stp-remote-method remote))))
               it)
       (-filter #'identity it)))

(defun stp-archives (pkg-name)
  "Find the archives that PKG-NAME is available on."
  (seq-sort-by (lambda (archive)
                 (cl-position archive (mapcar #'car package-archives) :test #'string=))
               #'<
               (mapcar #'package-desc-archive (stp-achive-get-descs pkg-name))))

(defun stp-archive-url (desc)
  "Return the URL for the `package-desc' DESC."
  (format "%s%s.%s"
          (package-archive-base desc)
          (package-desc-full-name desc)
          (symbol-name (package-desc-kind desc))))

(defun stp-archive-download-url (pkg-name archive)
  (let ((desc (or (stp-archive-get-desc pkg-name archive)
                  (error "Failed to find %s in the %s package archive" pkg-name archive))))
    (stp-archive-url desc)))

(defun stp-archive-version (pkg-name archive)
  (let ((desc (or (stp-archive-get-desc pkg-name archive)
                  (error "Failed to find %s in the %s package archive" pkg-name archive))))
    (package-version-join (package-desc-version desc))))

(cl-defun stp-archive-install-or-upgrade (controller pkg-name archive action options)
  "Install or upgrade PKG-NAME from ARCHIVE using CONTROLLER.

The current version is used instead of allowing the version to be
passed as an argument because archives do not support installing
older versions. ACTION should be either \\='install or
\\='upgrade depending on which operation should be performed.
OPTIONS are used when a callback to the CONTROLLER is needed."
  (when (symbolp archive)
    (setq archive (symbol-name archive)))
  (let* ((url (stp-archive-download-url pkg-name archive))
         (old-version (stp-get-attribute pkg-name 'version))
         (new-version (stp-archive-version pkg-name archive)))
    (when (and (eq action 'upgrade) (string= old-version new-version))
      (user-error "Version %s of %s is already installed" old-version pkg-name))
    (stp-url-install-or-upgrade-basic controller pkg-name url new-version action options)
    (when (eq action 'install)
      (stp-set-attribute pkg-name 'method 'archive))))

(defun stp-archive-install (controller pkg-name archive options)
  "Install PKG-NAME from ARCHIVE using CONTROLLER with OPTIONS."
  (stp-archive-install-or-upgrade controller pkg-name archive 'install options))

(defun stp-archive-upgrade (controller pkg-name archive options)
  "Upgrade PKG-NAME from ARCHIVE using CONTROLLER with OPTIONS."
  (stp-archive-install-or-upgrade controller pkg-name archive 'upgrade options))

(provide 'stp-archive)

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
;;; stp-archive.el ends here
