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

(require 'stp-url)
(require 'stp-utils)
(require 'url-parse)

(defun stp-elpa-valid-remote-p (remote)
  (and (stringp remote)
       (let ((url (url-generic-parse-url remote)))
         (and url
              (let* ((host (url-host url))
                     (filename (url-filename url))
                     (dir (f-dirname filename)))
                (and host
                     filename
                     (cond
                      ((string= host "elpa.gnu.org")
                       (string= dir "/packages"))
                      ((string= host "elpa.nongnu.org")
                       (string= dir "/nongnu")))))))))

(defvar stp-elpa-url-format-alist '(("gnu" . "https://elpa.gnu.org/packages/%s.html")
                                    ("nongnu" . "https://elpa.nongnu.org/nongnu/%s.html")))

(cl-defun stp-elpa-package-urls (pkg-name archives &key annotate)
  (-filter #'identity
           (mapcar (lambda (cell)
                     (db (archive . url-format)
                         cell
                       (when (member archive archives)
                         (concat (format url-format pkg-name)
                                 (if annotate
                                     " (elpa)"
                                   "")))))
                   stp-elpa-url-format-alist)))

(defvar stp-elpa-remote-history nil)

(defun stp-elpa-read-remote (prompt &optional default)
  (stp-read-remote-with-predicate prompt #'stp-elpa-valid-remote-p default 'stp-elpa-remote-history))

(defvar stp-elpa-version-history nil)

(cl-defun stp-elpa-read-version (prompt pkg-name remote &key default min-version)
  "Read a ELPA version."
  (let ((versions (stp-elpa-versions-sorted pkg-name remote)))
    (when min-version
      (setq versions (stp-filter-by-min-version min-version versions)))
    (rem-comp-read prompt
                 versions
                 :require-match t
                 :default default
                 :history 'stp-elpa-version-history
                 :sort-fun #'identity)))

(defun stp-elpa-version-url-alist (pkg-name remote)
  "Return an alist that maps versions to download URLs."
  (let* ((elpa-html-buf (or (url-retrieve-synchronously remote)
                            (error "Failed to retrieve %s" remote)))
         (elpa-html-tree (unwind-protect
                             (with-current-buffer elpa-html-buf
                               (libxml-parse-html-region (point-min) (point-max)))
                           (kill-buffer elpa-html-buf)))
         (elpa-version-url-regexp (concat "^" pkg-name "-\\(\\(?:[0-9]+\\.\\)*\\)\\([0-9]+\\)\\(\\.tar\\|\\.el\\)?\\(\\.lz\\)?$" ))
         ;; Find href attributes of tags. This will get all the links.
         (elpa-version-urls (mapcar #'cdr
                                    (rem-tree-find-if (lambda (x)
                                                        (and (consp x)
                                                             (eq (car x) 'href)
                                                             (stringp (cdr x))))
                                                      elpa-html-tree))))
    ;; Remove everything that is nil. These correspond to the URLs that did not
    ;; match `elpa-version-url-regexp'.
    (-filter #'identity
             ;; Construct an alist the maps versions to the corresponding URL.
             (mapcar (lambda (url)
                       (setq url (f-filename url))
                       (save-match-data
                         (when (string-match elpa-version-url-regexp url)
                           (cons (concat (match-string 1 url) (match-string 2 url))
                                 ;; f-expand and expand-file-name aren't smart
                                 ;; enough to handle URLs.
                                 (concat (f-slash (rem-url-dirname remote)) url)))))
                     elpa-version-urls))))

(defun stp-elpa-versions (pkg-name remote)
  (mapcar #'car (stp-elpa-version-url-alist pkg-name remote)))

(defun stp-elpa-versions-sorted (pkg-name remote)
  (reverse (-sort #'stp-version<
                  (stp-elpa-versions pkg-name remote))))

(defun stp-elpa-count-versions (pkg-name remote v1 v2)
  (let* ((versions (stp-elpa-versions-sorted pkg-name remote))
         (j (cl-position v1 versions :test #'equal))
         (k (cl-position v2 versions :test #'equal)))
    (and j k (- j k))))

(defun stp-elpa-latest-version (pkg-name remote)
  (car (stp-elpa-versions-sorted pkg-name remote)))

(defun stp-elpa-version-upgradable-p (count-to-stable)
  (and count-to-stable (> count-to-stable 0) t))

(defun stp-elpa-download-url (pkg-name remote version)
  (let* ((elpa-version-url-alist (stp-elpa-version-url-alist pkg-name remote))
         (url (or (cdr (assoc version elpa-version-url-alist))
                  (error "Version %s not found" version))))
    url))

(cl-defun stp-elpa-install-or-upgrade (pkg-name remote version action)
  "Install or upgrade to the specified VERSION of PKG-NAME.

The package is downloaded from REMOTE. If the file fetched from
REMOTE is an archive, it will be automatically extracted. TYPE
should be either \\='install or \\='upgrade depending on which
operation should be performed."
  (let ((url (stp-elpa-download-url pkg-name remote version))
        (new-version version)
        (old-version (stp-get-attribute pkg-name 'version)))
    (when (and (eq action 'upgrade)
               (string= old-version new-version))
      (user-error "Version %s of %s is already installed" old-version pkg-name))
    (stp-url-install-or-upgrade-basic pkg-name url new-version action :set-remote nil)
    (stp-set-attribute pkg-name 'remote remote)
    (when (eq action 'install)
      (stp-set-attribute pkg-name 'method 'elpa))))

(defun stp-elpa-install (pkg-name remote version)
  "Install the specified VERSION of PKG-NAME from REMOTE."
  (stp-elpa-install-or-upgrade pkg-name remote version 'install))

(defun stp-elpa-upgrade (pkg-name remote version)
  "Upgrade to the specified VERSION of PKG-NAME from REMOTE."
  (stp-elpa-install-or-upgrade pkg-name remote version 'upgrade))

(provide 'stp-elpa)
;;; stp-elpa.el ends here
