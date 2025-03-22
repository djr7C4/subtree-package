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

(require 'stp-utils)
(require 'url-parse)

(defun stp-elpa-valid-remote-p (remote)
  (let ((url (url-generic-parse-url remote)))
    (and url
         (let ((host (url-host url))
               (filename (url-filename url)))
           (and host
                filename
                (string= host "elpa.gnu.org")
                (string= (f-dirname filename) "/packages"))))))

(defvar stp-elpa-remote-history nil)

(defun stp-elpa-read-remote (prompt &optional default)
  (stp-read-remote-with-predicate prompt #'stp-elpa-valid-remote-p default 'stp-elpa-remote-history))

(defvar stp-elpa-version-history nil)

(defun stp-elpa-read-version (prompt pkg-name remote &optional default)
  "Read a ELPA version."
  (rem-comp-read prompt
                 (stp-elpa-versions-sorted pkg-name remote)
                 :require-match t
                 :default default
                 :history 'stp-elpa-version-history
                 :sort-fun #'identity))

(defun stp-elpa-version-url-alist (pkg-name remote)
  "Return an alist that maps each available version for the ELPA
package at remote to the URL where it can be downloaded."
  (when-let ((elpa-html-buf (url-retrieve-synchronously remote))
             (elpa-html-tree (unwind-protect
                                 (with-current-buffer elpa-html-buf
                                   (libxml-parse-html-region (point-min) (point-max)))
                               (kill-buffer elpa-html-buf)))
             (elpa-version-url-regexp (concat "\\`" pkg-name "-\\(\\(?:[0-9]+\\.\\)*\\)\\([0-9]+\\)\\(\\.tar\\|\\.el\\)?\\(\\.lz\\)?\\'" ))
             ;; Find href attributes of tags. This will get all the links.
             (elpa-version-urls (mapcar 'cdr
                                        (rem-tree-find-if (lambda (x)
                                                            (and (consp x)
                                                                 (eq (car x) 'href)
                                                                 (stringp (cdr x))))
                                                          elpa-html-tree))))
    ;; Remove everything that is nil. These correspond to the URLs that did not
    ;; match `elpa-version-url-regexp'.
    (-filter 'identity
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
  (mapcar 'car (stp-elpa-version-url-alist pkg-name remote)))

(defun stp-elpa-versions-sorted (pkg-name remote)
  (reverse (-sort 'stp-version<
                  (stp-elpa-versions pkg-name remote))))

(defun stp-elpa-count-versions (pkg-name remote v1 v2)
  (let* ((versions (stp-elpa-versions-sorted pkg-name remote))
         (j (cl-position v1 versions :test #'equal))
         (k (cl-position v2 versions :test #'equal)))
    (and j k (- j k))))

(defun stp-elpa-latest-version (pkg-name remote)
  (car (stp-elpa-versions-sorted pkg-name remote)))

(cl-defun stp-elpa-install-or-upgrade (pkg-info pkg-name remote version &key (type 'install))
  "Install or upgrade to the specified version of pkg-name from remote into
`stp-source-directory'. If the file fetched from remote is an
archive, it will be automatically extracted. type should be
either \\='install or upgrade depending on which operation should
be performed."
  (let ((pkg-path (stp-canonical-path pkg-name))
        (elpa-version-url-alist (stp-elpa-version-url-alist pkg-name remote)))
    (if (eq type 'install)
        (when (f-exists-p pkg-path)
          (error "%s already exists" pkg-name))
      (unless (f-exists-p pkg-path)
        (error "%s does not exist" pkg-name)))
    (when (eq type 'install)
      (make-directory pkg-path))
    (aif (assoc version elpa-version-url-alist)
        (stp-download-elisp pkg-name (cdr it))
      (error "Version %s not found" version))
    (setq pkg-info (stp-set-attribute pkg-info pkg-name 'remote remote))
    (setq pkg-info (stp-set-attribute pkg-info pkg-name 'version version))
    (when (eq type 'install)
      (setq pkg-info (stp-set-attribute pkg-info pkg-name 'method 'elpa)))
    ;; Add any new files to the git index.
    (stp-git-add pkg-path))
  pkg-info)

(defun stp-elpa-install (pkg-info pkg-name remote version)
  "Install the specified version of pkg-name from remote into
`stp-source-directory'."
  (stp-elpa-install-or-upgrade pkg-info pkg-name remote version :type 'install))

(defun stp-elpa-upgrade (pkg-info pkg-name remote version)
  "Upgrade the specified version of pkg-name from remote into
`stp-source-directory'."
  (stp-elpa-install-or-upgrade pkg-info pkg-name remote version :type 'upgrade))

(provide 'stp-elpa)
;;; stp-elpa.el ends here
