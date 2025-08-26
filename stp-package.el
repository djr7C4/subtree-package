;;; -*- lexical-binding: t; -*-

(require 'eieio)

(defclass stp-package ()
  (name :initarg :name)
  (remote :initarg :remote)
  (last-remote :initarg :last-remote)
  (version :initarg :version)
  (dependency :initarg :dependency)
  (requirements :initarg :requirements))

(defclass stp-git-package (stp-package)
  (update :initarg :update)
  (branch :initarg :branch))

(defclass stp-elpa-package (stp-package) ())

(defclass stp-archive-package (stp-package) ())

(defclass stp-url-package (stp-package) ())

;; TODO: use these classes from other code instead of the current alist
;; structure.

(provide 'stp-package)
;;; stp-package.el ends here
