;;; -*- lexical-binding: t; -*-

(require 'stp-utils)

;; TODO: use in code
(defclass stp-operation ()
  (pkg-name :initarg :pkg-name))

(defclass stp-package-operation (stp-operation) ())

(defclass stp-uninstall-operation (stp-package-operation) ())

(defvar stp-enforce-min-version nil
  "Determines if the user is allowed to select a version older than
the minimum required by another package.")

(defclass stp-additive-operation (stp-package-operation)
  ((min-version :initarg :min-version)
   (enforce-min-version :initarg :enforce-min-version :initform (symbol-value 'stp-enforce-min-version))))

(defclass stp-install-operation (stp-additive-operation) ())
(defclass stp-upgrade-operation (stp-additive-operation) ())
;; TODO: fill in other types of operations

;; TODO: fill in
;; Determines which remotes, versions and so forth should be used during
;; installation and upgrades. This can be done either interactively or via some
;; policy like preferring the latest stable or unstable.
(defclass stp-controller () ())

(defclass stp-task-options () ())

(defclass stp-basic-task-options (stp-task-options)
  ((do-commit :initform (symbol-value 'stp-auto-commit))
   (do-push :initform (symbol-value 'stp-auto-push))))

(defclass stp-package-task-options (stp-basic-task-options)
  ((do-lock :initform (symbol-value 'stp-auto-lock))))

(defclass stp-additive-task-options (stp-package-task-options)
  ((do-actions :initform (symbol-value 'stp-auto-post-actions))
   (do-audit :initform (symbol-value 'stp-audit-changes))
   (do-update-load-path :initform (symbol-value 'stp-auto-update-load-path))
   (do-load :initform (symbol-value 'stp-auto-load))
   (do-build :initform (symbol-value 'stp-auto-build))
   (do-build-info :initform (symbol-value 'stp-auto-build-info))
   (do-update-info-directories :initform (symbol-value 'stp-auto-update-info-directories))))

(defclass stp-bump-task-options (stp-basic-task-options)
  ((do-tag :initform (symbol-value 'stp-auto-tag))))

;; TODO: use in code
;; TODO: Combine features of `stp-ensure-requirements',
;; `stp-maybe-uninstall-requirements' and `stp-report-requirements'.
(defun stp-execute-tasks (tasks))

(provide 'stp-executor)
;;; stp-executor.el ends here
