;;; -*- lexical-binding: t; -*-

(require 'stp-utils)

;; TODO: use in code
(defclass stp-operation ()
  (pkg-name :initarg :pkg-name)
  (controller :initarg controller))

(defclass stp-package-operation (stp-operation) ())

(defclass stp-uninstall (stp-package-operation) ())

(defclass stp-additive-operation (stp-package-operation)
  ((min-version :initarg :min-version)
   (enforce-min-version :initarg :enforce-min-version)))

(defclass stp-install (stp-additive-operation) ())
(defclass stp-upgrade (stp-additive-operation) ())
;; TODO: fill in other types of operations

;; TODO: fill in
;; Determines which remotes, versions and so forth should be used during
;; installation and upgrades. This can be done either interactively or via some
;; policy like preferring the latest stable or unstable.
(defclass stp-controller () ())

(defclass stp-task-options ()
  ((do-commit :initarg :do-commit :initform (symbol-value 'stp-auto-commit))
   (do-push :initarg :do-push :initform (symbol-value 'stp-auto-push))
   (do-lock :initarg :do-lock :initform (symbol-value 'stp-auto-lock))
   (do-actions :initarg :do-actions :initform (symbol-value 'stp-auto-post-actions))
   (do-audit :initarg :do-audit :initform (symbol-value 'stp-audit-changes))
   (do-tag :initarg :do-tag :initform (symbol-value 'stp-auto-tag))
   (do-update-load-path :initarg :do-update-load-path :initform (symbol-value 'stp-auto-update-load-path))
   (do-load :initarg :do-load :initform (symbol-value 'stp-auto-load))
   (do-build :initarg :do-build :initform (symbol-value 'stp-auto-build))
   (do-build-info :initarg :do-build-info :initform (symbol-value 'stp-auto-build-info))
   (do-update-info-directories :initarg :do-update-info-directories :initform (symbol-value 'stp-auto-update-info-directories))))

(defun stp-toggle-options (options)
  (stp-toggle-object "Toggle options: " options))

(cl-defun stp-command-options (&key toggle-p (fn current-prefix-arg))
  (let ((options (make-instance 'stp-task-options)))
    (if (stp-maybe-call toggle-p)
        (stp-toggle-options options)
      options)))

;; TODO: use in code
;; TODO: Combine features of `stp-ensure-requirements',
;; `stp-maybe-uninstall-requirements' and `stp-report-requirements'.
(defun stp-execute-tasks (tasks))

(provide 'stp-executor)
;;; stp-executor.el ends here
