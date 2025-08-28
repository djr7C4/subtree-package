;;; -*- lexical-binding: t; -*-

(require 'stp-utils)

;; TODO: fill in
;; Determines which remotes, versions and so forth should be used during
;; installation and upgrades. This can be done either interactively or via some
;; policy like preferring the latest stable or unstable.
(defclass stp-controller ()
  (options :initarg :options))

;; TODO: use in code
(defclass stp-operation ()
  ((pkg-name :initarg :pkg-name :initform nil)
   ;; This slot overrides the controller argument in `stp-execute' when non-nil.
   (controller :initarg :controller :initform nil)
   ;; This slot overrides the options argument in `stp-execute' when non-nil.
   (options :initarg :options :initform nil)))

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

(defclass stp-task-options () ())

(defclass stp-basic-task-options (stp-task-options)
  ((do-commit :initarg :do-commit :initform (symbol-value 'stp-auto-commit))
   (do-push :initarg :do-push :initform (symbol-value 'stp-auto-push))))

(defclass stp-package-task-options (stp-basic-task-options)
  ((do-lock :initarg :do-lock :initform (symbol-value 'stp-auto-lock))))

(defclass stp-action-task-options (stp-task-options)
  ((do-actions :initarg :do-actions :initform (symbol-value 'stp-auto-post-actions))
   (do-update-load-path :initarg :do-update-load-path :initform (symbol-value 'stp-auto-update-load-path))
   (do-load :initarg :do-load :initform (symbol-value 'stp-auto-load))
   (do-build :initarg :do-build :initform (symbol-value 'stp-auto-build))
   (do-build-info :initarg :do-build-info :initform (symbol-value 'stp-auto-build-info))
   (do-update-info-directories :initarg :do-update-info-directories :initform (symbol-value 'stp-auto-update-info-directories))))

(defclass stp-additive-task-options (stp-package-task-options stp-action-task-options)
  ((do-audit :initarg :do-audit :initform (symbol-value 'stp-audit-changes))))

(defclass stp-bump-task-options (stp-basic-task-options)
  ((do-tag :initarg :do-tag :initform (symbol-value 'stp-auto-tag))))

(cl-defmethod stp-validate-options ((_options stp-task-options))
  t)

(cl-defmethod stp-validate-options ((options stp-basic-task-options))
  (with-slots (do-commit do-push)
      options
    (when (and (not do-commit) do-push)
      (user-error "Pushing without committing is not allowed")))
  (cl-call-next-method))

(cl-defmethod stp-validate-options ((options stp-bump-task-options))
  (with-slots (do-commit do-tag)
      options
    (when (and (not do-commit) do-tag)
      (user-error "Tagging without committing is not allowed"))
    (cl-call-next-method)))

;; TODO: use in code
;; TODO: Combine features of `stp-ensure-requirements',
;; `stp-maybe-uninstall-requirements' and `stp-report-requirements'.
(defun stp-execute (tasks))

(provide 'stp-executor)
;;; stp-executor.el ends here
