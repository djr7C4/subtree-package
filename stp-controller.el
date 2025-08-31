;;; -*- lexical-binding: t; -*-

(require 'stp-utils)

;; TODO: use in code
(defclass stp-operation ()
  ((pkg-name :initarg :pkg-name :initform nil)
   ;; This overrides the controller's options slot in `stp-execute' when
   ;; non-nil.
   (options :initarg :options :initform nil)))

(defclass stp-package-operation (stp-operation)
  (allow-skip :initarg :allow-skip :initform nil))

(defclass stp-uninstall-operation (stp-package-operation) ())

(defvar stp-enforce-min-version nil
  "Determines if the user is allowed to select a version older than
the minimum required by another package.")

(defclass stp-additive-operation (stp-package-operation)
  ((min-version :initarg :min-version)
   (enforce-min-version :initarg :enforce-min-version :initform (symbol-value 'stp-enforce-min-version))))

(defclass stp-install-operation (stp-additive-operation) ())
(defclass stp-upgrade-operation (stp-additive-operation) ())
(defclass stp-reinstall-operation (stp-additive-operation) ())

;; User options can be toggled interactively by the user when a command is run.
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

(defclass stp-audit-task-options (stp-task-options)
  ((do-audit :initarg :do-audit :initform (symbol-value 'stp-audit-changes))))

(defclass stp-additive-task-options (stp-package-task-options stp-audit-task-options stp-action-task-options) ())

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

;; Handles queries that might need to be done interactively such as determining
;; which remotes, versions and so forth should be used during installation and
;; upgrades. This can be done either interactively or via some policy like
;; preferring the latest stable or unstable. It also handles callbacks to
;; higher-level code (such as requesting that dependencies be installed).
(defclass stp-controller ()
  ((options :initarg :options)
   (operations :initarg :tasks :initform nil)))

;; TODO: add code to callback to the controller to get versions and such.
(defclass stp-interactive-controller (stp-controller) ())

(defclass stp-auto-controller (stp-controller)
  ((preferred-update :initarg :preferred-update :initform 'stable)))

(cl-defmethod stp-append-operations ((controller stp-controller) &rest new-operations)
  (with-slots (operations)
      controller
    (setf operations (append operations new-operations))))

(cl-defmethod stp-skip-msg ((operation stp-package-operation))
  (format "Skipping an unknown package operation on %s" (slot-value operation 'pkg-name)))

(cl-defmethod stp-skip-msg ((operation stp-uninstall-operation))
  (format "Skipping uninstalling %s" (slot-value operation 'pkg-name)))

(cl-defmethod stp-skip-msg ((operation stp-install-operation))
  (format "Skipping installing %s" (slot-value operation 'pkg-name)))

(cl-defmethod stp-skip-msg ((operation stp-upgrade-operation))
  (format "Skipping upgrading %s" (slot-value operation 'pkg-name)))

(cl-defmethod stp-skip-msg ((operation stp-reinstalling-operation))
  (format "Skipping reinstalling %s" (slot-value operation 'pkg-name)))

(cl-defmethod stp-operate ((_operation stp-operation) (_controller stp-controller)))

(cl-defmethod stp-operate :around ((operation stp-package-operation) (controller stp-controller))
  (when (slot-value operation 'allow-skip)
    (stp-allow-skip (stp-msg (stp-skip-msg operation))
      (cl-call-next-method))))

;; TODO: fill in
(cl-defmethod stp-operate ((operation stp-uninstall-operation) (controller stp-controller)))
(cl-defmethod stp-operate ((operation stp-install-operation) (controller stp-controller)))
(cl-defmethod stp-operate ((operation stp-upgrade-operation) (controller stp-controller)))
(cl-defmethod stp-operate ((operation stp-reinstall-operation) (controller stp-controller)))

;; TODO: Combine features of `stp-ensure-requirements',
;; `stp-maybe-uninstall-requirements' and `stp-report-requirements'.
(cl-defmethod stp-execute ((controller stp-controller))
  (with-slots (options operations)
      controller
    (while operations
      ;; Execute operation
      )
    ;; Perform certain tasks that should only happen at the end (pushing,
    ;; locking and post actions)
    ))

(provide 'stp-controller)
;;; stp-controller.el ends here
