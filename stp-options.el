;;; -*- lexical-binding: t; -*-
(require 'eieio)

;; User options can be toggled interactively by the user when a command is run.
(defclass stp-operation-options () ())

(defclass stp-basic-operation-options (stp-operation-options)
  ((do-commit :initarg :do-commit :initform (symbol-value 'stp-auto-commit))
   (do-push :initarg :do-push :initform (symbol-value 'stp-auto-push))))

(defclass stp-package-operation-options (stp-basic-operation-options)
  ((do-lock :initarg :do-lock :initform (symbol-value 'stp-auto-lock))
   (do-reset :initarg :do-reset :initform (symbol-value 'stp-auto-reset))
   (do-dependencies :initarg :do-dependencies :initform (symbol-value 'stp-auto-dependencies))))

(defclass stp-uninstall-operation-options (stp-package-operation-options) ())

(defclass stp-action-operation-options (stp-operation-options)
  ((do-actions :initarg :do-actions :initform (symbol-value 'stp-auto-post-actions))
   (do-update-load-path :initarg :do-update-load-path :initform (symbol-value 'stp-auto-update-load-path))
   (do-load :initarg :do-load :initform (symbol-value 'stp-auto-load))
   (do-build :initarg :do-build :initform (symbol-value 'stp-auto-build))
   (do-build-info :initarg :do-build-info :initform (symbol-value 'stp-auto-build-info))
   (do-update-info-directories :initarg :do-update-info-directories :initform (symbol-value 'stp-auto-update-info-directories))))

(defclass stp-audit-operation-options (stp-operation-options)
  ((do-audit :initarg :do-audit :initform (symbol-value 'stp-audit-changes))))

(defclass stp-additive-operation-options (stp-package-operation-options stp-audit-operation-options stp-action-operation-options) ())

(defclass stp-install-operation-options (stp-additive-operation-options) ())
(defclass stp-upgrade-operation-options (stp-additive-operation-options) ())
(defclass stp-reinstall-operation-options (stp-additive-operation-options) ())

(defclass stp-bump-operation-options (stp-basic-operation-options)
  ((do-tag :initarg :do-tag :initform (symbol-value 'stp-auto-tag))))

(provide 'stp-options)
;;; stp-options.el ends here
