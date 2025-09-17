;;; -*- lexical-binding: t; -*-
(require 'eieio)
(require 'stp-utils)
(require 'transient)

;; User options can be toggled interactively by the user when a command is run.
(defclass stp-operation-options () ())

(defclass stp-basic-operation-options (stp-operation-options)
  ((do-commit :initarg :do-commit :initform (symbol-value 'stp-auto-commit))
   (do-push :initarg :do-push :initform (symbol-value 'stp-auto-push))))

(defclass stp-package-operation-options (stp-basic-operation-options)
  ((do-lock :initarg :do-lock :initform (symbol-value 'stp-auto-lock))))

(defclass stp-package-change-operation-options (stp-package-operation-options)
  ((do-reset :initarg :do-reset :initform (symbol-value 'stp-auto-reset))
   (do-dependencies :initarg :do-dependencies :initform (symbol-value 'stp-auto-dependencies))))

(defclass stp-uninstall-operation-options (stp-package-change-operation-options) ())

(defclass stp-action-operation-options (stp-operation-options)
  ((do-actions :initarg :do-actions :initform (symbol-value 'stp-auto-post-actions))
   (do-update-load-path :initarg :do-update-load-path :initform (symbol-value 'stp-auto-update-load-path))
   (do-load :initarg :do-load :initform (symbol-value 'stp-auto-load))
   (do-build :initarg :do-build :initform (symbol-value 'stp-auto-build))
   (do-build-info :initarg :do-build-info :initform (symbol-value 'stp-auto-build-info))
   (do-update-info-directories :initarg :do-update-info-directories :initform (symbol-value 'stp-auto-update-info-directories))))

(defclass stp-audit-operation-options (stp-operation-options)
  ((do-audit :initarg :do-audit :initform (symbol-value 'stp-audit-changes))))

(defclass stp-additive-operation-options (stp-package-change-operation-options stp-audit-operation-options stp-action-operation-options) ())

(defclass stp-install-operation-options (stp-additive-operation-options) ())
(defclass stp-upgrade-operation-options (stp-additive-operation-options) ())
(defclass stp-reinstall-operation-options (stp-additive-operation-options) ())

(defclass stp-bump-operation-options (stp-basic-operation-options)
  ((do-tag :initarg :do-tag :initform (symbol-value 'stp-auto-tag))))

(cl-defun stp-transient-slot-toggler (slot &key &allow-other-keys)
  `(lambda ()
     (interactive)
     (let* ((scope (transient-scope))
            (options (car scope)))
       (oset options ,slot (not (slot-value options ',slot)))
       (transient-setup transient-current-command nil nil :scope scope))))

(defun stp-option-slot-base-name (slot)
  (s-replace "-" " " (s-chop-prefix "do-" (symbol-name slot))))

(cl-defun stp-transient-slot-description (slot &key action &allow-other-keys)
  `(lambda ()
     (let* ((scope (transient-scope))
            (options (car scope))
            (value (slot-value options ',slot))
            (value-desc (cl-case value
                          ((nil)
                           "disabled")
                          ((t)
                           "enabled")
                          (t
                           (s-truncate 12 (prin1-to-string value)))))
            (base-name ,(stp-option-slot-base-name slot)))
       (if (and ,action (not (slot-value options 'do-actions)))
         (setq base-name (propertize base-name 'face 'transient-inactive-argument)
               value-desc (propertize value-desc 'face 'transient-inactive-value))
         (setq value-desc (propertize value-desc 'face 'transient-value)))
       (concat base-name " " value-desc))))

(cl-defun stp-transient-toggle-binding (slot &rest args &key (key (substring (stp-option-slot-base-name slot) 0 1)) &allow-other-keys)
  `(,key
    ,(apply #'stp-transient-slot-description slot args)
    ,(apply #'stp-transient-slot-toggler slot args)
    :if (lambda ()
          (let* ((scope (transient-scope))
                 (options (car scope)))
            (slot-exists-p options ',slot)))))

(defun stp-transient-toggle-bindings (slots &rest args)
  (mapcar (fn (apply #'stp-transient-toggle-binding
                     (append (if (consp %)
                                 %
                               (list %))
                             args)))
          slots))

(cl-macrolet
    ((make-transient ()
       `(transient-define-prefix stp-toggle-options-transient (options normal-exit)
          [["Options"
            ,@(stp-transient-toggle-bindings '(do-commit
                                               do-push
                                               do-lock
                                               do-reset
                                               do-dependencies
                                               (do-audit :key "A")
                                               do-tag))]
           [:description
            (lambda ()
              (let* ((scope (transient-scope))
                     (options (car scope)))
                (if (slot-exists-p options 'do-actions)
                    "Actions"
                  "")))
            ,(stp-transient-toggle-binding 'do-actions)
            ,@(stp-transient-toggle-bindings '(do-update-load-path
                                               (do-load :key "g")
                                               do-build
                                               (do-build-info :key "m")
                                               (do-update-info-directories :key "I"))
                                             :action t)]]
          ["Commands"
           ("x" "execute" (lambda ()
                            (interactive)
                            (setf (caadr (transient-scope)) t)))]
          (interactive (error "This transient should be called as a function rather than interactively"))
          (transient-setup 'stp-toggle-options-transient nil nil :scope (list options normal-exit)))))
  (make-transient))

(defun stp-toggle-options (options)
  (let ((options (stp-install-operation-options))
        (normal-exit (list nil)))
    (rem-call-transient-synchronously #'stp-toggle-options-transient options normal-exit)
    (if (car normal-exit)
        options
      (keyboard-quit))))

(provide 'stp-options)

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("db" . "cl-destructuring-bind")
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
;;; stp-options.el ends here
