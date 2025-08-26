;;; -*- lexical-binding: t; -*-

(require 'eieio)

;; TODO: fill in
(defclass stp-task () ())

;; TODO: fill in
;; Determines which remotes, versions and so forth should be used during
;; installation and upgrades. This can be done either interactively or via some
;; policy like preferring the latest stable or unstable.
(defclass stp-controller () ())


(defclass stp-task-options ()
  ((do-commit :initarg :do-commit)
   (do-push :initarg :do-push)
   (do-lock :initarg :do-lock)
   (do-actions :initarg :do-actions)
   (do-audit :initarg :do-audit)
   ;; TODO: add options to control specific posts actions such as updating the
   ;; load path (see `stp-post-actions').
   ))

(provide 'stp-executor)
;;; stp-executor.el ends here
