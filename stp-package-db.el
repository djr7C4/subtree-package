;;; -*- lexical-binding: t; -*-

(require 'eieio)

(defclass stp-package-group ()
  (package-names :initarg package-names))

(defclass stp-package-db ()
  (groups :initarg groups)
  (packages :initarg packages))

;; TODO: use these class from other code instead of the current alist structure.

(provide 'stp-package-db)
;;; stp-package-db.el ends here
