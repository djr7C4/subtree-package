;;; -*- lexical-binding: t; -*-

(defvar stp-source-directory (expand-file-name "package-source/" user-emacs-directory)
  "The directory in which STP packages installed.")

(defvar stp-lock-file (expand-file-name "stp-pkg-lock.eld" user-emacs-directory))

(defun stp-checkout-locked-revision (&optional quiet)
  ;; Avoid using `cl-block'.
  (catch 'ret
    (let* ((default-directory stp-source-directory)
           ;; Get the hash in the lock file.
           (hash (with-temp-buffer
                   (insert-file-contents stp-lock-file)
                   (read (current-buffer))))
           (git-path (executable-find "git"))
           (cmd (format "git rev-parse HEAD"))
           (data (with-temp-buffer
                   (list (apply #'call-process git-path nil t nil (cdr (string-split cmd)))
                         (buffer-string))))
           (exit-code (car data))
           (output (cadr data))
           (head-hash (string-trim output)))
      (unless (= exit-code 0)
        (warn "The locked version of %s could not be restored: \"%s\" failed with exit code %d: %s" stp-source-directory cmd exit-code output)
        (throw 'ret nil))
      ;; If the locked version is already checked out then we don't need to do
      ;; anything.
      (unless (string= head-hash hash)
        (if (file-readable-p stp-lock-file)
            (let* ((cmd (format "git name-rev --name-only --no-undefined %s" hash))
                   ;; Convert the hash to a tag or branch if possible. This
                   ;; prevents HEAD from being detached in
                   ;; `stp-source-directory' which would cause issues later when
                   ;; committing to it.
                   (data (with-temp-buffer
                           (list (apply #'call-process git-path nil t nil (cdr (string-split cmd)))
                                 (buffer-string))))
                   ;; Avoid using `cl-destructuring-bind'.
                   (exit-code (car data))
                   (output (cadr data))
                   (rev (string-trim output)))
              (unless (= exit-code 0)
                (warn "The locked version of %s could not be restored: \"%s\" failed with exit code %d: %s" stp-source-directory cmd exit-code output)
                (throw 'ret nil))
              (let* ((rev-string (if (string= rev hash)
                                     rev
                                   (format "%s (%s)" hash rev)))
                     (cmd (format "git checkout %s" rev))
                     (data (with-temp-buffer
                             (list (apply #'call-process git-path nil t nil (cdr (string-split cmd)))
                                   (buffer-string))))
                     (exit-code (car data))
                     (output (cadr data)))
                (unless (= exit-code 0)
                  (warn "The locked version of %s could not be restored: \"%s\" failed with exit code %d: %s" stp-source-directory cmd exit-code output)
                  (throw 'ret nil))
                (unless quiet
                  (stp-msg "Checked out the locked revision %s of %s" rev-string stp-source-directory))))
          (warn "%s is missing: unable to restore the locked version of %s" stp-lock-file stp-source-directory))))))

(provide 'stp-locked)

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
;;; stp-locked.el ends here
