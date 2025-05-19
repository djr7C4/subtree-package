;;; -*- lexical-binding: t; -*-

(defvar stp-lock-file (expand-file-name user-emacs-directory "stp-pkg-lock.eld"))

(defun stp-checkout-locked-revision (&optional quiet)
  ;; Avoid using `cl-block'.
  (catch 'ret
    (if (file-readable-p stp-lock-file)
        (let* ((default-directory djr-emacs-packages-repository)
               ;; Get the hash in the lock file.
               (hash (with-temp-buffer
                       (insert-file-contents stp-lock-file)
                       (read (current-buffer))))
               (cmd (format "git name-rev --name-only --no-undefined %s" hash))
               ;; Convert the hash to a tag or branch if possible. This prevents
               ;; HEAD from being detached in `djr-emacs-packages-repository'
               ;; which would cause issues later when committing to it.
               (data (with-temp-buffer
                       (list (call-process-shell-command cmd nil t)
                             (buffer-string))))
               ;; Avoid using `cl-destructuring-bind'.
               (exit-code (car data))
               (output (cadr data)))
          (unless (= exit-code 0)
            (warn "The locked version of %s could not be restored: \"%s\" failed with exit code %d: %s" djr-emacs-packages-repository cmd exit-code output)
            (throw 'ret nil))
          (let* ((rev (string-trim output))
                 (rev-string (if (string= rev hash)
                                 rev
                               (format "%s (%s)" hash rev)))
                 (cmd (format "git checkout %s" rev))
                 (data (with-temp-buffer
                         (list (call-process-shell-command cmd nil t)
                               (buffer-string))))
                 (exit-code (car data))
                 (output (cadr data)))
            (unless (= exit-code 0)
              (warn "The locked version of %s could not be restored: \"%s\" failed with exit code %d: %s" djr-emacs-packages-repository cmd exit-code output)
              (throw 'ret nil))
            (unless quiet
              (message "Checked out the locked revision %s of %s" rev-string djr-emacs-packages-repository))))
      (warn "%s is missing: unable to restore the locked version of %s" stp-lock-file djr-emacs-packages-repository))))

(defun stp-lock-file-watcher (event)
  (db (_descriptor action _file &optional _file2)
      event
    (when (memq action '(created changed))
      (stp-checkout-locked-revision t))))

(provide 'stp-locked)
;;; stp-locked.el ends here
