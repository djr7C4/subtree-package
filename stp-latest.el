;;; stp-latest.el --- Compute the latest versions of packages -*- lexical-binding: t; -*-

(require 'queue nil t)
(require 'stp-git)
(require 'stp-elpa)
(require 'stp-archive)
(require 'stp-utils)

(defvar stp-latest-versions-stale-interval (timer-duration "1 day")
  "The number of seconds until the cached latest versions are stale.")

(defvar stp-latest-versions-cache nil)

(defvar stp-latest-version-async t
  "This indicates if latest versions should be computed asynchronously.")

(defun stp-prune-cached-latest-versions (&optional pkg-name)
  (let ((pkg-names (stp-info-names)))
    (if pkg-name
        (setq stp-latest-versions-cache (map-delete stp-latest-versions-cache pkg-name))
      (setq stp-latest-versions-cache (map-filter (lambda (pkg-name _pkg-alist)
                                                    (member pkg-name pkg-names))
                                                  stp-latest-versions-cache)))))

(defun stp-latest-version (pkg-name &optional pkg-alist)
  (setq pkg-alist (or pkg-alist (stp-get-alist pkg-name)))
  (let-alist pkg-alist
    (let ((remotes (cons .remote .other-remotes))
          (timestamp (float-time)))
      (cl-ecase .method
        (git
         ;; Use `cl-find' to make sure that the branch exists on the remote and
         ;; otherwise default to HEAD. This can be an issue when there is a
         ;; local fork of an upstream repository that the packages was upgraded
         ;; to (for example with a new local branch for a pull request).
         (let* ((branch (or (cl-find .branch
                                     (stp-git-remote-heads .remote)
                                     :test #'string=)
                            "HEAD"))
                ;; Only use the remote attribute to determining the latest versions.
                (latest-stable (stp-git-latest-stable-version .remote))
                (latest-unstable (->> branch
                                      (stp-git-latest-unstable-version .remote)
                                      (stp-git-remote-rev-to-tag .remote)))
                ;; Use both the remote attribute and any other remotes to count
                ;; commits. This is important since the installed version might
                ;; be from any remote.
                (commits-to-stable (and latest-stable
                                        (stp-git-count-remote-commits remotes .version latest-stable)))
                (commits-to-unstable (and latest-unstable
                                          (stp-git-count-remote-commits remotes .version latest-unstable)))
                (version-timestamp (and .version (stp-git-remote-timestamp remotes .version)))
                (stable-timestamp (and latest-stable (stp-git-remote-timestamp remotes latest-stable)))
                (unstable-timestamp (and latest-unstable (stp-git-remote-timestamp remotes latest-unstable))))
           (when (or latest-stable latest-unstable commits-to-stable commits-to-unstable)
             (append (list pkg-name)
                     (and latest-stable (list `(latest-stable . ,latest-stable)))
                     (and latest-unstable (list `(latest-unstable . ,latest-unstable)))
                     (and commits-to-stable (list `(count-to-stable . ,commits-to-stable)))
                     (and commits-to-unstable (list `(count-to-unstable . ,commits-to-unstable)))
                     (and version-timestamp (list `(version-timestamp . ,version-timestamp)))
                     (and stable-timestamp (list `(stable-timestamp . ,stable-timestamp)))
                     (and unstable-timestamp (list `(unstable-timestamp . ,unstable-timestamp)))
                     (list `(updated . ,timestamp))))))
        (elpa
         (let* ((latest-stable (stp-elpa-latest-version pkg-name .remote))
                (versions-to-stable (and latest-stable (stp-elpa-count-versions pkg-name .remote .version latest-stable))))
           (unless latest-stable
             (error "Failed to get the latest stable version for %s" pkg-name))
           ;; Occasionally, it is possible we may run into a package when
           ;; versions-to-stable is nil because the current version is invalid and
           ;; does not appear in the list of versions on ELPA.
           (append `(,pkg-name
                     (latest-stable . ,latest-stable))
                   (and versions-to-stable (list `(count-to-stable . ,versions-to-stable)))
                   (list `(updated . ,timestamp)))))
        (archive
         (let ((latest-stable (stp-archive-latest-stable-version pkg-name .remote))
               (latest-unstable (stp-archive-latest-unstable-version pkg-name .remote)))
           (append (list pkg-name)
                   (and latest-stable (list `(latest-stable . ,latest-stable)))
                   (and latest-unstable (list `(latest-unstable . ,latest-unstable)))
                   (list `(updated . ,timestamp)))))
        (url
         nil)))))

(defvar stp-latest-num-processes 16
  "The number of processes for computing the latest versions.

This only has an effect when the latest versions are computed
asynchronously. See `stp-latest-version-async'.")

(defvar stp-latest-retries 3
  "Retry computing latest versions up to this many times.")

(cl-defun stp-latest-versions (package-callback final-callback pkg-names &key quiet async (num-processes stp-latest-num-processes) (max-tries stp-latest-retries))
  "Compute the latest versions for the packages in PACKAGES.

Once the latest version becomes available for package, call
PACKAGE-CALLBACK with the latest version alist as the argument.
Once all latest versions are available, call FINAL-CALLBACK with
the alist mapping the names of the packages to their latest
version alists. he latest versions are computed asynchronously
using NUM-PROCESSES simultaneously. In case an error occurs while
computing the latest version for a package, it will be retried up
to TRIES times."
  (let (latest-versions
        (queue (make-queue))
        (running 0))
    (cl-dolist (pkg-name pkg-names)
      (queue-enqueue queue (list pkg-name 0)))
    (cl-labels
        ((process-latest-version (data)
           (cl-decf running)
           ;; Process the result of the last call to `stp-latest-version' and
           ;; put the package information back into the queue if there was an
           ;; error.
           (when data
             (dsb (pkg-name tries latest-version-data error-message)
                 data
               (cond
                (latest-version-data
                 (push latest-version-data latest-versions)
                 (when package-callback
                   (funcall package-callback latest-version-data)))
                (error-message
                 (if (>= tries max-tries)
                     (unless quiet
                       (stp-msg "Getting the latest version of %s failed %d times: skipping..." pkg-name tries))
                   (cl-incf tries)
                   (unless quiet
                     (stp-msg "Getting the latest version of %s failed (%d/%d): %s" pkg-name tries max-tries error-message))
                   (queue-enqueue queue (list pkg-name tries)))))))
           (compute-next-latest-version))
         (compute-next-latest-version ()
           ;; If there are more packages to process in the queue, start fetching
           ;; the latest version for pkg-name. This is done asynchronously if
           ;; async is non-nil.
           (if (queue-empty queue)
               (when (and final-callback (= running 0))
                 (funcall final-callback latest-versions))
             (dsb (pkg-name tries)
                 (queue-dequeue queue)
               (cl-incf running)
               (if async
                   ;; Binding `async-prompt-for-password' to nil avoids a bug on
                   ;; certain packages (in particular password-store).
                   (let ((async-prompt-for-password nil))
                     (async-start `(lambda ()
                                     ;; Inject the STP variables and the
                                     ;; caller's load path into the asynchronous
                                     ;; process. Some large variables are
                                     ;; excluded since that slows down the
                                     ;; parent process quite a bit.
                                     ,(async-inject-variables "^stp-" nil (concat stp-async-inject-variables-exclude-regexp stp-async-inject-large-variables-exclude-regexp))
                                     (setq load-path ',load-path)
                                     (require 'stp)
                                     ;; pkg-alist is read from disk every time
                                     ;; rather than stored in case some other
                                     ;; STP command (such as an upgrade has
                                     ;; modified the package information while
                                     ;; the latest versions were being updated).
                                     (stp-with-memoization
                                       (stp-refresh-info)
                                       (let (latest-version-data
                                             (pkg-alist (stp-get-alist ,pkg-name)))
                                         (condition-case err
                                             (setq latest-version-data (stp-latest-version ,pkg-name pkg-alist))
                                           (error
                                            (list ,pkg-name ,tries nil (error-message-string err)))
                                           (:success
                                            (list ,pkg-name ,tries latest-version-data nil))))))
                                  #'process-latest-version))
                 (let (latest-version-data
                       (pkg-alist (stp-get-alist pkg-name)))
                   (process-latest-version
                    (condition-case err
                        (stp-with-memoization
                          (setq latest-version-data (stp-latest-version pkg-name pkg-alist)))
                      (error
                       (list pkg-name tries nil (error-message-string err)))
                      (:success
                       (list pkg-name tries latest-version-data nil))))))))))
      (dotimes (_ (if async (min num-processes (length pkg-names)) 1))
        (unless (queue-empty queue)
          (compute-next-latest-version))))))

(provide 'stp-latest)

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("dsb" . "cl-destructuring-bind")
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
;;; stp-latest.el ends here
