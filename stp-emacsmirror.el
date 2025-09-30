;;; -*- lexical-binding: t; -*-

(require 'async)
(require 'cl-lib)
(require 'f)
(require 's)
(require 'stp-utils)

(defvar stp-emacsmirror-async-refresh-running nil)

(defvar stp-emacsmirror-last-refreshed most-negative-fixnum)

(defvar stp-emacsmirror-refresh-interval (timer-duration "1 day"))

(defvar stp-emacsmirror-directory (f-join user-emacs-directory "stp/emacsmirrors/"))

(defvar stp-emacsmirrors '("emacsmirror" "emacsorphanage" "emacsattic")
  "A list of GitHub organizations that contain repositories for
Emacs packages.")

(defvar stp-emacsmirror-alist nil)

(cl-defun stp-emacsmirror-async-refresh (&key force quiet)
  "Refresh the lists of URLs of Github repositories from
`stp-emacsmirrors'."
  (interactive (list :force current-prefix-arg))
  (when stp-emacsmirror-async-refresh-running
    (user-error "`stp-emacsmirror-async-refresh' is already running"))
  ;; Only refresh when it has been at least `stp-emacsmirror-refresh-interval'
  ;; seconds since the last refresh.
  (if (or force
          (> (- (float-time) stp-emacsmirror-last-refreshed)
             stp-emacsmirror-refresh-interval))
      (progn
        (setq stp-emacsmirror-async-refresh-running t
              stp-emacsmirror-last-refreshed (float-time))
        (unless quiet
          (stp-msg "Refreshing Emacs mirrors asynchronously"))
        (unless (executable-find "gh")
          (error "gh is required for refreshing Emacs mirrors"))
        (f-mkdir-full-path stp-emacsmirror-directory)
        (async-start
         `(lambda ()
            ,(async-inject-variables "^\\(stp-emacsmirror-directory\\|stp-emacsmirrors\\)$")
            (let ((default-directory stp-emacsmirror-directory))
              (dolist (mirror stp-emacsmirrors)
                (with-temp-buffer
                  (call-process "gh" nil t nil "repo" "list" "-L" "1000000" mirror)
                  ;; Keep only the first column of each row and delete the
                  ;; mirror prefix (e.g. change emacsorphanage/god-mode to
                  ;; god-mode).
                  (save-match-data
                    (goto-char (point-min))
                    (while (not (eobp))
                      (when (looking-at (format "%s/" mirror))
                        (replace-match ""))
                      (let ((end (line-end-position)))
                        (cond
                         ;; Ignore this internal emacsmirror repository.
                         ((looking-at-p "\\.github")
                          (delete-region (point) end))
                         ((re-search-forward "[ \t]" end t)
                          (goto-char (match-beginning 0))
                          (delete-region (point) end)))
                        (ignore-errors (forward-line)))))
                  (let ((backup-inhibited t))
                    (write-file mirror))))))
         (lambda (_result)
           (stp-emacsmirror-read-all)
           (setq stp-emacsmirror-async-refresh-running nil)
           (unless quiet
             (stp-msg "Asynchronous refresh of the Emacs mirrors finished")))))
    (unless quiet
      (stp-msg "The Emacs mirrors were last refreshed on %s: no refresh is necessary"
               (format-time-string "%c" stp-emacsmirror-last-refreshed)))))

(defun stp-emacsmirror-read-all ()
  (setq stp-emacsmirror-alist
        (mapcar (lambda (mirror)
                  (let ((target (f-join stp-emacsmirror-directory mirror)))
                    (cons mirror (when (f-exists-p target)
                                   (s-split "\n" (f-read target) t)))))
                stp-emacsmirrors)))

(defun stp-emacsmirror-ensure-loaded ()
  (unless stp-emacsmirror-alist
    (when (f-dir-p stp-emacsmirror-directory)
      (stp-emacsmirror-read-all))))

(defun stp-emacsmirror-package-names ()
  (->> stp-emacsmirror-alist
       (mapcar #'cdr)
       (apply #'append)
       -uniq
       (-sort #'string<)))

(defun stp-emacsmirror-package-repositories ()
  (->> stp-emacsmirror-alist
       (mapcar (lambda (cell)
                 (dsb (mirror . pkg-names)
                     cell
                   (mapcar (lambda (pkg-name)
                             (cons pkg-name (format "https://github.com/%s/%s/" mirror pkg-name)))
                           pkg-names))))
       (apply #'append)
       -uniq
       (-sort (fn (string< (car %1) (car %2))))))

(defun stp-emacsmirror-find-remotes (pkg-name)
  "Find remotes for PKG-NAME in `stp-emacsmirror-alist'. The result
is an alist that maps remotes to methods."
  (->> (stp-emacsmirror-package-repositories)
       (-filter (fn (string= pkg-name (car %))))
       (mapcar (fn (cons (cdr %) 'git)))))

(defun stp-emacsmirror-remote-mirror (remote)
  "Return the mirror that matches REMOTE."
  (save-match-data
    (when (string-match (format "^https://github.com/\\(%s\\)/"
                                (s-join "\\|" stp-emacsmirrors))
                        remote)
      (match-string 1 remote))))

(provide 'stp-emacsmirror)

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
;;; stp-emacsmirror.el ends here
