;;; -*- lexical-binding: t; -*-

(require 'stp-headers)
(require 'stp-latest)
(require 'stp-utils)

;; TODO: use in code
(defclass stp-operation ()
  ((pkg-name :initarg :pkg-name :initform nil)
   ;; This overrides the controller's options slot in `stp-execute' when
   ;; non-nil.
   (options :initarg :options :initform nil)))

(defclass stp-package-operation (stp-operation) ())

(defclass stp-post-action-operation (stp-package-operation) ())

(defclass stp-skippable-package-operation (stp-package-operation)
  (allow-skip :initarg :allow-skip :initform nil))

(defclass stp-uninstall-operation (stp-skippable-package-operation) ())

(defvar stp-enforce-min-version nil
  "Determines if the user is allowed to select a version older than
the minimum required by another package.")

(defclass stp-additive-operation (stp-skippable-package-operation)
  ((min-version :initarg :min-version)
   (enforce-min-version :initarg :enforce-min-version :initform (symbol-value 'stp-enforce-min-version))
   (prompt-prefix :initarg :prompt-prefix :initform "")))

(defclass stp-install-operation (stp-additive-operation)
  ((dependency :initarg :dependency :initform nil)
   (pkg-alist :initarg :pkg-alist :initform nil)))

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

(cl-defgeneric stp-validate-options (options)
  (:documentation
   "Determine if the options passed are valid and signal an
appropriate error if they are not."))

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

(defvar stp-normalize-versions nil
  "Indicates if versions should be printed in a standardized format.

This overrides the specific format used for versions by the
project.")

(defun stp-abbreviate-remote-version (pkg-name method remote version)
  "Abbreviate long hashes to make them more readable.

Other versions are not abbreviated."
  (cond
   ((and (eq method 'git) (not (stp-git-valid-remote-ref-p remote version)))
    (stp-git-abbreviate-hash version))
   (stp-normalize-versions
    (stp-normalize-version pkg-name remote version))
   (t
    version)))

(defvar stp-audit-auto-reset t
  "Indicates if git reset should be used to undo changes to the
prior state after an audit fails.")

(defun stp-audit-changes (pkg-name type last-hash)
  (unless (memq type '(install upgrade))
    (error "type must be either 'install or 'upgrade"))
  (stp-git-show-diff (list last-hash))
  (unless (yes-or-no-p "Are the changes to the package safe? ")
    (when stp-audit-auto-reset
      (stp-git-reset last-hash :mode 'hard))
    (signal 'quit
            (list (format "aborted %s %s due to a failed security audit%s"
                          (if (eq type 'install)
                              "installing"
                            "upgrading")
                          pkg-name
                          (if stp-audit-auto-reset
                              ""
                            ": use git reset to undo the suspicious commits"))))))

(defun stp-maybe-audit-changes (pkg-name type last-hash do-audit)
  (when (stp-maybe-call do-audit pkg-name)
    (stp-audit-changes pkg-name type last-hash)))

(cl-defun stp-read-remote-or-archive (prompt &key pkg-name default-remote (prompt-prefix ""))
  "Read a package name and remote of any type or a package archive.

When the input is ambiguous and could be package name or a local
path, it will be treated as a package name unless it contains a
slash. Return a cons cell the contains the package name and the
remote or archive. Archives are represented as symbols."
  (stp-archive-ensure-loaded)
  (let* ((archive-names (if pkg-name
                            (ensure-list (cl-find pkg-name (stp-archive-package-names) :test #'string=))
                          (stp-archive-package-names)))
         (name-or-remote (stp-comp-read-remote prompt archive-names :default default-remote :normalize nil)))
    (if (member name-or-remote archive-names)
        (progn
          ;; If the user chose a package name, find remotes from
          ;; `package-archive-contents' and allow the user to choose one.
          (setq pkg-name name-or-remote)
          (let* ((archives (stp-archives pkg-name))
                 (archive-alist (mapcar (lambda (archive)
                                          (cons (format "%s (package archive)" archive)
                                                (intern archive)))
                                        archives))
                 (remotes (append (stp-archive-find-remotes pkg-name)
                                  (mapcar (fn (cons % 'elpa))
                                          (stp-elpa-package-urls pkg-name archives :annotate t))))
                 (remote-or-archive (stp-comp-read-remote
                                     "Remote or archive: "
                                     (->> (append remotes archive-alist)
                                          stp-sort-remotes
                                          (mapcar #'car))
                                     :default (car remotes))))
            (cons pkg-name (or (map-elt archive-alist remote-or-archive)
                               (car (s-split " " remote-or-archive))))))
      ;; Otherwise the user chose a remote so prompt for its package name.
      (let ((remote (stp-normalize-remote name-or-remote)))
        (cons (or pkg-name (stp-read-name (stp-prefix-prompt prompt-prefix "Package name: ") :default (stp-default-name remote)))
              remote)))))

(cl-defun stp-read-package (&key pkg-name pkg-alist (prompt-prefix "") min-version enforce-min-version)
  (plet* ((`(,pkg-name . ,remote) (stp-read-remote-or-archive (stp-prefix-prompt prompt-prefix "Package name or remote: ")
                                                              :pkg-name pkg-name
                                                              :default-remote (map-elt pkg-alist 'remote)))
          (method (stp-remote-method remote)))
    (let (version update branch)
      (cl-ecase method
        (git
         (unless (stp-git-valid-remote-p remote)
           (user-error (stp-prefix-prompt prompt-prefix "Invalid git repository (or host is down): %s") remote))
         (unless update
           (setq update (stp-git-read-update (stp-prefix-prompt prompt-prefix "Update policy: ")
                                             :default (map-elt pkg-alist 'update)
                                             :remote remote
                                             :other-remotes (map-elt pkg-alist 'other-remotes))))
         (when (and (eq update 'unstable)
                    (not branch))
           (setq branch (stp-git-read-branch (stp-prefix-prompt prompt-prefix "Branch: ") remote (map-elt pkg-alist 'branch))))
         (unless version
           (setq version (stp-git-read-version
                          (stp-prefix-prompt prompt-prefix (format "Version%s: " (stp-min-version-annotation min-version enforce-min-version)))
                          remote
                          :extra-versions (list (map-elt pkg-alist 'version) branch)
                          :default (map-elt pkg-alist 'version)
                          :min-version (and enforce-min-version min-version))))
         `(,pkg-name
           (method . ,method)
           (remote . ,remote)
           (version . ,version)
           (update . ,update)
           (branch . ,branch)))
        ;; Archives only have one version so the minimum version cannot be
        ;; enforced.
        (archive
         `(,pkg-name
           (method . ,method)
           (remote . ,remote)))
        ((elpa url)
         (unless (or (and (string-match-p rem-strict-url-regexp remote)
                          (url-file-exists-p remote))
                     ;; Allow local files too.
                     (f-exists-p remote))
           (user-error (stp-prefix-prompt prompt-prefix "Invalid URL (or host is down): %s") remote))
         (unless version
           (cl-ecase method
             (elpa (setq version (stp-elpa-read-version
                                  (stp-prefix-prompt prompt-prefix "Version: ")
                                  pkg-name
                                  remote

                                  :min-version (and enforce-min-version min-version))))
             (url (setq version (stp-url-read-version (stp-prefix-prompt prompt-prefix "Version: "))))))
         `(,pkg-name
           (method . ,method)
           (remote . ,remote)
           (version . ,version)))))))

;; Handles queries that might need to be done interactively such as determining
;; which remotes, versions and so forth should be used during installation and
;; upgrades. This can be done either interactively or via some policy like
;; preferring the latest stable or unstable. It also handles callbacks to
;; higher-level code (such as requesting that dependencies be installed).
(defclass stp-controller ()
  ((errors :initform nil)
   (options :initarg :options)
   (operations :initarg :tasks :initform nil)))

;; TODO: add code to callback to the controller to get versions and such.
(defclass stp-interactive-controller (stp-controller) ())

(defclass stp-auto-controller (stp-controller)
  ((preferred-update :initarg :preferred-update :initform 'stable)))

(cl-defgeneric stp-controller-append-errors (controller &rest errors)
  (:documentation
   "Append the specified error messages (strings) to the controller's
list of error messages. These will be reported to the user after
all operations are completed."))

(cl-defmethod stp-controller-append-errors ((controller stp-controller) &rest new-errors)
  (with-slots (errors)
      controller
    (setq errors (append errors new-errors))))

(cl-defgeneric stp-controller-append-operations (controller &rest operations)
  (:documentation
   "Append the specified operations to the controller's list of
operations to perform."))

(cl-defmethod stp-controller-append-operations ((controller stp-controller) &rest new-operations)
  (with-slots (operations)
      controller
    (setf operations (append operations new-operations))))

(cl-defgeneric stp-controller-prepend-operations (controller &rest operations)
  (:documentation
   "Prepend the specified operations to the controller's list of
operations to perform."))

(cl-defmethod stp-controller-prepend-operations ((controller stp-controller) &rest new-operations)
  (with-slots (operations)
      controller
    (setf operations (append new-operations operations))))

(cl-defgeneric stp-controller-get-package (controller &key pkg-name prompt-prefix min-version enforce-min-version)
  (:documentation
   "Query the controller for a package."))

(cl-defmethod stp-controller-get-package ((_controller stp-interactive-controller) &key pkg-name (prompt-prefix "") min-version enforce-min-version)
  (stp-read-package :pkg-name pkg-name
                    :prompt-prefix prompt-prefix
                    :min-version min-version
                    :enforce-min-version enforce-min-version))

(cl-defgeneric stp-skip-msg (operation)
  (:documentation
   "Return a message that describes skipping OPERATION."))

(cl-defmethod stp-skip-msg ((operation stp-package-operation))
  (format "Skipping an unknown package operation on %s" (slot-value operation 'pkg-name)))

(cl-defmethod stp-skip-msg ((operation stp-uninstall-operation))
  (format "Skipping uninstalling %s" (slot-value operation 'pkg-name)))

(cl-defmethod stp-skip-msg ((operation stp-install-operation))
  (format "Skipping installing %s" (slot-value operation 'pkg-name)))

(cl-defmethod stp-skip-msg ((operation stp-upgrade-operation))
  (format "Skipping upgrading %s" (slot-value operation 'pkg-name)))

(cl-defmethod stp-skip-msg ((operation stp-reinstall-operation))
  (format "Skipping reinstalling %s" (slot-value operation 'pkg-name)))

(cl-defgeneric stp-operate (operation controller)
  (:documentation
   "Perform OPERATION using CONTROLLER. This may result in additional
operations being added to the controller."))

(cl-defmethod stp-operate ((_operation stp-operation) (_controller stp-controller)))

(cl-defmethod stp-operate :around ((operation stp-skippable-package-operation) (_controller stp-controller))
  ;; Sometimes, a single repository can contain multiple packages and so
  ;; installing the dependencies naively will result in multiple copies.
  (when (slot-value operation 'allow-skip)
    (stp-allow-skip (stp-msg (stp-skip-msg operation))
      (cl-call-next-method))))

(defun stp-options (operation controller)
  (or (slot-value operation 'options) (slot-value controller 'options)))

(cl-defgeneric stp-uninstall-requirements2 (controller requirements options)
  (:documentation
   "Uninstall those REQUIREMENTS that are no longer needed by any
package and were installed as dependencies."))

(cl-defmethod stp-uninstall-requirements2 ((controller stp-controller) requirements options)
  (let* ((to-uninstall (stp-requirements-to-names requirements))
         (old-to-uninstall t)
         pkg-name)
    (while to-uninstall
      (when (equal to-uninstall old-to-uninstall)
        (error "Cyclic dependencies encountered while uninstalling packages"))
      (setq old-to-uninstall (cl-copy-list to-uninstall)
            pkg-name (stp-symbol-package-name (pop to-uninstall)))
      ;; Only queue packages for uninstalling when they were installed as
      ;; dependencies and are no longer required by any package.
      (when (and (member pkg-name (stp-info-names))
                 (stp-get-attribute pkg-name 'dependency)
                 (not (stp-required-by pkg-name)))
        (stp-controller-prepend-operations controller (stp-uninstall-operation :pkg-name pkg-name :options options))))))

(cl-defmethod stp-operate ((operation stp-uninstall-operation) (controller stp-controller))
  (let ((options (stp-options operation controller)))
    (with-slots (do-commit)
        options
      (with-slots (pkg-name)
          operation
        (let ((features (stp-headers-directory-features (stp-full-path pkg-name)))
              (requirements (stp-get-attribute pkg-name 'requirements)))
          (let-alist (stp-get-alist pkg-name)
            (if (eql (car (rem-call-process-shell-command (format "git rm -r '%s'" pkg-name))) 0)
                (progn
                  (f-delete pkg-name t)
                  (stp-delete-alist pkg-name)
                  (stp-write-info)
                  (cl-dolist (feature features)
                    (push feature stp-headers-uninstalled-features))
                  (stp-delete-load-path pkg-name)
                  (stp-git-commit (format "Uninstalled version %s of %s"
                                          (stp-abbreviate-remote-version pkg-name .method .remote .version)
                                          pkg-name)
                                  :do-commit do-commit)
                  (stp-uninstall-requirements2 controller requirements options)
                  (stp-prune-cached-latest-versions pkg-name))
              (error "Failed to remove %s. This can happen when there are uncommitted changes in the git repository" pkg-name))))))))

(cl-defgeneric stp-ensure-requirements2 (controller requirements options)
  (:documentation
   "Install or upgrade the REQUIREMENTS that are not currently satisfied."))

(cl-defmethod stp-ensure-requirements2 ((controller stp-controller) requirements options)
  (stp-msg "Analyzing the load path for installed packages...")
  (stp-headers-update-features)
  (let (operations)
    (cl-dolist (requirement requirements)
      ;; Also allow a list of package names.
      (db (pkg-sym &optional version)
          (ensure-list requirement)
        (let* ((pkg-name (stp-symbol-package-name pkg-sym))
               (prefix (format "[%s] " pkg-name)))
          (unless (member pkg-name stp-headers-ignored-requirements)
            (push requirement stp-requirements))
          (cond
           ((string= pkg-name "emacs")
            (unless (stp-emacs-requirement-satisfied-p pkg-name version)
              (->> (format "Version %s of Emacs is required but %d.%d is installed"
                           version
                           emacs-major-version
                           emacs-minor-version)
                   (stp-controller-append-errors controller))))
           ;; Do nothing when a requirement is ignored or a new enough
           ;; version is installed.
           ((stp-package-requirement-satisfied-p pkg-name version t))
           ((not (member pkg-name (stp-info-names)))
            (push (stp-install-operation :pkg-name pkg-name
                                         :options options
                                         :prompt-prefix prefix
                                         :min-version version
                                         :dependency t)
                  operations))
           (t
            ;; The dependency attribute is left as is when upgrading because
            ;; the package might have been installed manually originally.
            (push (stp-upgrade-operation :pkg-name pkg-name
                                         :options options
                                         :prompt-prefix prefix
                                         :min-version version)
                  operations)))))
      (stp-headers-update-features))
    (stp-controller-prepend-operations controller (reverse operations))))

(cl-defmethod stp-operate ((operation stp-install-operation) (controller stp-controller))
  (let ((options (stp-options operation controller)))
    (with-slots (do-commit do-audit do-actions)
        options
      (with-slots (pkg-name min-version enforce-min-version prompt-prefix dependency)
          operation
        (let* ((pkg-alist (or (slot-value operation 'pkg-alist)
                              (stp-controller-get-package controller
                                                          :pkg-name pkg-name
                                                          :prompt-prefix prompt-prefix
                                                          :min-version min-version
                                                          :enforce-min-version enforce-min-version)))
               (last-hash (stp-git-head)))
          (let-alist pkg-alist
            ;; Guess the method if it isn't already known.
            (unless .method
              (setq .method (stp-remote-method .remote))
              (stp-set-attribute pkg-name 'method .method))
            (when (stp-url-safe-remote-p .remote)
              (cl-ecase .method
                (git (stp-git-install pkg-name .remote .version .update :branch .branch))
                (elpa (stp-elpa-install pkg-name .remote .version))
                (archive (stp-archive-install pkg-name .remote))
                (url (stp-url-install pkg-name .remote .version)))
              (stp-maybe-audit-changes pkg-name 'install last-hash do-audit)
              (stp-update-remotes pkg-name .remote .remote .other-remotes)
              (stp-update-requirements pkg-name)
              (when dependency
                (stp-set-attribute pkg-name 'dependency t))
              (stp-write-info)
              ;; For archives, the version is determined automatically instead of
              ;; being read and so .version will be nil here.
              (setq .version (stp-get-attribute pkg-name 'version))
              (stp-git-commit (format "Installed version %s of %s"
                                      (stp-abbreviate-remote-version pkg-name .method .remote .version)
                                      pkg-name)
                              :do-commit do-commit)
              (stp-ensure-requirements2 controller (stp-get-attribute pkg-name 'requirements) options)
              ;; Perform post actions for all packages after everything else.
              (when (stp-maybe-call do-actions)
                (stp-controller-append-operations controller (stp-post-action-operation :pkg-name pkg-name))))))))))

;; TODO
(cl-defmethod stp-operate ((operation stp-upgrade-operation) (controller stp-controller)))
(cl-defmethod stp-operate ((operation stp-reinstall-operation) (controller stp-controller)))

;; TODO: Combine features of `stp-ensure-requirements',
;; `stp-maybe-uninstall-requirements' and `stp-report-requirements'.
(cl-defmethod stp-execute ((controller stp-controller))
  (with-slots (options operations)
      controller
    (while operations
      ;; Execute operation
      ;; Keep track
      )
    ;; Report any errors that occurred
    ;; Perform certain tasks that should only happen at the end (pushing,
    ;; and locking)
    ))

(provide 'stp-controller)
;;; stp-controller.el ends here
