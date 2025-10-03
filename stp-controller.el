;;; stp-controller.el --- Controller for package operations -*- lexical-binding: t; -*-

;;; Code:

(require 'info)
(require 'stp-headers)
(require 'stp-latest)
(require 'stp-options)
(require 'stp-utils)
(require 'stp-git)
(require 'stp-elpa)
(require 'stp-archive)
(require 'stp-emacsmirror)
(require 'stp-url)

(defvar stp-auto-commit t
  "When non-nil, automatically commit changes.

Note that even if this is omitted, some operations (such as
subtree operations) inherently involve commits and this cannot be
disabled. When this variable is a function it will be called with
the name of the current package to determine the value when it is
needed. If there is no current package, no arguments will be
passed.")

(defvar stp-auto-push t
  "When non-nil, automatically push commits.

This has no effect unless `stp-auto-commit' is non-nil. The value
can also be a function as for `stp-auto-commit'.")

(defvar stp-auto-lock nil
  "When non-nil, automatically update `stp-lock-file' when packages are changed.

The value can also be a function as for `stp-auto-commit'.")

(defvar stp-audit-changes nil
  "Show diffs whenever a package changes or new code is added.

This is useful for security purposes. The value can also be a
function as for `stp-auto-commit'.")

(defvar stp-auto-reset '(:audit)
  "A list that indicates when git reset should be used.

If the list contains :audit, reset when an audit fails. If it
contains :error, reset when errors occur. t means to always reset
and nil means to never reset. Instead of a list, the value t is
also allowed. This is equivalent to \\='(:audit :reset). The
value can also be a function as for `stp-auto-commit'.")

(defvar stp-auto-dependencies t
  "When non-nil, automatically install or upgrade dependencies as needed.

This applies when installing, uninstalling, upgrading or
reinstalling packages.")

(defvar stp-auto-post-actions t
  "When non-nil, automatically perform post actions.

The value can also be a function as for `stp-auto-commit'. Post
actions can be individually enabled or disabled via
`stp-auto-update-load-path', `stp-auto-load', `stp-auto-build',
`stp-auto-build-info' and `stp-auto-update-info-directories'.")

(defvar stp-auto-tag t
  "When bumping the version, tag the commit with the new version.

The value can also be a function as for `stp-auto-commit'.")

(defvar stp-auto-update-load-path t
  "When non-nil, automatically update the load path.

The value can also be a function as for `stp-auto-commit'.")

(defvar stp-auto-load t
  "When non-nil, automatically load packages.

The value can also be a function as for `stp-auto-commit'.")

(defvar stp-auto-build nil
  "When non-nil, automatically build pacakges.

General methods which may fail for some packages are used. The
value can also be a function as for `stp-auto-commit'.")

(defvar stp-auto-build-info t
  "When non-nil, automatically build info manuals.

When this variable is a function it will be called to determine
the value when it is needed. The value can also be a function as
for `stp-auto-commit'.")

(defvar stp-auto-update-info-directories t
  "When non-nil, automatically update the info directories.

When this variable is a function it will be called to determine
the value when it is needed. The value can also be a function as
for `stp-auto-commit'.")


(defclass stp-operation ()
  ((pkg-name :initarg :pkg-name :initform nil)
   ;; This overrides the controller's options slot in `stp-execute' when
   ;; non-nil.
   (options :initarg :options :initform nil)))

(defclass stp-package-operation (stp-operation) ())
(defclass stp-package-change-operation (stp-package-operation) ())

(defclass stp-uninstall-operation (stp-package-change-operation) ())
(defclass stp-post-action-operation (stp-package-operation) ())

(defclass stp-skippable-package-operation (stp-package-operation)
  ((allow-skip :initarg :allow-skip :initform t)))

(defvar stp-enforce-min-version nil
  "Determines if the user is allowed to select a version older than the minimum.

The minimum is the version required by another package.")

(defclass stp-additive-operation (stp-package-change-operation stp-skippable-package-operation)
  ((dependency :initarg :dependency :initform nil)
   (min-version :initarg :min-version :initform nil)
   (enforce-min-version :initarg :enforce-min-version :initform (symbol-value 'stp-enforce-min-version))
   (prompt-prefix :initarg :prompt-prefix :initform "")))

(defclass stp-install-operation (stp-additive-operation)
  ((pkg-alist :initarg :pkg-alist :initform nil)))

(defclass stp-upgrade-operation (stp-additive-operation)
  ((new-version :initarg :new-version :initform nil)))

(defclass stp-install-or-upgrade-operation (stp-install-operation stp-upgrade-operation)
  ((new-version :initarg :new-version :initform nil)))

(defclass stp-reinstall-operation (stp-additive-operation)
  ((new-version :initarg :new-version :initform nil)))

(cl-defgeneric stp-validate-options (options)
  (:documentation
   "Determine if OPTIONS passed are valid and signal an
appropriate error if they are not."))

(cl-defmethod stp-validate-options ((_options stp-operation-options))
  t)

(cl-defmethod stp-validate-options ((options stp-basic-operation-options))
  (with-slots (do-commit do-push)
      options
    (when (and (not do-commit) do-push)
      (user-error "Pushing without committing is not allowed")))
  (cl-call-next-method))

(cl-defmethod stp-validate-options ((options stp-bump-operation-options))
  (with-slots (do-commit do-tag)
      options
    (when (and (not do-commit) do-tag)
      (user-error "Tagging without committing is not allowed"))
    (cl-call-next-method)))

(defun stp-package-candidate-names ()
  (->> (append (stp-archive-package-names) (stp-emacsmirror-package-names))
       -uniq
       (-sort #'string<)))

(defun stp-abbreviate-remote-version (method remote version)
  "Abbreviate long hashes to make them more readable.

Other versions are not abbreviated."
  (cond
   ((and (eq method 'git) (not (stp-git-valid-remote-ref-p remote version)))
    (stp-git-abbreviate-hash version))
   ((eq method 'git)
    (stp-git-normalize-version remote version))
   (t
    version)))

(cl-defun stp-list-read-name (prompt)
  "In `stp-list-mode', return the package on the current line if there
is one. Otherwise, prompt the user for a package."
  (stp-refresh-info)
  (or (and (derived-mode-p 'stp-list-mode)
           (stp-list-package-on-line))
      (stp-read-existing-name prompt)))

(defun stp-list-package-on-line (&optional offset)
  "Return the name of the package on the current line.

When OFFSET is non-nil, return the name of the packages that is
OFFSET lines from the current line or nil if no package
corresponds to that line."
  (stp-refresh-info)
  (when (derived-mode-p 'stp-list-mode)
    (setq offset (or offset 0))
    (let ((line (line-number-at-pos)))
      (save-excursion
        (forward-line offset)
        (when (= (line-number-at-pos) (+ line offset))
          (when-let ((pkg-name (save-excursion
                                 (beginning-of-line)
                                 (rem-plain-symbol-at-point))))
            (and (not (save-excursion
                        (beginning-of-line)
                        (bobp)))
                 (not (save-excursion
                        (end-of-line)
                        (eobp)))
                 (not (string= pkg-name ""))
                 (member pkg-name (stp-info-names))
                 pkg-name)))))))

(defun stp-unclean-fun ()
  "This function is intended as a value of `stp-allow-unclean'.

It requires the repository to be clean when run inside
`stp-source-directory'. Otherwise, it causes the user to be
prompted."
  (rem-ancestor-of-inclusive-p (f-canonical (stp-git-root))
                               (f-canonical stp-source-directory)))

(defvar stp-allow-unclean #'stp-unclean-fun
  "This variable determines the behavior when the git repository is unclean.

When the value is nil, an error occurs. :allow means that the
command should proceed without user intervention. If the value is
a function, it will be called with no arguments and the return
value will be interpreted as described here. Any other value
means that the user should be prompted to determine if the
command should proceed.")

(defun stp-maybe-ensure-clean ()
  (let ((unclean (if (functionp stp-allow-unclean)
                     (funcall stp-allow-unclean)
                   stp-allow-unclean)))
    (or (eq unclean :allow)
        (stp-git-clean-p)
        (and (not unclean)
             (user-error "Aborted: the repository is unclean"))
        (yes-or-no-p "The git repo is unclean. Proceed anyway?"))))

(defun stp-audit-changes (pkg-name type last-hash do-reset)
  (unless (memq type '(install upgrade))
    (error "type must be either 'install or 'upgrade"))
  (stp-git-show-diff (list last-hash))
  (unless (prog1
              (yes-or-no-p "Are the changes to the package safe? ")
            (stp-git-bury-diff-buffer))
    (let ((reset (stp-maybe-call do-reset)))
      (when (or (eq reset t) (memq :audit reset))
        (stp-git-reset last-hash :mode 'hard))
      (signal 'quit
              (list (format "aborted %s %s due to a failed security audit%s"
                            (if (eq type 'install)
                                "installing"
                              "upgrading")
                            pkg-name
                            (if reset
                                ""
                              ": use git reset to undo the suspicious commits")))))))

(defun stp-maybe-audit-changes (pkg-name type last-hash options)
  (with-slots (do-reset do-audit)
      options
      (when (stp-maybe-call do-audit pkg-name)
        (stp-audit-changes pkg-name type last-hash do-reset))))

(defun stp-upgrade-handle-merge-conflicts ()
  (let ((first t))
    (while (stp-git-merge-conflict-p)
      (stp-msg "%s Resolve the conflict(s) and then press M-x `exit-recursive-edit'"
               (if first
                   "One or more merge conflicts occurred while upgrading."
                 "One or more merge conflicts are still unresolved."))
      (recursive-edit)
      (setq first nil))))

(defun stp-sort-remotes (remotes)
  "Sort the alist REMOTES that maps remotes to methods by method.

This is done according to the order in `stp-methods-order'.
REMOTES may also contain strings that map to remote symbols
representing archives."
  (seq-sort-by (lambda (remote)
                 (let ((method-or-archive (cdr remote)))
                   (list (cl-position (if (not (memq method-or-archive stp-methods-order))
                                          'archive
                                        method-or-archive)
                                      stp-methods-order)
                         (or (and (stringp method-or-archive)
                                  (cl-position (stp-emacsmirror-remote-mirror method-or-archive)
                                               stp-emacsmirrors
                                               :test #'equal))
                             -1))))
               (fn (or (< (car %1) (car %2))
                       (and (= (car %1) (car %2))
                            (< (cadr %1) (cadr %2)))))
               remotes))

(defun stp-find-remotes (pkg-name)
  (let* ((archives (stp-archives pkg-name))
         (archive-alist (mapcar (lambda (archive)
                                  (cons (intern archive) (intern archive)))
                                archives))
         (remotes (append (stp-archive-find-remotes pkg-name)
                          (stp-emacsmirror-find-remotes pkg-name)
                          (mapcar (fn (cons % 'elpa))
                                  (stp-elpa-package-urls pkg-name archives)))))
    (->> (append remotes archive-alist)
         stp-sort-remotes
         (mapcar #'car))))

(cl-defun stp-read-remote-or-archive (prompt &key pkg-name default-remote (prompt-prefix "") (read-remote t))
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
                                  (stp-emacsmirror-find-remotes pkg-name)
                                  (mapcar (fn (cons % 'elpa))
                                          (stp-elpa-package-urls pkg-name archives :annotate t))))
                 (remote-or-archive (and read-remote
                                         (stp-comp-read-remote
                                          "Remote or archive: "
                                          (->> (append remotes archive-alist)
                                               stp-sort-remotes
                                               (mapcar #'car))
                                          :default (car remotes)))))
            (cons pkg-name (and remote-or-archive
                                (or (map-elt archive-alist remote-or-archive)
                                    (car (s-split " " remote-or-archive)))))))
      ;; Otherwise the user chose a remote so prompt for its package name.
      (let ((remote (stp-normalize-remote name-or-remote)))
        (cons (or pkg-name (stp-read-name (stp-prefix-prompt prompt-prefix "Package name: ") :default (stp-default-name remote)))
              remote)))))

(defun stp-download-url (pkg-name pkg-alist)
  (let-alist pkg-alist
    ;; Note that for the 'git method there is no download URL.
    (cl-ecase .method
      (elpa
       (stp-elpa-download-url pkg-name .remote .version))
      (archive
       ;; .remote is a symbol representing the archive for the 'archive method.
       (stp-archive-download-url pkg-name .remote))
      (url
       .remote))))

(defun stp-post-actions (pkg-name options)
  (with-slots (do-update-load-path
               do-load
               do-build
               do-build-info
               do-update-info-directories)
      options
    (when (stp-maybe-call do-update-load-path pkg-name)
      (stp-update-load-path (stp-full-path pkg-name)))
    (when (stp-maybe-call do-build pkg-name)
      (stp-build pkg-name))
    (when (stp-maybe-call do-load pkg-name)
      (condition-case err
          (stp-reload pkg-name)
        (error (display-warning 'STP "Error while loading %s modules: %s" pkg-name (error-message-string err)))))
    (when (stp-maybe-call do-build-info pkg-name)
      (stp-build-info pkg-name))
    (when (stp-maybe-call do-update-info-directories pkg-name)
      (stp-update-info-directories pkg-name))))

(defvar stp-build-output-buffer-name "*STP Build Output*")

(defun stp-build (pkg-name &optional allow-naive-byte-compile)
  "Build the package PKG-NAME.

This is done by running the appropriate build systems or
performing naive byte compilation. Return non-nil if there were
no errors."
  (when pkg-name
    (let* ((output-buffer stp-build-output-buffer-name)
           (pkg-path (stp-canonical-path pkg-name))
           (build-dir pkg-path))
      ;; Setup output buffer
      (get-buffer-create output-buffer)
      ;; Handle CMake separately. Since it generates makefiles, make may need
      ;; to be run afterwards.
      (when (f-exists-p (f-expand "CMakeLists.txt" pkg-path))
        (stp-msg "CMakeLists.txt was found in %s. Attempting to run cmake..." build-dir)
        ;; Try to use the directory build by default. It is fine if
        ;; this directory already exists as long as it is not tracked
        ;; by git.
        (setq build-dir (f-expand "build" pkg-path))
        (when (and (f-exists-p build-dir)
                   (stp-git-tracked-p build-dir))
          (setq build-dir (f-expand (make-temp-file "build-") pkg-path)))
        (unless (f-exists-p build-dir)
          (make-directory build-dir))
        (let ((default-directory build-dir))
          (let ((cmd '("cmake" "..")))
            (stp-before-build-command cmd output-buffer)
            ;; This will use `build-dir' as the build directory and
            ;; `pkg-path' as the source directory so there is no
            ;; ambiguity as to which CMakeLists.txt file should be
            ;; used.
            (unless (eql (rem-run-command cmd :buffer output-buffer) 0)
              (stp-msg "Failed to run cmake on %s" build-dir)))))
      (let ((success
             ;; Try different methods of building the package until one
             ;; succeeds.
             (or nil
                 ;; Handle GNU make. We use a separate binding for
                 ;; `default-directory' here because the cmake code above
                 ;; can change build-dir.
                 (let ((default-directory build-dir))
                   (when (-any (lambda (file)
                                 (f-exists-p file))
                               stp-gnu-makefile-names)
                     (stp-msg "A makefile was found in %s. Attempting to run make..." build-dir)
                     (let ((cmd '("make")))
                       (stp-before-build-command cmd output-buffer)
                       ;; Make expects a makefile to be in the current directory
                       ;; so there is no ambiguity over which makefile will be
                       ;; used.
                       (or (eql (rem-run-command cmd :buffer output-buffer) 0)
                           (and (stp-msg "Failed to run make on %s" pkg-path)
                                nil)))))
                 (and allow-naive-byte-compile
                      (let ((default-directory pkg-path))
                        (stp-msg "Attempting to byte compile files in %s..." pkg-path)
                        (condition-case err
                            (progn
                              ;; Put the messages from `byte-recompile-directory' in
                              ;; output-buffer.
                              (dflet ((stp-msg (&rest args)
                                               (with-current-buffer output-buffer
                                                 (insert (apply #'format args)))))
                                (stp-before-build-command "Byte compiling files" output-buffer)
                                ;; Packages have to be compiled and loaded twice
                                ;; to ensure that macros will work.
                                (byte-recompile-directory pkg-path 0)
                                (stp-reload-once pkg-name)
                                (byte-recompile-directory pkg-path 0)
                                (stp-reload-once pkg-name))
                              t)
                          (error (ignore err)
                                 (stp-msg "Byte-compiling %s failed" pkg-path)
                                 nil)))))))
        ;; Return success or failure
        (if success
            (stp-msg "Successfully built %s" pkg-name)
          (stp-msg "Build failed for %s" pkg-name))
        success))))

(cl-defun stp-reload (pkg-name &key quiet)
  "Reload the package."
  (interactive (list (stp-list-read-name "Package name: ")))
  ;; Reload the package twice so that macros are handled properly.
  (stp-reload-once pkg-name)
  (stp-reload-once pkg-name)
  (unless quiet
    (stp-msg "Reloaded %s" pkg-name)))

(defun stp-build-info (pkg-name)
  "Build the info manuals for PKG-NAME."
  (interactive (list (stp-list-read-name "Package name: ")))
  (when pkg-name
    (let* ((makefiles (f-entries (stp-canonical-path pkg-name)
                                 (lambda (path)
                                   (member (f-filename path) stp-gnu-makefile-names))
                                 t))
           (output-buffer stp-build-output-buffer-name)
           (texi-target (concat pkg-name ".texi"))
           (target (concat pkg-name ".info"))
           attempted
           (success
            ;; Try to build the info manual in different ways until one succeeds.
            (or nil
                ;; Try to find a makefile that has an appropriate target.
                (cl-dolist (makefile makefiles)
                  (when (member target (stp-make-targets makefile))
                    (let ((default-directory (f-dirname makefile)))
                      (setq attempted t)
                      (stp-msg "Makefile with target %s found in %s. Attempting to run make..." target (f-dirname makefile))
                      (let ((cmd (list "make" target)))
                        (stp-before-build-command cmd output-buffer)
                        (if (eql (rem-run-command cmd :buffer output-buffer) 0)
                            (progn
                              (stp-msg "Built the info manual for %s using make" pkg-name)
                              (cl-return t))
                          (stp-msg "'%s' failed in %s" cmd (f-dirname makefile)))))))

                ;; Try to compile a texi file directly.
                (cl-dolist (source (f-entries (stp-canonical-path pkg-name)
                                              (lambda (path)
                                                (string= (f-filename path) texi-target))
                                              t))
                  (let ((default-directory (f-dirname source)))
                    (setq attempted t)
                    (stp-msg "texi source file found at %s. Attempting to compile it with makeinfo..." source)
                    (let ((cmd (list "makeinfo" "--no-split" texi-target)))
                      (cond
                       (;; Don't build texi files unless they have changed since the info
                        ;; manual was last built.
                        (f-newer-p (f-swap-ext source "info") source)
                        (stp-msg "The info manual for %s is up to date" pkg-name)
                        (cl-return t))
                       ((progn
                          (stp-before-build-command cmd output-buffer)
                          (eql (rem-run-command cmd :buffer output-buffer) 0))
                        (stp-msg "Built the info manual for %s using makeinfo" pkg-name)
                        (cl-return t))
                       (t
                        (stp-msg "'%s' failed" cmd)))))))))
      (unless attempted
        (stp-msg "No makefiles or texi source files found for the %s info manual" pkg-name))
      success)))

(defun stp-update-info-directories (pkg-name &optional quiet)
  "Make the info files for PKG-NAME available to info commands."
  (interactive (list (stp-list-read-name "Package name: ")))
  (when pkg-name
    (let* ((directory (stp-canonical-path pkg-name))
           (new (mapcar 'f-dirname
                        (f-entries directory
                                   (-partial #'string-match-p "\\.info$")
                                   t))))
      (info-initialize)
      (setq Info-directory-list
            (cl-remove-duplicates (append Info-directory-list new)
                                  :test #'equal))
      (unless quiet
        (if new
            (stp-msg "Added info files for %s" pkg-name)
          (stp-msg "No info files found for %s" pkg-name))))))

(defun stp-update-lock-file (&optional interactive-p)
  "Write the hash of the git repository to the lock file."
  (interactive (list t))
  (stp-with-package-source-directory
    (let ((hash (stp-git-rev-to-hash stp-source-directory "HEAD")))
      (with-temp-buffer
        (insert (format "%S\n" hash))
        (f-write (buffer-string) 'utf-8 stp-lock-file)
        (when interactive-p
          (stp-msg "Updated the lock file at %s" stp-lock-file))))))

(cl-defun stp-read-package (&key pkg-name pkg-alist (prompt-prefix "") min-version enforce-min-version)
  (plet* ((`(,pkg-name . ,remote)
           (-> (stp-prefix-prompt prompt-prefix "Package name or remote: ")
               (stp-read-remote-or-archive
                :pkg-name pkg-name
                :default-remote (map-elt pkg-alist 'remote))))
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
   (operations :initarg :operations :initform nil)))

(defclass stp-interactive-controller (stp-controller) ())

(defclass stp-auto-controller (stp-controller)
  ((preferred-update :initarg :preferred-update :initform 'stable)
   (respect-update :initarg :respect-update :initform t)
   (development-directory-override :initarg :development-directory-override :initform 'unstable)))

(defvar stp-default-controller-class 'stp-auto-controller)
(defvar stp-default-controller-args nil)

(cl-defgeneric stp-make-controller-get-class-args (options))

(cl-defmethod stp-make-controller-get-class-args ((_options stp-operation-options))
  (list stp-default-controller-class stp-default-controller-args))

(cl-defmethod stp-make-controller-get-class-args ((options stp-controlled-operation-options))
  (list (oref options controller-class) (oref options make-controller-args)))

(cl-defgeneric stp-make-controller (options &rest args)
  "Make a new controller of class `stp-default-controller-class' by
merging `stp-default-controller-args' with ARGS.")

(cl-defmethod stp-make-controller ((options stp-operation-options) &rest args)
  (dsb (class default-args)
      (stp-make-controller-get-class-args options)
    (apply class :options options (map-merge 'plist default-args args))))

(cl-defgeneric stp-controller-append-errors (controller pkg-name &rest errors)
  (:documentation
   "Append the specified error messages (strings) to CONTROLLER's
list of error messages. These will be reported to the user after
all operations are completed."))

(cl-defmethod stp-controller-append-errors ((controller stp-controller) pkg-name &rest new-errors)
  (with-slots (errors)
      controller
    (setq errors (append errors (mapcar (fn (cons pkg-name %)) new-errors)))))

(cl-defgeneric stp-controller-append-operations (controller &rest operations)
  (:documentation
   "Append the specified operations to CONTROLLER's list of
operations to perform."))

(cl-defmethod stp-controller-append-operations ((controller stp-controller) &rest new-operations)
  (with-slots (operations)
      controller
    (setf operations (append operations new-operations))))

(cl-defgeneric stp-controller-prepend-operations (controller &rest operations)
  (:documentation
   "Prepend the specified operations to CONTROLLER's list of
operations to perform."))

(cl-defmethod stp-controller-prepend-operations ((controller stp-controller) &rest new-operations)
  (with-slots (operations)
      controller
    (setf operations (append new-operations operations))))

(cl-defgeneric stp-controller-get-package (controller pkg-name prompt-prefix min-version enforce-min-version)
  (:documentation
   "Query CONTROLLER for a package."))

(cl-defmethod stp-controller-get-package ((_controller stp-interactive-controller) pkg-name prompt-prefix min-version enforce-min-version)
  (stp-read-package :pkg-name pkg-name
                    :prompt-prefix prompt-prefix
                    :min-version min-version
                    :enforce-min-version enforce-min-version))

(defun stp-enforce-min-version (pkg-name version min-version enforce-min-version)
  (when (and min-version enforce-min-version (not (stp-version<= min-version version)))
    (error "The newest version for %s is %s but at least %s is required" pkg-name version min-version)))

(cl-defgeneric stp-controller-actual-update (controller pkg-name pkg-alist remote)
  "Determine the update parameter to use.")

(cl-defmethod stp-controller-actual-update ((controller stp-auto-controller) _pkg-name pkg-alist remote)
  "Determine the update parameter to use."
  (with-slots (preferred-update respect-update development-directory-override)
      controller
    (let-alist pkg-alist
      ;; Respect the package's update attribute if it is set and the controller is
      ;; set to do respect package's update attributes.
      (cond
       ((and respect-update .update)
        .update)
       ;; Always prefer the unstable version when the remote is in the development
       ;; directory.
       ((and development-directory-override
             (not (symbolp remote))
             (f-dir-p remote)
             (rem-ancestor-of-inclusive-p (f-canonical stp-development-directory)
                                          (f-canonical remote)))
        'unstable)
       (t
        preferred-update)))))

(cl-defgeneric stp-controller-preferred-git-version (controller pkg-name pkg-alist remote min-version))

(cl-defmethod stp-controller-preferred-git-version :around ((_controller stp-controller) _pkg-name _pkg-alist remote _min-version)
  (let ((version (cl-call-next-method)))
    (stp-git-normalize-version remote version)))

(cl-defmethod stp-controller-preferred-git-version ((controller stp-auto-controller) pkg-name pkg-alist remote min-version)
  (with-slots (preferred-update)
      controller
    (let (latest-stable
          (branch (car (stp-git-remote-heads-sorted remote)))
          (actual-update (stp-controller-actual-update controller pkg-name pkg-alist remote)))
      (if (and (eq actual-update 'stable)
               (setq latest-stable (stp-git-latest-stable-version remote))
               ;; If there's a minimum version and the
               ;; latest stable is not recent enough,
               ;; use the development version.
               (or (not min-version)
                   (stp-version<= min-version latest-stable)))
          latest-stable
        branch))))

(cl-defmethod stp-controller-get-package ((controller stp-auto-controller) pkg-name prompt-prefix min-version enforce-min-version)
  (plet* ((`(,pkg-name . ,remote)
           (or (and pkg-name
                    (cons pkg-name nil))
               (-> (stp-prefix-prompt prompt-prefix "Package name or remote: ")
               (stp-read-remote-or-archive :pkg-name pkg-name :read-remote nil))))
          (remote (or remote (car (stp-find-remotes pkg-name))))
          (method (stp-remote-method remote)))
    (append `(,pkg-name
              (method . ,method)
              (remote . ,remote))
            (cl-ecase method
              (git
               (let* ((branch (car (stp-git-remote-heads-sorted remote)))
                      (version (stp-controller-preferred-git-version controller pkg-name nil remote min-version))
                      (update (if (string= version branch) 'unstable 'stable)))
                 (stp-enforce-min-version pkg-name version min-version enforce-min-version)
                 `((version . ,version)
                   (update . ,update)
                   (branch . ,branch))))
              (elpa
               (let ((version (car (stp-elpa-versions-sorted pkg-name remote))))
                 (stp-enforce-min-version pkg-name version min-version enforce-min-version)
                 `((version . ,version))))
              (archive)
              (url
               `((version . ,(stp-url-default-version))))))))

(cl-defgeneric stp-controller-get-remote (controller prompt remote other-remotes)
  (:documentation
   "Query CONTROLLER for a remote."))

(cl-defmethod stp-controller-get-remote ((_controller stp-interactive-controller) prompt remote other-remotes)
  (stp-choose-remote prompt remote other-remotes))

(cl-defmethod stp-controller-get-remote ((_controller stp-auto-controller) _prompt remote _other-remotes)
  remote)

(defvar stp-git-upgrade-always-offer-remote-heads t)

(cl-defgeneric stp-controller-get-git-version (controller prompt pkg-name pkg-alist chosen-remote min-version enforce-min-version)
  (:documentation
   "Query CONTROLLER for a new version of a git package."))

(cl-defmethod stp-controller-get-git-version ((_controller stp-interactive-controller) prompt _pkg-name pkg-alist chosen-remote min-version enforce-min-version)
  (let-alist pkg-alist
    (let ((extra-versions (and (eq .method 'git)
                               (or stp-git-upgrade-always-offer-remote-heads
                                   (eq .update 'unstable))
                               (stp-git-remote-heads-sorted chosen-remote))))
      (when (and .branch (member .branch extra-versions))
        (setq extra-versions (cons .branch (remove .branch extra-versions))))
      (stp-git-read-version prompt
                            chosen-remote
                            :extra-versions extra-versions
                            :extra-versions-position (if (eq .update 'unstable) 'first 'last)
                            :branch-to-hash nil
                            :min-version (and enforce-min-version min-version)))))

(cl-defmethod stp-controller-get-git-version ((controller stp-auto-controller) _prompt pkg-name pkg-alist chosen-remote min-version enforce-min-version)
  (let ((version (stp-controller-preferred-git-version controller pkg-name pkg-alist chosen-remote min-version)))
    (stp-enforce-min-version pkg-name version min-version enforce-min-version)
    version))

(cl-defgeneric stp-controller-get-elpa-version (controller prompt pkg-name pkg-alist chosen-remote min-version enforce-min-version)
  (:documentation
   "Query CONTROLLER for the new version of an ELPA package."))

(cl-defmethod stp-controller-get-elpa-version ((_controller stp-interactive-controller) prompt pkg-name _pkg-alist chosen-remote min-version enforce-min-version)
  (stp-elpa-read-version prompt
                         pkg-name
                         chosen-remote
                         :min-version (and enforce-min-version min-version)))

(cl-defmethod stp-controller-get-elpa-version ((_controller stp-auto-controller) _prompt pkg-name _pkg-alist chosen-remote min-version enforce-min-version)
  (let ((version (car (stp-elpa-versions-sorted pkg-name chosen-remote))))
    (stp-enforce-min-version pkg-name version min-version enforce-min-version)
    version))

(cl-defgeneric stp-operation-verb (operation)
  "Return a verb that describes OPERATION.")

(cl-defmethod stp-operation-verb ((_operation stp-package-operation))
  "performing an unknown package operation on")

(cl-defmethod stp-operation-verb ((_operation stp-uninstall-operation))
  "uninstalling")

(cl-defmethod stp-operation-verb ((_operation stp-install-operation))
  "installing")

(cl-defmethod stp-operation-verb ((_operation stp-upgrade-operation))
  "upgrading")

(cl-defmethod stp-operation-verb ((_operation stp-reinstall-operation))
  "reinstalling")

(cl-defmethod stp-operation-verb ((_operation stp-post-action-operation))
  "performing post actions on")

(cl-defgeneric stp-ensure-prerequistites (controller operation)
  (:documentation
   "Determine if the prerequisites for OPERATION are satisfied."))

(cl-defmethod stp-ensure-prerequistites ((_controller stp-controller) (_operation stp-operation))
  t)

(cl-defmethod stp-ensure-prerequistites ((_controller stp-controller) (_operation stp-package-change-operation))
  (stp-maybe-ensure-clean)
  (cl-call-next-method))

(defun stp-skip-package ()
  "Skip installing or upgrading this package."
  (interactive)
  (throw 'stp-skip 'skip))

(defvar-keymap stp-skip-map
  "C-c C-k" #'stp-skip-package)

(defmacro stp-allow-skip (skip-form &rest body)
  (declare (indent 1))
  (with-gensyms (result)
    `(cl-flet ((setup-keymap ()
                 (use-local-map (make-composed-keymap (list stp-skip-map) (current-local-map)))))
       (let ((,result (minibuffer-with-setup-hook (:append #'setup-keymap)
                        (catch 'stp-skip
                          ,@body))))
         (when (eq ,result 'skip)
           ,skip-form)
         ,result))))

(cl-defgeneric stp-operate (controller operation)
  (:documentation
   "Perform OPERATION using CONTROLLER. This may result in additional
operations being added to the controller."))

(cl-defmethod stp-operate ((_controller stp-controller) (_operation stp-operation)))

(cl-defmethod stp-operate :around ((_controller stp-controller) (operation stp-skippable-package-operation))
  ;; Sometimes, a single repository can contain multiple packages and so
  ;; installing the dependencies naively will result in multiple copies.
  (if (oref operation allow-skip)
      (stp-allow-skip (stp-msg "Skipping %s %s"
                               (stp-operation-verb operation)
                               (oref operation pkg-name))
        (cl-call-next-method))
    (cl-call-next-method)))

(cl-defgeneric stp-describe (operation))

(cl-defmethod stp-describe ((operation stp-operation))
  (format "%s %s" (stp-operation-verb operation) (oref operation pkg-name)))

(defun stp-options (controller operation)
  (or (oref controller options) (oref operation options)))

(cl-defgeneric stp-uninstall-requirements (controller requirements options)
  (:documentation
   "Uninstall those REQUIREMENTS that are no longer needed by any
package and were installed as dependencies."))

(cl-defmethod stp-uninstall-requirements ((controller stp-controller) requirements options)
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
        (stp-controller-prepend-operations
         controller
         (stp-uninstall-operation :pkg-name pkg-name
                                  :options options))))))

;; Skip additive operations that were added due to a dependency that is now
;; satisfied. This can happen when multiple packages that were installed have
;; the same dependency.
(cl-defmethod stp-operate :around ((_controller stp-controller) (operation stp-additive-operation))
  (with-slots (pkg-name dependency min-version)
      operation
    (if (and dependency
             ;; It isn't necessary to search the load path because we just
             ;; want to know if the package was already installed or
             ;; upgraded within STP.
             (stp-package-requirement-satisfied-p pkg-name min-version))
        'ignore
      (cl-call-next-method))))

(cl-defmethod stp-operate ((controller stp-controller) (operation stp-uninstall-operation))
  (let ((options (stp-options controller operation)))
    (with-slots (do-commit do-dependencies)
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
                                          (stp-abbreviate-remote-version .method .remote .version)
                                          pkg-name)
                                  :do-commit do-commit)
                  (stp-headers-update-features)
                  (when (stp-maybe-call do-dependencies)
                    (stp-uninstall-requirements controller requirements options))
                  (stp-prune-cached-latest-versions pkg-name))
              (error "Failed to remove %s. This can happen when there are uncommitted changes in the git repository" pkg-name))))))))

(cl-defgeneric stp-ensure-requirements (controller requirements options)
  (:documentation
   "Install or upgrade the REQUIREMENTS that are not currently satisfied."))

(cl-defmethod stp-ensure-requirements ((controller stp-controller) requirements options)
  (stp-msg "Analyzing the load path for installed packages...")
  (cl-dolist (requirement requirements)
    ;; Also allow a list of package names.
    (dsb (pkg-sym &optional version)
        (ensure-list requirement)
      (let* ((pkg-name (stp-symbol-package-name pkg-sym))
             (prefix (format "[%s] " pkg-name)))
        (cond
         ((string= pkg-name "emacs")
          (unless (stp-emacs-requirement-satisfied-p pkg-name version)
            (->> (format "Version %s of Emacs is required but %d.%d is installed"
                         version
                         emacs-major-version
                         emacs-minor-version)
                 (stp-controller-append-errors pkg-name controller))))
         ;; Do nothing when a requirement is ignored or a new enough
         ;; version is installed.
         ((stp-package-requirement-satisfied-p pkg-name version t))
         ((not (member pkg-name (stp-info-names)))
          (stp-controller-prepend-operations
           controller
           (stp-install-operation :pkg-name pkg-name
                                  :options options
                                  :prompt-prefix prefix
                                  :min-version version
                                  :dependency t)))
         (t
          ;; The dependency attribute is left as is when upgrading because
          ;; the package might have been installed manually originally.
          (stp-controller-prepend-operations
           controller
           (stp-upgrade-operation :pkg-name pkg-name
                                  :options options
                                  :prompt-prefix prefix
                                  :min-version version
                                  :dependency t))))))))

(cl-defmethod stp-operate ((controller stp-controller) (operation stp-install-operation))
  (let ((options (stp-options controller operation)))
    (with-slots (do-commit do-audit do-dependencies do-actions)
        options
      (with-slots (pkg-name min-version enforce-min-version prompt-prefix dependency)
          operation
        (let* ((pkg-alist (or (oref operation pkg-alist)
                              (stp-controller-get-package controller pkg-name prompt-prefix min-version enforce-min-version)))
               (last-hash (stp-git-head)))
          (let-alist pkg-alist
            ;; Guess the method if it isn't already known.
            (unless .method
              (setq .method (stp-remote-method .remote))
              (stp-set-attribute pkg-name 'method .method))
            (when (stp-url-safe-remote-p .remote)
              (cl-ecase .method
                (git (stp-git-install controller pkg-name .remote .version .update options :branch .branch))
                (elpa (stp-elpa-install controller pkg-name .remote .version options))
                (archive (stp-archive-install controller pkg-name .remote options))
                (url (stp-url-install controller pkg-name .remote .version options)))
              (stp-maybe-audit-changes pkg-name 'install last-hash options)
              (stp-update-remotes pkg-name .remote .remote .other-remotes)
              (stp-update-requirements pkg-name)
              (when dependency
                (stp-set-attribute pkg-name 'dependency t))
              (stp-write-info)
              ;; For archives, the version is determined automatically instead of
              ;; being read and so .version will be nil here.
              (setq .version (stp-get-attribute pkg-name 'version))
              (stp-git-commit (format "Installed version %s of %s"
                                      (stp-abbreviate-remote-version .method .remote .version)
                                      pkg-name)
                              :do-commit do-commit)
              ;; Features need to be updated before resolving dependencies. For
              ;; this reason, handling feature updates in a `stp-operate' :after
              ;; method doesn't work well.
              (stp-headers-update-features)
              (when (stp-maybe-call do-dependencies)
                (stp-ensure-requirements controller (stp-get-attribute pkg-name 'requirements) options))
              ;; Perform post actions for all packages after everything else.
              (when (stp-maybe-call do-actions)
                (stp-controller-append-operations controller (stp-post-action-operation :pkg-name pkg-name :options options))))))))))

(cl-defmethod stp-operate ((controller stp-controller) (operation stp-upgrade-operation))
  (let ((options (stp-options controller operation)))
    (with-slots (do-commit do-actions do-dependencies do-audit)
        options
      (with-slots (pkg-name min-version enforce-min-version prompt-prefix new-version)
          operation
        (let ((last-hash (stp-git-head))
              (pkg-alist (stp-get-alist pkg-name)))
          (let-alist pkg-alist
            ;; Automatically determine missing other remotes for archive packages.
            (when (eq .method 'archive)
              (setq .other-remotes (cl-set-difference (stp-archives pkg-name) (cons .remote .other-remotes))))
            (let* ((chosen-remote (stp-controller-get-remote controller "Remote: " .remote .other-remotes))
                   (prompt (and (not new-version)
                                (format "Upgrade from %s to version%s: "
                                        (stp-abbreviate-remote-version .method chosen-remote .version)
                                        (stp-min-version-annotation min-version enforce-min-version)))))
              (when (stp-url-safe-remote-p chosen-remote)
                (unless new-version
                  (setq new-version
                        (cl-case .method
                          (git (stp-controller-get-git-version controller prompt pkg-name pkg-alist chosen-remote min-version enforce-min-version))
                          (elpa (stp-controller-get-elpa-version controller prompt pkg-name pkg-alist chosen-remote min-version enforce-min-version)))))
                (cl-ecase .method
                  (git (stp-git-upgrade controller pkg-name chosen-remote new-version options))
                  (elpa (stp-elpa-upgrade controller pkg-name chosen-remote new-version options))
                  (archive (stp-archive-upgrade controller pkg-name chosen-remote options))
                  (url (stp-url-upgrade controller pkg-name chosen-remote (or new-version (stp-url-read-version prompt)) options)))
                (stp-maybe-audit-changes pkg-name 'upgrade last-hash options)
                ;; The call to `stp-get-attribute' can't be replaced with
                ;; .version because the 'version attribute will have changed
                ;; after the call to `stp-git-upgrade', `stp-elpa-upgrade' or
                ;; `stp-url-upgrade'.
                (setq new-version (stp-get-attribute pkg-name 'version))
                (stp-update-remotes pkg-name chosen-remote .remote .other-remotes)
                (stp-update-requirements pkg-name)
                (stp-write-info)
                ;; Don't commit, push or perform push actions until the user
                ;; resolves any merge conflicts.
                (stp-upgrade-handle-merge-conflicts)
                (stp-git-commit (format "Upgraded to version %s of %s"
                                        (stp-abbreviate-remote-version .method chosen-remote new-version)
                                        pkg-name)
                                :do-commit do-commit)
                (stp-headers-update-features)
                (when (stp-maybe-call do-dependencies)
                  (stp-ensure-requirements controller (stp-get-attribute pkg-name 'requirements) options))
                ;; Perform post actions for all packages after everything else.
                (when (stp-maybe-call do-actions)
                  (stp-controller-append-operations controller (stp-post-action-operation :pkg-name pkg-name :options options)))))))))))

(cl-defmethod stp-operate ((controller stp-controller) (operation stp-install-or-upgrade-operation))
  (let ((class (if (member (oref operation pkg-name) (stp-info-names))
                   'stp-upgrade-operation
                 'stp-install-operation)))
    (stp-controller-prepend-operations controller (rem-change-class operation class))))

(cl-defmethod stp-operate ((controller stp-controller) (operation stp-reinstall-operation))
  (let ((options (stp-options controller operation)))
    (with-slots (pkg-name new-version)
        operation
      (when (and (stp-git-tree-package-modified-p pkg-name)
                 (not (yes-or-no-p (format "The package %s has been modified since the last commit in the working tree. Reinstalling will delete these changes. Do you wish to proceed?" pkg-name))))
        (user-error "Reinstall aborted"))
      (let-alist (stp-get-alist pkg-name)
        (let* ((pkg-alist (stp-get-alist pkg-name))
               (tree-hashes (and (if (eq .method 'git)
                                     (stp-git-subtree-package-modified-p pkg-name .remote .version)
                                   ;; For methods other than 'git, we need to create
                                   ;; a synthetic git repository for comparision
                                   ;; purposes.
                                   (stp-git-subtree-package-modified-p pkg-name (stp-git-download-as-synthetic-repo pkg-name (stp-download-url pkg-name pkg-alist)) "HEAD")))))
          ;; Warn the user about reinstalling if there are modifications to the
          ;; subtree that were not the result of git subtree merge as this will
          ;; result in the loss of their customizations to the package.
          (save-window-excursion
            (when (and tree-hashes
                       (unwind-protect
                           ;; curr-hash is the hash of the most recent version of
                           ;; the subtree (which may include user modifications).
                           ;; last-hash is the hash of the last subtree that was
                           ;; merged (e.g. by installing or upgrading the package).
                           (and (dsb (curr-hash last-hash)
                                    tree-hashes
                                  (stp-git-show-diff (list last-hash curr-hash))
                                  t)
                                (not (yes-or-no-p (format "The package %s has been modified locally. Reinstalling will delete these changes. Do you wish to proceed?" pkg-name))))
                         (stp-git-bury-diff-buffer)))
              (user-error "Reinstall aborted")))
          (when new-version
            (setf pkg-alist (copy-tree pkg-alist)
                  (map-elt pkg-alist 'version) new-version))
          (stp-controller-prepend-operations
           controller
           ;; Reinstalling will fail if the uninstall operation does not commit.
           (stp-uninstall-operation :pkg-name pkg-name :options (clone options :do-commit t))
           (stp-install-operation :pkg-name pkg-name :options options :pkg-alist pkg-alist)))))))

(cl-defmethod stp-operate ((controller stp-controller) (operation stp-post-action-operation))
  (let ((options (stp-options controller operation)))
    (with-slots (pkg-name)
        operation
      (stp-post-actions pkg-name options))))

(defun stp-report-operations (successful-operations skipped-operations failed-operations)
  (cl-flet ((breakdown (operations word)
              (let ((counts (make-hash-table :test #'equal))
                    (verbs (mapcar #'stp-operation-verb operations)))
                (cl-dolist (verb verbs)
                  (cl-incf (gethash verb counts 0)))
                (setq verbs (-sort #'string< (-uniq verbs)))
                (s-join "\n" (mapcar (lambda (verb)
                                       (format "%s %d packages %s"
                                               (s-capitalize verb)
                                               (gethash verb counts)
                                               word))
                                     verbs)))))
    (let ((total (+ (length successful-operations)
                    (length skipped-operations)
                    (length failed-operations))))
      (when successful-operations
        (stp-msg "Successfully completed %d operations:\n%s"
                 (length successful-operations)
                 (breakdown successful-operations "succeeded")))
      (when failed-operations
        (stp-msg "%d/%d operations failed:\n%s"
                 (length failed-operations)
                 total
                 (breakdown (mapcar #'car failed-operations) "failed"))
        (cl-dolist (cell failed-operations)
          (dsb (operation . err)
              cell
            (stp-msg "%s failed: %s" (s-capitalize (stp-describe operation)) err)))
        (pop-to-buffer stp-log-buffer-name)))))

(cl-defgeneric stp-execute (controller)
  (:documentation
   "Execute the operations for CONTROLLER."))

(cl-defmethod stp-execute ((controller stp-controller))
  (with-slots (options operations)
      controller
    (with-slots (do-push do-lock do-reset)
        options
      (let ((last-hash (stp-git-head))
            operation
            failed-operations
            skipped-operations
            successful-operations)
        (while (setq operation (pop operations))
          (condition-case err
              (progn
                (stp-ensure-prerequistites controller operation)
                (cl-case (stp-operate controller operation)
                  (skip (push operation skipped-operations))
                  (ignore)
                  (t (push operation successful-operations))))
            (error (push (cons operation err) failed-operations))))
        ;; Resetting should be done before pushing or locking if an error occurred.
        (let ((reset (stp-maybe-call do-reset)))
          (when (and failed-operations (or (eq reset t) (memq :error reset)))
            (stp-msg "Resetting due to %s"
                     (if (cdr failed-operations)
                         "errors"
                       "an error"))
            (stp-git-reset last-hash :mode 'hard)))
        (stp-git-push :do-push (stp-maybe-call do-push))
        (when (stp-maybe-call do-lock)
          (stp-update-lock-file))
        (stp-report-operations successful-operations skipped-operations failed-operations)))))

(defun stp-execute-operations (operations options &rest args)
  (stp-execute (apply #'stp-make-controller options :operations operations args)))

(provide 'stp-controller)

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
;;; stp-controller.el ends here
