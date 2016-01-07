;; misc-gtags.el --- Navigation between method definitions
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is a fork version of [ggtags](https://github.com/leoliu/ggtags)

;;; Code:
(prelude-require-packages '(projectile))

;;; Custom stuff
(eval-when-compile
  (require 'url-parse))

(require 'cl-lib)
(require 'ewoc)
(require 'compile)
(require 'etags)
(require 'tabulated-list)               ;preloaded since 24.3

(eval-when-compile
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      (list 'set (list 'make-local-variable (list 'quote var)) val)))

  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional docstring)
      (declare (debug defvar) (doc-string 3))
      (list 'progn (list 'defvar var val docstring)
            (list 'make-variable-buffer-local (list 'quote var)))))

  (or (fboundp 'add-function) (defmacro add-function (&rest _))) ;24.4
  (or (fboundp 'remove-function) (defmacro remove-function (&rest _)))

  (defmacro ignore-errors-unless-debug (&rest body)
    "Ignore all errors while executing BODY unless debug is on."
    (declare (debug t) (indent 0))
    `(condition-case-unless-debug nil (progn ,@body) (error nil)))

  (defmacro with-display-buffer-no-window (&rest body)
    (declare (debug t) (indent 0))
    ;; See http://debbugs.gnu.org/13594
    `(let ((display-buffer-overriding-action
            (if (and ggtags-auto-jump-to-match
                     ;; Appeared in emacs 24.4.
                     (fboundp 'display-buffer-no-window))
                (list #'display-buffer-no-window)
              display-buffer-overriding-action)))
       ,@body)))

(eval-and-compile
  (or (fboundp 'user-error)             ;24.3
      (defalias 'user-error 'error))
  (or (fboundp 'read-only-mode)         ;24.3
      (defalias 'read-only-mode 'toggle-read-only))
  (or (fboundp 'register-read-with-preview) ;24.4
      (defalias 'register-read-with-preview 'read-char)))

(defgroup ggtags nil
  "GNU Global source code tagging system."
  :group 'tools)

(defface ggtags-highlight '((t (:underline t)))
  "Face used to highlight a valid tag at point."
  :group 'ggtags)

(defface ggtags-global-line '((t (:inherit secondary-selection)))
  "Face used to highlight matched line in Global buffer."
  :group 'ggtags)

(defcustom ggtags-oversize-limit (* 10 1024 1024)
  "The over size limit for the  GTAGS file.
When the size of the GTAGS file is below this limit, ggtags
always maintains up-to-date tags for the whole source tree by
running `global -u'.  For projects with GTAGS larger than this
limit, only files edited in Ggtags mode are updated (via `global
--single-update')."
  :safe 'numberp
  :type '(choice (const :tag "None" nil)
                 (const :tag "Always" t)
                 number)
  :group 'ggtags)

(defcustom ggtags-include-pattern
  '("^\\s-*#\\s-*\\(?:include\\|import\\)\\s-*[\"<]\\(?:[./]*\\)?\\(.*?\\)[\">]" . 1)
  "Pattern used to detect #include files.
Value can be (REGEXP . SUB) or a function with no arguments.
REGEXP should match from the beginning of line."
  :type '(choice (const :tag "Disable" nil)
                 (cons regexp integer)
                 function)
  :safe 'stringp
  :group 'ggtags)

(defcustom ggtags-project-duration 600
  "Seconds to keep information of a project in memory."
  :type 'number
  :group 'ggtags)

(defcustom ggtags-process-environment nil
  "Similar to `process-environment' with higher precedence.
Elements are run through `substitute-env-vars' before use.
GTAGSROOT will always be expanded to current project root
directory.  This is intended for project-wise ggtags-specific
process environment settings.  Note on remote hosts (e.g. tramp)
directory local variables is not enabled by default per
`enable-remote-dir-locals' (which see)."
  :safe 'ggtags-list-of-string-p
  :type '(repeat string)
  :group 'ggtags)

(defcustom ggtags-auto-jump-to-match 'history
  "Strategy on how to jump to match: nil, first or history.

    nil: never automatically jump to any match;
  first: jump to the first match;
history: jump to the match stored in search history."
  :type '(choice (const :tag "First match" first)
                 (const :tag "Search History" history)
                 (const :tag "Never" nil))
  :group 'ggtags)

(defcustom ggtags-global-window-height 8 ; ggtags-global-mode
  "Number of lines for the *ggtags-global* popup window.
If nil, use Emacs default."
  :type '(choice (const :tag "Default" nil) integer)
  :group 'ggtags)

(defcustom ggtags-global-abbreviate-filename 60
  "Non-nil to display file names abbreviated e.g. \"/u/b/env\".
If an integer abbreviate only names longer than that number."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Always" t)
                 integer)
  :group 'ggtags)

(defcustom ggtags-split-window-function split-window-preferred-function
  "A function to control how ggtags pops up the auxiliary window."
  :type 'function
  :group 'ggtags)

(defcustom ggtags-sort-by-nearness nil
  "Sort tags by nearness to current directory.
GNU Global 6.5+ required."
  :type 'boolean
  :safe #'booleanp
  :group 'ggtags)

(defcustom ggtags-update-on-save t
  "Non-nil to update tags for current buffer on saving."
  ;; It is reported that `global --single-update' can be slow in sshfs
  ;; directories. See https://github.com/leoliu/ggtags/issues/85.
  :safe #'booleanp
  :type 'boolean
  :group 'ggtags)

(defcustom ggtags-global-output-format 'grep
  "Global output format: path, ctags, ctags-x, grep or cscope."
  :type '(choice (const path)
                 (const ctags)
                 (const ctags-x)
                 (const grep)
                 (const cscope))
  :group 'ggtags)

(defcustom ggtags-global-use-color t
  "Non-nil to use color in output if supported by Global.
Note: processing colored output takes noticeable time
particularly when the output is large."
  :type 'boolean
  :safe 'booleanp
  :group 'ggtags)

(defcustom ggtags-global-ignore-case nil
  "Non-nil if Global should ignore case in the search pattern."
  :safe 'booleanp
  :type 'boolean
  :group 'ggtags)

(defcustom ggtags-global-large-output 1000
  "Number of lines in the Global buffer to indicate large output."
  :type 'number
  :group 'ggtags)

(defcustom ggtags-global-history-length history-length
  "Maximum number of items to keep in `ggtags-global-search-history'."
  :type 'integer
  :group 'ggtags)

(defcustom ggtags-find-tag-hook nil
  "Hook run immediately after finding a tag."
  :options '(recenter reposition-window)
  :type 'hook
  :group 'ggtags)

(defcustom ggtags-mode-sticky nil
  "If non-nil enable Ggtags Mode in files visited."
  :safe 'booleanp
  :type 'boolean
  :group 'ggtags)

(defcustom ggtags-completing-read-function nil
  "Ggtags specific `completing-read-function' (which see).
Nil means using the value of `completing-read-function'."
  :type '(choice (const :tag "Use completing-read-function" nil)
                 function)
  :group 'ggtags)

(define-obsolete-variable-alias 'ggtags-highlight-tag-delay 'ggtags-highlight-tag
  "0.8.11")

(defcustom ggtags-highlight-tag 0.25
  "If non-nil time in seconds before highlighting tag at point.
Set to `nil' to disable tag highlighting."
  :set (lambda (sym value)
         (when (fboundp 'ggtags-setup-highlight-tag-at-point)
           (ggtags-setup-highlight-tag-at-point value))
         (set-default sym value))
  :type '(choice (const :tag "Disable" nil) number)
  :group 'ggtags)

(defcustom ggtags-bounds-of-tag-function (lambda ()
                                           (bounds-of-thing-at-point 'symbol))
  "Function to get the start and end positions of the tag at point."
  :type 'function
  :group 'ggtags)

;; Used by ggtags-global-mode
(defvar ggtags-global-error "match"
  "Stem of message to print when no matches are found.")

(defvar ggtags-global-last-buffer nil)

(defvar ggtags-global-continuation nil)

(defvar ggtags-current-tag-name nil)

(defvar ggtags-highlight-tag-overlay nil)

(defvar ggtags-highlight-tag-timer nil)

(defmacro ggtags-with-temp-message (message &rest body)
  (declare (debug t) (indent 1))
  (let ((init-time (make-symbol "-init-time-"))
        (tmp-msg (make-symbol "-tmp-msg-")))
    `(let ((,init-time (float-time))
           (,tmp-msg ,message))
       (with-temp-message ,tmp-msg
         (prog1 (progn ,@body)
           (message "%sdone (%.2fs)" ,(or tmp-msg "")
                    (- (float-time) ,init-time)))))))

(defmacro ggtags-delay-finish-functions (&rest body)
  "Delay running `compilation-finish-functions' until after BODY."
  (declare (indent 0) (debug t))
  (let ((saved (make-symbol "-saved-"))
        (exit-args (make-symbol "-exit-args-")))
    `(let ((,saved compilation-finish-functions)
           ,exit-args)
       (setq-local compilation-finish-functions nil)
       (add-hook 'compilation-finish-functions
                 (lambda (&rest args) (setq ,exit-args args))
                 nil t)
       (unwind-protect (progn ,@body)
         (setq-local compilation-finish-functions ,saved)
         (and ,exit-args (apply #'run-hook-with-args
                                'compilation-finish-functions ,exit-args))))))

(defmacro ggtags-ensure-global-buffer (&rest body)
  (declare (debug t) (indent 0))
  `(progn
     (or (and (buffer-live-p ggtags-global-last-buffer)
              (with-current-buffer ggtags-global-last-buffer
                (derived-mode-p 'ggtags-global-mode)))
         (error "No global buffer found"))
     (with-current-buffer ggtags-global-last-buffer ,@body)))

(defun ggtags-list-of-string-p (xs)
  "Return non-nil if XS is a list of strings."
  (cl-every #'stringp xs))

(defun ggtags-ensure-localname (file)
  (and file (or (file-remote-p file 'localname) file)))

(defun ggtags-echo (format-string &rest args)
  "Print formatted text to echo area."
  (let (message-log-max) (apply #'message format-string args)))

(defun ggtags-forward-to-line (line)
  "Move to line number LINE in current buffer."
  (cl-check-type line (integer 1))
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun ggtags-process-succeed-p (program &rest args)
  "Return non-nil if successfully running PROGRAM with ARGS."
  (condition-case err
      (zerop (apply #'process-file program nil nil nil args))
    (error (message "`%s' failed: %s" program (error-message-string err))
           nil)))

(defun ggtags-process-string (program &rest args)
  (with-temp-buffer
    (let ((exit (apply #'process-file
                       program nil t nil args))
          (output (progn
                    (goto-char (point-max))
                    (skip-chars-backward " \t\n")
                    (buffer-substring (point-min) (point)))))
      (or (zerop exit)
          (error "`%s' non-zero exit: %s" program output))
      output)))

(defun ggtags-tag-at-point ()
  "Get ggtags tag at point.

1. thing at 'current_user'   get current_user;
2. thing at '!current_user'  get current_user;
3. thing at 'current_user!'  get current_user!;
4. thing at 'current_user='  get current_user=;
5. thing at 'current_user =' get current_user=;
6. thing at 'current_user ==' get current_user;
7. thing at 'current_user ||=' get current_user=;
Otherwise, get `find-tag-default symbol."
  (regexp-quote
   (if (member (symbol-name major-mode)
               '("ruby-mode" "rhtml-mode" "haml-mode" "slim-mode"))
       (let ((symbol (thing-at-point 'symbol))
             (remain (if (thing-at-point 'symbol)
                         (save-excursion
                           (let ((from (beginning-of-thing 'symbol))
                                 (to   (end-of-thing 'line)))
                             (and (> to from)
                                  (buffer-substring-no-properties from to)))))))
         (if (and symbol remain)
             (let ((sym (s-chop-prefixes '("!!" "!") symbol))
                   (rem (s-chop-prefixes '("!!" "!") remain)))
               (if (s-matches? (concat "^" sym "\\( *\\(||\\)?=[^=]\\)") rem)
                   (concat sym "=")
                 sym))
           (find-tag-default)))
     (find-tag-default))))

;;; Store for project info and settings

(defvar ggtags-projects (make-hash-table :size 7 :test #'equal))

(cl-defstruct (ggtags-project (:constructor ggtags-project--make)
                              (:copier nil)
                              (:type vector)
                              :named)
  root tag-size has-refs has-path-style has-color dirty-p mtime timestamp)

(defun ggtags-make-project (root)
  (cl-check-type root string)
  (pcase (nthcdr 5 (file-attributes (expand-file-name "GTAGS" root)))
    (`(,mtime ,_ ,tag-size . ,_)
     (let* ((default-directory (file-name-as-directory root))
            (rtags-size (nth 7 (file-attributes "GRTAGS")))
            (has-refs
             (when rtags-size
               (and (or (> rtags-size (* 32 1024))
                        (with-demoted-errors "ggtags-make-project: %S"
                          (not (equal "" (ggtags-process-string "global" "-crs")))))
                    'has-refs)))
            ;; http://thread.gmane.org/gmane.comp.gnu.global.bugs/1518
            (has-path-style
             (and (ggtags-process-succeed-p "global" "--path-style" "shorter" "--help")
                  'has-path-style))
            ;; http://thread.gmane.org/gmane.comp.gnu.global.bugs/1542
            (has-color (and (ggtags-process-succeed-p "global" "--color" "--help")
                            'has-color)))
       (puthash default-directory
                (ggtags-project--make :root default-directory
                                      :tag-size tag-size
                                      :has-refs has-refs
                                      :has-path-style has-path-style
                                      :has-color has-color
                                      :mtime (float-time mtime)
                                      :timestamp (float-time))
                ggtags-projects)))))

(defun ggtags-project-expired-p (project)
  (or (< (ggtags-project-timestamp project) 0)
      (> (- (float-time)
            (ggtags-project-timestamp project))
         ggtags-project-duration)))

(defun ggtags-project-update-mtime-maybe (&optional project)
  "Update PROJECT's modtime and if current file is newer.
Value is new modtime if updated."
  (let ((project (or project (ggtags-find-project))))
    (when (and (ggtags-project-p project)
               (consp (visited-file-modtime))
               (> (float-time (visited-file-modtime))
                  (ggtags-project-mtime project)))
      (setf (ggtags-project-dirty-p project) t)
      (setf (ggtags-project-mtime project)
            (float-time (visited-file-modtime))))))

(defun ggtags-project-oversize-p (&optional project)
  (pcase ggtags-oversize-limit
    (`nil nil)
    (`t t)
    (size (let ((project (or project (ggtags-find-project))))
            (and project (> (ggtags-project-tag-size project) size))))))

(defvar-local ggtags-last-default-directory nil)
(defvar-local ggtags-project-root 'unset
  "Internal variable for project root directory.")

;;;###autoload
(defun ggtags-find-project ()
  ;; See https://github.com/leoliu/ggtags/issues/42
  ;;
  ;; It is unsafe to cache `ggtags-project-root' in non-file buffers
  ;; whose `default-directory' can often change.
  (unless (equal ggtags-last-default-directory default-directory)
    (kill-local-variable 'ggtags-project-root))
  (let ((project (gethash ggtags-project-root ggtags-projects)))
    (if (ggtags-project-p project)
        (if (ggtags-project-expired-p project)
            (progn
              (remhash ggtags-project-root ggtags-projects)
              (ggtags-find-project))
          project)
      (setq ggtags-last-default-directory default-directory)
      (setq ggtags-project-root
            (or (ignore-errors-unless-debug
                  (file-name-as-directory
                   (concat (file-remote-p default-directory)
                           ;; Resolves symbolic links
                           (ggtags-process-string "global" "-pr"))))
                ;; 'global -pr' resolves symlinks before checking the
                ;; GTAGS file which could cause issues such as
                ;; https://github.com/leoliu/ggtags/issues/22, so
                ;; let's help it out.
                ;;
                ;; Note: `locate-dominating-file' doesn't accept
                ;; function for NAME before 24.3.
                (let ((dir (locate-dominating-file default-directory "GTAGS")))
                  ;; `file-truename' may strip the trailing '/' on
                  ;; remote hosts, see http://debbugs.gnu.org/16851
                  (and dir (file-regular-p (expand-file-name "GTAGS" dir))
                       (file-name-as-directory (file-truename dir))))))
      (when ggtags-project-root
        (if (gethash ggtags-project-root ggtags-projects)
            (ggtags-find-project)
          (ggtags-make-project ggtags-project-root))))))

(defun ggtags-current-project-root ()
  (and (ggtags-find-project)
       (ggtags-project-root (ggtags-find-project))))

(defun ggtags-check-project ()
  (or (ggtags-find-project) (error "File GTAGS not found")))

(defun ggtags-ensure-project ()
  (when (projectile-project-p)
    (let ((gtags-file (expand-file-name "GTAGS" (projectile-project-root))))
      (unless (file-exists-p gtags-file)
        (ggtags-create-tags (projectile-project-root))))
    (ggtags-check-project)))

(defvar delete-trailing-lines)          ;new in 24.3

(defun ggtags-save-project-settings (&optional noconfirm)
  "Save Gnu Global's specific environment variables."
  (interactive "P")
  (ggtags-check-project)
  (let* ((inhibit-read-only t)          ; for `add-dir-local-variable'
         (default-directory (ggtags-current-project-root))
         ;; Not using `ggtags-with-current-project' to preserve
         ;; environment variables that may be present in
         ;; `ggtags-process-environment'.
         (process-environment
          (append ggtags-process-environment
                  process-environment
                  (and (not (ggtags-project-has-refs (ggtags-find-project)))
                       (list "GTAGSLABEL=ctags"))))
         (envlist (delete-dups
                   (cl-loop for x in process-environment
                            when (string-match
                                  "^\\(GTAGS[^=\n]*\\|MAKEOBJDIRPREFIX\\)=" x)
                            ;; May have duplicates thus `delete-dups'.
                            collect (concat (match-string 1 x)
                                            "="
                                            (getenv (match-string 1 x))))))
         (help-form (format "y: save\nn: don't save\n=: diff\n?: help\n")))
    (add-dir-local-variable nil 'ggtags-process-environment envlist)
    ;; Remove trailing newlines by `add-dir-local-variable'.
    (let ((delete-trailing-lines t)) (delete-trailing-whitespace))
    (or noconfirm
        (while (pcase (read-char-choice
                       (format "Save `%s'? (y/n/=/?) " buffer-file-name)
                       '(?y ?n ?= ??))
                 ;; ` required for 24.1 and 24.2
                 (`?n (user-error "Aborted"))
                 (`?y nil)
                 (`?= (diff-buffer-with-file) 'loop)
                 (`?? (help-form-show) 'loop))))
    (save-buffer)
    (kill-buffer)))

(defun ggtags-toggle-project-read-only ()
  (interactive)
  (ggtags-check-project)
  (let ((inhibit-read-only t)           ; for `add-dir-local-variable'
        (val (not buffer-read-only))
        (default-directory (ggtags-current-project-root)))
    (add-dir-local-variable nil 'buffer-read-only val)
    (save-buffer)
    (kill-buffer)
    (when buffer-file-name
      (read-only-mode (if val +1 -1)))
    (when (called-interactively-p 'interactive)
      (message "Project read-only-mode is %s" (if val "on" "off")))
    val))

(defmacro ggtags-with-current-project (&rest body)
  "Eval BODY in current project's `process-environment'."
  (declare (debug t) (indent 0))
  (let ((gtagsroot (make-symbol "-gtagsroot-"))
        (root (make-symbol "-ggtags-project-root-")))
    `(let* ((,root ggtags-project-root)
            (,gtagsroot (when (ggtags-find-project)
                          (ggtags-ensure-localname
                           (directory-file-name (ggtags-current-project-root)))))
            (process-environment
             (append (let ((process-environment (copy-sequence process-environment)))
                       (and ,gtagsroot (setenv "GTAGSROOT" ,gtagsroot))
                       (mapcar #'substitute-env-vars ggtags-process-environment))
                     process-environment
                     (and ,gtagsroot (list (concat "GTAGSROOT=" ,gtagsroot)))
                     (and (ggtags-find-project)
                          (not (ggtags-project-has-refs (ggtags-find-project)))
                          (list "GTAGSLABEL=ctags")))))
       (unwind-protect (save-current-buffer ,@body)
         (setq ggtags-project-root ,root)))))

(defun ggtags-project-relative-file (file)
  "Get file name relative to current project root."
  (ggtags-check-project)
  (if (file-name-absolute-p file)
      (file-relative-name file (if (string-prefix-p (ggtags-current-project-root)
                                                    file)
                                   (ggtags-current-project-root)
                                 (locate-dominating-file file "GTAGS")))
    file))

(defun ggtags-project-file-p (file)
  "Return non-nil if FILE is part of current project."
  (when (ggtags-find-project)
    (with-temp-buffer
      (ggtags-with-current-project
        ;; NOTE: `process-file' requires all files in ARGS be relative
        ;; to `default-directory'; see its doc string for details.
        (let ((default-directory (ggtags-current-project-root)))
          (process-file "global" nil t nil
                        "-vP" (concat "^" (ggtags-project-relative-file file) "$"))))
      (goto-char (point-min))
      (not (re-search-forward "^file not found" nil t)))))

(defun ggtags-invalidate-buffer-project-root (root)
  (mapc (lambda (buf)
          (with-current-buffer buf
            (and buffer-file-truename
                 (string-prefix-p root buffer-file-truename)
                 (kill-local-variable 'ggtags-project-root))))
        (buffer-list)))

(defun ggtags-create-tags (root)
  "Create tag files (e.g. GTAGS) in directory ROOT.

If file gtags.files exists in ROOT, it should be a list of source
files to index, which can be used to speed gtags up in large
source trees.  See Info node `(global)gtags' for details."
  (interactive "DRoot directory: ")
  (let ((process-environment (copy-sequence process-environment)))
    (when (zerop (length root)) (error "No root directory provided"))
    (setenv "GTAGSROOT" (ggtags-ensure-localname
                         (expand-file-name
                          (directory-file-name (file-name-as-directory root)))))
    (ggtags-with-current-project
      (let ((conf (cl-loop for name in '(".globalrc" "gtags.conf")
                           for full = (expand-file-name name root)
                           thereis (and (file-exists-p full) full))))
        (setenv "GTAGSLABEL" "ctags")
        (ggtags-with-temp-message "`gtags' in progress..."
          (let ((default-directory (file-name-as-directory root))
                (args (cl-remove-if
                       #'null
                       (list (and conf "--gtagsconf")
                             (and conf (ggtags-ensure-localname conf))))))
            (condition-case err
                (apply #'ggtags-process-string "gtags" args)
              (error (if (and (stringp (cadr err))
                              (string-match-p "mkid not found" (cadr err)))
                         (apply #'ggtags-process-string "gtags" args)
                       (signal (car err) (cdr err)))))))))
    (ggtags-invalidate-buffer-project-root (file-truename root))
    (message "GTAGS generated in `%s'" root)
    root))

(defun ggtags-update-tags (&optional force)
  "Update GNU Global tag database.
Do nothing if GTAGS exceeds the oversize limit unless FORCE.

When called interactively on large (per `ggtags-oversize-limit')
projects, the update process runs in the background without
blocking emacs."
  (interactive (progn
                 (ggtags-check-project)
                 ;; Mark project info expired.
                 (setf (ggtags-project-timestamp (ggtags-find-project)) -1)
                 (list 'interactive)))
  (cond ((and (eq force 'interactive) (ggtags-project-oversize-p))
         (ggtags-with-current-project
           (with-display-buffer-no-window
             (with-current-buffer (compilation-start "global -u")
               ;; A hack to fool compilation mode to display `global
               ;; -u finished' on finish.
               (setq mode-name "global -u")
               (add-hook 'compilation-finish-functions
                         #'ggtags-update-tags-finish nil t)))))
        ((or force (and (ggtags-find-project)
                        (not (ggtags-project-oversize-p))
                        (ggtags-project-dirty-p (ggtags-find-project))))
         (ggtags-with-current-project
           (ggtags-with-temp-message "`global -u' in progress..."
             (ggtags-process-string "global" "-u")
             (ggtags-update-tags-finish))))))

(defun ggtags-update-tags-finish (&optional buf how)
  (if (and how buf (string-prefix-p "exited abnormally" how))
      (display-buffer buf)
    (setf (ggtags-project-dirty-p (ggtags-find-project)) nil)
    (setf (ggtags-project-mtime (ggtags-find-project)) (float-time))))

(defun ggtags-update-tags-single (file &optional nowait)
  ;; NOTE: NOWAIT is ignored if file is remote file; see
  ;; `tramp-sh-handle-process-file'.
  (cl-check-type file string)
  (let ((nowait (unless (file-remote-p file) nowait)))
    (ggtags-with-current-project
      ;; See comment in `ggtags-project-file-p'.
      (let ((default-directory (ggtags-current-project-root)))
        (process-file "global" nil (and nowait 0) nil
                      "--single-update" (ggtags-project-relative-file file))))))

(defvar-local ggtags-completion-cache nil)

;; See global/libutil/char.c
;; (defconst ggtags-regexp-metachars "[][$()*+.?\\{}|^]")
(defvar ggtags-completion-flag "")      ;internal use

(defvar ggtags-completion-table
  (completion-table-dynamic
   (lambda (prefix)
     (let ((cache-key (concat prefix "$" ggtags-completion-flag)))
       (unless (equal cache-key (car ggtags-completion-cache))
         (setq ggtags-completion-cache
               (cons cache-key
                     (ignore-errors-unless-debug
                       ;; May throw global: only name char is allowed
                       ;; with -c option.
                       (ggtags-with-current-project
                         (split-string
                          (apply #'ggtags-process-string
                                 "global"
                                 (append (and completion-ignore-case '("--ignore-case"))
                                         ;; Note -c alone returns only definitions
                                         (list (concat "-c" ggtags-completion-flag) prefix)))
                          "\n" t)))))))
     (cdr ggtags-completion-cache))))

(defun ggtags-completion-at-point ()
  "A function for `completion-at-point-functions'."
  (pcase (funcall ggtags-bounds-of-tag-function)
    (`(,beg . ,end)
     (and (< beg end) (list beg end ggtags-completion-table)))))

(defun ggtags-read-tag (&optional type confirm prompt require-match default)
  (ggtags-ensure-project)
  (let ((default (or default (ggtags-tag-at-point)))
        (prompt (or prompt (capitalize (symbol-name (or type 'tag)))))
        (ggtags-completion-flag (pcase type
                                  (`(or nil definition) "T")
                                  (`symbol "s")
                                  (`reference "r")
                                  (`id "I")
                                  (`path "P")
                                  ((pred stringp) type)
                                  (_ ggtags-completion-flag))))
    (setq ggtags-current-tag-name
          (cond (confirm
                 (ggtags-update-tags)
                 (let ((completing-read-function
                        (or ggtags-completing-read-function
                            completing-read-function)))
                   (completing-read
                    (format (if default "%s (default %s): " "%s: ") prompt default)
                    ggtags-completion-table nil require-match nil nil default)))
                (default (substring-no-properties default))
                (t (ggtags-read-tag type t prompt require-match default))))))

(defun ggtags-sort-by-nearness-p ()
  (and ggtags-sort-by-nearness
       (ggtags-process-succeed-p "global" "--nearness" "--help")))

(defun ggtags-global-build-command (cmd &rest args)
  ;; CMD can be definition, reference, symbol, grep
  (let ((xs (append (list (shell-quote-argument "global")
                          "-v"
                          (format "--result=%s" ggtags-global-output-format)
                          (and ggtags-global-ignore-case "--ignore-case")
                          (and ggtags-global-use-color
                               (ggtags-find-project)
                               (ggtags-project-has-color (ggtags-find-project))
                               "--color=always")
                          (and (ggtags-sort-by-nearness-p) "--nearness")
                          (and (ggtags-find-project)
                               (ggtags-project-has-path-style (ggtags-find-project))
                               "--path-style=shorter")
                          (pcase cmd
                            ((pred stringp) cmd)
                            (`definition nil) ;-d not supported by Global 5.7.1
                            (`reference "--reference")
                            (`symbol "--symbol")
                            (`path "--path")
                            (`grep "--grep")))
                    args)))
    (mapconcat #'identity (delq nil xs) " ")))

;; Can be three values: nil, t and a marker; t means start marker has
;; been saved in the tag ring.
(defvar ggtags-global-start-marker nil)
(defvar ggtags-global-start-file nil)
(defvar ggtags-tag-ring-index nil)
(defvar ggtags-global-search-history nil)

(defvar ggtags-auto-jump-to-match-target nil)

(defun ggtags-global-save-start-marker ()
  (when (markerp ggtags-global-start-marker)
    (setq ggtags-tag-ring-index nil)
    (ring-insert find-tag-marker-ring ggtags-global-start-marker)
    (setq ggtags-global-start-marker t)))

(defun ggtags-global-start (command &optional directory)
  (let* ((default-directory (or directory (ggtags-current-project-root)))
         (split-window-preferred-function ggtags-split-window-function)
         (env ggtags-process-environment))
    (unless (and (markerp ggtags-global-start-marker)
                 (marker-position ggtags-global-start-marker))
      (setq ggtags-global-start-marker (point-marker)))
    (setq ggtags-global-start-file buffer-file-name)
    (setq ggtags-auto-jump-to-match-target
          (nth 4 (assoc (ggtags-global-search-id command default-directory)
                        ggtags-global-search-history)))
    (ggtags-navigation-mode +1)
    (ggtags-update-tags)
    (ggtags-with-current-project
      (with-current-buffer (with-display-buffer-no-window
                             (compilation-start command 'ggtags-global-mode))
        (setq-local ggtags-process-environment env)
        (setq ggtags-global-last-buffer (current-buffer))))))

(defun ggtags-find-tag-continue ()
  (interactive)
  (ggtags-ensure-global-buffer
    (ggtags-navigation-mode +1)
    (let ((split-window-preferred-function ggtags-split-window-function))
      (ignore-errors (compilation-next-error 1))
      (compile-goto-error))))

(defun ggtags-find-tag (cmd &rest args)
  (ggtags-check-project)
  (ggtags-global-start (apply #'ggtags-global-build-command cmd args)
                       (and (ggtags-sort-by-nearness-p) default-directory)))

(defun ggtags-include-file ()
  "Calculate the include file based on `ggtags-include-pattern'."
  (pcase ggtags-include-pattern
    (`nil nil)
    ((pred functionp)
     (funcall ggtags-include-pattern))
    (`(,re . ,sub)
     (save-excursion
       (beginning-of-line)
       (and (looking-at re) (match-string sub))))
    (_ (warn "Invalid value for `ggtags-include-pattern': %s"
             ggtags-include-pattern)
       nil)))

;;;###autoload
(defun ggtags-find-tag-dwim (name &optional what)
  "Find NAME by context.
If point is at a definition tag, find references, and vice versa.
If point is at a line that matches `ggtags-include-pattern', find
the include file instead.

When called interactively with a prefix arg, always find
definition tags."
  (interactive
   (let ((include (and (not current-prefix-arg) (ggtags-include-file))))
     (ggtags-ensure-project)
     (if include (list include 'include)
       (list (ggtags-read-tag 'definition current-prefix-arg)
             (and current-prefix-arg 'definition)))))
  (ggtags-check-project)    ; For `ggtags-current-project-root' below.
  (cond
   ((or (eq what 'definition)
        (not buffer-file-name)
        (not (ggtags-project-has-refs (ggtags-find-project)))
        (not (ggtags-project-file-p buffer-file-name)))
    (ggtags-find-definition name))
   (t (ggtags-find-tag
       (format "--from-here=%d:%s"
               (line-number-at-pos)
               (shell-quote-argument
                ;; Note `ggtags-find-tag' may bind `default-directory'
                ;; to project root.
                (funcall (if (ggtags-sort-by-nearness-p)
                             #'file-relative-name #'ggtags-project-relative-file)
                         buffer-file-name)))
       (shell-quote-argument name)))))

;; Another option for `M-.'.
(defun ggtags-find-definition (name)
  (interactive (list (ggtags-read-tag 'definition current-prefix-arg)))
  (ggtags-find-tag 'definition (shell-quote-argument name)))

(defun ggtags-quote-pattern (pattern)
  (prin1-to-string (substring-no-properties pattern)))

(defun ggtags-find-tag-regexp (regexp directory)
  "List tags matching REGEXP in DIRECTORY (default to project root).
When called interactively with a prefix, ask for the directory."
  (interactive
   (progn
     (ggtags-check-project)
     (list (ggtags-read-tag "" t "POSIX regexp")
           (if current-prefix-arg
               (read-directory-name "Directory: " nil nil t)
             (ggtags-current-project-root)))))
  (ggtags-check-project)
  (ggtags-global-start
   (ggtags-global-build-command nil nil "-l" "--" (ggtags-quote-pattern regexp))
   (file-name-as-directory directory)))

(defvar ggtags-navigation-mode)

(defun ggtags-foreach-file (fn)
  "Invoke FN with each file found.
FN is invoked while *ggtags-global* buffer is current."
  (ggtags-ensure-global-buffer
    (save-excursion
      (goto-char (point-min))
      (while (with-demoted-errors "compilation-next-error: %S"
               (compilation-next-error 1 'file)
               t)
        (funcall fn (caar
                     (compilation--loc->file-struct
                      (compilation--message->loc
                       (get-text-property (point) 'compilation-message)))))))))

(defun ggtags-global-normalise-command (cmd)
  (if (string-match
       (concat (regexp-quote (ggtags-global-build-command nil)) "\\s-*")
       cmd)
      (substring-no-properties cmd (match-end 0))
    cmd))

(defun ggtags-global-search-id (cmd directory)
  (sha1 (concat directory (make-string 1 0)
                (ggtags-global-normalise-command cmd))))

(defun ggtags-global-current-search ()
  ;; CMD DIR ENV LINE TEXT
  (ggtags-ensure-global-buffer
    (list (ggtags-global-normalise-command (car compilation-arguments))
          default-directory
          ggtags-process-environment
          (line-number-at-pos)
          (buffer-substring-no-properties
           (line-beginning-position) (line-end-position)))))

(defun ggtags-global-exit-message-1 ()
  "Get the total of matches and db file used."
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward
         "^\\w+ \\(not found\\)\\|^\\([0-9]+\\) \\w+ located" nil t)
        (cons (or (and (match-string 1) 0)
                  (string-to-number (match-string 2)))
              (when (re-search-forward
                     "using \\(?:\\(idutils\\)\\|'[^']*/\\(\\w+\\)'\\)"
                     (line-end-position)
                     t)
                (or (and (match-string 1) "ID")
                    (match-string 2))))
      (cons 0 nil))))

(defun ggtags-global-exit-message-function (_process-status exit-status msg)
  "A function for `compilation-exit-message-function'."
  (pcase (ggtags-global-exit-message-1)
    (`(,count . ,db)
     ;; Clear the start marker in case of zero matches.
     (and (zerop count)
          (markerp ggtags-global-start-marker)
          (not ggtags-global-continuation)
          (setq ggtags-global-start-marker nil))
     (cons (if (> exit-status 0)
               msg
             (format "found %d %s" count
                     (funcall (if (= count 1) #'car #'cadr)
                              (pcase db
                                ;; ` required for 24.1 and 24.2
                                (`"GTAGS"  '("definition" "definitions"))
                                (`"GSYMS"  '("symbol"     "symbols"))
                                (`"GRTAGS" '("reference"  "references"))
                                (`"GPATH"  '("file"       "files"))
                                (`"ID"     '("identifier" "identifiers"))
                                (_         '("match"      "matches"))))))
           exit-status))))

(defun ggtags-global-column (start)
  ;; START is the beginning position of source text.
  (let ((mbeg (text-property-any start (line-end-position) 'global-color t)))
    (and mbeg (- mbeg start))))

;;; NOTE: Must not match the 'Global started at Mon Jun 3 10:24:13'
;;; line or `compilation-auto-jump' will jump there and fail. See
;;; comments before the 'gnu' entry in
;;; `compilation-error-regexp-alist-alist'.
(defvar ggtags-global-error-regexp-alist-alist
  (append
   `((path "^\\(?:[^\"'\n]*/\\)?[^ )\t\n]+$" 0)
     ;; ACTIVE_ESCAPE   src/dialog.cc   172
     (ctags "^\\([^ \t\n]+\\)[ \t]+\\(.*?\\)[ \t]+\\([0-9]+\\)$"
            2 3 nil nil 2 (1 font-lock-function-name-face))
     ;; ACTIVE_ESCAPE     172 src/dialog.cc    #undef ACTIVE_ESCAPE
     (ctags-x "^\\([^ \t\n]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\(\\(?:[^/\n]*/\\)?[^ \t\n]+\\)"
              3 2 (,(lambda () (ggtags-global-column (1+ (match-end 0)))))
              nil 3 (1 font-lock-function-name-face))
     ;; src/dialog.cc:172:#undef ACTIVE_ESCAPE
     (grep "^\\(.+?\\):\\([0-9]+\\):\\(?:$\\|[^0-9\n]\\|[0-9][^0-9\n]\\|[0-9][0-9].\\)"
           1 2 (,(lambda () (ggtags-global-column (1+ (match-end 2))))) nil 1)
     ;; src/dialog.cc ACTIVE_ESCAPE 172 #undef ACTIVE_ESCAPE
     (cscope "^\\(.+?\\)[ \t]+\\([^ \t\n]+\\)[ \t]+\\([0-9]+\\).*\\(?:[^0-9\n]\\|[^0-9\n][0-9]\\|[^:\n][0-9][0-9]\\)$"
             1 3 nil nil 1 (2 font-lock-function-name-face)))
   compilation-error-regexp-alist-alist))

(defun ggtags-abbreviate-file (start end)
  (let ((inhibit-read-only t)
        (amount (if (numberp ggtags-global-abbreviate-filename)
                    (- (- end start) ggtags-global-abbreviate-filename)
                  999))
        (advance-word (lambda ()
                        "Return the length of the text made invisible."
                        (let ((wend (min end (progn (forward-word 1) (point))))
                              (wbeg (max start (progn (backward-word 1) (point)))))
                          (goto-char wend)
                          (if (<= (- wend wbeg) 1)
                              0
                            (put-text-property (1+ wbeg) wend 'invisible t)
                            (1- (- wend wbeg)))))))
    (goto-char start)
    (while (and (> amount 0) (> end (point)))
      (cl-decf amount (funcall advance-word)))))

(defun ggtags-abbreviate-files (start end)
  (goto-char start)
  (let* ((error-re (cdr (assq (car compilation-error-regexp-alist)
                              ggtags-global-error-regexp-alist-alist)))
         (sub (cadr error-re)))
    (when (and ggtags-global-abbreviate-filename error-re)
      (while (re-search-forward (car error-re) end t)
        (when (and (or (not (numberp ggtags-global-abbreviate-filename))
                       (> (length (match-string sub))
                          ggtags-global-abbreviate-filename))
                   ;; Ignore bogus file lines such as:
                   ;;     Global found 2 matches at Thu Jan 31 13:45:19
                   (get-text-property (match-beginning sub) 'compilation-message))
          (ggtags-abbreviate-file (match-beginning sub) (match-end sub)))))))

(defvar-local ggtags-global-output-lines 0)

(defun ggtags-global--display-buffer (&optional buffer desired-point)
  (pcase (let ((buffer (or buffer (current-buffer)))
               (split-window-preferred-function ggtags-split-window-function))
           (and (not (get-buffer-window buffer))
                (display-buffer buffer '(nil (allow-no-window . t)))))
    ((and (pred windowp) w)
     (with-selected-window w
       (compilation-set-window-height w)
       (and desired-point (goto-char desired-point))))))

(defun ggtags-global-filter ()
  "Called from `compilation-filter-hook' (which see)."
  (let ((ansi-color-apply-face-function
         (lambda (beg end face)
           (when face
             (ansi-color-apply-overlay-face beg end face)
             (put-text-property beg end 'global-color t)))))
    (ansi-color-apply-on-region compilation-filter-start (point)))
  ;; Get rid of line "Using config file '/PATH/TO/.globalrc'." or
  ;; "Using default configuration."
  (when (re-search-backward
         "^ *Using \\(?:config file '.*\\|default configuration.\\)\n"
         compilation-filter-start t)
    (replace-match ""))
  (cl-incf ggtags-global-output-lines
           (count-lines compilation-filter-start (point)))
  ;; If the number of output lines is small
  ;; `ggtags-global-handle-exit' takes care of displaying the buffer.
  (when (and (> ggtags-global-output-lines 30) ggtags-navigation-mode)
    (ggtags-global--display-buffer nil (or compilation-current-error (point-min))))
  (when (and (eq ggtags-auto-jump-to-match 'history)
             (numberp ggtags-auto-jump-to-match-target)
             (not compilation-current-error)
             ;; `ggtags-global-output-lines' is imprecise but use it
             ;; as first approximation.
             (> (+ 10 ggtags-global-output-lines) ggtags-auto-jump-to-match-target)
             (> (line-number-at-pos (point-max))
                ggtags-auto-jump-to-match-target))
    (ggtags-forward-to-line ggtags-auto-jump-to-match-target)
    (ggtags-delay-finish-functions
      (with-display-buffer-no-window
        (condition-case nil
            (let ((compilation-auto-jump-to-first-error t))
              (compilation-auto-jump (current-buffer) (point)))
          (error (message "\
ggtags: history match invalid, jump to first match instead")
                 (first-error)))))
    (setq-local ggtags-auto-jump-to-match-target nil)
    (run-with-idle-timer
     0 nil
     (lambda (buf pt)
       (and (buffer-live-p buf)
            (with-current-buffer buf (goto-char pt))))
     (current-buffer) (point)))
  (make-local-variable 'ggtags-global-large-output)
  (when (> ggtags-global-output-lines ggtags-global-large-output)
    (cl-incf ggtags-global-large-output 500)
    (ggtags-echo "Output %d lines (Type `C-c C-k' to cancel)"
                 ggtags-global-output-lines)))

(defun ggtags-global-handle-exit (buf how)
  "A function for `compilation-finish-functions' (which see)."
  (cond
   (ggtags-global-continuation
    (let ((cont (prog1 ggtags-global-continuation
                  (setq ggtags-global-continuation nil))))
      (funcall cont buf how)))
   ((string-prefix-p "exited abnormally" how)
    ;; If exit abnormally display the buffer for inspection.
    (ggtags-global--display-buffer)
    (when (save-excursion
            (goto-char (point-max))
            (re-search-backward
             (eval-when-compile
               (format "^global: %s not found.$"
                       (regexp-opt '("GTAGS" "GRTAGS" "GSYMS" "GPATH"))))
             nil t))
      (ggtags-echo "WARNING: Global tag files missing in `%s'"
                   ggtags-project-root)
      (remhash ggtags-project-root ggtags-projects)))
   (ggtags-auto-jump-to-match
    (if (pcase (compilation-next-single-property-change
                (point-min) 'compilation-message)
          ((and pt (guard pt))
           (compilation-next-single-property-change
            (save-excursion (goto-char pt) (end-of-line) (point))
            'compilation-message)))
        ;; There are multiple matches so pop up the buffer.
        (and ggtags-navigation-mode (ggtags-global--display-buffer))
      ;; For the `compilation-auto-jump' in idle timer to run.
      ;; See also: http://debbugs.gnu.org/13829
      (sit-for 0)
      (ggtags-navigation-mode -1)
      (ggtags-navigation-mode-cleanup buf 0)))))

(defvar compilation-always-kill)        ;new in 24.3

(define-compilation-mode ggtags-global-mode "Global"
  "A mode for showing outputs from gnu global."
  ;; Note: Place `ggtags-global-output-format' as first element for
  ;; `ggtags-abbreviate-files'.
  (setq-local compilation-error-regexp-alist (list ggtags-global-output-format))
  (when (markerp ggtags-global-start-marker)
    (setq ggtags-project-root
          (buffer-local-value 'ggtags-project-root
                              (marker-buffer ggtags-global-start-marker))))
  (pcase ggtags-auto-jump-to-match
    (`history (make-local-variable 'ggtags-auto-jump-to-match-target)
              (setq-local compilation-auto-jump-to-first-error
                          (not ggtags-auto-jump-to-match-target)))
    (`nil (setq-local compilation-auto-jump-to-first-error nil))
    (_ (setq-local compilation-auto-jump-to-first-error t)))
  (setq-local compilation-scroll-output nil)
  ;; See `compilation-move-to-column' for details.
  (setq-local compilation-first-column 0)
  (setq-local compilation-error-screen-columns nil)
  (setq-local compilation-disable-input t)
  (setq-local compilation-always-kill t)
  (setq-local compilation-error-face 'compilation-info)
  (setq-local compilation-exit-message-function
              'ggtags-global-exit-message-function)
  ;; See: https://github.com/leoliu/ggtags/issues/26
  (setq-local find-file-suppress-same-file-warnings t)
  (setq-local truncate-lines t)
  (jit-lock-register #'ggtags-abbreviate-files)
  (add-hook 'compilation-filter-hook 'ggtags-global-filter nil 'local)
  (add-hook 'compilation-finish-functions 'ggtags-global-handle-exit nil t)
  (add-hook 'kill-buffer-hook (lambda () (ggtags-navigation-mode -1)) nil t))

(defun ggtags-move-to-tag (&optional name)
  "Move to NAME tag in current line."
  (let ((tag (or name ggtags-current-tag-name)))
    ;; Do nothing if on the tag already i.e. by `ggtags-global-column'.
    (unless (or (not tag) (looking-at (concat (regexp-quote tag) "\\_>")))
      (let ((orig (point))
            (regexps (mapcar (lambda (fmtstr)
                               (format fmtstr (regexp-quote tag)))
                             '("\\_<%s\\_>" "%s\\_>" "%s"))))
        (beginning-of-line)
        (if (cl-loop for re in regexps
                     ;; Note: tag might not agree with current
                     ;; major-mode's symbol, so try harder. For
                     ;; example, in `php-mode' $cacheBackend is a
                     ;; symbol, but cacheBackend is a tag.
                     thereis (re-search-forward re (line-end-position) t))
            (goto-char (match-beginning 0))
          (goto-char orig))))))

(defun ggtags-navigation-mode-cleanup (&optional buf time)
  (let ((buf (or buf ggtags-global-last-buffer)))
    (and (buffer-live-p buf)
         (with-current-buffer buf
           (when (get-buffer-process (current-buffer))
             (kill-compilation))
           (when (and (derived-mode-p 'ggtags-global-mode)
                      (get-buffer-window))
             (quit-windows-on (current-buffer)))
           (and time (run-with-idle-timer time nil #'kill-buffer buf))))))

(defvar ggtags-global-line-overlay nil)

(defun ggtags-global-next-error-function ()
  (when (eq next-error-last-buffer ggtags-global-last-buffer)
    (ggtags-move-to-tag)
    (ggtags-global-save-start-marker)
    (and (ggtags-project-update-mtime-maybe)
         (message "File `%s' is newer than GTAGS"
                  (file-name-nondirectory buffer-file-name)))
    (and ggtags-mode-sticky (ggtags-mode 1))
    (ignore-errors
      (ggtags-ensure-global-buffer
        (unless (overlayp ggtags-global-line-overlay)
          (setq ggtags-global-line-overlay (make-overlay (point) (point)))
          (overlay-put ggtags-global-line-overlay 'face 'ggtags-global-line))
        (move-overlay ggtags-global-line-overlay
                      (line-beginning-position) (line-end-position)
                      (current-buffer))
        ;; Update search history
        (let ((id (ggtags-global-search-id (car compilation-arguments)
                                           default-directory)))
          (setq ggtags-global-search-history
                (cl-remove id ggtags-global-search-history :test #'equal :key #'car))
          (add-to-history 'ggtags-global-search-history
                          (cons id (ggtags-global-current-search))
                          ggtags-global-history-length))))
    (run-hooks 'ggtags-find-tag-hook)))

(define-minor-mode ggtags-navigation-mode nil
  :lighter nil
  :global t
  (if ggtags-navigation-mode
      (progn
        (add-hook 'next-error-hook 'ggtags-global-next-error-function))
    (remove-hook 'next-error-hook 'ggtags-global-next-error-function)))

(defun ggtags-after-save-function ()
  (when (ggtags-find-project)
    (ggtags-project-update-mtime-maybe)
    (and buffer-file-name ggtags-update-on-save
         (ggtags-update-tags-single buffer-file-name 'nowait))))

(defun ggtags-global-output (buffer cmds callback &optional cutoff)
  "Asynchronously pipe the output of running CMDS to BUFFER.
When finished invoke CALLBACK in BUFFER with process exit status."
  (or buffer (error "Output buffer required"))
  (let* ((program (car cmds))
         (args (cdr cmds))
         (cutoff (and cutoff (+ cutoff (if (get-buffer buffer)
                                           (with-current-buffer buffer
                                             (line-number-at-pos (point-max)))
                                         0))))
         (proc (apply #'start-file-process program buffer program args))
         (filter (lambda (proc string)
                   (and (buffer-live-p (process-buffer proc))
                        (with-current-buffer (process-buffer proc)
                          (goto-char (process-mark proc))
                          (insert string)
                          (when (and (> (line-number-at-pos (point-max)) cutoff)
                                     (process-live-p proc))
                            (interrupt-process (current-buffer)))))))
         (sentinel (lambda (proc _msg)
                     (when (memq (process-status proc) '(exit signal))
                       (with-current-buffer (process-buffer proc)
                         (set-process-buffer proc nil)
                         (funcall callback (process-exit-status proc)))))))
    (set-process-query-on-exit-flag proc nil)
    (and cutoff (set-process-filter proc filter))
    (set-process-sentinel proc sentinel)
    proc))

(cl-defun ggtags-fontify-code (code &optional (mode major-mode))
  (cl-check-type mode function)
  (cl-typecase code
    ((not string) code)
    (string (cl-labels ((prepare-buffer ()
                                        (with-current-buffer
                                            (get-buffer-create " *Code-Fontify*")
                                          (delay-mode-hooks (funcall mode))
                                          (setq font-lock-mode t)
                                          (funcall font-lock-function font-lock-mode)
                                          (setq jit-lock-mode nil)
                                          (current-buffer))))
              (with-current-buffer (prepare-buffer)
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert code)
                  (font-lock-default-fontify-region
                   (point-min) (point-max) nil))
                (buffer-string))))))

(defvar ggtags-mode-map
  (let ((map (make-sparse-keymap))
        (menu (make-sparse-keymap "Ggtags")))
    (define-key map "\M-." 'ggtags-find-tag-dwim)
    (define-key map (kbd "C-M-.") 'ggtags-find-tag-regexp)
    ;; Menu items
    (define-key map [menu-bar ggtags] (cons "Ggtags" menu))
    (define-key menu [custom-ggtags]
      '(menu-item "Customize Ggtags"
                  (lambda () (interactive) (customize-group 'ggtags))))
    (define-key menu [save-project]
      '(menu-item "Save project settings" ggtags-save-project-settings))
    (define-key menu [toggle-read-only]
      '(menu-item "Toggle project read-only" ggtags-toggle-project-read-only
                  :button (:toggle . buffer-read-only)))
    (define-key menu [sep2] menu-bar-separator)
    (define-key menu [pop-mark]
      '(menu-item "Pop mark" pop-tag-mark
                  :help "Pop to previous mark and destroy it"))
    (define-key menu [sep1] menu-bar-separator)
    (define-key menu [previous-error]
      '(menu-item "Previous match" previous-error))
    (define-key menu [next-error]
      '(menu-item "Next match" next-error))
    (define-key menu [find-tag-regexp]
      '(menu-item "Find tag matching regexp" ggtags-find-tag-regexp))
    (define-key menu [find-tag-continue]
      '(menu-item "Continue find tag" tags-loop-continue))
    (define-key menu [find-tag]
      '(menu-item "Find tag" ggtags-find-tag-dwim))
    (define-key menu [update-tags]
      '(menu-item "Update tag files" ggtags-update-tags
                  :visible (ggtags-find-project)))
    (define-key menu [run-gtags]
      '(menu-item "Run gtags" ggtags-create-tags
                  :visible (not (ggtags-find-project))))
    map))

;;;###autoload
(define-minor-mode ggtags-mode nil
  :lighter (:eval (if ggtags-navigation-mode "" " GG"))
  (ggtags-setup-highlight-tag-at-point ggtags-highlight-tag)
  (if ggtags-mode
      (progn
        (add-hook 'after-save-hook 'ggtags-after-save-function nil t)
        ;; Append to serve as a fallback method.
        (add-hook 'completion-at-point-functions
                  #'ggtags-completion-at-point t t))
    (remove-hook 'after-save-hook 'ggtags-after-save-function t)
    (remove-hook 'completion-at-point-functions #'ggtags-completion-at-point t)
    (ggtags-cancel-highlight-tag-at-point 'keep-timer)))

(put 'ggtags-active-tag 'face 'ggtags-highlight)

(defun ggtags-setup-highlight-tag-at-point (flag)
  (cond ((null flag) (ggtags-cancel-highlight-tag-at-point))
        ((not (timerp ggtags-highlight-tag-timer))
         (setq ggtags-highlight-tag-timer
               (run-with-idle-timer flag t #'ggtags-highlight-tag-at-point)))
        (t (timer-set-idle-time ggtags-highlight-tag-timer flag t))))

(defun ggtags-cancel-highlight-tag-at-point (&optional keep-timer)
  (when (and (not keep-timer)
             (timerp ggtags-highlight-tag-timer))
    (cancel-timer ggtags-highlight-tag-timer)
    (setq ggtags-highlight-tag-timer nil))
  (when ggtags-highlight-tag-overlay
    (delete-overlay ggtags-highlight-tag-overlay)
    (setq ggtags-highlight-tag-overlay nil)))

(defun ggtags-highlight-tag-at-point ()
  (when (and ggtags-mode ggtags-project-root (ggtags-find-project))
    (unless (overlayp ggtags-highlight-tag-overlay)
      (setq ggtags-highlight-tag-overlay (make-overlay (point) (point) nil t))
      (overlay-put ggtags-highlight-tag-overlay 'modification-hooks
                   (list (lambda (o after &rest _args)
                           (and (not after) (delete-overlay o))))))
    (let ((bounds (funcall ggtags-bounds-of-tag-function))
          (o ggtags-highlight-tag-overlay))
      (cond
       ((and bounds
             (eq (overlay-buffer o) (current-buffer))
             (= (overlay-start o) (car bounds))
             (= (overlay-end o) (cdr bounds)))
        ;; Overlay matches current tag so do nothing.
        nil)
       ((and bounds (let ((completion-ignore-case nil))
                      (test-completion
                       (buffer-substring (car bounds) (cdr bounds))
                       ggtags-completion-table)))
        (move-overlay o (car bounds) (cdr bounds) (current-buffer))
        (overlay-put o 'category 'ggtags-active-tag))
       (t (move-overlay o
                        (or (car bounds) (point))
                        (or (cdr bounds) (point))
                        (current-buffer))
          (overlay-put o 'category nil))))))

(provide 'misc-tags)
;;; misc-gtags.el ends here
