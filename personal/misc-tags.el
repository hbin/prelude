;; misc-tags.el --- Navigation between method definitions
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.
;; This file is a variant version of `etags-select` which created by
;; Scott Frazer.

;;; Code:
(prelude-require-package 'eproject)

(require 'eproject)
(require 'custom)
(require 'etags)

;; reread TAGS without querying if it has changed.
(setq tags-revert-without-query t)

;;; Custom stuff

;;;###autoload
(defgroup etags-select-mode nil
  "*etags select mode."
  :group 'etags)

;;;###autoload
(defcustom etags-select-mode-hook nil
  "*List of functions to call on entry to etags-select-mode mode."
  :group 'etags-select-mode
  :type 'hook)

;;;###autoload
(defcustom etags-select-highlight-tag-after-jump t
  "*If non-nil, temporarily highlight the tag after you jump to it."
  :group 'etags-select-mode
  :type 'boolean)

;;;###autoload
(defcustom etags-select-highlight-delay 1.0
  "*How long to highlight the tag."
  :group 'etags-select-mode
  :type 'number)

;;;###autoload
(defface etags-select-highlight-tag-face
  '((t (:foreground "white" :background "cadetblue4" :bold t)))
  "Font Lock mode face used to highlight tags."
  :group 'etags-select-mode)

;;;###autoload
(defcustom etags-select-use-short-name-completion nil
  "*Use short tag names during completion.  For example, say you
have a function named foobar in several classes and you invoke
`etags-select-find-tag'.  If this variable is nil, you would have
to type ClassA::foo<TAB> to start completion.  Since avoiding
knowing which class a function is in is the basic idea of this
package, if you set this to t you can just type foo<TAB>.

Only works with GNU Emacs."
  :group 'etags-select-mode
  :type 'boolean)

;;;###autoload
(defcustom etags-select-go-if-unambiguous nil
  "*If non-nil, jump by tag number if it is unambiguous."
  :group 'etags-select-mode
  :type 'boolean)

 ;;; Variables

(defvar etags-select-buffer-name "*etags-select*"
  "etags-select buffer name.")

(defvar etags-select-mode-font-lock-keywords nil
  "etags-select font-lock-keywords.")

(defvar etags-select-source-buffer nil
  "etags-select source buffer tag was found from.")

(defconst etags-select-non-tag-regexp "\\(\\s-*$\\|In:\\|Finding tag:\\)"
  "etags-select non-tag regex.")

;;; Functions

(fset 'etags-select-match-string 'match-string-no-properties)

(defun etags-select-case-fold-search ()
  "Get case-fold search."
  (when (boundp 'tags-case-fold-search)
    (if (memq tags-case-fold-search '(nil t))
        tags-case-fold-search
      case-fold-search)))

(defun etags-select-insert-matches (tagname tag-file tag-count)
  "Insert matches to tagname in tag-file."
  (let ((tag-table-buffer (etags-select-get-tag-table-buffer tag-file))
        (tag-file-path (file-name-directory tag-file))
        (tag-regex (concat "\" tagname "\"))
        (case-fold-search (etags-select-case-fold-search))
        full-tagname tag-line filename current-filename)
    (set-buffer tag-table-buffer)
    (modify-syntax-entry ?_ "w")

    (goto-char (point-min))
    (while (search-forward tagname nil t)
      (beginning-of-line)
      (when (search-forward tag-regex (point-at-eol) 'goto-eol)
        (setq full-tagname (or (etags-select-match-string 2) tagname))
        (setq tag-count (1+ tag-count))
        (beginning-of-line)
        (re-search-forward "\\s-*\\(.*?\\)\\s-*\^?")
        (setq tag-line (etags-select-match-string 1))
        (end-of-line)
        (save-excursion
          (re-search-backward "\f")
          (re-search-forward "^\\(.*?\\),")
          (setq filename (etags-select-match-string 1))
          (unless (file-name-absolute-p filename)
            (setq filename (concat tag-file-path filename))))
        (save-excursion
          (set-buffer etags-select-buffer-name)
          (when (not (string= filename current-filename))
            (insert "\nIn: " filename "\n")
            (setq current-filename filename))
          (insert (int-to-string tag-count) " [" full-tagname "] " tag-line "\n"))))
    (modify-syntax-entry ?_ "_")
    tag-count))

(defun etags-select-get-tag-table-buffer (tag-file)
  "Get tag table buffer for a tag file."
  (visit-tags-table-buffer tag-file)
  (get-file-buffer tag-file))

;;;###autoload
(defun etags-select-find-tag-at-point ()
  "Do a find-tag-at-point, and display all exact matches.  If only one match is
found, don't open the selection window."
  (interactive)
  (etags-select-find (find-tag-default)))

;;;###autoload
(defun etags-select-find-tag ()
  "Do a find-tag, and display all exact matches.  If only one match is
found, don't open the selection window."
  (interactive)
  (let* ((default (find-tag-default))
         (tagname (completing-read
                   (format "Find tag (default %s): " default)
                   (lambda (string predicate what)
                     (etags-select-complete-tag string predicate what (buffer-name)))
                   nil nil nil 'find-tag-history default)))
    (etags-select-find tagname)))

(defun etags-select-complete-tag (string predicate what buffer)
  "Tag completion."
  (etags-select-build-completion-table buffer)
  (if (eq what t)
      (all-completions string (etags-select-get-completion-table) predicate)
    (try-completion string (etags-select-get-completion-table) predicate)))

(defun etags-select-build-completion-table (buffer)
  "Build tag completion table."
  (save-excursion
    (set-buffer buffer)
    (let ((tag-files (etags-select-get-tag-files)))
      (mapcar (lambda (tag-file) (etags-select-get-tag-table-buffer tag-file)) tag-files))))

(defun etags-select-get-tag-files ()
  "Get tag files."
  (mapcar 'tags-expand-table-name tags-table-list))

(defun etags-select-get-completion-table ()
  "Get the tag completion table."
  (tags-completion-table))

(defun etags-select-tags-completion-table-function ()
  "Short tag name completion."
  (let ((table (make-vector 16383 0))
        (tag-regex "^.*?\\(\^?\\(.+\\)\^A\\|\\<\\(.+\\)[ \f\t()=,;]*\^?[0-9,]\\)")
        (progress-reporter
         (make-progress-reporter
          (format "Making tags completion table for %s..." buffer-file-name)
          (point-min) (point-max))))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at tag-regex)
          (intern (replace-regexp-in-string ".*[:.']" "" (or (match-string 2) (match-string 3))) table))
        (forward-line 1)
        (progress-reporter-update progress-reporter (point))))
    table))

(defadvice etags-recognize-tags-table (after etags-select-short-name-completion activate)
  "Turn on short tag name completion (maybe)"
  (when etags-select-use-short-name-completion
    (setq tags-completion-table-function 'etags-select-tags-completion-table-function)))

(defun etags-select-find (tagname)
  "Finding TAGNAME finding function."
  (when tagname
    (let ((tag-files (etags-select-get-tag-files))
          (tag-count 0))
      (setq etags-select-source-buffer (buffer-name))
      (get-buffer-create etags-select-buffer-name)
      (set-buffer etags-select-buffer-name)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "Finding tag: " tagname "\n")
      (mapc (lambda (tag-file)
              (setq tag-count (etags-select-insert-matches tagname tag-file tag-count)))
            tag-files)
      (cond ((= tag-count 0)
             (message (concat "No matches for tag \"" tagname "\""))
             (ding))
            ((= tag-count 1)
             (set-buffer etags-select-buffer-name)
             (goto-char (point-min))
             (etags-select-next-tag)
             (etags-select-goto-tag))
            (t
             (set-buffer etags-select-buffer-name)
             (goto-char (point-min))
             (etags-select-next-tag)
             (set-buffer-modified-p nil)
             (setq buffer-read-only t)
             (switch-to-buffer etags-select-buffer-name)
             (etags-select-mode tagname))))))

(defun etags-select-goto-tag (&optional other-window)
  "Goto the file/line of the tag under the cursor."
  (interactive "P")
  (let ((case-fold-search (etags-select-case-fold-search))
        tagname tag-point text-to-search-for filename filename-point (search-count 1))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "Finding tag: \\(.*\\)$")
      (setq tagname (etags-select-match-string 1)))
    (beginning-of-line)
    (if (looking-at etags-select-non-tag-regexp)
        (message "Please put the cursor on a line with the tag.")
      (setq tag-point (point))
      (setq overlay-arrow-position (point-marker))
      (re-search-forward "\\]\\s-+\\(.+?\\)\\s-*$")
      (setq text-to-search-for (regexp-quote (etags-select-match-string 1)))
      (goto-char tag-point)
      (re-search-backward "^In: \\(.*\\)$")
      (setq filename (etags-select-match-string 1))
      (setq filename-point (point))
      (goto-char tag-point)
      (while (re-search-backward (concat "^.*?\\]\\s-+" text-to-search-for "\\( +\\|#.*\\| +#.*\\)?$") filename-point t)
        (setq search-count (1+ search-count)))
      (goto-char tag-point)
      (switch-to-buffer etags-select-source-buffer)
      (ring-insert find-tag-marker-ring (point-marker)))
    (if other-window
        (find-file-other-window filename)
      (find-file filename))
    (goto-char (point-min))
    (while (> search-count 0)
      (unless (re-search-forward (concat "^\\s-*" text-to-search-for "\\( +\\|#.*\\| +#.*\\)?$") nil t)
        (message "TAGS file out of date ... stopping at closest match")
        (setq search-count 1))
      (setq search-count (1- search-count)))
    (beginning-of-line)
    (re-search-forward tagname)
    (goto-char (match-beginning 0))
    (when etags-select-highlight-tag-after-jump
      (etags-select-highlight (match-beginning 0) (match-end 0)))))

(defun etags-select-highlight (beg end)
  "Highlight a region temporarily."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face 'etags-select-highlight-tag-face)
    (sit-for etags-select-highlight-delay)
    (delete-overlay ov)))

(defun etags-select-goto-tag-other-window ()
  "Goto the file/line of the tag under the cursor in other window."
  (interactive)
  (etags-select-goto-tag t))

(defun etags-select-next-tag ()
  "Move to next tag in buffer."
  (interactive)
  (beginning-of-line)
  (when (not (eobp))
    (forward-line))
  (while (and (looking-at etags-select-non-tag-regexp) (not (eobp)))
    (forward-line))
  (when (eobp)
    (ding)))

(defun etags-select-previous-tag ()
  "Move to previous tag in buffer."
  (interactive)
  (beginning-of-line)
  (when (not (bobp))
    (forward-line -1))
  (while (and (looking-at etags-select-non-tag-regexp) (not (bobp)))
    (forward-line -1))
  (when (bobp)
    (ding)))

(defun etags-select-quit ()
  "Quit etags-select buffer."
  (interactive)
  (kill-buffer nil))

(defun etags-select-by-tag-number (number)
  "Select a tag by NUMBER."
  (let ((current-point (point)) tag-num)
    (if (and etags-select-go-if-unambiguous (not (re-search-forward (concat "^" number) nil t 2)))
        (setq tag-num number)
      (setq tag-num (read-from-minibuffer "Tag number? " number)))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" tag-num) nil t)
        (etags-select-goto-tag)
      (goto-char current-point)
      (message (concat "Couldn't find tag number " tag-num))
      (ding))))

(defvar etags-select-mode-map nil "'etags-select-mode' keymap.")
(if (not etags-select-mode-map)
    (let ((map (make-keymap)))
      (define-key map [(return)] 'etags-select-goto-tag)
      (define-key map [(ctrl return)] 'etags-select-goto-tag-other-window)
      (define-key map "n" 'etags-select-next-tag)
      (define-key map "p" 'etags-select-previous-tag)
      (define-key map "q" 'etags-select-quit)
      (define-key map "0" (lambda () (interactive) (etags-select-by-tag-number "0")))
      (define-key map "1" (lambda () (interactive) (etags-select-by-tag-number "1")))
      (define-key map "2" (lambda () (interactive) (etags-select-by-tag-number "2")))
      (define-key map "3" (lambda () (interactive) (etags-select-by-tag-number "3")))
      (define-key map "4" (lambda () (interactive) (etags-select-by-tag-number "4")))
      (define-key map "5" (lambda () (interactive) (etags-select-by-tag-number "5")))
      (define-key map "6" (lambda () (interactive) (etags-select-by-tag-number "6")))
      (define-key map "7" (lambda () (interactive) (etags-select-by-tag-number "7")))
      (define-key map "8" (lambda () (interactive) (etags-select-by-tag-number "8")))
      (define-key map "9" (lambda () (interactive) (etags-select-by-tag-number "9")))
      (setq etags-select-mode-map map)))

(defun etags-select-mode (tagname)
  "The etags-select-mode is a mode for browsing through the TAGNAME."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'etags-select-mode)
  (setq mode-name "etags-select")
  (set-syntax-table text-mode-syntax-table)
  (use-local-map etags-select-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq etags-select-mode-font-lock-keywords
        (list (list "^\\(Finding tag:\\)" '(1 font-lock-keyword-face))
              (list "^\\(In:\\) \\(.*\\)" '(1 font-lock-keyword-face) '(2 font-lock-string-face))
              (list "^[0-9]+ \\[\\(.+?\\)\\]" '(1 font-lock-type-face))
              (list tagname '(0 font-lock-function-name-face))))
  (setq font-lock-defaults '(etags-select-mode-font-lock-keywords))
  (setq overlay-arrow-position nil)
  (run-hooks 'etags-select-mode-hook))

(defun thing-after-point ()
  "Things after point, including current symbol."
  (if (thing-at-point 'symbol)
      (save-excursion
        (let ((from (beginning-of-thing 'symbol))
              (to   (end-of-thing 'line)))
          (and (> to from)
               (buffer-substring-no-properties from to))))))

(defun ruby-thing-at-point ()
  "Get ruby thing at point.
   1. thing at 'current_user'   get current_user;
   2. thing at '!current_user'  get current_user;
   3. thing at 'current_user!'  get current_user!;
   4. thing at 'current_user='  get current_user=;
   5. thing at 'current_user =' get current_user=;
   6. thing at 'current_user ==' get current_user;
   7. thing at 'current_user ||=' get current_user=;
   Otherwise, get `find-tag-default symbol."
  (if (member (symbol-name major-mode)
              '("ruby-mode" "rhtml-mode" "haml-mode" "slim-mode"))
      (let ((symbol (thing-at-point 'symbol))
            (remain (thing-after-point)))
        (if (and symbol remain)
            (let ((sym (s-chop-prefixes '("!!" "!") symbol))
                  (rem (s-chop-prefixes '("!!" "!") remain)))
              (if (s-matches? (concat "^" sym "\\( *\\(||\\)?=[^=]\\)") rem)
                  (concat sym "=")
                sym))
          (find-tag-default)))
    (find-tag-default)))

(defun visit-project-tags ()
  "Visit the TAGS file in the project root."
  (let ((tags-file (concat (eproject-root) "TAGS")))
    (visit-tags-table tags-file)
    (message (concat "Loaded " tags-file))))

(defun hbin-build-ctags ()
  "Build ctags file at the root of current project."
  (interactive)
  (let ((root (eproject-root)))
    (shell-command
     (concat "ctags -e -R --extra=+fq "
             "--exclude=db --exclude=doc --exclude=log --exclude=tmp --exclude=.git --exclude=public "
             "-f " root "TAGS " root)))
  (visit-project-tags)
  (message "TAGS built successfully"))

(defun hbin-etags-find-tag ()
  "Borrow from http://mattbriggs.net/blog/2012/03/18/awesome-emacs-plugins-ctags."
  (interactive)
  (if (file-exists-p (concat (eproject-root) "TAGS"))
      (visit-project-tags)
    (hbin-build-ctags))
  (etags-select-find (ruby-thing-at-point)))

(global-set-key (kbd "M-.") 'hbin-etags-find-tag)

(provide 'misc-tags)
;;; misc-tags.el ends here
