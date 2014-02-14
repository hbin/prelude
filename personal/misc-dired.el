;; misc-dired.el --- Extends build-in dired
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

(prelude-require-packages '(dired+ dired-details dired-details+))

(custom-set-variables
 '(dired-dwim-target t)
 '(dired-recursive-copies (quote top))
 '(dired-recursive-deletes (quote top)))

(defun hbin-dired-mode-setup ()
  (define-key dired-mode-map (kbd "`") 'dired-clean-directory)
  (define-key dired-mode-map (kbd ".") 'dired-omit-mode)
  (define-key dired-mode-map (kbd "M-o") 'other-window)
  (define-key dired-mode-map (kbd "/") 'diredp-omit-marked)
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))

  ;; Dired reuse directory buffer
  (toggle-diredp-find-file-reuse-dir 1)

  (setq dired-omit-files
        (rx (or (seq bol "#")
                (seq bol ".")
                (seq "~" eol))))
  (setq dired-omit-extensions
        (append dired-omit-extensions
                (list
                 ".temp"
                 ".bak"
                 ))))

(defun hbin-dired-mode-init ()
  (hl-line-mode 1)
  (dired-omit-mode 1))

(eval-after-load "dired" '(hbin-dired-mode-setup))
(add-hook 'dired-mode-hook 'hbin-dired-mode-init)

;;; Project explorer tree
(prelude-require-package 'project-explorer)
(custom-set-variables '(pe/width 30))
(require 'project-explorer)

(defun pe/return (&optional arg)
  "ARG stands for file on the cursor."
  (interactive)
  (if (file-directory-p (pe/user-get-filename))
      (pe/tab arg)
    (pe/find-file arg)))

(custom-set-variables '(pe/width 30)
                      '(pe/omit-regex "^\\.\\|^#\\|~$\\|\\.elc$"))

(define-key project-explorer-mode-map (kbd "I") 'pe/toggle-omit)
(define-key project-explorer-mode-map (kbd "C") 'pe/change-directory)
(define-key project-explorer-mode-map (kbd "o") 'pe/return)
(define-key project-explorer-mode-map (kbd "m a") 'pe/create-file)
(define-key project-explorer-mode-map (kbd "m d ") 'pe/delete-file)
(define-key project-explorer-mode-map (kbd "m c") 'pe/copy-file)
(global-set-key (kbd "C-x C-j") 'project-explorer-open)

(provide 'misc-dired)
;;; misc-dired.el ends here
