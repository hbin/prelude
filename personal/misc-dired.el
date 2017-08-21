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

(provide 'misc-dired)
;;; misc-dired.el ends here
