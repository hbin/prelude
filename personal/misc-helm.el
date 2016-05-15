;; misc-helm.el --- Configuration for Helm
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

(helm-autoresize-mode 1)

(setq grep-find-ignored-files
      (append grep-find-ignored-files
              '("*.svg")))

(custom-set-variables '(helm-M-x-fuzzy-match t)
                      '(helm-buffers-fuzzy-matching t)
                      '(helm-recentf-fuzzy-match t)
                      '(helm-semantic-fuzzy-match t)
                      '(helm-imenu-fuzzy-match t)
                      '(helm-autoresize-min-height 1)
                      '(helm-ag-use-agignore t)
                      '(helm-ag-use-grep-ignore-list t)
                      '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
                      '(helm-ag-command-option "--all-text")
                      '(helm-ag-insert-at-point 'symbol))

(global-set-key (kbd "M-z") 'helm-resume)

;; https://www.reddit.com/r/emacs/comments/345vtl/make_helm_window_at_the_bottom_without_using_any/
(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))

(provide 'misc-helm)
;;; misc-helm.el ends here
