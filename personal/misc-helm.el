;; misc-helm.el --- Configuration for Helm
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file is not part of GNU Emacs.
;; Auto Completion - http://cx4a.org/software/auto-complete/manual.html#Introduction

;;; Code:

(helm-autoresize-mode 1)

(setq helm-M-x-fuzzy-match t)
(setq helm-recentf-fuzzy-match t)

;; https://www.reddit.com/r/emacs/comments/345vtl/make_helm_window_at_the_bottom_without_using_any/
(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))

(provide 'misc-helm)
;;; misc-helm.el ends here
