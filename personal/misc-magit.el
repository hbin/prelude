;; misc-magit.el --- Configuration for magit
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.
;; Git for Emacs - awesome extension!

;;; Code:
(require 'magit)

;; magit diff 时颜色区别
(eval-after-load 'magit
  (progn
    (set-face-foreground 'magit-diff-add "green4")
    (set-face-foreground 'magit-diff-del "red")))

;; diff-mode 也一样
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red")))

;;; Need flyspell against my poor english
(add-hook 'magit-log-edit-mode-hook 'turn-on-flyspell)

(defadvice magit-status (around magit-fullscreen activate)
  "Full screen magit-status."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Back to previous window."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(defun magit-toggle-whitespace ()
  "Toggle ignore whitespace."
  (interactive)
  (if (member "-w" magit-diff-options)
      (setq magit-diff-options (remove "-w" magit-diff-options))
    (add-to-list 'magit-diff-options "-w"))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

;;; Frontend for git blame
(defalias 'git-blame 'vc-annotate)

(provide 'misc-magit)
;;; misc-magit.el ends here
