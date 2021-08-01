;; misc-projectile.el --- Configuration for Projectile
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

;; VARS---------------------
(defvar projectile-mode-map nil)

(prelude-require-packages '(projectile))

;;; Customizes
(custom-set-variables
 '(projectile-enable-idle-timer nil)
 '(projectile-ignored-projects '("/usr/local/")))
(remove-hook 'projectile-idle-timer-hook 'projectile-regenerate-tags) ;; No need to regenerate the project's [e|g]tags.

;;; Keybindings
(define-key projectile-mode-map (kbd "M-e") 'projectile-recentf)
(define-key projectile-mode-map (kbd "M-t") 'projectile-find-file)
(define-key projectile-mode-map (kbd "M-T") 'projectile-find-file-other-window)
(define-key projectile-mode-map (kbd "M-R") 'projectile-replace)
(define-key projectile-mode-map (kbd "C-c d") 'projectile-find-dir)
(define-key projectile-mode-map (kbd "C-c D") 'projectile-dired)
(define-key projectile-mode-map (kbd "C-c C-p") 'projectile-switch-project)

;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-projectile)

;;; misc-projectile.el ends here
