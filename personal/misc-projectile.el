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
(defvar prelude-mode-map nil)
(defvar projectile-mode-map nil)

(prelude-require-packages '(projectile))

;;; Customizes
(custom-set-variables
 '(projectile-enable-idle-timer nil)
 '(projectile-ignored-projects '("/usr/local/")))
(remove-hook 'projectile-idle-timer-hook 'projectile-regenerate-tags) ;; No need to regenerate the project's [e|g]tags.

;;; Keybindings
(define-key prelude-mode-map [?\s-d] nil)
(define-key prelude-mode-map [?\s-p] nil)
(define-key prelude-mode-map [?\s-f] nil)
(define-key prelude-mode-map [?\s-g] nil)

(define-key projectile-mode-map (kbd "M-t") 'projectile-find-file)
(define-key projectile-mode-map (kbd "M-T") 'projectile-find-file-other-window)
(define-key projectile-mode-map (kbd "M-g") 'helm-projectile-ag)
(define-key projectile-mode-map (kbd "M-G") 'helm-do-ag-this-file)
(define-key projectile-mode-map (kbd "M-R") 'projectile-replace)
(define-key projectile-mode-map (kbd "C-x d") 'projectile-find-dir)
(define-key projectile-mode-map (kbd "C-x D") 'projectile-dired)
(define-key projectile-mode-map (kbd "C-x C-p") 'projectile-switch-project)

;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-projectile)

;;; misc-projectile.el ends here
