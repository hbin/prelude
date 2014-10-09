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
(prelude-require-packages '(projectile))
(require 'projectile)

;;; Keybindings
(define-key prelude-mode-map [?\s-d] nil)
(define-key prelude-mode-map [?\s-p] nil)
(define-key prelude-mode-map [?\s-f] nil)
(define-key prelude-mode-map [?\s-g] nil)

(define-key projectile-mode-map (kbd "M-t") 'projectile-find-file)
(define-key projectile-mode-map (kbd "M-T") 'projectile-find-file-other-window)
(define-key projectile-mode-map (kbd "M-g") 'projectile-ack)
(define-key projectile-mode-map (kbd "M-R") 'projectile-replace)
(define-key projectile-mode-map (kbd "C-x d") 'projectile-find-dir)
(define-key projectile-mode-map (kbd "C-x D") 'projectile-dired)
(define-key projectile-mode-map (kbd "C-x C-i") 'projectile-find-tag)
(define-key projectile-mode-map (kbd "C-x C-p") 'projectile-switch-project)

;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-projectile)

;;; misc-projectile.el ends here
