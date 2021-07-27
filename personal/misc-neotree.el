;; misc-neotree.el --- Neotree
;;
;; Copyright (C) 2012-2017 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

(prelude-require-packages '(neotree all-the-icons))

(require 'all-the-icons)
(setq all-the-icons-scale-factor 1)

(require 'neotree)
(setq neo-banner-message "\" Press ? for help.")
(setq neo-smart-open t)
(setq neo-window-width 30)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(add-to-list 'neo-hidden-regexp-list "TAGS\\|GPATH\\|GRTAGS\\|GTAGS")

(defun neotree-projectile-file ()
  (interactive)
  (cond
   ((and (fboundp 'projectile-project-root)
         (buffer-file-name)
         (projectile-file-exists-p (buffer-file-name)))
    (let ((root (projectile-project-root))
          (file (buffer-file-name)))
      (neotree-dir root)
      (neotree-find file)))
   (t
    (error "You're not in a project"))))

(define-key neotree-mode-map (kbd "o") 'neotree-enter)
(define-key neotree-mode-map (kbd "R") 'neotree-refresh)
(define-key neotree-mode-map (kbd "r") 'neotree-refresh)
(define-key neotree-mode-map (kbd "s") (neotree-make-executor
                                        :file-fn 'neo-open-file-vertical-split))
(define-key neotree-mode-map (kbd "i") (neotree-make-executor
                                        :file-fn 'neo-open-file-horizontal-split))
(define-key neotree-mode-map (kbd "q") 'neotree-hide)
(define-key neotree-mode-map (kbd "k") 'neotree-select-previous-sibling-node)
(define-key neotree-mode-map (kbd "j") 'neotree-select-next-sibling-node)
(define-key neotree-mode-map (kbd "I") 'neotree-hidden-file-toggle)
(define-key neotree-mode-map (kbd "C") 'neotree-change-root)
(define-key neotree-mode-map (kbd "m a") 'neotree-create-node)
(define-key neotree-mode-map (kbd "m c") 'neotree-copy-node)
(define-key neotree-mode-map (kbd "m d") 'neotree-delete-node)
(define-key neotree-mode-map (kbd "m m") 'neotree-rename-node)

(add-hook 'neotree-mode-hook (lambda () (hl-line-mode 1)))

(global-set-key (kbd "C-c C-f") 'neotree-projectile-file)
(global-set-key (kbd "C-c C-s") 'neo-global--select-window)

(define-key projectile-mode-map (kbd "C-c C-f")  'neotree-projectile-file)
(define-key projectile-mode-map (kbd "C-c C-s") 'neo-global--select-window)

(provide 'misc-neotree)
;;; misc-neotree.el ends here
