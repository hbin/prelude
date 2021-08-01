;; misc-tree.el --- Tree
;;
;; Copyright (C) 2012-2017 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

(prelude-require-packages '(treemacs treemacs-projectile))
(require 'treemacs)
(require 'treemacs-projectile)

(setq treemacs-show-hidden-files nil
      treemacs--icon-size 10)

(treemacs-follow-mode -1)

;;; I'd prefer simple directory icon
(treemacs-modify-theme "Default"
  :icon-directory "/other/icons/dir"
  :config
  (progn
    (treemacs-create-icon :icon "+ " :extensions (dir-closed))
    (treemacs-create-icon :icon "- " :extensions (dir-open))))

;; (global-set-key (kbd "C-c C-n") 'treemacs)
;; (global-set-key (kbd "C-x C-j") 'treemacs-add-and-display-current-project)

(provide 'misc-tree)
;;; misc-tree.el ends here
