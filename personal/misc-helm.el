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

(prelude-require-packages '(helm-swoop))

(helm-autoresize-mode 1)

(custom-set-variables '(helm-completion-style 'emacs)
                      '(helm-buffers-fuzzy-matching t)
                      '(helm-recentf-fuzzy-match t)
                      '(helm-semantic-fuzzy-match t)
                      '(helm-imenu-fuzzy-match t)
                      '(helm-autoresize-min-height 1)
                      '(helm-ag-use-agignore t)
                      '(helm-ag-use-grep-ignore-list t)
                      '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
                      '(helm-ag-command-option "--all-text")
                      '(helm-ag-insert-at-point 'symbol)
                      '(helm-swoop-split-with-multiple-windows t))

;; See: https://github.com/emacsorphanage/helm-ag/issues/283
(defun helm-projectile-ag (&optional options)
  "Helm version of projectile-ag."
  (interactive (if current-prefix-arg (list (read-string "option: " "" 'helm-ag--extra-options-history))))
  (if (require 'helm-ag nil  'noerror)
      (if (projectile-project-p)
          (let ((helm-ag-command-option options)
                (current-prefix-arg nil))
            (helm-do-ag (projectile-project-root) (car (projectile-parse-dirconfig-file))))
        (error "You're not in a project"))
    (error "helm-ag not available")))

(require 'helm-swoop)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-multi-swoop-projectile)
(global-set-key (kbd "C-x C-i") 'helm-imenu)
(global-set-key (kbd "C-c C-i") 'helm-imenu-anywhere)
(global-set-key (kbd "M-g") 'helm-projectile-ag)
(global-set-key (kbd "M-G") 'helm-do-ag-this-file)

(define-key helm-map (kbd "C-w") 'backward-kill-word-or-region)
(define-key helm-map (kbd "C-M-v") 'helm-previous-page)
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

(global-set-key (kbd "M-z") 'helm-resume)

;; https://www.reddit.com/r/emacs/comments/345vtl/make_helm_window_at_the_bottom_without_using_any/
(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))

(provide 'misc-helm)
;;; misc-helm.el ends here
