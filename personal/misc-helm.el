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

(prelude-require-packages '(helm helm-projectile))

(helm-autoresize-mode 1)

(custom-set-variables '(helm-completion-style 'emacs)
                      '(completion-styles '(flex)) ; For fuzzy completion
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

;; Search
(global-set-key (kbd "M-g") 'helm-do-ag-this-file)
(global-set-key (kbd "M-G") 'helm-projectile-rg)

(global-set-key (kbd "M-z") 'helm-resume)
(global-set-key (kbd "M-x") 'helm-M-x)

(define-key helm-map (kbd "C-v") 'helm-next-page)
(define-key helm-map (kbd "M-v") 'helm-previous-page)
(define-key helm-map (kbd "C-w") 'backward-kill-word)
(define-key helm-map (kbd "C-M-v") nil)

;; https://www.reddit.com/r/emacs/comments/345vtl/make_helm_window_at_the_bottom_without_using_any/
(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))

(provide 'misc-helm)
;;; misc-helm.el ends here
