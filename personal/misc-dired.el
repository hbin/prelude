;; misc-dired.el --- Extends build-in dired
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

(prelude-require-packages '(dired+ dired-details dired-details+))

(custom-set-variables
 '(dired-dwim-target t)
 '(dired-recursive-copies (quote top))
 '(dired-recursive-deletes (quote top)))

(defun hbin-dired-mode-setup ()
  (define-key dired-mode-map (kbd "`") 'dired-clean-directory)
  (define-key dired-mode-map (kbd ".") 'dired-omit-mode)
  (define-key dired-mode-map (kbd "M-o") 'other-window)
  (define-key dired-mode-map (kbd "/") 'diredp-omit-marked)
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))

  ;; Dired reuse directory buffer
  (toggle-diredp-find-file-reuse-dir 1)

  (setq dired-omit-files
        (rx (or (seq bol "#")
                (seq bol ".")
                (seq "~" eol))))
  (setq dired-omit-extensions
        (append dired-omit-extensions
                (list
                 ".temp"
                 ".bak"
                 ))))

(defun hbin-dired-mode-init ()
  (hl-line-mode 1)
  (dired-omit-mode 1))

(eval-after-load "dired" '(hbin-dired-mode-setup))
(add-hook 'dired-mode-hook 'hbin-dired-mode-init)

;;; Change the `project-explorer' by the `emacs-neotree'
(prelude-require-package 'neotree)
(require 'neotree)

(setq neo-banner-message "\" Press ? for help.")
(setq neo-theme 'ascii)
(setq neo-smart-open t)
(setq neo-window-width 28)
(setq neo-hidden-files-regexp
      (s-concat "^\\."                             ; hidden files
                "\\|.*\\.elc"                      ; Emacs
                "\\|TAGS\\|GPATH\\|GRTAGS\\|GTAGS" ; TAG files
                "\\|__pycache__\\|.*\\.py[cod]"))  ; Python

(defun neo-buffer--insert-fold-symbol (name &optional node-name)
  "Write icon by NAME, the icon style affected by neo-theme.
`open' write opened folder icon.
`close' write closed folder icon.
`leaf' write leaf icon."
  (let ((n-insert-image (lambda (n)
                          (insert-image (neo-buffer--get-icon n))))
        (n-insert-symbol (lambda (n)
                           (neo-buffer--insert-with-face
                            n 'neo-expand-btn-face))))
    (cond
     ((and window-system (equal neo-theme 'classic))
      (or (and (equal name 'open)  (funcall n-insert-image "open"))
          (and (equal name 'close) (funcall n-insert-image "close"))
          (and (equal name 'leaf)  (funcall n-insert-image "leaf"))))
     ((equal neo-theme 'arrow)
      (or (and (equal name 'open)  (funcall n-insert-symbol "▾ "))
          (and (equal name 'close) (funcall n-insert-symbol "▸ "))))
     (t
      (or (and (equal name 'open)  (funcall n-insert-symbol "- "))
          (and (equal name 'close) (funcall n-insert-symbol "+ ")))))))

(defun neo-buffer--insert-file-entry (node depth)
  (let ((node-short-name (neo-path--file-short-name node)))
    (insert-char ?\s (* depth 2)) ; indent
    (neo-buffer--insert-fold-symbol 'leaf)
    (insert-button node-short-name
                   'action '(lambda (_) (neotree-enter current-prefix-arg))
                   'follow-link t
                   'face neo-file-link-face
                   'neo-full-path node)
    (neo-buffer--node-list-set nil node)
    (neo-buffer--newline-and-begin)))

(defun neotree-folder-toggle ()
  "Toggle a folder."
  (interactive)
  (let ((btn-full-path (neo-buffer--get-filename-current-line)))
    (unless (null btn-full-path)
      (if (file-directory-p btn-full-path)
          (progn
            (let ((new-state (neo-buffer--toggle-expand btn-full-path)))
              (neo-buffer--refresh t)
              (when neo-auto-indent-point
                (when new-state (forward-line))
                (neo-point-auto-indent))))))))

(defun neotree-projectile-action ()
  "Integration with `Projectile'.
Usage:
    (setq projectile-switch-project-action 'neotree-projectile-action).
When running `projectile-switch-project' (C-c p p), `neotree' will change root
automatically."
  (interactive)
  (cond
   ((fboundp 'projectile-project-root)
    (let ((path (projectile-project-root)))
      (neo-global--open-dir path)
      (if neo-smart-open
          (neotree-find)
        (neo-global--open))))
   (t
    (error "Projectile is not available"))))

(define-key neotree-mode-map (kbd "TAB") 'neotree-folder-toggle)
(define-key neotree-mode-map (kbd "RET") 'neotree-enter)
(define-key neotree-mode-map (kbd "o") 'neotree-enter)
(define-key neotree-mode-map (kbd "R") 'neotree-refresh)
(define-key neotree-mode-map (kbd "r") 'neotree-refresh)
(define-key neotree-mode-map (kbd "q") 'neotree-hide)
(define-key neotree-mode-map (kbd "k") 'neotree-select-previous-sibling-node)
(define-key neotree-mode-map (kbd "j") 'neotree-select-next-sibling-node)
(define-key neotree-mode-map (kbd "p") 'previous-line)
(define-key neotree-mode-map (kbd "n") 'next-line)
(define-key neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(define-key neotree-mode-map (kbd "U") 'neotree-select-up-node)
(define-key neotree-mode-map (kbd "D") 'neotree-select-down-node)
(define-key neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
(define-key neotree-mode-map (kbd "I") 'neotree-hidden-file-toggle)
(define-key neotree-mode-map (kbd "C-x C-f") 'find-file-other-window)
(define-key neotree-mode-map (kbd "C") 'neotree-change-root)
(define-key neotree-mode-map (kbd "m a") 'neotree-create-node)
(define-key neotree-mode-map (kbd "m d") 'neotree-delete-node)
(define-key neotree-mode-map (kbd "m m") 'neotree-rename-node)
(define-key neotree-mode-map (kbd "C-p") 'previous-line)
(define-key neotree-mode-map (kbd "C-n") 'next-line)

(add-hook 'neotree-mode-hook (lambda () (hl-line-mode 1)))

(global-set-key (kbd "C-x C-j") 'neotree-projectile-action)

(provide 'misc-dired)
;;; misc-dired.el ends here
