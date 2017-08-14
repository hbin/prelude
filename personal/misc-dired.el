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
(prelude-require-packages '(neotree all-the-icons))

(require 'all-the-icons)
(setq all-the-icons-scale-factor 1)

(require 'neotree)
(setq neo-banner-message "\" Press ? for help.")
(setq neo-smart-open t)
(setq neo-window-width 28)
(setq neo-hidden-files-regexp
      (s-concat "^\\."                             ; hidden files
                "\\|.*\\.elc"                      ; Emacs
                "\\|TAGS\\|GPATH\\|GRTAGS\\|GTAGS" ; TAG files
                "\\|__pycache__\\|.*\\.py[cod]"))  ; Python
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

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
(define-key neotree-mode-map (kbd "m d") 'neotree-delete-node)
(define-key neotree-mode-map (kbd "m m") 'neotree-rename-node)

(add-hook 'neotree-mode-hook (lambda () (hl-line-mode 1)))

(global-set-key (kbd "C-x C-j") 'neotree-projectile-action)

(provide 'misc-dired)
;;; misc-dired.el ends here
