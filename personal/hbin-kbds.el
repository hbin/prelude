;;; hbin-kbds --- Personal kbds
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Key bindings.

;;; Code:

;; Swap the mapping of Meta and Super if Mac OSX
(when (string= system-type "darwin")
  (if (eq mac-command-modifier 'super)
      (progn
        (setq mac-command-modifier 'meta)
        (setq mac-option-modifier 'super))))

;; disable several ido kbds
(add-hook 'ido-setup-hook 'ido-my-keys)
(defun ido-my-keys ()
  "Add my keybindings for ido."
  (define-key ido-file-dir-completion-map "\C-x\C-f" nil)
  (define-key ido-common-completion-map "\C-f" 'forward-char))

;; define my own keymap
(defvar hbin-map (make-sparse-keymap))
(global-set-key (kbd "M-z") hbin-map)

;;; Unbinding keys
(global-unset-key (kbd "C-x C-p"))      ; used to mark page
(global-unset-key (kbd "C-x C-n"))      ; used to set-goal-column
(global-unset-key (kbd "C-x 0"))        ; used to delete-window
(global-unset-key (kbd "C-x 1"))        ; used to delete-other-windows
(global-unset-key (kbd "C-x 2"))        ; used to split-window-vertically
(global-unset-key (kbd "C-x 3"))        ; used to split-window-horizontally

;; Align the code
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Help command
(global-set-key (kbd "<f1>") 'help-command)

;; Back killing
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-M-h") 'backward-kill-word)
(define-key key-translation-map [?\C-h] [?\C-?])

;; Vim like open previous/next line
(global-set-key (kbd "C-o") 'prelude-smart-open-line)
(global-set-key (kbd "C-M-o") 'prelude-smart-open-line-above)

;; Window switching
(define-key prelude-mode-map (kbd "M-0") 'delete-window)
(define-key prelude-mode-map (kbd "M-1") 'delete-other-windows)
(define-key prelude-mode-map (kbd "M-2") 'split-window-vertically)
(define-key prelude-mode-map (kbd "M-3") 'split-window-horizontally)
(define-key prelude-mode-map (kbd "M-o") 'other-window)
(define-key prelude-mode-map (kbd "M-O") (lambda () (interactive) (other-window -1)))
(define-key prelude-mode-map (kbd "M-k") 'kill-this-buffer)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)
(global-set-key (kbd "M-r") 'highlight-symbol-query-replace)

;; Ack
(define-key hbin-map (kbd "a") 'ack)

;;; Winner Mode
(winner-mode 1)
(global-set-key (kbd "C-x <left>") 'winner-undo)
(global-set-key (kbd "C-x <right>") 'winner-redo)

(provide 'hbin-kbds)
;;; hbin-kbds.el ends here
