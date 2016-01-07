;; hbin-prog.el --- Feel fly when I program.
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:
(prelude-require-packages '(rainbow-delimiters dash-at-point yasnippet))

(add-to-list 'auto-mode-alist '("\\.env\\'" . shell-script-mode))

(require 'yasnippet)
(setq yas-snippet-dirs (concat user-emacs-directory "snippets")
      yas-prompt-functions '(yas-dropdown-prompt
                             yas-ido-prompt
                             yas-completing-prompt))
(yas-global-mode 1)

(defun hbin-prog-mode-init ()
  "Common settings for programming."
  (rainbow-delimiters-mode 1)

  (ggtags-mode 1)
  (linum-mode 1)

  (local-set-key (kbd "C-M-h") 'backward-kill-word)
  (local-set-key (kbd "C-j") 'reindent-then-newline-and-indent)
  (local-set-key (kbd "C-c C-c") 'whole-line-or-region-comment-dwim-2))

(add-hook 'prog-mode-hook 'hbin-prog-mode-init)
(add-hook 'scss-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
(add-hook 'yaml-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

;; Disable ggtags-mode for Lisp
(add-hook 'emacs-lisp-mode (lambda () (ggtags-mode -1)))
(add-hook 'lisp-mode (lambda () (ggtags-mode -1)))

(global-set-key (kbd "s-g") 'dash-at-point)

(provide 'hbin-prog)
;;; hbin-prog.el ends here
