;; hbin-prog.el --- Feel fly when I program.
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:
(prelude-require-packages '(rainbow-delimiters
                            dash-at-point
                            yasnippet))

(add-to-list 'auto-mode-alist '("\\.env\\'" . shell-script-mode))

(require 'yasnippet)
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-ido-prompt
                             yas-completing-prompt))
(yas-global-mode 1)
(diminish 'yas-minor-mode)

(defun hbin-prog-mode-init ()
  "Common settings for programming."
  (rainbow-delimiters-mode 1)

  (linum-mode 1)

  (prelude-prog-mode-defaults)

  (local-set-key (kbd "C-M-h") 'backward-kill-word)
  (local-set-key (kbd "C-j") 'reindent-then-newline-and-indent)
  (local-set-key (kbd "C-J") 'crux-top-join-line)
  (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region))

(add-hook 'prog-mode-hook 'hbin-prog-mode-init)
(add-hook 'scss-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
(add-hook 'yaml-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
(add-hook 'protobuf-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

(require 'dash-at-point)
(global-set-key (kbd "s-.") 'dash-at-point)

(provide 'hbin-prog)
;;; hbin-prog.el ends here
