;; prog-elixir.el --- Enhance Programming Elixir
;;
;; Copyright (C) 2012-2017 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

(require 'prelude-programming)

(prelude-require-packages '(elixir-mode alchemist))

(require 'elixir-mode)

(eval-after-load 'elixir-mode
  '(progn
     (defun hbin-elixir-mode-defaults ()
       (show-paren-mode -1)
       )

     (setq hbin-elixir-mode-hook 'hbin-elixir-mode-defaults)
     (add-hook 'elixir-mode-hook (lambda () (run-hooks 'hbin-elixir-mode-hook)))))

(provide 'prog-elixir)
;;; prog-elixir.el ends here
