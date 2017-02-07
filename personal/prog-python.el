;; prog-python.el --- Enhance Programming Django
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

(prelude-require-packages '(anaconda-mode f))

(eval-after-load 'python-mode
  '(progn
     (defun hbin-python-mode-defaults ()
       "Settings for `python-mode'."
       (ruby-tools-mode +1)

       (local-set-key (kbd "C-j") 'newline-and-indent)

       ;; Code navigation for Python.
       (if (fboundp 'anaconda-mode) (anaconda-mode 1)))

     (setq hbin-python-mode-hook 'hbin-python-mode-defaults)
     (add-hook 'python-mode-hook (lambda ()
                                   (run-hook 'hbin-python-mode-hook)))))

(provide 'prog-python)
;;; prog-python.el ends here
