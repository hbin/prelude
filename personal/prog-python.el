;; prog-python.el --- Enhance Programming Django
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.dtl\\'" . web-mode))

(prelude-require-packages '(anaconda-mode))

(defun hbin-python-mode-init ()
  "Settings for `python-mode'."

  (local-set-key (kbd "C-j") 'newline-and-indent)

  ;; Code navigation for Python.
  (if (fboundp 'ggtags-mode) (ggtags-mode -1))
  (if (fboundp 'anaconda-mode) (anaconda-mode 1)))

(add-hook 'python-mode-hook 'hbin-python-mode-init)

(provide 'prog-python)
;;; prog-python.el ends here
