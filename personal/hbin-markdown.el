;; prog-markdown.el --- markdown-mode configuration
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

(defun hbin-markdown-mode-init ()
  "Settings for markdown-mode."
  (if (fboundp 'yas-minor-mode)
      (yas-minor-mode -1))
  (if (fboundp 'company-mode)
      (company-mode 1)))

(add-hook 'markdown-mode-hook 'hbin-markdown-mode-init)

;;; hbin-markdown.el ends here
