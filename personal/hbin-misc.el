;;; hbin-misc --- miscellany
;;
;; Copyright (C) 2012-2013 Huang Bin
;;
;; Author: Huang Bin <embrace.hbin@gmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

;; Displays search information in the mode-line
(prelude-require-package 'anzu)
(global-anzu-mode +1)

;; Expand region
(global-set-key (kbd "M-h") 'er/expand-region)

;; Highlight symbols
(prelude-require-package 'highlight-symbol)
(global-set-key (kbd "M-m") 'highlight-symbol-at-point)
(global-set-key (kbd "M-M") 'highlight-symbol-remove-all)
(global-set-key (kbd "M-n") 'highlight-symbol-next)
(global-set-key (kbd "M-p") 'highlight-symbol-prev)
(global-set-key (kbd "M-N") 'highlight-symbol-next-in-defun)
(global-set-key (kbd "M-P") 'highlight-symbol-prev-in-defun)

;; Multiple Cursors
(prelude-require-package 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "S-<down>") 'mc/mark-next-lines)
(global-set-key (kbd "S-<up>") 'mc/mark-previous-lines)
(global-set-key (kbd "C-*") 'mc/mark-all-symbols-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-symbol-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-symbol-like-this)

;; Whole line or region
(prelude-require-package 'whole-line-or-region)
(whole-line-or-region-mode)

;; 使用英文拼写词库
(setq ispell-dictionary "english")

;; 使用 aspell 做拼写检查，会突出错误拼写，而不是像 ispell 在 minibuffer 中显示
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(add-hook 'message-mode-hook 'turn-on-flyspell)
;; (add-hook 'text-mode-hook 'turn-on-flyspell)

;; Popwin
(prelude-require-package 'popwin)
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

;; Ignore '/'
(require 'ffap)
(defvar ffap-c-commment-regexp "^/\\*+"
  "Matches an opening C-style comment, like \"/***\".")

(defadvice ffap-file-at-point (after avoid-c-comments activate)
  "Don't return paths like \"/******\" unless they actually exist.
This fixes the bug where ido would try to suggest a C-style
comment as a filename."
  (ignore-errors
    (when (and ad-return-value
               (string-match-p ffap-c-commment-regexp
                               ad-return-value)
               (not (ffap-file-exists-string ad-return-value)))
      (setq ad-return-value nil))))

(defadvice ffap-file-at-point (after ffap-file-at-point-after-advice ())
  "The ffap should ignore the '/'."
  (if (string= ad-return-value "/")
      (setq ad-return-value nil)))
(ad-activate 'ffap-file-at-point)

(provide 'hbin-misc)
;;; hbin-misc.el ends here
