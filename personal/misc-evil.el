;; misc-evil.el --- evil
;;
;; Copyright (C) 2012-2021 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

(prelude-require-packages '(evil))

(require 'evil)
(global-set-key (kbd "M-[") 'evil-jump-backward)
(global-set-key (kbd "M-]") 'evil-jump-forward)

(provide 'misc-evil)
