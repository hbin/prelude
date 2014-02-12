;; misc-yas.el --- Configuration for Yasnippet
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.
;; Yasnippet - https://github.com/capitaomorte/yasnippet

;;; Code:
(prelude-require-package 'yasnippet)

;; ido-stype candidates
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-ido-prompt
                             yas-completing-prompt))

;; Set personal snippets as default
(setq yas-snippet-dirs (concat user-emacs-directory "snippets"))
(yas-global-mode 1)

(provide 'misc-yas)
;;; misc-yas.el ends here
