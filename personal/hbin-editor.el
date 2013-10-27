;; hbin-editor.el --- Enhance the editor
;;
;; Copyright (C) 2012-2013 Huang Bin
;;
;; Author: Huang Bin <embrace.hbin@gmail.com>
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; It's my handy Emacs.

;;; Code:
(line-number-mode t)                     ; 显示行号
(column-number-mode t)                   ; 显示列号
(size-indication-mode t)                 ; 显示文件大小

(global-auto-revert-mode t)              ; 当 Buffer 访问的文件修改时，自动更新 Buffer 中的内容
(delete-selection-mode t)                ; 有选择域时，先删除再插入

(require 'linum)
(setq linum-format "%4d ")               ; 自定义行号格式
(global-linum-mode -1)                   ; 不显示 Linum 行号
(global-hl-line-mode -1)                 ; 不高亮当前行

(toggle-indicate-empty-lines 1)

(mouse-avoidance-mode 'exile)            ; 鼠标自动移动到右上角，以免挡住视线
(fset 'yes-or-no-p 'y-or-n-p)            ; 以 y/n 代表 yes/no

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)      ; 空格代替制表符
(setq-default imenu-auto-rescan t)       ; Rescanning Imenu automatically
(setq-default truncate-lines t)          ; Disable line wrap

(setq suggest-key-bindings 1)            ; 当使用命令后，过 1 秒显示绑定的键
(setq require-final-newline t)           ; 存盘的时候，要求最后一个是换行符

(setq delete-by-moving-to-trash t)       ; 删除的文件放到回收站
(setq ring-bell-function #'ignore)       ; No beep

;; Scroll
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 5) ((control) . nil)))

;; Sweet window-splits
(defadvice split-window-right (after balance activate) (balance-windows))
(defadvice delete-window (after balance activate) (balance-windows))

;; File encoding
(require 'un-define "un-define" t)
(set-buffer-file-coding-system 'utf-8 'utf-8-unix)
(set-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default buffer-file-coding-system 'utf-8-unix)

(provide 'hbin-editor)
;;; hbin-editor.el ends here
