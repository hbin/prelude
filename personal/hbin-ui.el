;;; hbin-ui --- Personal themes
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.
;; Monaco: http://s.yunio.com/3FuQfa
;; Menlo: http://s.yunio.com/8XBaSx
;; YaHei Consolas Hybrid: http://s.yunio.com/ZFORNb

;;; Code:
(setq inhibit-startup-screen t)

;; Set frame
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(defun hbin-frame-init (frame)
  "Custom behaviours for FRAME."
  (set-frame-font "Monaco:pixelsize=18")
  (set-fontset-font "fontset-default" 'chinese-gbk "Hei:pixelsize=16")

  ;; UI
  (tooltip-mode -1)            ; 不要 tooltip
  (menu-bar-mode 1)            ; 需要菜单栏
  (tool-bar-mode -1)           ; 不需要工具栏
  (scroll-bar-mode -1)         ; 不需要滚动条
  (set-fringe-mode '(1 . 1))   ; 小的 fringe 边距
  (beacon-mode -1)
  (blink-cursor-mode -1)       ; 光标不闪
  (mouse-wheel-mode t))

;; run now
(hbin-frame-init (selected-frame))

;; and later
(add-hook 'after-make-frame-functions 'hbin-frame-init)

(defconst preldue-themes-dir (expand-file-name "themes/" user-emacs-directory)
  "This directory houses themes.")

(defun add-subfolders-to-theme-load-path (parent-dir)
  "Add subfolders of the PARENT-DIR to theme load path."
  (dolist (f (directory-files parent-dir))
    (let ((name (concat parent-dir f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'custom-theme-load-path name)))))

;; Add themes to load path
(add-subfolders-to-theme-load-path preldue-themes-dir)

(load-theme 'molokai)

(provide 'hbin-ui)
;;; hbin-ui.el ends here
