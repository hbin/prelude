;;; hbin-ui --- Personal themes

;;; Commentary:

;; Monaco: http://s.yunio.com/3FuQfa
;; Menlo: http://s.yunio.com/8XBaSx
;; YaHei Consolas Hybrid: http://s.yunio.com/ZFORNb

;;; Code:
(setq inhibit-startup-screen t)

;; Set frame
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(defcustom hbin-frame-font "Monaco:pixelsize=20" "Default font.")
(defcustom hbin-frame-font-chinese "YaHei Consolas Hybrid:pixelsize=18" "Chinese font.")

(defun hbin-frame-init (frame)
  "Custom behaviours for FRAME."
  (set-frame-font hbin-frame-font)
  (set-fontset-font "fontset-default" 'chinese-gbk hbin-frame-font-chinese)
  (add-to-list 'default-frame-alist
               (cons 'font hbin-frame-font))
  ;; UI
  (tooltip-mode -1)            ; 不要 tooltip
  (menu-bar-mode -1)           ; 不要菜单栏
  (tool-bar-mode -1)           ; 不需要工具栏
  (scroll-bar-mode -1)         ; 不需要滚动条
  (set-fringe-mode '(1 . 1))   ; 小的 fringe 边距
  (blink-cursor-mode -1)       ; 光标不闪
  (mouse-wheel-mode t))

;; run now
(hbin-frame-init (selected-frame))

;; and later
(add-hook 'after-make-frame-functions 'hbin-frame-init)

(provide 'hbin-ui)
;;; hbin-ui.el ends here
