;; prog-python.el --- Enhance Programming Django
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

(prelude-require-packages '(anaconda-mode pyenv-mode f))

(defun projectile-pyenv-mode-set ()
  "Setting PYTHON_VERSION projectile.

Reading from the application-specific .python-version file,
and set the PYTHON_VERSION to it."
  (let* ((specific-file (expand-file-name ".python-version"
                                          (projectile-project-root)))
         (specific-version (if (file-exists-p specific-file)
                               (replace-regexp-in-string
                                "\n" "" (f-read specific-file 'utf-8)))))
    (if specific-version
        (pyenv-mode-set specific-version)
      (pyenv-mode-unset))))
(add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)

(defun projectile-pyenv-mode-set-if ()
  "Setting PYTHON_VERSION if in `python-mode'."
  (if (and (equal major-mode 'python-mode)
           (projectile-project-p))
      (projectile-pyenv-mode-set)
    (pyenv-mode-unset)))
(add-hook 'find-file-hook 'projectile-pyenv-mode-set-if)

(defun hbin-python-mode-init ()
  "Settings for `python-mode'."
  (ruby-tools-mode +1)

  (local-set-key (kbd "C-j") 'newline-and-indent)

  ;; Code navigation for Python.
  (if (fboundp 'anaconda-mode) (anaconda-mode 1)))

(add-hook 'python-mode-hook 'hbin-python-mode-init)

(provide 'prog-python)
;;; prog-python.el ends here
