;;; init-python.el --- Shaobin Jiang's emacs configuration  -*- lexical-binding: t -*-

;; Author: Shaobin Jiang
;; URL: https://github.com/Shaobin-Jiang/Icemacs

;;; Commentary:
;; Configuration for python dev

;;; Code:

(use-package lsp-pyright
  :straight t
  :custom (lsp-pyright-langserver-command "pyright")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(provide 'init-python)

;;; init-python.el ends here
