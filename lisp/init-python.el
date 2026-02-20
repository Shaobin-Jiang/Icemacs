;;; init-python.el --- Shaobin Jiang's emacs configuration  -*- lexical-binding: t -*-

;; Author: Shaobin Jiang
;; URL: https://github.com/Shaobin-Jiang/Icemacs

;;; Commentary:
;; Configuration for python dev

;;; Code:

(use-package lsp-pyright
  :straight t
  :defer t
  :custom (lsp-pyright-langserver-command "pyright")
  :hook ((python-mode python-ts-mode) . (lambda ()
										  (require 'lsp-pyright)
										  (lsp)))
  ((python-mode python-ts-mode) . (lambda () (set-fill-column 120))))

(provide 'init-python)

;;; init-python.el ends here
