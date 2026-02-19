;;; init-markdown.el --- Shaobin Jiang's emacs configuration  -*- lexical-binding: t -*-

;; Author: Shaobin Jiang
;; URL: https://github.com/Shaobin-Jiang/Icemacs

;;; Commentary:
;; Configuration for markdown editing

;;; Code:

(use-package markdown-mode
  :straight t
  :hook (markdown-mode . visual-line-mode)
  :mode ("README\\.md\\'" . gfm-mode))

(provide 'init-markdown)

;;; init-markdown.el ends here
