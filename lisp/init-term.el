;;; init-term.el --- Shaobin Jiang's emacs configuration  -*- lexical-binding: t -*-

;; Author: Shaobin Jiang
;; URL: https://github.com/Shaobin-Jiang/Icemacs

;;; Commentary:
;; Sets up vterm

;;; Code:

(use-package vterm
  :straight t
  :defer t
  :hook (vterm-mode . (lambda () (interactive)
						(display-line-numbers-mode -1))))

(provide 'init-term)

;;; init-term.el ends here
