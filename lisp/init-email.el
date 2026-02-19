;;; init-email.el --- Shaobin Jiang's emacs configuration  -*- lexical-binding: t -*-

;; Author: Shaobin Jiang
;; URL: https://github.com/Shaobin-Jiang/Icemacs

;;; Commentary:
;; Emacs email client using himalaya cli

;;; Code:

(use-package himalaya
  :straight t
  :commands (himalaya himalaya-list-envelopes)
  :init
  ;; Stop complaining about `himalaya--update-mode-line' being void
  (defun himalaya--update-mode-line () (interactive))
  :config
  (evil-define-key 'normal himalaya-list-envelopes-mode-map
    (kbd "RET") 'himalaya-read-message-at-point))

(provide 'init-email)

;;; init-email.el ends here
