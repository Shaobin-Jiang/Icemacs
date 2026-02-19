;;; init-email.el --- Shaobin Jiang's emacs configuration  -*- lexical-binding: t -*-

;; Author: Shaobin Jiang
;; URL: https://github.com/Shaobin-Jiang/Icemacs

;;; Commentary:
;; Emacs email client using himalaya cli

;;; Code:

(use-package himalaya
  :straight t
  :commands (himalaya himalaya-list-envelopes)
  :custom
  (himalaya-account "outlook")
  (himalaya-folder "Apple")
  (himalaya-executable "~/hm.sh")
  (himalaya-config-path "~/.config/himalaya/config.toml")
  :config
  (evil-define-key 'normal himalaya-list-envelopes-mode-map
    (kbd "RET") 'himalaya-read-message-at-point))

(provide 'init-email)

;;; init-email.el ends here
