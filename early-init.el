;;; early-init.el --- Shaobin Jiang's emacs configuration  -*- lexical-binding: t -*-

;; Author: Shaobin Jiang
;; URL: https://github.com/Shaobin-Jiang/Icemacs

;;; Commentary:
;; Configuration that takes effect in the early stage of startup.

;;; Code:

(setq no-littering-var-directory "~/.local/share/emacs/var"
      no-littering-etc-directory "~/.local/share/emacs/etc"
      package-enable-at-startup  nil
      gc-cons-threshold          (* 200 1024 1024))

(startup-redirect-eln-cache (expand-file-name
							 "eln-cache/" no-littering-var-directory))

;;; early-init.el ends here
