;;; init-straight.el --- Shaobin Jiang's emacs configuration  -*- lexical-binding: t -*-

;; Author: Shaobin-Jiang <shaobin-jiang@outlook.com>
;; URL: https://github.com/Shaobin-Jiang/Icemacs

;;; Commentary:
;; Set up `straight.el' package manager.

;;; Code:

(setq straight-check-for-modifications nil
      straight-base-dir                "~/.local/share/emacs/")

;; Install `straight.el' if it is not yet installed
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         straight-base-dir))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Integrate with `use-package'
(straight-use-package 'use-package)

(require 'package)

(provide 'init-straight)

;;; init-straight.el ends here
