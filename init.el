;;; init.el --- Shaobin Jiang's emacs configuration  -*- lexical-binding: t -*-

;; Author: Shaobin Jiang
;; URL: https://github.com/Shaobin-Jiang/Icemacs

;;; Commentary:
;; Bootstraps the configuration

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; `init-basic' contains `use-package' syntax; this has to load first
(require 'init-straight)

(require 'init-basic)
(require 'init-edit)

;; This is almost as important as the basic settings. Icemacs heavily
;; relies on vim-styled editing and evil mode has to be set up in the
;; early stages.
(require 'init-evil)

(require 'init-completion)
(require 'init-prog)

(require 'init-dired)
(require 'init-org)
(require 'init-git)

(require 'init-look)

(require 'init-markdown)
(require 'init-python)
(require 'init-web)

(require 'init-email)

(provide 'init)

;;; init.el ends here
