;;; init-eaf.el --- Shaobin Jiang's emacs configuration  -*- lexical-binding: t -*-

;; Author: Shaobin Jiang
;; URL: https://github.com/Shaobin-Jiang/Icemacs

;;; Commentary:
;; Sets up EAF (Emacs Application Framework) for browser / PDF / ...

;;; Code:
(use-package eaf
  :load-path "~/.local/share/emacs/emacs-application-framework"
  :defer 1
  :custom
  (eaf-config-location(expand-file-name
					   "eaf/" no-littering-var-directory))
  (eaf-browser-enable-adblocker t)
  (eaf-browser-dark-mode nil)
  (eaf-wm-name "aerospace")
  (browse-url-browser-function 'eaf-open-browser)
  :config
  (defalias 'browse-web #'eaf-open-browser)

  (require 'eaf-browser)
  (require 'eaf-pdf-viewer)
  (require 'eaf-markdown-previewer)
  (require 'eaf-org-previewer)
  (require 'eaf-image-viewer)

  (defun eaf-open-office (file)
	"Use `soffice' for converting office file to PDF. This replaces
the default `eaf-open-office' which uses the `libreoffice' executable
that cannot be found on MacOS even with libreoffice installed."
	(interactive "f[EAF/office] Open Office file as PDF: ")
	(if (executable-find "soffice")
		(let* ((file-md5 (eaf-get-file-md5 file))
               (basename (file-name-base file))
               (pdf-file (format "/tmp/%s.pdf" file-md5))
               (pdf-argument (format "%s.%s_office_pdf" basename (file-name-extension file))))
          (if (file-exists-p pdf-file)
              (eaf-open pdf-file "pdf-viewer" pdf-argument)
			(message "Converting %s to PDF, EAF will start shortly..." file)
			(make-process
			 :name ""
			 :buffer " *eaf-open-office*"
			 :command (list "soffice" "--headless" "--convert-to" "pdf" (file-truename file) "--outdir" "/tmp")
			 :sentinel (lambda (_ event)
						 (when (string= (substring event 0 -1) "finished")
                           (rename-file (format "/tmp/%s.pdf" basename) pdf-file)
                           (eaf-open pdf-file "pdf-viewer" pdf-argument)
                           )))))
      (error "[EAF/office] libreoffice is required convert Office file to PDF!")))

  (eaf-bind-key nil "C-w" eaf-markdown-previewer-keybinding)
  (eaf-bind-key nil "SPC" eaf-markdown-previewer-keybinding)

  (eaf-bind-key nil "C-w" eaf-org-previewer-keybinding)
  (eaf-bind-key nil "SPC" eaf-org-previewer-keybinding)

  (eaf-bind-key nil "C-w" eaf-pdf-viewer-keybinding)
  (eaf-bind-key nil "SPC" eaf-pdf-viewer-keybinding)

  (eaf-bind-key nil "C-w" eaf-browser-keybinding)
  (eaf-bind-key nil "SPC" eaf-browser-keybinding)

  (eaf-bind-key nil "SPC" eaf-image-viewer-keybinding))

(provide 'init-eaf)

;;; init-eaf.el ends here
