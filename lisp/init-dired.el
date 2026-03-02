;;; init-dired.el --- Shaobin Jiang's emacs configuration  -*- lexical-binding: t -*-

;; Author: Shaobin-Jiang <shaobin-jiang@outlook.com>
;; URL: https://github.com/Shaobin-Jiang/Icemacs

;;; Commentary:
;; Configuration for Dired major mode
;;
;; For better experience, the GNU ls command is used instead of the
;; native ls on MacOS because the latter does not offer a
;; `--group-directories-first` flag.  This would therefore require the
;; coreutils package to be installed on MacOS.

;;; Code:

(defun ice/dired-open ()
  "A cleverer callback for the RET key press.  It improves the default by:
- determines how to open the file / directory at point, i.e., in Emacs or via shell command
- closes the Dired buffer if a file is opened in Emacs"
  (interactive nil dired-mode)
  (let* ((file (dired-get-file-for-visit))
         (external-extension '("png" "jpg" "jpeg" "gif" "webp" "bmp"
							   "tif" "tiff" "svg" "mp4" "mkv" "mov"
							   "webm" "avi" "mp3" "m4a" "flac" "wav"
							   "ogg" "pdf" "docx" "doc" "xlsx" "xls"
							   "pptx"))
		 (eaf-extension '("png" "jpg" "jpeg" "gif" "webp" "bmp" "tif"
						  "tiff" "svg" "pdf"))
         (ext (downcase (or (file-name-extension file) ""))))

    (cond
     ((file-directory-p file)
      (dired-find-file))

	 ((and (member ext eaf-extension) (commandp 'eaf-open))
	  (eaf-open file)
	  (kill-buffer dired-buf))

     ((member ext external-extension)
      (let ((cmd (format "open %s"
                         (shell-quote-argument file))))
        (dired-do-shell-command cmd nil (list file))))

     (t (let ((dired-buf (current-buffer)))
          (dired-find-file)
          (kill-buffer dired-buf))))))

(defun ice/dired-open-dir ()
  "Opens the dir if item at point is indeed a directory."
  (interactive nil dired-mode)
  (when (file-directory-p (dired-get-file-for-visit))
    (progn (dired-find-file))))

(defun ice/dired-split (&optional split)
  "If item at point is a file that should be opened in Emacs, split.
If SPLIT is non nil, the file is opened in a horizontal split.
Otherwise, it is opened in a vsplit."
  (interactive nil dired-mode)
  (let ((file (dired-get-file-for-visit)))
    (quit-window t)
    (if split
        (evil-window-split nil file)
      (evil-window-vsplit nil file))))

(defun ice/dired-quit ()
  "Quits Dired buffer and kills the buffer when doing so."
  (interactive nil dired-mode)
  (quit-window t))

(defun ice/add-dired-bindings ()
  "Add keymap for Dired mode.  This is hooked to Dired mode."
  (evil-define-key 'normal dired-mode-map
    (kbd "a") #'find-file
    (kbd "h") #'dired-up-directory
    (kbd "l") #'ice/dired-open-dir
    (kbd "q") #'ice/dired-quit
    (kbd "y") #'dired-do-copy
    (kbd "V") #'ice/dired-split
    (kbd "S") (lambda () (interactive) (ice/dired-split t))
    (kbd "g g") (lambda () (interactive) (evil-goto-line 2)) ; ignore first line
    (kbd "G") (lambda () (interactive)
				(evil-goto-line (1- (line-number-at-pos (point-max)))))
    (kbd "<leader> u f") #'ice/dired-quit))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-lAh --group-directories-first")
  (dired-guess-shell-alist-user '((".*" "open")))
  (dired-kill-when-opening-new-dired-buffer t)
  :hook
  (dired-mode . (lambda () (define-key dired-mode-map (kbd "SPC") nil)))
  (dired-mode . ice/add-dired-bindings)
  :config
  ;; Use GNU ls on MacOS if possible
  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))
      (when gls
        (setq insert-directory-program gls))))
  :bind (:map dired-mode-map
              ("RET"      . ice/dired-open)
              ("<return>" . ice/dired-open)))

(provide 'init-dired)

;;; init-dired.el ends here
