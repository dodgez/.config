;;; init.el --- Emacs config for dodgez

;;; Commentary:
;; This is the init file for dodgez's Emacs config

;;; Code:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'straight)
(straight-use-package 'use-package)
(require 'use-package)
(setq straight-use-package-by-default t)

(defun org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq org-agenda-files (list org-directory))
  (setq evil-auto-indent nil)
  (electric-indent-local-mode -1))

(use-package org
  :demand
  :ensure t
  :hook (org-mode . org-mode-setup)
  :custom
  (org-hide-emphasis-markers t)
  (org-catch-invisible-edits 'show))

(org-babel-load-file
 (expand-file-name "README.org"
                   user-emacs-directory))

(provide 'init)
;;; init.el ends here
