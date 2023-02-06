;;; init.el --- Emacs config for dodgez

;;; Commentary:
;; This is an Emacs private configuration file.

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
(setq straight-use-package-by-default t)

(require 'use-package)

(defun org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq org-agenda-files (list org-directory))
  (setq evil-auto-indent nil))

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
