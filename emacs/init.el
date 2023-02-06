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

(defun display-startup-time ()
  "Displays the startup time in the echo buffer when Emacs is finished loading."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'display-startup-time)

(use-package ivy
  :bind (("C-s" . swiper))
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  :config
  (ivy-mode t))

(use-package counsel
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file))
  :custom
  (ivy-initial-inputs-alist nil)
  :config
  (counsel-mode t))

(defun +ivy-rich-describe-variable-transformer (cand)
  "Preview the value of CAND in the minibuffer."
  (let* ((sym (intern cand))
         (val (and (boundp sym) (symbol-value sym)))
         (print-level 3))
    (replace-regexp-in-string
     "[\n\t\^[\^M\^@\^G]" " "
     (cond ((booleanp val)
            (propertize (format "%s" val) 'face
                        (if (null val)
                            'font-lock-comment-face
                          'success)))
           ((symbolp val)
            (propertize (format "'%s" val)
                        'face 'highlight-quoted-symbol))
           ((keymapp val)
            (propertize "<keymap>" 'face 'font-lock-constant-face))
           ((listp val)
            (prin1-to-string val))
           ((stringp val)
            (propertize (format "%S" val) 'face 'font-lock-string-face))
           ((numberp val)
            (propertize (format "%s" val) 'face 'highlight-numbers-number))
           ((format "%s" val)))
     t)))

(use-package ivy-rich
  :after (ivy counsel)
  :config
  (plist-put ivy-rich-display-transformers-list
             'counsel-describe-variable
             '(:columns
               ((counsel-describe-variable-transformer (:width 40))
                (+ivy-rich-describe-variable-transformer (:width 50))
                (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face)))))
  (plist-put ivy-rich-display-transformers-list
             'counsel-M-x
             '(:columns
               ((counsel-M-x-transformer (:width 60))
                (ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))))
  (plist-put ivy-rich-display-transformers-list
             'counsel-projectile-switch-to-buffer
             (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer))
  (plist-put ivy-rich-display-transformers-list
             'counsel-bookmark
             '(:columns
               ((ivy-rich-candidate (:width 0.5))
                (ivy-rich-bookmark-filename-or-empty (:width 60)))))
  (ivy-rich-mode t))

(use-package all-the-icons)

(use-package all-the-icons-ivy
  :after (all-the-icons ivy)
  :config
  (when (display-graphic-p) (all-the-icons-ivy-setup)))

(use-package no-littering
  :custom
  (auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (custom-file (expand-file-name "custom.el" user-emacs-directory)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(use-package projectile)

(use-package counsel-projectile
  :after (projectile counsel)
  :custom
  (counsel-projectile-sort-buffers t)
  (counsel-projectile-sort-files t)
  (counsel-projectile-sort-projects t))

(use-package which-key
  :custom
  (which-key-idle-delay 0.3)
  :config
  (which-key-mode))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :config
  (evil-collection-init))

(use-package general)

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package git-gutter
  :config
  (global-git-gutter-mode t))

(use-package blamer
  :config
  (setq blamer-view 'overlay-right)
  :hook ((text-mode . blamer-mode)
         (prog-mode . blamer-mode)))

(use-package vterm
  :commands vterm
  :config
  (when (not (or (eq system-type 'windows-nt) (eq system-type 'ms-dos))) (setq vterm-shell (executable-find "fish"))))

(use-package web-mode
  :after flycheck
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.[jt]sx?\\'" . web-mode))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  :hook
  (web-mode . (lambda () (prettier-js-mode))))

(use-package prettier-js
  :commands (prettier-js-mode prettier-js))

(use-package json-mode
  :commands 'json-mode
  :mode (("\\.json\\'" . json-mode)))

(use-package doom-modeline
  :config
  (doom-modeline-mode t))

(use-package undo-tree
  :hook ((text-mode . undo-tree-mode)
         (prog-mode . undo-tree-mode)))

(use-package paren
  :config
  (show-paren-mode t))

(use-package flycheck
  :config
  (global-flycheck-mode t))

(use-package markdown-mode
  :commands 'markdown-mode
  :mode (("\\.md\\'" . markdown-mode)))

(use-package clipetty
  :straight (clipetty :type git :host github :repo "spudlyo/clipetty")
  :config
  (global-clipetty-mode t))

(use-package evil-terminal-cursor-changer
  :straight (evil-terminal-cursor-changer :type git :host github :repo "kisaragi-hiu/evil-terminal-cursor-changer")
  :config
  (evil-terminal-cursor-changer-activate))

(use-package lsp-mode
  :hook (web-mode . lsp))

(use-package lsp-ui
  :after 'lsp-mode
  :config
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-location 'at-point))

(defun org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq org-agenda-files (list org-directory))
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . org-mode-setup)
  :commands (org-capture org-agenda)
  :custom
  (org-todo-keywords '((sequence "TODO(t)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-support-shift-select t)
  (org-hide-emphasis-markers t)
  (org-catch-invisible-edits t))

(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode))

(defun visual-fill-setup ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook
  (org-mode . visual-fill-setup))

(use-package evil-org
  :after (evil org)
  :hook
  (org-mode . (lambda () evil-org-mode)))

(use-package avy
  :custom
  (avy-style 'pre)
  :commands (avy-goto-char avy-goto-word-0 avy-goto-line))

(use-package fish-mode
  :commands 'fish-mode
  :mode (("\\.fish\\'" . fish-mode)))

(use-package highlight-indentation
  :hook (prog-mode . highlight-indentation-mode)
  :hook (prog-mode . highlight-indentation-current-column-mode))

(use-package exec-path-from-shell
  :config
  (when (eq window-system 'ns)
    (exec-path-from-shell-initialize)))

(load (expand-file-name "custom.el" user-emacs-directory) t t)

(use-package doom-themes
  :config
  (load-theme 'doom-zenburn))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

; Customization
(global-subword-mode t)
(tool-bar-mode 0)
(set-face-attribute 'default nil :font "Hack Nerd Font Mono" :height 130)
(column-number-mode)
(add-hook 'prog-mode-hook (display-line-numbers-mode 1))
(add-hook 'prog-mode-hook 'hs-minor-mode)
(global-hl-line-mode)
(global-auto-revert-mode t)
(xterm-mouse-mode)
(set-window-scroll-bars (minibuffer-window) nil nil)

; Keybindings
(general-define-key
 :states 'normal
 "q" nil)

(general-define-key
 :keymaps 'global
 "<mouse-3>" 'mouse-set-point
 "<drag-mouse-3>" 'mouse-set-region)

(general-define-key
 :states 'normal
 "f" 'avy-goto-word-0)

(general-define-key
 :states 'visual
 "<tab>" 'evil-indent-line)

(when (not (fboundp 'revert-buffer-quick)) (defun revert-buffer-quick () (interactive) (revert-buffer t (not (buffer-modified-p)))))
(general-define-key
 :states '(normal visual)
 :keymaps 'override
 :prefix "SPC"
 "SPC" '(counsel-M-x :which-key "M-x")
 "b" '(:ignore t :which-key "buffer")
 "b b" '(counsel-switch-buffer :which-key)
 "b d" '(kill-current-buffer :which-key)
 "b n" '(next-buffer :which-key)
 "b p" '(previous-buffer :which-key)
 "b r" '(revert-buffer-quick :which-key)
 "c" '(:ignore t :which-key "code")
 "c c" '(evilnc-comment-or-uncomment-lines :which-key)
 "c f" '(hs-toggle-hiding :which-key)
 "e" '(:ignore t :which-key "eval")
 "e b" '(eval-buffer :which-key)
 "e e" '(eval-expression :which-key)
 "e i" '((lambda () (interactive) (load (expand-file-name "init.el" user-emacs-directory))) :which-key "Load init file")
 "e l" '(eval-last-sexp :which-key)
 "f" '(:ignore t :which-key "file")
 "f f" '(counsel-find-file :which-key)
 "f i" '((lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))) :which-key "Edit init file")
 "f p" '((lambda () (interactive) (counsel-find-file "" user-emacs-directory)) :which-key "Browse private config")
 "f s" '(save-buffer :which-key)
 "g" '(:ignore t :which-key "magit")
 "g b" '(magit-blame :which-key)
 "g g" '(magit-status :which-key)
 "h" '(:ignore t :which-key "help")
 "h f" '(counsel-describe-function :which-key)
 "h k" '(describe-key :which-key)
 "h m" '(describe-mode :which-key)
 "h o" '(counsel-describe-symbol :which-key)
 "h v" '(counsel-describe-variable :which-key)
 "j" '(:ignore t :which-key "jump")
 "j c" '(avy-goto-char :which-key)
 "j l" '(avy-goto-line :which-key)
 "j w" '(avy-goto-word-0 :which-key)
 "o" '(:ignore t :which-key "open/org")
 "o c" '(org-toggle-checkbox :which-key)
 "o t" '(org-todo :which-key)
 "o v" '(projectile-run-vterm :which-key)
 "o w" '((lambda () (interactive) (find-file (concat (file-name-as-directory "~/org") "work.org"))) :which-key "Edit work org file")
 "p" '(:ignore t :which-key "project")
 "p a" '(projectile-add-known-project :which-key)
 "p b" '(counsel-projectile-switch-to-buffer :which-key)
 "p f" '(counsel-projectile-find-file :which-key)
 "p p" '(counsel-projectile-switch-project :which-key)
 "p s" '(counsel-projectile-rg :which-key)
 "q" '(:ignore t :which-key "quit")
 "q q" '(evil-quit-all :which-key)
 "s" '(:ignore t :which-key "search")
 "s p" '(counsel-projectile-rg :which-key)
 "s r" '(counsel-rg :which-key)
 "s R" '(rg-menu :which-key)
 "s s" '(swiper :which-key)
 "u" '(:ignore t :which-key "undo")
 "u b" '(undo-tree-switch-branch :which-key)
 "u r" '(undo-tree-redo :which-key)
 "u u" '(undo-tree-undo :which-key)
 "u v" '(undo-tree-visualize :which-key)
 "w" '(:ignore t :which-key "window")
 "w d" '(evil-window-delete :which-key)
 "w h" '(evil-window-left :which-key)
 "w j" '(evil-window-down :which-key)
 "w k" '(evil-window-up :which-key)
 "w l" '(evil-window-right :which-key)
 "w <left>" '(evil-window-left :which-key)
 "w <down>" '(evil-window-down :which-key)
 "w <up>" '(evil-window-up :which-key)
 "w <right>" '(evil-window-right :which-key)
 "w s" '(evil-window-split :which-key)
 "w v" '(evil-window-vsplit :which-key))

(load (expand-file-name "private.el" user-emacs-directory) t t)

(provide 'init)
;;; init.el ends here
