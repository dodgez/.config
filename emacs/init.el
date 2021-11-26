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

(setq gc-cons-threshold (* 50 1000 1000))
(defun display-startup-time ()
	"Displays the startup time in the echo buffer when Emacs is finished loading."
	(message "Emacs loaded in %s with %d garbage collections."
					 (format "%.2f seconds"
									 (float-time (time-subtract after-init-time before-init-time)))
					 gcs-done))
(add-hook 'emacs-startup-hook #'display-startup-time)

(add-to-list 'exec-path "/usr/local/bin")

(defun set-ideal-frame-size (&optional proportion)
	"Set the frame to fill a PROPORTION of the screen."
	(interactive)
	(when (display-graphic-p)
		(pcase (frame-monitor-workarea)
			(`(,display-x ,display-y ,display-raw-width ,display-raw-height)
			 (let* ((proportion (or proportion 0.9))
							(margin (/ (- 1 proportion) 2))
							(display-width (- display-raw-width (if (< display-x 1920) display-x 0)))
							(display-height (- display-raw-height display-y))
							(width (truncate (* display-width proportion)))
							(height (truncate (* display-height proportion)))
							(margin-left (+ display-x (truncate (* display-width margin))))
							(margin-top (+ display-y (truncate (* display-height margin)))))
				 (set-frame-position (selected-frame) margin-left margin-top)
				 (set-frame-size (selected-frame) width height t))))))
(add-hook 'emacs-startup-hook #'set-ideal-frame-size)

(setq flycheck-emacs-lisp-load-path 'inherit)

(require 'straight)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(require 'use-package)
(use-package ivy
	:bind (("C-s" . swiper))
	:custom
	(ivy-use-virtual-buffers t)
	(enable-recursive-minibuffers t)
	:config
	(ivy-mode t))

(use-package counsel
	:bind (("M-x" . counsel-M-x)
				 ("C-x b" . counsel-ibuffer)
				 ("C-x C-f" . counsel-find-file))
	:custom
	(ivy-initial-inputs-alist nil)
	:config
	(counsel-mode t))

(use-package company-box
	:hook (emacs-startup . global-company-mode))

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

(use-package all-the-icons-ivy
	:after ivy
	:config
	(all-the-icons-ivy-setup))

(use-package prescient
	:after counsel)

(use-package ivy-prescient
	:after prescient
	:config
	(ivy-prescient-mode 1))

(use-package no-littering
	:custom
	(auto-save-file-name-transforms
				`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
	(custom-file (expand-file-name "custom.el" user-emacs-directory)))

(use-package all-the-icons)

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

(use-package rg
	:commands (rg ripgrep))

(use-package popper
	:after projectile
	:init
	(setq popper-reference-buffers
				'("\\*Messages\\*"
					"^\\*Warnings\\*"
					"^\\*IBuffer\\*"
					"^\\*Compile-Log\\*"
					"^\\*Backtrace\\*"
					"[Oo]utput\\*$"
					"\\*Help\\*"
					"\\*helpful\\*"
					"\\*vterm\\*"
					"\\*Excorporate\\*"
					"\\*xref\\*"
					help-mode
					helpful-mode
					compilation-mode
					term-mode
					vterm-mode)
				popper-group-function #'popper-group-by-projectile)
	(popper-mode t))

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

(use-package general)

(use-package magit
	:commands magit-status
	:custom
	(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-collection
	:config
	(evil-collection-init))

(use-package vterm
	:commands vterm
	:config
	(when (not (or (eq system-type 'windows-nt) (eq system-type 'ms-dos))) (setq vterm-shell (executable-find "fish"))))
(use-package vterm-toggle
	:commands vterm-toggle)

(use-package web-mode
	:custom
	(web-mode-code-indent-offset 2)
	:config
	(add-to-list 'auto-mode-alist '("\\.[jt]sx?\\'" . web-mode)))

(use-package js
  :commands 'js-mode
  :custom
  (js-indent-level 2))

(use-package json-mode
  :commands 'json-mode
  :mode (("\\.json\\'" . json-mode)))

(use-package rust-mode
	:commands 'rust-mode)

(use-package doom-modeline
	:config
	(doom-modeline-mode t))

(use-package undo-tree
	:hook ((text-mode . undo-tree-mode)
				 (prog-mode . undo-tree-mode)))

(use-package paren
	:config
	(show-paren-mode t))

(use-package evil-nerd-commenter
	:commands evilnc-comment-or-uncomment-lines)

(use-package git-gutter
	:config
	(global-git-gutter-mode t))

(use-package blamer
	:hook ((text-mode . blamer-mode)
				 (prog-mode . blamer-mode)))

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
	:hook ((web-mode . lsp)
				 (rust-mode . lsp)))

(use-package lsp-ui
	:after 'lsp-mode)

(use-package org
	:custom
	(org-support-shift-select t))

(use-package avy
	:custom
	(avy-style 'pre)
	:commands (avy-goto-char avy-goto-word-0 avy-goto-line))

(load (expand-file-name "custom.el" user-emacs-directory) t t)

(use-package doom-themes
	:config
	(load-theme 'doom-dracula))

(load (expand-file-name "private.el" user-emacs-directory) t t)

(use-package auto-package-update
	:custom
	(auto-package-update-interval 7)
	(auto-package-update-prompt-before-update t)
	(auto-package-update-hide-results t)
	:config
	(auto-package-update-maybe)
	(auto-package-update-at-time "09:00"))

; Customization
(setq delete-by-moving-to-trash t
			mouse-wheel-progressive-speed nil
			mouse-wheel-scroll-amount '(5 ((shift) . hscroll)
																		((meta) . 1)
																		((control) . text-scale))
			warning-minimum-level :error
			inhibit-startup-message t)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default standard-indent 2)

(global-subword-mode t)
(tool-bar-mode 0)
(tooltip-mode 0)
(menu-bar-mode 0)
(set-face-attribute 'default nil :font "Hack Nerd Font Mono" :height 130)
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(vterm-mode-hook
								eshell-mode-hook))
	(add-hook mode (lambda () (display-line-numbers-mode 0))))
(global-hl-line-mode)

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode t)

(xterm-mouse-mode)
(setq xterm-set-window-title t)

(add-hook 'prog-mode-hook 'hs-minor-mode)

; Keybindings
(when (eq system-type 'darwin) (setq mac-command-modifier 'control))

(general-define-key
 "<escape>" 'keyboard-escape-quit
 :keymaps 'override
 "C-/" 'evilnc-comment-or-uncomment-lines
 "<home>" 'beginning-of-line
 "<end>" 'end-of-line)

(general-define-key
 :states 'normal
 "q" nil)

(general-define-key
 :keymaps 'global
 "<mouse-3>" 'mouse-set-point
 "<drag-mouse-3>" 'mouse-set-region)

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
 "f" '(:ignore t :which-key "file")
 "f f" '(counsel-find-file :which-key)
 "f i" '((lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))) :which-key "Edit init file")
 "f p" '((lambda () (interactive) (counsel-find-file "" user-emacs-directory)) :which-key "Browse private config")
 "f s" '(save-buffer :which-key)
 "g" '(:ignore t :which-key "magit")
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
 "o" '(:ignore t :which-key "open")
 "o t" '(vterm-toggle :which-key)
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
 "w s" '(evil-window-split :which-key)
 "w v" '(evil-window-vsplit :which-key))

(provide 'init)
;;; init.el ends here
