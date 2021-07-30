(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(eval-when-compile (require 'use-package))

;; Emacs Settings
(global-display-line-numbers-mode 1) ; Always show line numbers
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))) ; Locally store backups in .emacs.d/backups
(setq create-lockfiles nil) ; Don't use lockfiles (e.g. #.foo.txt files)
(set-default 'truncate-lines t) ; Don't wrap lines horizontally
(menu-bar-mode 0) ; No menu-bar
(tool-bar-mode 0) ; No tool-bar
(setq mouse-wheel-progressive-speed nil) ; Remove scroll acceleration

;; Key Bindings
(bind-keys :prefix-map quit-map
	   :prefix "C-c q"
           ("q" . save-buffers-kill-terminal)
	   ("r" . restart-emacs))

(defun load-user-init-file () (interactive) (load user-init-file))
(bind-keys :prefix-map editor-map
	   :prefix "C-c e"
	   ("r" . load-user-init-file))

(defun force-kill-buffer () (interactive) (kill-buffer nil))
(bind-keys :prefix-map buffer-map
	   :prefix "C-c b"
	   ("b" . switch-to-buffer)
	   ("d" . force-kill-buffer)
	   ("n" . next-buffer)
	   ("p" . previous-buffer))

(bind-keys :prefix-map window-map
	   :prefix "C-c w"
	   ("d" . delete-window)
	   ("n" . next-window-any-frame)
	   ("p" . previous-window-any-frame)
	   ("h" . split-window-horizontally)
	   ("v" . split-window-vertically))

(defun find-user-init-file () (interactive) (find-file user-init-file))
(bind-key "C-c f" 'find-user-init-file)

(bind-key "C-s" 'swiper)

(bind-key "C-;" 'comment-or-uncomment-region)

;; Package Configuration

(use-package centaur-tabs
  :config
  (centaur-tabs-mode 1))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1))

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 10)
  :config
  (ivy-mode 1))

(use-package ivy-prescient
  :config
  (ivy-prescient-mode 1))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode)))

(use-package treemacs
  :bind
  ("C-c t" . treemacs))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons))

;; Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("c1284dd4c650d6d74cfaf0106b8ae42270cab6c58f78efc5b7c825b6a4580417" "549ccbd11c125a4e671a1e8d3609063a91228e918ffb269e57bd2cd2c0a6f1c6" default)))
 '(mouse-wheel-scroll-amount (quote (2 ((shift) . 5) ((meta)) ((control) . text-scale))))
 '(package-selected-packages
   (quote
    (centaur-tabs highlight-parentheses markdown-mode dracula-theme restart-emacs treemacs treemacs-all-the-icons treemacs-magit treemacs-projectile projectile counsel ivy-prescient web-mode use-package which-key swiper ivy magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
