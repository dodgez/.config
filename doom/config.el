(setq user-full-name "Zachary Dodge"
      user-mail-address "zacharysdodge@gmail.com")

(setq doom-theme 'doom-dracula)
(setq doom-font (font-spec :family "Hack Nerd Font Mono" :size 16))

(setq org-support-shift-select t)

(setq display-line-numbers-type t)
(setq projectile-indexing-method 'alien)
(blink-cursor-mode)
(setq confirm-kill-emacs nil)
(setq mouse-wheel-progressive-speed nil)

(defun centaur-tabs-buffer-groups ()
  (list
    (cond
      ((string-equal "*" (substring (buffer-name) 0 1)) "Emacs")
      (t "User"))))
(use-package! magit-delta
  :hook (magit-mode . magit-delta-mode))
(use-package! minimap
  :config
  (minimap-mode))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package! treemacs
  :config
  (defun project-treemacs ()
    (if (and (doom-project-p) (not (equal (treemacs-current-visibility) 'visible)))
      (treemacs)
      t))
  (add-hook! prog-mode project-treemacs))
