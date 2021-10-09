(setq user-full-name "Zachary Dodge"
      user-mail-address "zacharysdodge@gmail.com")
(setq doom-theme 'doom-dracula)
(setq org-support-shift-select t)
(setq display-line-numbers-type t)
(setq projectile-indexing-method 'alien)
(defun centaur-tabs-buffer-groups ()
  (list
    (cond
      ((string-equal "*" (substring (buffer-name) 0 1)) "Emacs")
      (t "User"))))
(setq doom-font (font-spec :family "Hack Nerd Font Mono" :size 16))
(show-smartparens-global-mode)
(blink-cursor-mode)
(setq confirm-kill-emacs nil)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(magit-delta-mode)
(use-package! magit-delta
  :hook (magit-mode . magit-delta-mode))