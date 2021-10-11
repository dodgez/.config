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

(defun wsl-p () (string-match "WSL" operating-system-release))
(defun wsl-default (a b) (if (wsl-p) a b))

(defun set-ideal-frame-size (&optional proportion)
  (when window-system
    (let* ((proportion (or proportion 0.8))
           (margin (/ (- 1 proportion) 2))
           (display-width (wsl-default 2560 (display-pixel-width)))
           (display-height (wsl-default 1440 (display-pixel-height)))
           (width (truncate (* display-width proportion)))
           (height (truncate (* display-height proportion)))
           (margin-left (truncate (* display-width margin)))
           (margin-top (truncate (* display-height margin))))

          (set-frame-size (selected-frame) width height t)
          (set-frame-position (selected-frame) margin-left margin-top))))
(set-ideal-frame-size 0.8)
