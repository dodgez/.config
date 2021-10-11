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

(defun set-ideal-frame-size ()
  (when window-system
    (let ((width (* (display-pixel-width) 0.8))
          (height (* (display-pixel-height) 0.8))
          (default-width (* 2560 0.8))
          (default-height (* 1440 0.8)))
      (if (wsl-p)
        (set-frame-size (selected-frame) (truncate default-width) (truncate default-height) t)
        (set-frame-size (selected-frame) (truncate width) (truncate height) t))
      (if (wsl-p)
       (set-frame-position (selected-frame) (truncate (* default-width 0.125)) (truncate (* default-height 0.125)))
       (set-frame-position (selected-frame) (truncate (* width 0.125)) (truncate (* height 0.125)))))))
(set-ideal-frame-size)
