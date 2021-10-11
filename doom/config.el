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

(defun custom-centaur-tabs-buffer-groups ()
  (list
   (cond
    ((string-equal "*" (substring (buffer-name) 0 1)) "Emacs")
    (t "User"))))
(use-package! centaur-tabs
  :config
  (setq centaur-tabs-buffer-groups-function #'custom-centaur-tabs-buffer-groups))
(use-package! magit-delta
  :hook (magit-mode . magit-delta-mode))
(use-package! minimap
  :config
  (minimap-mode))

(map! "<escape>" #'keyboard-escape-quit)
(map! "<home>" #'beginning-of-line)
(map! "<end>" #'end-of-line)
(when IS-MAC (map! :i "<M-S-right>" "<C-S-right>"))
(when IS-MAC (map! :i "<M-S-left>" "<C-S-left>"))

(defun set-ideal-frame-size (&optional proportion)
  (interactive)
  (when (display-graphic-p)
    (pcase (frame-monitor-workarea)
      (`(,display-x ,display-y ,display-raw-width ,display-raw-height)
       (let* ((proportion (or proportion 0.9))
              (margin (/ (- 1 proportion) 2))
              (display-width (- display-raw-width display-x))
              (display-height (- display-raw-height display-y))
              (width (truncate (* display-width proportion)))
              (height (truncate (* display-height proportion)))
              (margin-left (+ display-x (truncate (* display-width margin))))
              (margin-top (+ display-y (truncate (* display-height margin)))))
         (set-frame-size (selected-frame) width height t)
         (set-frame-position (selected-frame) margin-left margin-top))))))
(set-ideal-frame-size)
(map! :leader
      :desc "Set ideal frame size" "F" #'set-ideal-frame-size)
