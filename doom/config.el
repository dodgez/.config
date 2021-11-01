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
    ((and (string-equal "*" (substring (buffer-name) 0 1)) (not (string-prefix-p "*eshell" (buffer-name)))) "Emacs")
    (t "User"))))
(when (not (string-equal system-type "windows-nt"))
  (use-package! centaur-tabs
    :config
    (setq centaur-tabs-buffer-groups-function #'custom-centaur-tabs-buffer-groups)))
(use-package! magit-delta
  :hook (magit-mode . magit-delta-mode))

(use-package evil
  :custom
  (evil-toggle-key "C-v"))
(map! "C-z" #'undo)

(map! "<escape>" #'keyboard-escape-quit)
(map! "<home>" #'beginning-of-line)
(map! "<end>" #'end-of-line)
(map! :when IS-MAC :i "<M-S-right>" "<C-S-right>")
(map! :when IS-MAC :i "<M-S-left>" "<C-S-left>")
(map! "C-/" #'comment-line)

(defun set-ideal-frame-size (&optional proportion)
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
(set-ideal-frame-size)
(map! :leader
      :desc "Set ideal frame size" "F" #'set-ideal-frame-size)
