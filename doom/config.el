(setq user-full-name "Zachary Dodge"
      user-mail-address "zacharysdodge@gmail.com")

(setq doom-theme 'doom-zenburn)
(setq doom-font (font-spec :family "Hack Nerd Font Mono" :size 16))

(setq org-support-shift-select t)

(setq projectile-indexing-method 'alien)
(blink-cursor-mode t)
(setq confirm-kill-emacs nil)
(setq mouse-wheel-progressive-speed nil)

(global-display-line-numbers-mode)
(dolist (mode '(vterm-mode-hook
                eshell-mode-hook
                org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq js-indent-level 2
      typescript-indent-level 2)
(setq web-mode-code-indent-offset 2
      web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2)

; Performance isn't great: https://github.com/dandavison/magit-delta/issues/9
;(use-package! magit-delta
;  :hook (magit-mode . magit-delta-mode))

(map! "<escape>" #'keyboard-escape-quit)
(map! "<home>" #'beginning-of-line)
(map! "<end>" #'end-of-line)
(map! "C-/" #'comment-line)
(map! :leader "e" #'eval-last-sexp)
(map! :leader :desc "M-x" "SPC" #'counsel-M-x)

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
(map! :leader
      :desc "Set ideal frame size" "F" #'set-ideal-frame-size)
(setq initial-frame-alist '((fullscreen . maximized)))

(map! "C-s" #'swiper)

(when IS-WINDOWS (setq default-directory doom-private-dir))

(use-package! ivy-prescient
  :after (ivy counsel prescient)
  :config
  (setq prescient-sort-length-enable t)
  (setq ivy-sort-max-size 100000))

(map! :after undo-tree
      :leader (:prefix ("U" . "undo")
               :desc "redo" "r" #'undo-tree-redo
               :desc "undo" "u" #'undo-tree-undo
               :desc "visualize" "v" #'undo-tree-visualize))

(let ((work-config (doom-dir doom-private-dir "+work-config.el")))
  (when (file-exists-p work-config)
    (load! "+work-config.el")))
