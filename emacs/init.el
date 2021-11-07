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

(defun display-startup-time ()
  "Displays the startup time in the echo buffer when Emacs is finished loading."
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'display-startup-time)

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
(add-hook 'emacs-startup-hook #'set-ideal-frame-size)

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package ivy
	:bind (("C-s" . swiper))
  :config
  (ivy-mode))

(use-package counsel
	:bind (("M-x" . counsel-M-x)
				 ("C-x b" . counsel-ibuffer)
				 ("C-x C-f" . counsel-find-file))
	:config
	(setq ivy-initial-inputs-alist nil)
	(counsel-mode t))

(use-package prescient
	:after counsel)

(use-package ivy-prescient
	:after prescient
	:config
	(ivy-prescient-mode 1))

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
				`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
	(setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(use-package all-the-icons)

(use-package rainbow-delimiters
	:hook (prog-mode . rainbow-delimiters-mode))

(use-package ws-butler
	:hook ((text-mode . ws-butler-mode)
				 (prog-mode . ws-butler-mode)))

(use-package projectile)

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
	:config
	(which-key-mode)
	(setq which-key-idle-delay 0.3))

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
	:commands vterm)
(use-package vterm-toggle
	:commands vterm-toggle)

(use-package web-mode
	:config
	(add-to-list 'auto-mode-alist '("\\.[jt]sx?\\'" . web-mode)))

(use-package doom-modeline
	:config
	(doom-modeline-mode 1))(use-package doom-modeline
	:config
	(doom-modeline-mode 1))

(load (expand-file-name "custom.el" user-emacs-directory) t t)

(use-package doom-themes
	:config
	(load-theme 'doom-dracula))

; Customization
(setq-default
 delete-by-moving-to-trash t
 tab-width 2
 mouse-wheel-progressive-speed nil
 mouse-wheel-scroll-amount '(2 ((shift) . hscroll)
																((meta))
																((control) . text-scale)))
(setq warning-minimum-level :error)
(global-subword-mode t)
(tool-bar-mode 0)
(tooltip-mode 0)
(menu-bar-mode 0)
(set-face-attribute 'default nil :font "Hack Nerd Font Mono" :height 130)
(column-number-mode)
(global-display-line-numbers-mode t)
(global-hl-line-mode)

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode t)

; Keybindings
(general-define-key
 "<escape>" 'keyboard-escape-quit
 "<delete>" 'evil-delete)

(general-define-key
 :states 'normal
 "q" nil)

(general-define-key
 :states 'insert
 :keymaps 'override
 "<delete>" 'evil-delete)

(general-define-key
 :states '(normal visual)
 :keymaps 'override
 :prefix "SPC"
 "SPC" '(counsel-M-x :which-key "M-x")
 "e" '(:ignore t :which-key "eval")
 "e b" '(eval-buffer :which-key)
 "e e" '(eval-expression :which-key)
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
 "h o" '(counsel-describe-symbol :which-key)
 "h v" '(counsel-describe-variable :which-key)
 "w" '(:ignore t :which-key "window")
 "w d" '(evil-window-delete :which-key)
 "w h" '(evil-window-left :which-key)
 "w j" '(evil-window-down :which-key)
 "w k" '(evil-window-up :which-key)
 "w l" '(evil-window-right :which-key)
 "q" '(:ignore t :which-key "quit")
 "q q" '(evil-quit :which-key))
