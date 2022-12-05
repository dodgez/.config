(setq user-full-name "Zachary Dodge"
      user-mail-address "zacharysdodge@gmail.com")

(setq doom-theme 'doom-material-dark)
(setq doom-font (font-spec :family "Hack Nerd Font Mono" :size 16))

(blink-cursor-mode t)
(setq mouse-wheel-progressive-speed nil)

(setq confirm-kill-emacs nil)

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
(use-package! magit-delta
  :hook (magit-mode . magit-delta-mode))

(map! "<escape>" #'keyboard-escape-quit)
(map! :n "q" #'keyboard-escape-quit)
(map! "<home>" #'beginning-of-line)
(map! "<end>" #'end-of-line)
(map! :leader "e" #'eval-last-sexp)
(map! :leader :desc "M-x" "SPC" #'execute-extended-command)
(map! :leader (:prefix "w"
               :desc "evil-window-left" "<left>" #'evil-window-left
               :desc "evil-window-down" "<down>" #'evil-window-down
               :desc "evil-window-up" "<up>" #'evil-window-up
               :desc "evil-window-right" "<right>" #'evil-window-right))

(setq initial-frame-alist '((fullscreen . maximized)))

(map! :leader :desc "evil-avy-goto-char" "j" #'evil-avy-goto-char)
(map! :leader (:prefix ("J" . "avy")
               :desc "char" "c" #'evil-avy-goto-char
               :desc "line" "l" #'evil-avy-goto-line
               :desc "word" "w" #'evil-avy-goto-word-1))

(when IS-WINDOWS (setq default-directory doom-private-dir))

(use-package! undo-tree
  :hook ((text-mode . undo-tree-mode)
         (prog-mode . undo-tree-mode)))
(map! :after undo-tree
      :leader (:prefix-map ("u" . "undo")
               :desc "redo" "r" #'undo-tree-redo
               :desc "undo" "u" #'undo-tree-undo
               :desc "visualize" "v" #'undo-tree-visualize))

(use-package! org
  :custom
  (org-hide-emphasis-markers t)
  (org-directory "~/org")
  (org-agenda-files (directory-files-recursively "~/org" "\\.org$"))
  (org-log-done 'time))
(use-package! org-appear
  :hook (org-mode . org-appear-mode))
(defun org-mode-visual-fill ()
  (setq visual-fill-column-width 120)
  (setq visual-fill-column-center-text t)
  (visual-fill-column-mode 1))
(use-package! visual-fill-column
  :hook (org-mode . org-mode-visual-fill))
(use-package visual-fill-column)

(use-package! highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'column))

(after! projectile (setq projectile-indexing-method 'alien))

(use-package! rg)
(map! :after projectile :leader "/"
      #'(lambda ()
          (interactive)
          (let ((shell-file-name "/bin/sh")) (call-interactively #'projectile-ripgrep))))

(map! :mode lisp-mode :n "SPC E" #'sly-pprint-eval-last-expression)

(defvar my/re-builder-positions nil
  "Store point and region bounds before calling re-builder")
(advice-add 're-builder
            :before
            (defun my/re-builder-save-state (&rest _)
              "Save into `my/re-builder-positions' the point and region
positions before calling `re-builder'."
              (setq my/re-builder-positions
                    (cons (point)
                          (when (region-active-p)
                            (list (region-beginning)
                                  (region-end)))))))

(defun reb-replace-regexp (&optional delimited)
  "Run `query-replace-regexp' with the contents of re-builder. With
non-nil optional argument DELIMITED, only replace matches
surrounded by word boundaries."
  (interactive "P")
  (reb-update-regexp)
  (let* ((re (reb-target-binding reb-regexp))
         (replacement (query-replace-read-to
                       re
                       (concat "Query replace"
                               (if current-prefix-arg
                                   (if (eq current-prefix-arg '-) " backward" " word")
                                 "")
                               " regexp"
                               (if (with-selected-window reb-target-window
                                     (region-active-p)) " in region" ""))
                       t))
         (pnt (car my/re-builder-positions))
         (beg (cadr my/re-builder-positions))
         (end (caddr my/re-builder-positions)))
    (with-selected-window reb-target-window
      (goto-char pnt) ; replace with (goto-char (match-beginning 0)) if you want
                                        ; to control where in the buffer the replacement starts
                                        ; with re-builder
      (setq my/re-builder-positions nil)
      (reb-quit)
      (query-replace-regexp re replacement delimited beg end))))
(use-package! re-builder
  :bind (:map doom-leader-map ("r" . re-builder))
  :hook ((reb-lisp-mode . centaur-tabs-local-mode)
         (reb-lisp-mode . hide-mode-line-mode))
  :custom
  (reb-re-syntax 'string)
  :config
  (map! :map reb-mode-map :n "RET" #'reb-replace-regexp)
  (map! :map reb-mode-map :n "<escape>" #'reb-quit))

(defun eshell-prompt ()
  (setenv "TERM" "xterm-256color")
  (ansi-color-apply (shell-command-to-string "starship prompt")))
(after! eshell
  (setenv "TERM" "xterm-256color")
  (setq eshell-prompt-function #'eshell-prompt)
  (setq eshell-highlight-prompt nil))

(after! evil
  (setq evil-cross-lines t))

(after! evil
  (map! "<next>" #'evil-scroll-down)
  (map! "<prior>" #'evil-scroll-up))

(setq inferior-lisp-program "ros -Q run")

(map! :nvm "C-/" #'evilnc-comment-or-uncomment-lines)

(let ((work-config (doom-dir doom-private-dir "+work-config.el")))
  (when (file-exists-p work-config)
    (load! "+work-config.el")))
