;; installations via packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package ag
  :ensure t)
(use-package alchemist
  :ensure t)
(use-package coffee-mode
  :ensure t)
(use-package clojure-mode
  :ensure t)
(use-package clojure-mode-extra-font-locking
  :ensure t)
(use-package cider
  :ensure t)
(use-package dash-at-point
  :ensure t)
(use-package handlebars-mode
  :ensure t)
(use-package helm-ag
  :init
  (global-set-key (kbd "<f6>") 'helm-projectile-ag)
  :ensure t)
(use-package helm-projectile
  :init
  (global-set-key (kbd "<f5>") 'helm-projectile)
  :ensure t)
(use-package helm-company
  :init
  (global-company-mode)
  :ensure t)
(use-package ido-ubiquitous
  ;; few ido changes from https://www.masteringemacs.org/article/introduction-to-ido-mode
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)
  (setq ido-create-new-buffer 'always)
  :ensure t)
(use-package less-css-mode
  :ensure t)
(use-package magit
  :init
  (global-set-key (kbd "C-x g") 'magit-status)
  :ensure t)
(use-package markdown-mode+
  :ensure t)
(use-package markdown-preview-mode
  :ensure t)
(use-package paredit
  :ensure t)
(use-package polymode
  :ensure t)
(use-package projectile
  :init
  (projectile-global-mode)
  :ensure t)
(use-package rainbow-delimiters
  :ensure t)
(use-package rainbow-mode
  :init
  (add-hook 'css-mode-hook 'rainbow-mode)
  :ensure t)
(use-package recentf
  :init
  (recentf-mode)
  (global-set-key (kbd "C-x f") 'recentf-open-files)
  :ensure t)
(use-package saveplace
  :init
  (setq-default save-place t)
  (setq backup-directory-alist `(("." . "~/.saves")))
  (setq backup-by-copying t)
  (setq save-place-file (concat user-emacs-directory "places"))
  :ensure t)
(use-package smartparens
  :init
  (show-paren-mode)
  :ensure t)
(use-package smex
  :ensure t)
(use-package tagedit
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package yasnippet
  :init
  (yas-global-mode)
  :ensure t)
(use-package dockerfile-mode
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  :ensure t)
(use-package ruby-mode
  :init
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist
               '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|Procfile|[rR]akefile\\)\\'" . ruby-mode))
  :ensure t)
(use-package web-mode
  :init
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-enable-engine-detection t)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-current-column-highlight t)
    (setq web-mode-markup-indent-offset 2))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-hook 'web-mode-hook  'my-web-mode-hook)
  :ensure t)

;; defaults
(setq-default tab-width 2)
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)
(setq-default js-indent-level 2)
(setq-default python-indent 4)
(setq-default indent-tabs-mode nil)
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)
(setq apropos-sort-by-scores t)
(setq visible-bell 1)
(setq css-indent-offset 2)
(setq sql-indent-offset 2)
(setq inhibit-startup-message t)
(setq max-lisp-eval-depth 10000)
(setq debug-on-error t)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(fset 'yes-or-no-p 'y-or-n-p)

;; configure emacs built-in modes
(blink-cursor-mode t)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(global-subword-mode 1)
(menu-bar-mode 0)
(line-number-mode)
(column-number-mode)
(delete-selection-mode t)
(toggle-debug-on-error 1)

;; functions
(defun kill-region-or-backward-kill-word (&optional arg region)
  "`kill-region' if the region is active, otherwise `backward-kill-word'"
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun prev-window ()
  (interactive)
  (other-window -1))

(defun indent-buffer ()
  "indent whole buffer"
  (interactive)
  (whitespace-cleanup)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; global keybindings
(global-set-key (kbd "C-w") 'kill-region-or-backward-kill-word)
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-\M-s" 'isearch-forward)
(global-set-key "\C-\M-r" 'isearch-backward)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c n") 'indent-buffer)
(global-set-key (kbd "C-x p") 'prev-window)
(global-unset-key (kbd "s-m"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (tango-dark)))
 '(safe-local-variable-values (quote ((docker-image-name . "rails")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
