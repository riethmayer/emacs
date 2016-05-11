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
  :ensure t)
(use-package helm-projectile
  :ensure t)
(use-package ido-ubiquitous
  :ensure t)
(use-package magit
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
  :ensure t)
(use-package rainbow-delimiters
  :ensure t)
(use-package rainbow-mode
  :ensure t)
(use-package smartparens
  :ensure t)
(use-package smex
  :ensure t)
(use-package tagedit
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package yasnippet
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

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

(setq apropos-sort-by-scores t)
(setq visible-bell 1)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq css-indent-offset 2)
(setq sql-indent-offset 2)
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)
(setq-default js-indent-level 2)
(setq-default python-indent 4)
(toggle-debug-on-error 1)
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t)
(blink-cursor-mode t)
;; (show-smartparens-global-mode +1)
(show-paren-mode 1)
(recentf-mode 1)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(global-subword-mode 1)
(menu-bar-mode 0)
;; (tool-bar-mode -1)
(column-number-mode 1)
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

(defun kill-region-or-backward-kill-word (&optional arg region)
  "`kill-region' if the region is active, otherwise `backward-kill-word'"
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))
(global-set-key (kbd "C-w") 'kill-region-or-backward-kill-word)

(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-\M-s" 'isearch-forward)
(global-set-key "\C-\M-r" 'isearch-backward)
(global-set-key (kbd "C-x f") 'recentf-open-files)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "<f5>") 'helm-projectile)
(global-set-key (kbd "<f6>") 'helm-projectile-ag)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-unset-key (kbd "s-m"))
(add-hook 'css-mode-hook 'rainbow-mode)
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

(defun indent-buffer ()
  "indent whole buffer"
  (interactive)
  (whitespace-cleanup)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(global-set-key (kbd "C-c n") 'indent-buffer)

(projectile-global-mode)

;; few ido changes from https://www.masteringemacs.org/article/introduction-to-ido-mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-create-new-buffer 'always)

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
