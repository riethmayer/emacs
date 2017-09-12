;;; installations via packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/vendor")

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package ag
  :ensure t)
(use-package alchemist
  :init
  (setq alchemist-mix-env "dev")
  (setq alchemist-goto-elixir-source-dir "~/github/elixir/")
  (setq alchemist-goto-erlang-source-dir "~/github/otp/")
  :ensure t)
(use-package ansible
  :init
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
  :ensure t)
(use-package coffee-mode
  :ensure t)
(use-package clojure-mode
  :ensure t)
(use-package clojure-mode-extra-font-locking
  :ensure t)
(use-package cider
  :ensure t)
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :ensure t)
(use-package company-ansible
  :init
  (add-to-list 'company-backends 'company-ansible)
  :ensure t)
(use-package company-inf-ruby
  :init
  (add-to-list 'company-backends 'company-inf-ruby)
  :ensure t)
(use-package company-jedi
  :init
  (add-to-list 'company-backends 'company-jedi)
  :ensure t)
(use-package company-web
  :init
  (add-to-list 'company-backends 'company-web-html)
  (add-to-list 'company-backends 'company-web-jade)
  (add-to-list 'company-backends 'company-web-slim)
  :ensure t)
(use-package dash-at-point
  :ensure t)
(use-package docker
  :ensure t)
(use-package dockerfile-mode
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  :ensure t)
(use-package docker-tramp
  :ensure t)
(use-package ess
  :init
  (remove-hook 'ess-mode 'flycheck-mode)
  (add-hook 'ess-mode-hook
            (lambda ()
              (setq ess-fancy-comments nil)
              (ess-toggle-underscore nil)))
  :ensure t)
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
  :ensure t)
(use-package feature-mode
  :ensure t)
(use-package flycheck
  :init
  ;; http://www.flycheck.org/manual/latest/index.html
  (require 'flycheck)
  ;; turn on flychecking globally
  (add-hook 'after-init-hook #'global-flycheck-mode)
  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")

  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(json-jsonlist)))

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
(use-package ido-completing-read+
  ;; few ido changes from https://www.masteringemacs.org/article/introduction-to-ido-mode
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)
  (setq ido-create-new-buffer 'always)
  :ensure t)
(use-package jinja2-mode
  :ensure t)
(use-package json-mode
  :ensure t)
(use-package less-css-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.css\\'"    . less-css-mode))
  :ensure t)
(use-package magit
  :init
  (global-set-key (kbd "C-x g") 'magit-status)
  :ensure t)
(use-package markdown-mode+
  :init
  (require 'poly-R)
  (require 'poly-markdown)
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:Rmd\\|rmarkdown\\|RMD\\)\\'" . poly-markdown+r-mode))
  :ensure t)
(use-package markdown-preview-mode
  :ensure t)
(use-package mwim
  :init
  (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)
  :ensure t)
(use-package nginx-mode
  :ensure t)
(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :ensure t)
(use-package paredit
  :ensure t)
(use-package polymode
  :ensure t)
(use-package projectile
  :init
  (projectile-global-mode)
  :ensure t)
(use-package projectile-rails
  :ensure t)
(use-package plantuml-mode
  :init
  ;; tell org-mode where to find the plantuml JAR file (specify the JAR file)
  (setq org-plantuml-jar-path (expand-file-name "~/bin/plantuml.jar"))
  ;; use plantuml as org-babel language
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
  ;; helper function
  (defun my-org-confirm-babel-evaluate (lang body)
    "Do not ask for confirmation to evaluate code for specified languages."
    (member lang '("plantuml")))
  ;; trust certain code as being safe
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  ;; automatically show the resulting image
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.plu\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (setq plantuml-jar-path (expand-file-name "~/bin/plantuml.jar"))
  (add-to-list
   'org-src-lang-modes '("plantuml" . plantuml))
  :ensure t)
(use-package rainbow-delimiters
  :ensure t)
(use-package rainbow-mode
  :init
  (add-hook 'css-mode-hook 'rainbow-mode)
  :ensure t)
(use-package recentf
  :init
  (setq recentf-auto-cleanup 'never)
  (setq recentf-max-menu-items 25)
  (recentf-mode 1)
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
  (show-smartparens-global-mode t)
  :ensure t)
(use-package smex
  :ensure t)
(use-package spray
  :init
  (global-set-key (kbd "<f8>") 'spray-mode)
  :ensure t)
(use-package tagedit
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package yasnippet
  :init
  (yas-global-mode)
  :ensure t)
(use-package flymake-ruby
  :init
  (add-hook 'ruby-mode-hook 'flymake-ruby-load)
  :ensure t)
(use-package rubocop
  :init
  (add-hook 'ruby-mode-hook #'rubocop-mode)
  (require 'reek)
  (add-hook 'ruby-mode-hook #'reek-mode)
  :ensure t)
(use-package ruby-mode
  :init
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist
               '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|Procfile|[rR]akefile\\)\\'" . ruby-mode))
  (setq ruby-deep-indent-paren nil)
  :ensure t)
(use-package gh
  :ensure t)
(use-package gist
  :ensure t)
(use-package rvm
  :init
  (global-set-key (kbd "C-c r a") 'rvm-activate-corresponding-ruby)
  :ensure t)
(use-package terraform-mode
  :ensure t)
(use-package web-mode
  :init
  (setq-default
   web-mode-code-indent-offset 2
   web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")
                                  ("javascript" . "\\.es6?\\'"))
   web-mode-css-indent-offset 2
   web-mode-enable-auto-pairing t
   web-mode-enable-css-colorization t
   web-mode-markup-indent-offset 2
   web-mode-engines-alist '(("blade"  . "\\.blade\\.")))
  (add-to-list 'auto-mode-alist '("\\.erb\\'"   . web-mode))        ;; ERB
  (add-to-list 'auto-mode-alist '("\\.es6\\'"    . web-mode))       ;; ES6
  (add-to-list 'auto-mode-alist '("\\.html?\\'"  . web-mode))       ;; Plain HTML
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))       ;; JS + JSX
  (add-to-list 'auto-mode-alist '("\\.scss\\'"   . web-mode))       ;; SCSS
  (add-to-list 'auto-mode-alist '("\\.erb\\'"    . web-mode))       ;; ERB

  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))

  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "js")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))

  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint)))
    (flycheck-add-mode 'javascript-eslint 'web-mode))
  :ensure t)

(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph]
                #'endless/fill-or-unfill)
;; TODO


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
(setq max-lisp-eval-depth 100000)
(setq max-specpdl-size 100000)
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
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)
(add-hook 'term-setup-hook
          '(lambda ()
             (define-key function-key-map "\e[1;9A" [M-up])
             (define-key function-key-map "\e[1;9B" [M-down])
             (define-key function-key-map "\e[1;9C" [M-right])
             (define-key function-key-map "\e[1;9D" [M-left])))
(winner-mode 1)
(add-hook 'find-file-hook 'find-file-check-line-endings)
(defun dos-file-endings-p ()
  (string-match "dos" (symbol-name buffer-file-coding-system)))
(defun find-file-check-line-endings ()
  (when (dos-file-endings-p)
    (set-buffer-file-coding-system 'undecided-unix)
    (set-buffer-modified-p nil)))

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
(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
(global-unset-key (kbd "s-m"))

;; font setup

(when (eq system-type 'darwin)
  ;; remove tool bar
  (toggle-tool-bar-mode-from-frame 0)

  ;; default Latin font (e.g. Consolas)
  (set-face-attribute 'default nil :family "Monaco")

  ;; default font size (point * 10)
  ;;
  ;; WARNING!  Depending on the default font,
  ;; if the size is not supported very well, the frame will be clipped
  ;; so that the beginning of the buffer may not be visible correctly.
  (set-face-attribute 'default nil :height 150)
  ;; you may want to add different for other charset in this way.
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face (quote default))
 '(ess-default-style (quote OWN))
 '(ess-own-style-list
   (quote
    ((ess-indent-offset . 2)
     (ess-offset-arguments . open-delim)
     (ess-offset-arguments-newline . prev-call)
     (ess-offset-block . prev-line)
     (ess-offset-continued . straight)
     (ess-align-nested-calls "ifelse")
     (ess-align-arguments-in-calls "function[   ]*(")
     (ess-align-continuations-in-calls . t)
     (ess-align-blocks control-flow)
     (ess-indent-from-lhs arguments fun-decl-opening)
     (ess-indent-from-chain-start . t)
     (ess-indent-with-fancy-comments . t))))
 '(fci-rule-color "#3C3D37")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(hl-sexp-background-color "#efebe9")
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (monokai-alt-theme rbenv haml-mode reek ruby-reek rubocop leuven-theme yasnippet yaml-mode web-mode use-package terraform-mode tagedit spray smex smartparens rainbow-mode rainbow-delimiters plantuml-mode projectile-rails polymode php-mode php+-mode paredit org-wunderlist nginx-mode mwim markdown-preview-mode markdown-mode+ magit less-css-mode json-mode js2-mode jinja2-mode ido-ubiquitous helm-projectile helm-company helm-ag handlebars-mode flycheck feature-mode exec-path-from-shell ess dockerfile-mode docker-tramp docker dash-at-point company-web company-jedi company-inf-ruby company-ansible coffee-mode clojure-mode-extra-font-locking cider ansible alchemist ag)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(safe-local-variable-values (quote ((docker-image-name . "rails"))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(visible-bell nil)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
