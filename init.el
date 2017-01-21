;;; installations via packages
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
(use-package ido-ubiquitous
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
               '("\\.\\(?:rmd\\|rmarkdown\\|RMD\\)\\'" . poly-markdown+r-mode))
  :ensure t)
(use-package markdown-preview-mode
  :ensure t)
(use-package monokai-theme
  :init
  (load-theme 'monokai t)
  :ensure t)
(use-package mwim
  :init
  (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)
  :ensure t)
(use-package nginx-mode
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
(use-package puml-mode
  :init
  (setq puml-plantuml-jar-path "/Users/riethmayer/bin/plantuml.jar")
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . puml-mode))
  (add-to-list 'auto-mode-alist '("\\.plu\\'" . puml-mode))
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . puml-mode))
  (setq plantuml-jar-path (expand-file-name "~/bin/plantuml.jar"))
  (eval-after-load "puml-mode"
    '(progn
       (setq whitespace-line-column 250)
       (defun puml-compile ()
         "Run plantuml over current file and open the result png."
         (interactive)
         (let ((file buffer-file-name))
           (shell-command (concat "java -jar '" plantuml-jar-path
                                  "' '" file "' -tpng"))
           (shell-command (concat "open -a Preview "
                                  (concat (file-name-directory file)
                                          (file-name-sans-extension
                                           (file-name-nondirectory file))
                                          ".png")))))

       (let ((map (make-sparse-keymap)))
         (define-key map "\C-c\C-c" 'puml-preview)
         (define-key map "\C-c\C-i" 'puml-compile)
         (setq puml-mode-map map))))
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
(use-package ruby-mode
  :init
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist
               '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|Procfile|[rR]akefile\\)\\'" . ruby-mode))
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

  (add-to-list 'auto-mode-alist '("\\.css\\'"    . web-mode))       ;; CSS
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
(global-unset-key (kbd "s-m"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-safe-themes
   (quote
    ("4156d0da4d9b715c6f7244be34f2622716fb563d185b6facedca2c0985751334" default)))
 '(hl-sexp-background-color "#efebe9")
 '(package-selected-packages
   (quote
    (monokai-theme leuven-theme yasnippet yaml-mode web-mode use-package terraform-mode tagedit spray smex smartparens rainbow-mode rainbow-delimiters puml-mode projectile-rails polymode php-mode php+-mode paredit org-wunderlist nginx-mode mwim markdown-preview-mode markdown-mode+ magit less-css-mode json-mode js2-mode jinja2-mode ido-ubiquitous helm-projectile helm-company helm-ag handlebars-mode flycheck feature-mode exec-path-from-shell ess dockerfile-mode docker-tramp docker dash-at-point company-web company-jedi company-inf-ruby company-ansible coffee-mode clojure-mode-extra-font-locking cider ansible alchemist ag)))
 '(safe-local-variable-values (quote ((docker-image-name . "rails")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
