;; package system
;; see https://github.com/technomancy/emacs-starter-kit
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; my packages

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit
                      starter-kit-js
                      starter-kit-eshell
                      exec-path-from-shell
                      projectile
                      yasnippet)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/vendor")
;; (require 'rcodetools)

(require 'yasnippet)
(yas/load-directory "~/.emacs.d/snippets")
(yas-global-mode 1)
(add-hook 'prog-mode-hook
          '(lambda ()
             (yas-minor-mode)))

;; my defaults
(toggle-debug-on-error 1)
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t)
(scroll-bar-mode -1)
(blink-cursor-mode t)
(show-paren-mode t)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(global-subword-mode 1)
(menu-bar-mode 0)
(tool-bar-mode -1)

;; my keys

;; unlearning meta key for windows
;;(setq mac-option-key-is-meta nil
;;      mac-command-key-is-meta t
;;      mac-command-modifier 'meta
;;      mac-option-modifier 'super
;;      mac-pass-command-to-system nil)

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
(global-set-key (kbd "M-n") 'ns-toggle-fullscreen)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "<f5>") 'projectile-find-file)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
;; (define-key ruby-mode-map (kbd "C-c C-c") 'xmp)

(setq visible-bell 1)

;; my functions

(defun recompile-emacs ()
  "Recompile .emacs.d"
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.emacs.d") 0))

(defun indent-buffer ()
  "indent whole buffer"
  (interactive)
  (whitespace-cleanup)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(global-set-key (kbd "C-c n") 'indent-buffer)

(projectile-global-mode)

;; apply modes for filenames

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

;; my modes
(recentf-mode)
(global-whitespace-mode 1)

;; start server

(server-force-delete)
(server-start)

;; cocoa specifics
(when (memq window-system '(mac ns))
  (set-face-attribute 'default nil :font "Menlo-20")
  (run-with-idle-timer 0.1 nil 'ns-toggle-fullscreen)
  (exec-path-from-shell-initialize))

;; (set-face-font 'default "-apple-mensch-medium-r-normal--14-0-72-72-m-0-iso10646-1")
;; (setq-default indent-tabs-mode nil)
;; (column-number-mode 1)
;; (ido-mode 1)
(load-theme 'tango-dark)
(eval-after-load "magit"
  '(set-face-attribute 'magit-item-highlight nil :foreground "#ffffff" :background "#3f4747"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(debug-on-error t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "Magenta" :foreground "Black"))))
 '(hl-line ((t (:inherit highlight :foreground "Black")))))
