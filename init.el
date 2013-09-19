;; package system
;; see https://github.com/technomancy/emacs-starter-kit
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
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
                      ack-and-a-half
                      rvm
                      feature-mode
                      puppet-mode
                      coffee-mode
                      zenburn-theme
                      dash-at-point
                      monokai-theme
                      plantuml-mode
                      yasnippet)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/vendor")

(require 'yasnippet)
(yas-load-directory "~/.emacs.d/snippets")
(yas-global-mode 1)
(add-hook 'prog-mode-hook
          '(lambda ()
             (yas-minor-mode)))

;; plantuml-mode
(setq plantuml-jar-path (expand-file-name "~/bin/plantuml.jar"))
(eval-after-load "plantuml-mode"
  '(progn
     (defun plantuml-compile ()
       "Run plantuml over current file and open the result png."
       (interactive)
       (let ((file buffer-file-name))
         (shell-command (concat "java -jar '" plantuml-jar-path
                                "' '" file "' -tpng"))
         (display-buffer (find-file-noselect
                          (concat (file-name-directory file)
                                  (file-name-sans-extension
                                   (file-name-nondirectory file))
                                  ".png")))))

     (let ((map (make-sparse-keymap)))
       (define-key map "\C-c\C-c" 'plantuml-compile)
       (setq plantuml-mode-map map))))
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
(column-number-mode 1)
;; (global-linum-mode 1)
;; (setq linum-format "%d ")

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
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "<f5>") 'projectile-find-file)
(global-set-key (kbd "<f6>") 'projectile-ack)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-unset-key (kbd "s-m"))

(setq tramp-default-method "ssh")
(setq tramp-auto-save-directory "~/tmp/tramp/")
(setq tramp-chunksize 2000)

(eval-after-load 'ruby-mode
  '(progn
     (rvm-use-default)
     (require 'rcodetools)
     (define-key ruby-mode-map (kbd "M-/") 'comment-dwim)
     (define-key ruby-mode-map (kbd "TAB") 'smart-tab)
     (define-key ruby-mode-map (kbd "C-c C-c") 'xmp)))

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
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
(add-to-list 'auto-mode-alist '("\\.uml$\\'" . plantuml-mode))

;; my modes
(recentf-mode)
(global-whitespace-mode 1)
(auto-fill-mode 0)
;; start server
(server-force-delete)
(server-start)

;; cocoa specifics
(when (memq window-system '(mac ns))
  (set-face-attribute 'default nil :font "Menlo-22")
  (exec-path-from-shell-initialize))

(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (dabbrev-expand nil))
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (looking-at "\\_>")
          (let ((yas-fallback-behavior nil))
            (unless (yas-expand)
              (dabbrev-expand nil)))
        (indent-for-tab-command)))))

(add-to-list 'load-path "/path/to/dash-at-point")
(autoload 'dash-at-point "dash-at-point"
  "Search the word at point with Dash." t nil)
(global-set-key "\C-cd" 'dash-at-point)

;; (set-face-font 'default "-apple-mensch-medium-r-normal--14-0-72-72-m-0-iso10646-1")
;; (setq-default indent-tabs-mode nil)

;; (ido-mode 1)
;; (eval-after-load "magit"
;;   '(set-face-attribute 'magit-item-highlight nil :foreground "#ffffff" :background "#3f4747"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fa189fcf5074d4964f0a53f58d17c7e360bb8f879bd968ec4a56dc36b0013d29" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "9f443833deb3412a34d2d2c912247349d4bd1b09e0f5eaba11a3ea7872892000" default)))
 '(debug-on-error t))
(load-theme 'monokai)
