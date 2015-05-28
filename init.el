;; package system
;; see https://github.com/technomancy/emacs-starter-kit
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize) ;; You might already have this line

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(ag
                      cider
                      clojure-mode
                      coffee-mode
                      dash-at-point
                      erlang
                      exec-path-from-shell
                      feature-mode
                      handlebars-mode
                      helm-ag
                      helm-projectile
                      markdown-mode
                      monokai-theme
                      plantuml-mode
                      projectile
                      puppet-mode
                      rainbow-mode
                      rvm
                      sass-mode
                      scss-mode
                      smartparens
                      starter-kit
                      starter-kit-bindings
                      starter-kit-eshell
                      starter-kit-js
                      starter-kit-lisp
                      yaml-mode
                      yasnippet
                      zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/vendor")

(require 'pbcopy)
(turn-on-pbcopy)

(require 'yasnippet)
(yas-load-directory "~/.emacs.d/snippets")
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)
(add-hook 'prog-mode-hook
          '(lambda ()
             (yas-minor-mode)))
;; plantuml-mode
(setq plantuml-jar-path (expand-file-name "~/bin/plantuml.jar"))
(eval-after-load "plantuml-mode"
  '(progn
     (setq whitespace-line-column 250)
     (defun plantuml-compile ()
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
       (define-key map "\C-c\C-c" 'plantuml-compile)
       (setq plantuml-mode-map map))))
;; my defaults
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq css-indent-offset 2)
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)
(setq-default js-indent-level 2)
(setq-default python-indent 2)
(toggle-debug-on-error 1)
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t)
(blink-cursor-mode t)
(show-smartparens-global-mode +1)
(show-paren-mode 0)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(global-subword-mode 1)
(menu-bar-mode 0)
(tool-bar-mode -1)
(column-number-mode 1)

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

;; (global-set-key "\C-x\C-k" 'kill-region)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "<f5>") 'helm-projectile)
(global-set-key (kbd "<f6>") 'helm-projectile-ag)
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
     (define-key ruby-mode-map (kbd "TAB") 'smart-tab)))

(add-hook 'css-mode-hook 'rainbow-mode)

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
(add-to-list 'auto-mode-alist '("\\.plu$" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; my modes
(recentf-mode)
(global-whitespace-mode 1)
(auto-fill-mode 0)
(global-linum-mode 0)
;; (setq linum-format "%d ")
;; (setq linum-format "%4d \u2502")

(unless window-system
  (add-hook 'linum-before-numbering-hook
            (lambda ()
              (setq-local linum-format-fmt
                          (let ((w (length (number-to-string
                                            (count-lines (point-min) (point-max))))))
                            (concat "%" (number-to-string w) "d"))))))

(defun linum-format-func (line)
  (concat
   (propertize (format linum-format-fmt line) 'face 'linum)
   (propertize " " 'face 'mode-line-buffer-id)))

(unless window-system
  (setq linum-format 'linum-format-func))

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

;; helm and search

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a041a61c0387c57bb65150f002862ebcfe41135a3e3425268de24200b82d6ec9" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" "aa0cff9f0399a01e35a884bebe67039e3f8890dbe69ebaaa6e8d307dce50dfcd" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "fa189fcf5074d4964f0a53f58d17c7e360bb8f879bd968ec4a56dc36b0013d29" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "9f443833deb3412a34d2d2c912247349d4bd1b09e0f5eaba11a3ea7872892000" default)))
 '(debug-on-error t)
 '(magit-use-overlays nil))
;;(load-theme 'monokai)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
