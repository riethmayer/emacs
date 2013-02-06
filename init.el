;; package
;; see https://github.com/technomancy/emacs-starter-kit
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; my defaults
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t)
(scroll-bar-mode 0)
(blink-cursor-mode t)
(show-paren-mode t)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(global-subword-mode 1)
(menu-bar-mode 0)
(tool-bar-mode -1)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; my functions

(defun recompile ()
  "Recompile .emacs.d"
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.emacs.d") 0))

;; start server and fullscreen
(run-with-idle-timer 0.1 nil 'ns-toggle-fullscreen)
(server-force-delete)
(server-start)

;; (setq-default indent-tabs-mode nil)
;; (column-number-mode 1)
;; (ido-mode 1)
