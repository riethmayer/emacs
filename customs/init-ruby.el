(use-package company-inf-ruby
  :init
  (add-to-list 'company-backends 'company-inf-ruby)
  :ensure t)

(use-package flymake-ruby
  :init
  (add-hook 'ruby-mode-hook 'flymake-ruby-load)
  :ensure t)

(use-package rubocop
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'rubocop-mode)
  :diminish rubocop-mode)
(use-package rbenv
  :init
  (global-rbenv-mode)
  :ensure t)
;; switch from ruby-mode
(use-package enh-ruby-mode
  :init
  (progn
    (add-hook 'ruby-mode-hook 'rbenv-use-corresponding)
    (add-hook 'ruby-mode-hook 'rspec-mode)
    (add-hook 'enh-ruby-mode-hook 'robe-mode)
    (add-to-list 'auto-mode-alist
                 '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . enh-ruby-mode))
    (add-to-list 'auto-mode-alist
                 '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|Procfile|[rR]akefile\\)\\'" . enh-ruby-mode)))
  (setq ruby-insert-encoding-magic-comment nil)
  (setq ruby-deep-indent-paren nil)
  :ensure t)
(use-package robe
  :init
  (eval-after-load 'company
    '(push 'company-robe company-backends))
  :ensure t)
(use-package rvm
  :init
  (global-set-key (kbd "C-c r a") 'rvm-activate-corresponding-ruby)
  :ensure t)

(provide 'init-ruby)
