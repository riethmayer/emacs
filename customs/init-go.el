(use-package go-mode
  :config (use-package godoctor)
  :config (use-package go-eldoc)
  :config (use-package golint
            :init
            (add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/golang/lint/misc/emacs"))
            :demand t
            :ensure t)
  :config (use-package go-guru
            :demand t
            :ensure t)
  :init
  (defun go-run-buffer()
    (interactive)
    (shell-command (concat "go run " (buffer-name))))
  (defun go-mode-setup ()
    ;; setup go-eldoc
    (go-eldoc-setup)
    ;; use goimports instead of go-fmt
    (setq gofmt-command "goimports")
    ;; Call go-fmt before saving
    (add-hook 'before-save-hook 'gofmt-before-save)
    ;; Customize compile command to run go build
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go vet && golint"))
    ;; Godef bindings
    (local-set-key (kbd "M-.") 'godef-jump)
    ;; compile

    (define-key (current-local-map) "\C-c\C-c" 'compile)
    (define-key (current-local-map) "\C-x\C-e" 'go-run-buffer)
    (define-key (current-local-map) "\C-c\C-r\C-e" 'godoctor-extract)
    (define-key (current-local-map) "\C-c\C-r\C-r" 'godoctor-rename)
    (define-key (current-local-map) "\C-c\C-r\C-t" 'godoctor-toggle)
    (define-key (current-local-map) "\C-c\C-r\C-g" 'godoctor-godoc)
    (go-guru-hl-identifier-mode)
    )
  (add-hook 'go-mode-hook 'go-mode-setup)
  :ensure t)

(use-package company-go
  :init
  (add-hook 'go-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '(company-go))
              (company-mode)))
  :ensure t)
(use-package flymake-go
  :ensure t)
(use-package flycheck-gometalinter
  :ensure t
  :config
  (progn
    (flycheck-gometalinter-setup)))

(provide 'init-go)
