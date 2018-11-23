;;; package --- init-js.el
;;; Commentary:
;;  init-js.el for js specific setup


;;; Code:
;; JSON setup
(use-package json-mode
  :ensure t)

(defun my/prettier ()
  (interactive)
  (shell-command
    (format "%s --write %s"
      (shell-quote-argument (executable-find "prettier"))
      (shell-quote-argument (expand-file-name buffer-file-name))))
  (revert-buffer t t t))

;; use eslint config from current project directory
(use-package flycheck
  :config
  (setq-default flycheck-disabled-checker 'javascript-jshint)
  (setq-default flycheck-disabled-checker 'json-jsonlist)
  (setq-default flycheck-disabled-checker 'javascript-eslint)
  (setq-default flycheck-javascript-eslint-executable "eslint-project-relative")
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
    (or (buffer-file-name) default-directory)
      "node_modules"))
      (eslint (and root
      (expand-file-name "node_modules/eslint/bin/eslint.js"
        root))))
      (when (and eslint (file-executable-p eslint))
    (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (global-flycheck-mode)
  )

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (company-mode +1)
  (eldoc-mode +1)
  (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode +1))
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :config
  (setq company-tooltip-align-annotations t)
  (add-hook 'web-mode-hook #'setup-tide-mode))

(use-package web-mode
  :config
  (defun my-web-mode-hook ()
    "Hooks for Web mode. Adjust indents"
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-attr-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-css-colorization t)
    (setq web-mode-code-indent-offset 2)
    (setq css-indent-offset 2)
    (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")
                                         ("typescript" . "\\.js[x]?\\'"))))
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts[x]?\\'" . web-mode))
  (defun my-js-save-hook ()
    (add-hook 'before-save-hook 'whitespace-cleanup nil 'make-it-local))
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
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "ts")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "tsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))
  (add-hook 'web-mode-hook 'my-js-save-hook)
  (add-hook 'web-mode-hook 'my-web-mode-hook)
  :ensure t)

(provide 'init-js)
