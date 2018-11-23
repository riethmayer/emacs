;; init-js.el for js specific setup

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  :commands (js2-mode)
  :config
  (setq prettier-js-args '("--single-quote"  ))
  (evil-leader/set-key-for-mode 'js2-mode "j"  'js2-jump-to-definition)
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'js2-mode-hook
            (defun my-js2-mode-setup ()

              (flycheck-add-mode 'javascript-eslint 'js2-mode)
              (when (executable-find "eslint")
                (flycheck-select-checker 'javascript-eslint)))))

(flycheck-def-config-file-var flycheck-typescript-tsconfig
    typescript-tslint "tslint.json"
  :safe #'stringp
  :package-version '(flycheck . "27"))

(flycheck-define-checker typescript-tslint-cesco
  "TypeScript style checker using TSLint."
  :command ("tslint" "--format" "json"
            (config-file "--config" flycheck-typescript-tslint-config)
            (config-file "--project" flycheck-typescript-tsconfig)
            (option "--rules-dir" flycheck-typescript-tslint-rulesdir)
            (eval flycheck-tslint-args)
            source-inplace)
  :error-parser flycheck-parse-tslint
  :modes (web-mode typescript-mode))

(add-to-list 'flycheck-checkers 'javascript-eslint)
(flycheck-add-mode 'typescript-tslint-cesco 'web-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)

(defun cesco/tslint ()
  ;; (flycheck-select-checker 'typescript-tslint)
  )
(defun cesco/custom-tslint ()
  (if (projectile-project-p)
      (if (file-exists-p (concat ( projectile-project-root ) "tsconfig.json"))
          (progn
            (setq flycheck-typescript-tsconfig . ( (concat projectile-project-root "tsconfig.json" )))
            ;; (flycheck-select-checker 'typescript-tslint-cesco)
            )
        (cesco/tslint)
        )))

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

(defun setup-tide-mode ()
   (interactive)
   (tide-setup)
   (flycheck-mode +1)
   (setq flycheck-check-syntax-automatically '(save mode-enabled))
   (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append)
   (eldoc-mode +1)
   (company-mode +1))

(defun cesco/tide-mode ()
  (interactive)
  (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))
  (setq tide-tsserver-executable "node_modules/.bin/tsserver")
  (tide-setup)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (add-hook 'before-save-hook 'tide-format-before-save)  
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (company-mode +1)
  ;; (add-to-list 'company-backends '(company-tide :with company-yasnippet))
  ;; (set (make-local-variable 'company-backends)
  ;;      '((company-tide company-files company-yasnippet)))
  )

(defun cesco/tide-evil ()
  (evil-leader/set-key-for-mode 'web-mode
                                "j" (lambda () (interactive)(tide-jump-to-definition))
                                )
  )

(defun cesco/hugo ()
  (if (projectile-project-p)
      (if (file-exists-p (concat (projectile-project-root) "config.toml"))
          (web-mode-set-engine "go")
        (message "do not exists") ;; You can safely delet this line
        )
    )
  )

(use-package tide
  :diminish tide-mode
  :after (flycheck evil-leader)
  :config
  (cesco/tide-evil)
  (add-hook 'typescript-mode-hook #'cesco/tide-mode) )
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  )
(use-package web-mode :ensure t
  :ensure tide
  :mode (
         ("\\.tsx$" . web-mode)
         ("\\.html" . web-mode)
         ("\\.jsx$" . web-mode))
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (cesco/tide-mode))))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "jsx" (file-name-extension buffer-file-name))
                (cesco/tide-mode))))
  (add-hook 'web-mode-hook  'my-web-mode-hook)
  (add-hook 'web-mode-hook 'cesco/hugo)
  )

(provide 'init-js)
