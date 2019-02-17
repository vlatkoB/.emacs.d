;;; package --- haskell-ide-engine-init
;;; Commentary:
;;; Code:


;; Haskell IDE engine


(use-package lsp-mode
  :ensure t
  :config

  ;; make sure we have lsp-imenu everywhere we have LSP
  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

  ;; lsp extras
  (use-package lsp-ui :ensure t
    :config
		  (setq lsp-ui-sideline-ignore-duplicate t)
      (add-hook 'lsp-mode-hook 'lsp-ui-mode))

  (use-package company-lsp :ensure t
    :config
      (push 'company-lsp company-backends))
	)

(with-eval-after-load 'lsp-mode
  (require 'lsp-flycheck)
	)


(use-package lsp-haskell :ensure t ;; :disabled
  :init
	  (add-hook 'haskell-mode-hook #'lsp-haskell-enable)
	  (add-hook 'haskell-mode-hook 'flycheck-mode)
 )



(provide 'haskell-ide-engine-init)

;;; haskell-ide-engine-init ends here
