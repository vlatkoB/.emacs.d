;;; package --- haskell-init
;;; Commentary:
;;; Code:


;; Define everything needed for Haskell development

;; Applying hlint suggestion -- doesn't work
(use-package hlint-refactor
  :ensure t
  :diminish hlint-refactor-mode
  :init
    (add-hook 'haskell-mode-hook 'hlint-refactor-mode)
    (bind-key "C-c , b" 'hlint-refactor-refactor-buffer   haskell-mode-map)
    (bind-key "C-c , r" 'hlint-refactor-refactor-at-point haskell-mode-map)
 )


;; Setup auto-completion with company
(use-package company-cabal
  :ensure t
  :config
    (eval-after-load 'company-mode (add-to-list 'company-backends 'company-cabal))
 )


;; Setup mode for hamlet, julius and lucius templates.
(use-package shakespeare-mode
  :ensure t
  :bind (:map shakespeare-mode-map
           ("C-<"   . 'haskell-move-nested-left)
           ("C->"   . 'haskell-move-nested-right)
           ;; ("<tab>" . 'shakespeare-hamlet-mode-indent-line)
           )
  :init

  )
;; (use-package mmm-mode :disabled -- slows down typing
;;   :ensure t
;;   :diminish mmm-mode
;;   :init
;;     (use-package mmm-auto)
;;     (use-package mmm-vars)
;;     (setq mmm-global-mode 'maybe
;;           mmm-submode-decoration-level 2)
;;     ;; (mmm-define-key (kbd "<f8>") 'haskell-navigate-imports haskell-mode-map)
;;     (mmm-add-classes
;;      '((hamlet-quasiquote
;; 	:submode hamlet-mode
;; 	:delimiter-mode nil
;; 	:front "\\[?hamlet|"
;; 	:back "|\\]")))
;;     (mmm-add-mode-ext-class 'haskell-mode nil 'hamlet-quasiquote)
;;  )


;; Prepare Snippets
(use-package haskell-snippets :ensure t)


;; Custom variables for Haskell
(setq tags-case-fold-search t) ;; tags operations case-sensitive


(use-package haskell-mode-init)

;; (use-package haskell-ide-engine-init :disabled)

;; Disable indentation after RET in Hamlet
;; (add-hook 'hamlet-mode (lambda() (electric-indent-mode -1)))
;; (add-hook 'shakespeare-mode (lambda() (electric-indent-mode -1)))


(provide 'haskell-init)

;;; haskell-init ends here
