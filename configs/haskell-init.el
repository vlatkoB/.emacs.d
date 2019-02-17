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


;; ;; Prepare Hamlet, Shakespeare, ...
;; (use-package hamlet-mode      :ensure t)
;; (use-package shakespeare-mode :ensure t)
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


;; Rules for automatic alignement
(eval-after-load "align"
  '(add-to-list 'align-rules-list '(haskell-types
     (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
     (modes quote (haskell-mode literate-haskell-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list '(haskell-assignment
     (regexp . "\\(\\s-+\\)=\\s-+")
     (modes quote (haskell-mode literate-haskell-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list '(haskell-arrows
     (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
     (modes quote (haskell-mode literate-haskell-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list '(haskell-left-arrows
     (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
     (modes quote (haskell-mode literate-haskell-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list '(haskell-comments
     (regexp . "\\(\\s-+\\)\\(--\\)\\s-+")
     (modes quote (haskell-mode)))))

;; Custom variables for Haskell
(setq tags-case-fold-search t) ;; tags operations case-sensitive


(use-package haskell-mode-init)

(use-package haskell-ide-engine-init :disabled)

;; Disable indentation after RET in Hamlet
;; (add-hook 'hamlet-mode (lambda() (electric-indent-mode -1)))
;; (add-hook 'shakespeare-mode (lambda() (electric-indent-mode -1)))


(provide 'haskell-init)

;;; haskell-init ends here
