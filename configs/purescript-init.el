;;; package --- purescript-init
;;; Commentary:
;;; Code:


;; Main purescript mode
(use-package purescript-mode
  :ensure  t
  )

;; REPL mode for purescript
(use-package psci
  ;; :load-path "~/.emacs.d/elisp/psci/"
 :ensure  t
  :custom
    (psci/arguments '("src/**/*.purs" "test/**/*.purs"))
  :bind
    (:map psci-mode-map
  		 ("C-l" . 'comint-clear-buffer)
    )
  )



;; Purs IDE
(use-package psc-ide
  :ensure  t
  :config
    (add-hook 'purescript-mode-hook
              (lambda ()
                (company-mode)
                (flycheck-mode)
                (inferior-psci-mode)
                (psc-ide-mode)
                (turn-on-purescript-indentation)
                ))
  :bind
    (:map psc-ide-mode-map
       ("  <f6>"    . 'xref-find-references)
			 ("M-đ"       . 'purescript-align-sort-imports)
			 ("S-<f8>"    . 'haskell-align-imports)
       ("C-<f8>"    . 'purescript-sort-imports)
       ("C-<"       . 'purescript-move-nested-left)
			 ("C->"       . 'purescript-move-nested-right)
       ("  <f5>"    . 'psci/load-current-file!)
       ("M-<f5>"    . 'purescript-load-current-module-and-prelude-plus)
			 (  "<f4>"    . 'purescript-pursuit)
       (  "<f12>"   . 'psc-ide-flycheck-insert-suggestion)
       ("M-<f12>"   . 'psci)
       ("C-SPC"     . 'company-complete)
       ;; ("<backtab>" . 'purescript-indentation-delete-backward-char)
       ;; ("M-i"      . 'haskell-process-do-info)
			 ;; ("M-t"      . 'haskell-process-do-type)
			 ;; (  "<f6>"   . 'haskell-who-calls)

    )
  :custom
    (psc-ide-add-import-on-completion        t)
    ;; (psc-ide-rebuild-on-save                 t) ;; otherwise opens build windows on each save
    (psc-ide-editor-mode                     t)
    (purescript-align-imports-pad-after-name nil)
    ;; (psc-ide-flycheck-ignored-error-codes
    ;;   '("UnusedImport" "UnusedExplicitImport" "DebugWarning" "ImplicitImport")
    ;; )
 )

(defun purescript-load-current-module-and-prelude-plus ()
"Load the current file in the psci repl and import Prelude."
  (interactive)
  (save-buffer)
  (progn
    (psci/load-current-file!)
    (psci--run-psci-command! "import PreludePlus")
  )
)

(defun purescript-align-sort-imports ()
"Align and sort imports."
  (interactive)
  (progn
    (haskell-align-imports)
    (haskell-sort-imports)
   )
)


(provide 'purescript-init)

;;; purescript-init.el ends here
