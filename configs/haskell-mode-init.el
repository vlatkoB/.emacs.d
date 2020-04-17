;;; package --- haskell-mode-init
;;; Commentary:
;;; Code:


;; Restart REPL process as it can eat lot of memory after a while
;; (run-with-idle-timer 600 t '(lambda ()
;;   (when (hsession-active) (haskell-process-restart))))

;; Disable haskell-doc-mode before suggestions, otherwise emacs blocks
;; (advice-add 'haskell-process-trigger-suggestions
;;    :around #'haskell-process-trigger-suggestions-ad)

;; (custom-set-variables
;;  '(haskell-mode-hook
;;    (quote
;;     (capitalized-words-mode flyspell-prog-mode haskell-decl-scan-mode haskell-indentation-mode highlight-uses-mode imenu-add-menubar-index interactive-haskell-mode
;; 			    (lambda nil
;; 			      (set
;; 			       (make-local-variable
;; 				(quote company-backends))
;; 			       (append
;; 				(quote
;; 				 ((company-capf company-dabbrev-code)))
;; 				company-backends))))))
;; )


;; Workaround fo haskell-mode stopped clearing REPL before load/reload
;; Not needed any more. All works as normal. ????
(defun clear-then-load ()
"Clear REPL and then load file."
  (interactive)
  (haskell-interactive-mode-clear)
  (haskell-process-load-file)
  )
(defun clear-then-reload ()
"Clear REPL and then reload file."
  (interactive)
  (haskell-interactive-mode-clear)
  (haskell-process-reload)
  )


;; Setup haskell-mode
(use-package haskell-mode :ensure t
  :config
    ;; Change major modes names from modeline
    (add-hook        'haskell-mode-hook             (lambda() (setq mode-name   "λ")))
    (add-hook        'haskell-cabal-mode-hook       (lambda() (setq mode-name "C-λ")))
    (add-hook        'haskell-interactive-mode-hook (lambda() (setq mode-name "I-λ")))
    (eval-after-load 'hi-lock-mode                 '(diminish hi-lock-mode))
    ;; "αβψδεφγηιξκλμνοπρστθωχυζς" "¿©£¶®§¥¢µ"

    (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
    ;; (add-hook 'haskell-mode-hook 'flyspell-prog-mode)


    (use-package haskell-interactive-mode)
    (use-package haskell-process)
  :bind (:map haskell-mode-map
			     ;; Repl stuff
					 ;; ("<f5>"     . 'haskell-process-reload)
					 ;; ("M-<f5>"   . 'haskell-process-load-file)
					 ("<f5>"     . 'clear-then-reload)
					 ("M-<f5>"   . 'clear-then-load)
					 ("C-<f5>"   . 'haskell-interactive-bring)
					 ("S-<f5>"   . 'haskell-interactive-switch)
					 ("M-."      . 'haskell-mode-goto-loc)
					 ("<f8>"     . 'haskell-navigate-imports)
					 ("C-M-p"    . 'beginning-of-defun) ;;--???
					 ("C-M-n"    . 'end-of-defun) ;;--???
					 ("M-c"      . 'haskell-compile)
					 ("M-b"      . 'haskell-process-cabal-build)
					 ("M-r"      . 'haskell-process-cabal)
					 ("C-q"      . 'haskell-cabal-visit-file)
					 ;; Editing
					 ;; ("SPC"      . 'haskell-mode-contextual-space)
					 (  "C-<f8>" . 'haskell-sort-imports)
					 (  "S-<f8>" . 'haskell-align-imports)
					 ("C-S-<f8>" . 'haskell-mode-format-imports)
					 ("M-đ"      . 'haskell-mode-stylish-buffer)
					 ("C-<"      . 'haskell-move-nested-left)
					 ("C->"      . 'haskell-move-nested-right)
					 ;; Documentation & info
					 ("M-i"      . 'haskell-process-do-info)
					 ("M-t"      . 'haskell-process-do-type)
					 ("S-<f4>"   . 'haskell-hayoo)
					 (  "<f4>"   . 'haskell-hoogle)
					 ("C-<f4>"   . 'haskell-w3m-open-haddock)
					 ;; Testing
					 ("S-<f12>"  . 'haskell-session-target-test)
					 (  "<f12>"  . 'haskell-session-rerun-test)
					 ;; Misc
					 (  "<f6>"   . 'haskell-who-calls)
					 ("S-<f6>"   . 'haskell-process-all-types)
					 ("C-c r"    . 'tags-reset-tags-tables)
	  	  :map haskell-cabal-mode-map
  	  	   ("C-<f5>"   . 'haskell-interactive-bring)
					 ("S-<f5>"   . 'haskell-interactive-switch)
					 ("C-l"      . 'haskell-interactive-mode-clear)
					 ;; Compile & build
					 ("M-c"      . 'haskell-compile)
					 ("M-b"      . 'haskell-process-cabal-build)
					 ("M-r"      . 'haskell-process-cabal)
	      :map haskell-interactive-mode-map
   				 ;; ("<f5>"     . 'haskell-process-reload)
   				 ("<f5>"     . 'clear-then-reload)
					 ("C-l"      . 'haskell-interactive-mode-clear)
					 ;; Moving & jumping
					 ("M-."      . 'haskell-mode-goto-loc)
					 ;; ("M-."      . 'haskell-mode-goto-loc)
					 ;; ("M-."      . 'haskell-mode-jump-to-def-or-tag)
					 ;; ("C-M-."    . 'haskell-mode-tag-find) ;; FIXME Use instead of M-. ?NOT needed ?
					 ;; Cabal stuff
					 ("M-r"      . 'haskell-process-cabal)
					 ("C-t"      . 'haskell-session-change-target)
					 ("C-c C-c"  . 'haskell-process-interrupt)
				 :map haskell-interactive-mode-map
				   ("C-<f4>"   . 'haskell-w3m-open-haddock)
					 ;; ("C-c C-e"  . 'haskell-mode-show-type-at) ;; Same as C-t
	   )
	:custom
		(add-to-list 'completion-at-point-functions     'haskell-process-completions-at-point)
		(haskell-align-imports-pad-after-name            t) ;; nil)
		(haskell-ask-also-kill-buffers                   nil)
		(haskell-completions-complete-operators          nil)
		(haskell-compile-cabal-build-command            "stack build -j 7 --fast --ghc-options=\"-j +RTS -A32M -RTS\"")
		;; (haskell-hoogle-url                             "https://hoogle.haskell.org/?hoogle=%s")
		(haskell-hoogle-command                          "hoogle --color -l --numbers --count=10")
		(haskell-indentation-electric-flag               t)
		(haskell-interactive-mode-eval-mode             'haskell-mode)
		(haskell-interactive-mode-hide-multi-line-errors nil)
		(haskell-interactive-mode-include-file-name      nil)
		(haskell-interactive-mode-scroll-to-bottom       t)
		(haskell-interactive-types-for-show-ambiguous    t) ;; check
		(haskell-interactive-popup-errors                nil)
    (haskell-mode-stylish-haskell-path              "stylish-haskell")
		(haskell-notify-p                                t)
		(haskell-process-args-cabal-repl                '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
		(haskell-process-args-cabal-new-repl            '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
		(haskell-process-args-ghci                      '("-ferror-spans" "-fshow-loaded-modules"))
		(haskell-process-args-stack-ghci                '("--ghci-options=-ferror-spans"
																											"--ghci-options=-fshow-loaded-modules"
																											"--ghci-options=-fno-diagnostics-show-caret"
																											"--no-build"
                                                      "--no-load"
																											))
		(haskell-interactive-set-+c                     nil) ;; testing
		(haskell-process-auto-import-loaded-modules      t) ;;nil)
		(haskell-process-do-cabal-format-string         ":!cd %s && %s")
		(haskell-process-load-or-reload-prompt           nil)
		(haskell-process-log                             t)
		(haskell-process-path-ghci                       "stack ghci")
		(haskell-process-reload-with-fbytecode           t) ;; nil)
		(haskell-process-suggest-haskell-docs-imports    t)
		(haskell-process-suggest-hoogle-imports          t)
		(haskell-process-suggest-hayoo-imports           nil)
		(haskell-process-suggest-remove-import-lines     t)

		(haskell-process-suggest-add-package             t)
		(haskell-process-type                           'stack-ghci)
		(haskell-process-use-presentation-mode           t)
		(haskell-process-show-debug-tips nil)
		(haskell-stylish-on-save                         nil) ;; twice undo-tree saving
		(haskell-tags-on-save                            t)
		(haskell-complete-module-preferred
		  '("ClassyPrelude" "Data.Conduit" "Data.Function" "Data.List" "Data.Map"))
		(haskell-language-extensions
		 '("ConstraintKinds"
			 "BangPatterns"
			 "ConstraintKinds"
			 "DataKinds"
			 "DeriveGeneric"
			 "EmptyDataDecls"
			 "ExistentialQuantification"
			 "FlexibleContexts"
			 "FlexibleInstances"
			 "GADTs"
			 "GeneralizedNewtypeDeriving"
			 "KindSignatures"
			 "LambdaCase"
			 "MultiParamTypeClasses"
			 "MultiWayIf"
			 "NoImplicitPrelude"
			 "NoMonomorphismRestriction"
			 "OverloadedStrings"
			 "PartialTypeSignatures"
			 "PatternGuards"
			 "QuasiQuotes"
			 "RankNTypes"
			 "RecordWildCards"
			 "ScopedTypeVariables"
			 "StandaloneDeriving"
			 "TemplateHaskell"
			 "TupleSections"
			 "TypeApplications"
			 "TypeFamilies"
			 "TypeOperators"
			 "TypeSynonymInstances"
			 "ViewPatterns"
			 ))
 )


;; Setup haskell-doc
(use-package haskell-doc
  :diminish haskell-doc-mode
  :config
    (setq haskell-doc-show-global-types nil
	  haskell-doc-show-reserved     t
	  haskell-doc-show-prelude      nil ;;t
	  haskell-doc-show-strategy     t
	  haskell-doc-show-user-defined t
	  haskell-doc-chop-off-context  nil ;; t
	  haskell-doc-chop-off-fctname  nil
	  haskell-doc-prettify-types    nil)
    (add-hook 'haskell-mode-hook 'haskell-doc-mode)
 )


;; Prepare auto completion with company
(use-package company-ghci
  :ensure t
  :config
    (eval-after-load 'company-mode (add-to-list 'company-backends 'company-ghci))
    )


;; Prepare flyCheck
(use-package flycheck-haskell :ensure t
	:bind (:map haskell-mode-map
					 ("M-n" . 'flycheck-next-error)
					 ("M-p" . 'flycheck-previous-error)
				)
  :init (add-hook 'haskell-mode-hook 'flycheck-haskell-setup)
	)



(setq haskell-import-mapping
  '(("Data.Text" . "import qualified Data.Text as T
import Data.Text (Text)")
        ("Data.Text.Lazy" . "import qualified Data.Text.Lazy as LT")
        ("Data.ByteString" . "import qualified Data.ByteString as B
import Data.ByteString (ByteString)")
        ("Data.ByteString.Lazy" . "import qualified Data.ByteString.Lazy as BL")
        ("Data.Map" . "import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)")
        ("Data.Map.Strict" . "import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)")
        ("Data.Set" . "import qualified Data.Set as Set
import Data.Set (Set)")
        ("Data.Vector" . "import qualified Data.Vector as V
import Data.Vector (Vector)")))


(setq completion-at-point-functions
	(append '(haskell-process-completions-at-point)
		completion-at-point-functions)
 )


(diminish 'interactive-haskell-mode)

(provide 'haskell-mode-init)

;;; haskell-mode-init ends here
