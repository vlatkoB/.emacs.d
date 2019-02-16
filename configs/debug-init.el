;;; package --- debug-init
;;; Commentary:
;;; Code:


(setq debug-on-message "Compiler version mismatched")
;; (setq debug-on-message "ghc-with")
(setq debug-on-error t)
(setq debug-on-quit t)
;; (setq debug-on-entry 'haskell-process-suggest-imports)
;; (setq debug-on-entry 'haskell-process-trigger-suggestions)

(provide 'debug-init)

;;; debug-init ends here
