;;; package --- ecb-init
;;; Commentary:
;;; Code:

(use-package ecb :ensure t
	:demand t
  :bind(
    ("C-S-m" . ecb-toggle-ecb-windows)
    ("C-d"   . ecb-goto-window-directories)
    ("C-e"   . ecb-goto-window-edit1)
    ("C-f"   . ecb-goto-window-methods)
    ("C-k"   . ecb-goto-window-compilation)
		)
	:custom
 		  (ecb-windows-width                        0.5)
      (ecb-auto-activate                        t)
      (ecb-mode-line-display-window-number      nil)
      (ecb-tip-of-the-day                       nil)
      (ecb-show-sources-in-directories-buffer  'always)
      (ecb-compile-window-height                12)
      (ecb-compile-window-width                'frame)
      (ecb-compilation-major-modes             '(compilation-mode
																					 			haskell-interactive-mode
																					 			))
      (ecb-layout-name                         "my-layout")
      (ecb-excluded-directories-regexps        '("^\\(CVS\\|\\.[^xX]*\\)$" ".stack-work" "dist-newstyle"))
      (ecb-maximize-ecb-window-after-selection t)
      ;; (ecb-layout-window-sizes '(("left15" (ecb-directories-buffer-name 0.15 . 0.5))))
      ;; (ecb-layout-name "left15")
			;; (ecb-edit-w)
	:init (custom-set-variables '(ecb-options-version "2.50"))
  :config (add-to-list 'ecb-compilation-buffer-names '("*Compile-Log*"))
	        (add-to-list 'ecb-compilation-buffer-names '("*Warnings*"))
					(add-to-list 'ecb-compilation-buffer-names '("*haskell-process-log*"))
					(add-to-list 'ecb-compilation-buffer-names '("*Haskell Presentation*"))
					(add-to-list 'ecb-compilation-buffer-names '("*Flycheck errors*"))
					(add-to-list 'ecb-compilation-buffer-names '("*hoogle*"))
 )
(ecb-layout-define "my-layout" left nil (ecb-select-edit-window 1))



;; Adding keybinding to all or some ECB buffers
;; (add-hook 'ecb-common-tree-buffer-after-create-hook
;;   (lambda ()
;;     (local-set-key (kbd "C-a")
;;       (lambda ()
;;         (interactive)
;;         (message "ECB is great!")))))

;; (add-hook 'ecb-directories-buffer-after-create-hook
;;   (lambda ()
;;     (local-set-key (kbd "C-d")
;;       (lambda ()
;;         (interactive)
;;         (message "ECB is wonderful!")
;;       ))))



(provide 'ecb-init)

;;; ecb-init ends here
