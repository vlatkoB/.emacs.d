;;; package --- dummy
;;; Commentary:
;;; Code:

;; Dummy file for saving customizations done with `customize'

(provide 'dummy)
;;; dummy.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(company-backends
   (quote
    (company-etags company-bbdb company-capf company-dabbrev-code company-gtags company-dabbrev company-files company-nxml company-css)))
 '(custom-safe-themes
   (quote
    ("26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" default)))
 '(delete-selection-mode t)
 '(ecb-options-version "2.50")
 '(git-gutter:hide-gutter t)
 '(git-gutter:verbosity 0)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" ".stack-work" "dist-newstyle")))
 '(grep-find-ignored-files
   (quote
    ("TAGS" ".#*" "*.hi" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo")))
 '(js-indent-level 2)
 '(line-number-mode t)
 '(menu-bar-mode t)
 '(normal-erase-is-backspace-mode t)
 '(package-selected-packages
   (quote
    (company-ghci yaml-mode w3m use-package undo-tree tabbar smex restclient rainbow-delimiters projectile powerline pkgbuild-mode origami monky moe-theme magit ido-yes-or-no ido-vertical-mode ido-sort-mtime ido-complete-space-or-hyphen idle-highlight-mode hlint-refactor helm-swoop haskell-snippets google-translate git-gutter flycheck-status-emoji flycheck-pos-tip flycheck-haskell flycheck-color-mode-line flx-ido fixmee ecb discover-my-major diminish company-cabal anzu)))
 '(scroll-bar-mode (quote right))
 '(scroll-step 1)
 '(setq-default indent-tabs-mode t)
 '(show-paren-mode t)
 '(size-indication-mode nil)
 '(tab-width 2)
 '(tags-revert-without-query t)
 '(tool-bar-mode nil)
 '(transient-mark-mode nil)
 '(uniquify-ask-about-buffer-names-p t)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(use-dialog-box nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "SlateBlue1" :slant italic))))
 '(idle-highlight ((t (:background "dim gray"))))
 '(powerline-active2 ((t (:background "grey40" :foreground "white smoke"))))
 '(tabbar-button ((t (:inherit tabbar-default))))
 '(tabbar-selected ((t (:background "#f2f2f6" :foreground "black" :box nil :weight bold :height 1.0))))
 '(tabbar-unselected ((t (:inherit tabbar-default :background "gray95" :foreground "grey30" :weight normal :height 1.2))))
 '(whitespace-line ((t (:background "gray20" :foreground "violet" :underline t)))))
